open ReWeb;
module D = Decoders_yojson.Safe.Decode;

module Login = {
  type userLoginPayload = {
    email: Validations.Email.t,
    password: string,
  };

  type loginPayload = {user: userLoginPayload};

  let makePayload = (email, password) => {
    user: {
      email,
      password,
    },
  };

  type loginReturn = User__Model.t;

  let payloadOfYojson = {
    open D;
    let decoder =
      at(["user", "email"], string)
      >>= (
        email => {
          switch (Validations.Email.fromString(email)) {
          | None => fail("Invalid email")
          | Some(email) =>
            at(["user", "password"], string)
            >>= (
              password => {
                succeed(makePayload(email, password));
              }
            )
          };
        }
      );
    decode_value(decoder);
  };

  let login = req => {
    let json_body: Yojson.Safe.t = Request.context(req);
    let loginPayloadResult = payloadOfYojson(json_body);
    switch (loginPayloadResult) {
    | Error(e) =>
      Response.of_json(
        ~status=`Code(422),
        Filters.makeErrorJson([e |> D.string_of_error])
        |> Filters.errorJson_to_yojson,
      )
      |> Lwt.return
    | Ok(loginPayload) =>
      let {user: {email, password}} = loginPayload;
      let (>>=) = Lwt.Infix.(>>=);
      User__Repository.Repository.get_one_by_email(
        ~email=email |> Validations.Email.toString,
      )
      >>= (
        fun
        | Ok(Some(user_entity)) =>
          if (Bcrypt.verify(
                password,
                user_entity.password |> Bcrypt.hash_of_string,
              )) {
            let token =
              Jose.Jwt.sign(
                ~header=Jose.Header.make_header(AppConfig.jwk),
                ~payload=[%yojson
                  {
                    username: [%y `String(user_entity.username)],
                    email: [%y `String(email |> Validations.Email.toString)],
                    bio: [%y `String(user_entity.bio)],
                    image:
                      switch%y (user_entity.image) {
                      | Some(str) => `String(str)
                      | None => `Null
                      },
                  }
                ],
                Jose.Jwk.make_oct("RealWorldApp"),
              );
            switch (token) {
            | Ok(token) =>
              let user =
                User__Model.make_from_entity(
                  ~username=user_entity.username,
                  ~token=token |> Jose.Jwt.to_string,
                  ~bio=user_entity.bio,
                  ~image=user_entity.image,
                  ~email=user_entity.email,
                );
              switch (user) {
              | Error(err) =>
                print_endline(err);
                Response.of_status(`Internal_server_error) |> Lwt.return;
              | Ok(user) =>
                Response.of_json(
                  ~status=`OK,
                  [%yojson {user: [%y user |> User__Model.to_yojson]}],
                )
                |> Lwt.return
              };
            | Error(`Msg(message)) =>
              prerr_endline(message);
              Response.of_status(`Internal_server_error) |> Lwt.return;
            };
          } else {
            [%yojson {
                       errors: {
                         password: "Invalid credentials",
                       },
                     }]
            |> Response.of_json(~status=`Unauthorized)
            |> Lwt.return;
          }
        | Ok(None) =>
          [%yojson {
                     errors: {
                       email: "There is no user with this email",
                     },
                   }]
          |> Response.of_json(~status=`Not_found)
          |> Lwt.return
        | Error(Database.Connection.Database_error(e)) => {
            prerr_endline(e);
            Response.of_status(`Internal_server_error) |> Lwt.return;
          }
      );
    };
  };
};

let server =
  fun
  | (meth, ["login"]) => Filters.body_json @@ Login.login
  | (meth, path) => User__Resource.resource @@ (meth, path);
