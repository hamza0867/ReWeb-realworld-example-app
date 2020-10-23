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
    let json_body = Request.context(req);
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
                AppConfig.jwk,
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

module Update = {
  [@deriving of_yojson]
  type payload = {
    [@default None]
    email: option(Validations.Email.t),
    [@default None]
    username: option(string),
    [@default None]
    password: option(string),
    [@default None]
    image: option(option(Validations.Url.t)),
    [@default None]
    bio: option(string),
  };

  let handler =
    Filters.body_json @@
    Filters.bearer_auth @@
    (
      req => {
        let payload_json: Yojson.Safe.t = Request.context(req)#prev;
        let user_json: Yojson.Safe.t = Request.context(req)#token.payload;
        switch (
          payload_of_yojson(payload_json |> Yojson.Safe.Util.member("user")),
          user_json |> Yojson.Safe.Util.member("email"),
        ) {
        | (Error(e), _) =>
          prerr_endline("\n" ++ e);
          Response.of_json(
            ~status=`Code(422),
            [%yojson {
                       errors: {
                         body: [[%y `String(e)]],
                       },
                     }],
          )
          |> Lwt.return;
        | (Ok(payload), `String(email)) =>
          let maybe_user =
            User__Repository.Repository.get_one_by_email(~email);
          switch%lwt (maybe_user) {
          | Error(Database.Connection.Database_error(e)) =>
            prerr_endline(e);
            Response.of_status(`Internal_server_error) |> Lwt.return;
          | Ok(None) =>
            Response.of_json(
              ~status=`Unauthorized,
              [%yojson {
                         errors: {
                           token: {
                             invalid: "Invalid token",
                           },
                         },
                       }],
            )
            |> Lwt.return
          | Ok(Some(user)) =>
            let (<||>) = (opt, default) => Option.value(opt, ~default);
            let (>>) = (f, g, x) => g(f(x));
            let updated_user: User__Entity.t = {
              id: user.id,
              email:
                payload.email
                |> Option.map(Validations.Email.toString)
                <||> user.email,
              username: payload.username <||> user.username,
              bio: payload.bio <||> user.bio,
              image:
                switch (payload.image) {
                | None => user.image
                | Some(url_opt) =>
                  url_opt |> Option.map(Validations.Url.toString)
                },

              password:
                payload.password
                |> Option.map(Bcrypt.hash >> Bcrypt.string_of_hash)
                <||> user.password,
            };
            let (>>=) = Lwt.(>>=);
            User__Repository.Repository.update_one(updated_user)
            >>= (
              fun
              | Error(Database.Connection.Database_error(e)) => {
                  prerr_endline("\n" ++ e);
                  Response.of_status(`Internal_server_error) |> Lwt.return;
                }
              | Ok () => {
                  let token =
                    Jose.Jwt.sign(
                      ~header=Jose.Header.make_header(AppConfig.jwk),
                      ~payload=[%yojson
                        {
                          username: [%y `String(updated_user.username)],
                          email: [%y `String(updated_user.email)],
                          bio: [%y `String(updated_user.bio)],
                          image:
                            switch%y (updated_user.image) {
                            | Some(str) => `String(str)
                            | None => `Null
                            },
                        }
                      ],
                      AppConfig.jwk,
                    );
                  switch (token) {
                  | Error(`Msg(e)) =>
                    prerr_endline("\n" ++ e);
                    Response.of_status(`Internal_server_error) |> Lwt.return;
                  | Error(_) =>
                    Response.of_status(`Internal_server_error) |> Lwt.return
                  | Ok(jwt) =>
                    let token = jwt |> Jose.Jwt.to_string;
                    let return_payload = [%yojson
                      {
                        user: {
                          email: [%y `String(updated_user.email)],
                          username: [%y `String(updated_user.username)],
                          bio: [%y `String(updated_user.bio)],
                          image: [%y
                            updated_user.image
                            |> Option.map(x => `String(x))
                            <||> `Null
                          ],
                          token: [%y `String(token)],
                        },
                      }
                    ];
                    Response.of_json(~status=`OK, return_payload)
                    |> Lwt.return;
                  };
                }
            );
          };
        | (_, _) =>
          Response.of_json(
            ~status=`Unauthorized,
            [%yojson {
                       errors: {
                         token: {
                           invalid: "Invalid token",
                         },
                       },
                     }],
          )
          |> Lwt.return
        };
      }
    );
};

let server =
  fun
  | (meth, ["login"]) => Filters.body_json @@ Login.login
  | (`PUT, []) => Update.handler
  | (meth, path) => User__Resource.resource @@ (meth, path);
