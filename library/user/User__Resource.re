open ReWeb;
module D = Decoders_yojson.Safe.Decode;
module Repository = User__Repository.MakeRepository(Database.Connection);

module Create = {
  type payload = {
    username: string,
    email: Validations.Email.t,
    password: string,
  };

  let deocdeEmail = {
    open D;
    let decoder: D.decoder(Validations.Email.t) =
      string
      >>= (
        email => {
          switch (Validations.Email.fromString(email)) {
          | Some(s) => succeed(s)
          | None => fail("Invalid email")
          };
        }
      );
    decoder;
  };

  let make_payload = (username, email, password) => {
    username,
    email,
    password,
  };

  type return = User__Model.t;

  let payloadOfYojson = {
    open D;
    let (<$>) = D.map;
    make_payload
    <$> at(["user", "username"], string)
    <*> at(["user", "email"], deocdeEmail)
    <*> at(["user", "password"], string)
    |> decode_value;
  };

  let make_return = (~username, ~email, ~token): return => {
    username,
    email,
    bio: "",
    token,
    image: None,
  };

  let handler =
    Filters.body_json @@
    (
      req => {
        let payload_json: Yojson.Safe.t = Request.context(req);
        switch (payloadOfYojson(payload_json)) {
        | Error(e) =>
          Response.of_json(
            ~status=`Code(422),
            Filters.makeErrorJson([e |> D.string_of_error])
            |> Filters.errorJson_to_yojson,
          )
          |> Lwt.return
        | Ok(payload) =>
          let (>>=) = Lwt.Infix.(>>=);
          Repository.create_one({
            username: payload.username,
            password: payload.password |> Bcrypt.hash |> Bcrypt.string_of_hash,
            email: payload.email |> Validations.Email.toString,
          })
          >>= (
            fun
            | Ok () => {
                let token =
                  Jose.Jwt.sign(
                    ~header=Jose.Header.make_header(AppConfig.jwk),
                    ~payload=[%yojson
                      {
                        username: [%y `String(payload.username)],
                        email: [%y
                          `String(payload.email |> Validations.Email.toString)
                        ],
                      }
                    ],
                    AppConfig.jwk,
                  );
                switch (token) {
                | Ok(token) =>
                  Response.of_json(
                    ~status=`OK,
                    [%yojson
                      {
                        user: [%y
                          make_return(
                            ~username=payload.username,
                            ~email=payload.email,
                            ~token=token |> Jose.Jwt.to_string,
                          )
                          |> User__Model.to_yojson
                        ],
                      }
                    ],
                  )
                  |> Lwt.return
                | Error(`Msg(message)) =>
                  print_endline(message);
                  Response.of_status(`Internal_server_error) |> Lwt.return;
                };
              }
            | Error(Database.Connection.Database_error(e)) => {
                print_endline(e);
                Response.of_status(`Internal_server_error) |> Lwt.return;
              }
          );
        };
      }
    );
};

let create = Create.handler;

let index =
  Filters.bearer_auth @@
  (
    req => {
      let context = Request.context(req);
      let token = context;
      let (>>=) = Result.bind;
      let jwt =
        token |> Jose.Jwt.of_string >>= Jose.Jwt.validate(~jwk=AppConfig.jwk);
      switch (jwt) {
      | Ok(jwt) =>
        let user = jwt.payload;
        let user_model = User__Model.make_from_token(user, token);
        switch (user_model) {
        | Ok(user) =>
          Response.of_json(
            ~status=`OK,
            [%yojson {user: [%y user |> User__Model.to_yojson]}],
          )
          |> Lwt.return
        | Error(e) =>
          prerr_endline(e);
          Response.of_status(`Internal_server_error) |> Lwt.return;
        };
      | Error(`Expired) =>
        Response.of_json(
          ~status=`Unauthorized,
          [%yojson {
                     errors: {
                       authentication: {
                         expired: "expired token",
                       },
                     },
                   }],
        )
        |> Lwt.return
      | Error(`Invalid_signature) =>
        Response.of_json(
          ~status=`Unauthorized,
          [%yojson {
                     errors: {
                       authentication: {
                         invalid: "invalid token",
                       },
                     },
                   }],
        )
        |> Lwt.return
      | Error(`Msg(msg)) =>
        prerr_endline(msg);
        Response.of_json(
          ~status=`Unauthorized,
          [%yojson {
                     errors: {
                       authentication: {
                         invalid: "invalid token",
                       },
                     },
                   }],
        )
        |> Lwt.return;
      };
    }
  );

let resource = Server.resource(~create, ~index);
