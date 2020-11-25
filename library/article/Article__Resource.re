open ReWeb;
module D = Decoders_yojson.Safe.Decode;
module Repository = Article__Repository.MakeRepository(Database.Connection);

module Create = {
  [@deriving of_yojson]
  type payload = {
    title: string,
    description: string,
    body: string,
    [@default []]
    tagList: list(string),
  };
  let create =
    Filters.body_json @@
    Filters.bearer_auth @@
    (
      req => {
        let email =
          Request.context(req)#token.payload
          |> Yojson.Safe.Util.member("email");
        switch (email) {
        | `String(email) =>
          let%lwt user_opt = User.Repository.get_one_by_email(~email);
          switch (user_opt) {
          | Error(Database.Connection.Database_error(e)) =>
            prerr_endline("\n" ++ e);
            Response.of_status(`Internal_server_error) |> Lwt.return;
          | Ok(None) => Filters.unauthorized
          | Ok(Some(user)) =>
            let payload =
              Request.context(req)#prev
              |> Yojson.Safe.Util.member("article")
              |> payload_of_yojson;
            switch (payload) {
            | Error(e) =>
              Filters.makeErrorJson([e])
              |> Filters.errorJson_to_yojson
              |> Response.of_json(~status=`Code(422))
              |> Lwt.return
            | Ok(payload) =>
              let%lwt author =
                Profile__Repository.Repository.get_one(
                  ~follower_username=Some(user.username),
                  ~followed_username=user.username,
                );
              switch (author) {
              | Error(Database.Connection.Database_error(e)) =>
                prerr_endline("\n" ++ e);
                Response.of_status(`Internal_server_error) |> Lwt.return;
              | Ok(None) => Filters.unauthorized
              | Ok(Some(author)) =>
                let author =
                  Profile.Model.make_from_entity(
                    ~username=author.username,
                    ~bio=author.bio,
                    ~image=author.image,
                    ~following=author.following,
                  );
                switch (author) {
                | Error(e) =>
                  prerr_endline("\n" ++ e);
                  Response.of_status(`Internal_server_error) |> Lwt.return;
                | Ok(author) =>
                  let return_object =
                    Article__Entity.t_of_create_entity(
                      ~create_entity={
                        title: payload.title,
                        description: payload.description,
                        body: payload.body,
                        tagList: payload.tagList,
                      },
                      ~author,
                    );
                  let (>>=) = Lwt.(>>=);
                  Article__Repository.Repository.create_one(
                    ~unregistered={
                      title: payload.title,
                      description: payload.description,
                      body: payload.body,
                      tagList: payload.tagList,
                    },
                    ~author_id=user.id,
                  )
                  >>= (
                    fun
                    | Ok () =>
                      [%yojson
                        {
                          article: [%y
                            return_object |> Article__Model.to_yojson
                          ],
                        }
                      ]
                      |> Response.of_json(~status=`OK)
                      |> Lwt.return
                    | Error(Database.Connection.Database_error(e)) => {
                        prerr_endline("\n" ++ e);
                        Response.of_status(`Internal_server_error)
                        |> Lwt.return;
                      }
                  );
                };
              };
            };
          };
        | _ => Response.of_status(`Unauthorized) |> Lwt.return
        };
      }
    );
};

module Show = {
  let show = slug =>
    Filters.bearer_auth_optional @@
    (
      req => {
        let jwt_opt = Request.context(req)#token;
        switch (jwt_opt) {
        | Some(jwt) =>
          let username_opt =
            jwt.payload |> Yojson.Safe.Util.member("username");
          switch (username_opt) {
          | `String(username) =>
            let%lwt user_id_opt_result =
              Profile.Repository.find_user_id_by_username(~username);
            switch (user_id_opt_result) {
            | Error(Database.Connection.Database_error(e)) =>
              prerr_endline("\n" ++ e);
              Response.of_status(`Internal_server_error) |> Lwt.return;
            | Ok(user_id_opt) =>
              switch (user_id_opt) {
              | None => Filters.unauthorized
              | Some(user_id) =>
                let%lwt article_opt_result =
                  Article__Repository.Repository.get_one_by_slug(
                    ~slug,
                    ~username=Some(username),
                    ~user_id=Some(user_id),
                  );
                switch (article_opt_result) {
                | Error(Database.Connection.Database_error(e)) =>
                  prerr_endline("\n" ++ e);
                  Response.of_status(`Internal_server_error) |> Lwt.return;
                | Ok(article_opt) =>
                  switch (article_opt) {
                  | None => Response.of_status(`Not_found) |> Lwt.return
                  | Some(article) =>
                    [%yojson
                      {article: [%y article |> Article__Model.to_yojson]}
                    ]
                    |> Response.of_json(~status=`OK)
                    |> Lwt.return
                  }
                };
              }
            };
          | _ => Filters.unauthorized
          };
        | None =>
          let%lwt article_opt_result =
            Article__Repository.Repository.get_one_by_slug(
              ~slug,
              ~username=None,
              ~user_id=None,
            );
          switch (article_opt_result) {
          | Error(Database.Connection.Database_error(e)) =>
            prerr_endline("\n" ++ e);
            Response.of_status(`Internal_server_error) |> Lwt.return;
          | Ok(article_opt) =>
            switch (article_opt) {
            | None => Response.of_status(`Not_found) |> Lwt.return
            | Some(article) =>
              [%yojson {article: [%y article |> Article__Model.to_yojson]}]
              |> Response.of_json(~status=`OK)
              |> Lwt.return
            }
          };
        };
      }
    );
};

module Index = {
  let index = req => {
    let query =
      Request.query(req)
      |> String.split(~on='&')
      |> List.fold(~init=[], ~f=(acc, str) =>
           str
           |> String.split(~on='=')
           |> (
             fun
             | [fst, snd] => List.concat([acc, [(fst, snd)]])
             | _ => acc
           )
         );

    Response.of_status(`OK) |> Lwt.return;
  };
};

let resource =
  Server.resource(~create=Create.create, ~index=Index.index, ~show=Show.show);
