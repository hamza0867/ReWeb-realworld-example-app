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
    let author_name = List.Assoc.find(query, ~equal=String.equal, "author");
    let tag = List.Assoc.find(query, ~equal=String.equal, "tag");
    let favorited_by_username =
      List.Assoc.find(query, ~equal=String.equal, "favorited")
      |> Option.value(~default="");

    let limit =
      List.Assoc.find(query, ~equal=String.equal, "limit")
      |> Option.bind(~f=Caml.int_of_string_opt)
      |> Option.value(~default=20);

    let offset =
      List.Assoc.find(query, ~equal=String.equal, "offset")
      |> Option.bind(~f=Caml.int_of_string_opt)
      |> Option.value(~default=0);

    let%lwt articles_result =
      Article__Repository.Repository.get_many(
        ~author_name,
        ~tag,
        ~favorited_by_username,
        ~limit,
        ~offset,
      );

    switch (articles_result) {
    | Ok(articles) =>
      let json = [%yojson
        {
          articles: [%y
            `List(articles |> List.map(~f=Article__Model.to_yojson))
          ],
          articlesCount: [%y `Int(articles |> List.length)],
        }
      ];
      json |> Response.of_json(~status=`OK) |> Lwt.return;
    | Error(Database.Connection.Database_error(e)) =>
      prerr_endline("\n" ++ e);
      Response.of_status(`Internal_server_error) |> Lwt.return;
    };
  };
};

module Update = {
  [@deriving of_yojson]
  type payload = {
    [@default None]
    title: option(string),
    [@default None]
    description: option(string),
    [@default None]
    body: option(string),
  };
  let update = (_meth, slug) =>
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
              prerr_endline("\n " ++ e);
              Filters.makeErrorJson([e])
              |> Filters.errorJson_to_yojson
              |> Response.of_json(~status=`Code(422))
              |> Lwt.return;
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
                  let (>>=) = Lwt.(>>=);
                  Article__Repository.Repository.get_one_by_slug(
                    ~slug,
                    ~user_id=Some(user.id),
                    ~username=Some(user.username),
                  )
                  >>= (
                    fun
                    | Ok(Some(article)) => {
                        let new_title =
                          payload.title
                          |> Option.value(~default=article.title);
                        let new_slug =
                          payload.title
                          |> Option.map(~f=Util.slugify)
                          |> Option.value(~default=article.slug);

                        let new_description =
                          payload.description
                          |> Option.value(~default=article.description);

                        let new_body =
                          payload.body |> Option.value(~default=article.body);

                        let return_object = {
                          ...article,
                          title: new_title,
                          slug: new_slug,
                          description: new_description,
                          body: new_body,
                        };

                        Article__Repository.Repository.update_one(
                          ~original_slug=article.slug,
                          ~new_slug,
                          ~new_title,
                          ~new_body,
                          ~new_description,
                        )
                        >>= (
                          fun
                          | Ok () => {
                              [%yojson
                                {
                                  article: [%y
                                    return_object |> Article__Model.to_yojson
                                  ],
                                }
                              ]
                              |> Response.of_json(~status=`OK)
                              |> Lwt.return;
                            }
                          | Error(Database.Connection.Database_error(e)) => {
                              prerr_endline("\n" ++ e);
                              Response.of_status(`Internal_server_error)
                              |> Lwt.return;
                            }
                        );
                      }
                    | Ok(None) =>
                      Response.of_status(`Not_found) |> Lwt.return
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

module Destroy = {
  let destroy = slug => {
    Filters.bearer_auth @@
    (
      _req => {
        let (>>=) = Lwt.(>>=);
        Article__Repository.Repository.delete_one_by_slug(~slug)
        >>= (
          fun
          | Ok () => Response.of_status(`OK) |> Lwt.return

          | Error(Database.Connection.Database_error(e)) => {
              prerr_endline("\n" ++ e);
              Response.of_status(`Internal_server_error) |> Lwt.return;
            }
        );
      }
    );
  };
};

let resource =
  Server.resource(
    ~create=Create.create,
    ~index=Index.index,
    ~show=Show.show,
    ~update=Update.update,
    ~destroy=Destroy.destroy,
  );
