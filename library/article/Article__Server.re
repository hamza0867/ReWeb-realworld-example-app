open ReWeb;
module D = Decoders_yojson.Safe.Decode;

let favoriteArticleService = slug =>
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
          let article_promise =
            Article__Repository.Repository.get_one_by_slug(
              ~slug,
              ~user_id=Some(user.id),
              ~username=Some(user.username),
            );
          switch%lwt (article_promise) {
          | Ok(None) => Response.of_status(`Not_found) |> Lwt.return
          | Ok(Some(article)) =>
            if (!article.favorited) {
              let favorite_promise =
                Article__Repository.Repository.favorite_one_by_slug(
                  ~slug,
                  ~user_id=user.id,
                );
              switch%lwt (favorite_promise) {
              | Ok () =>
                [%yojson
                  {
                    article: [%y
                      {
                        ...article,
                        favorited: true,
                        favoritesCount: article.favoritesCount + 1,
                      }
                      |> Article__Model.to_yojson
                    ],
                  }
                ]
                |> Response.of_json(~status=`OK)
                |> Lwt.return
              | Error(Database.Connection.Database_error(e)) =>
                prerr_endline("\n" ++ e);
                Response.of_status(`Internal_server_error) |> Lwt.return;
              };
            } else {
              [%yojson {article: [%y article |> Article__Model.to_yojson]}]
              |> Response.of_json(~status=`OK)
              |> Lwt.return;
            }
          | Error(Database.Connection.Database_error(e)) =>
            prerr_endline("\n" ++ e);
            Response.of_status(`Internal_server_error) |> Lwt.return;
          };
        };
      | _ => Response.of_status(`Unauthorized) |> Lwt.return
      };
    }
  );

let unfavoriteArticleService = slug =>
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
          let article_promise =
            Article__Repository.Repository.get_one_by_slug(
              ~slug,
              ~user_id=Some(user.id),
              ~username=Some(user.username),
            );
          switch%lwt (article_promise) {
          | Ok(None) => Response.of_status(`Not_found) |> Lwt.return
          | Ok(Some(article)) =>
            if (article.favorited) {
              let favorite_promise =
                Article__Repository.Repository.unfavorite_one_by_slug(
                  ~slug,
                  ~user_id=user.id,
                );
              switch%lwt (favorite_promise) {
              | Ok () =>
                [%yojson
                  {
                    article: [%y
                      {
                        ...article,
                        favorited: false,
                        favoritesCount: article.favoritesCount - 1,
                      }
                      |> Article__Model.to_yojson
                    ],
                  }
                ]
                |> Response.of_json(~status=`OK)
                |> Lwt.return
              | Error(Database.Connection.Database_error(e)) =>
                prerr_endline("\n" ++ e);
                Response.of_status(`Internal_server_error) |> Lwt.return;
              };
            } else {
              [%yojson {article: [%y article |> Article__Model.to_yojson]}]
              |> Response.of_json(~status=`OK)
              |> Lwt.return;
            }
          | Error(Database.Connection.Database_error(e)) =>
            prerr_endline("\n" ++ e);
            Response.of_status(`Internal_server_error) |> Lwt.return;
          };
        };
      | _ => Response.of_status(`Unauthorized) |> Lwt.return
      };
    }
  );

let server =
  fun
  | (`POST, [slug, "favorite"]) => favoriteArticleService(slug)
  | (`DELETE, [slug, "favorite"]) => unfavoriteArticleService(slug)
  | (meth, path) => Article__Resource.resource @@ (meth, path);
