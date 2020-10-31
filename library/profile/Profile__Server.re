open ReWeb;
module D = Decoders_yojson.Safe.Decode;

type routes =
  | Get
  | Follow
  | Unfollow;

let return_by_follower_username = (~follower_username, ~followed_username) => {
  let%lwt profile =
    Profile__Repository.Repository.get_one(
      ~follower_username,
      ~followed_username,
    );
  switch (profile) {
  | Error(Database.Connection.Database_error(e)) =>
    prerr_endline("\n" ++ e);
    Response.of_status(`Internal_server_error) |> Lwt.return;
  | Ok(None) => Response.of_status(`Not_found) |> Lwt.return
  | Ok(Some(profile)) =>
    let valid_profile =
      Profile__Model.make_from_entity(
        ~username=profile.username,
        ~bio=profile.bio,
        ~image=profile.image,
        ~following=profile.following,
      );
    switch (valid_profile) {
    | Ok(profile) =>
      [%yojson {profile: [%y profile |> Profile__Model.to_yojson]}]
      |> Response.of_json(~status=`OK)
      |> Lwt.return
    | Error(e) =>
      prerr_endline("\n" ++ e);
      Response.of_status(`Internal_server_error) |> Lwt.return;
    };
  };
};

let server = username =>
  fun
  | Get =>
    Filters.bearer_auth_optional @@
    (
      req => {
        let jwt_opt = (req |> Request.context)#token;
        switch (jwt_opt) {
        | None =>
          return_by_follower_username(
            ~follower_username=None,
            ~followed_username=username,
          )
        | Some(jwt) =>
          let follower_username =
            jwt.payload
            |> Yojson.Safe.Util.member("username")
            |> Yojson.Safe.Util.to_string_option;
          switch (follower_username) {
          | None => Filters.unauthorized
          | Some(follower_username) =>
            return_by_follower_username(
              ~follower_username=Some(follower_username),
              ~followed_username=username,
            )
          };
        };
      }
    )
  | Follow =>
    Filters.bearer_auth @@
    (
      req => {
        let jwt = (req |> Request.context)#token;
        let follower_username =
          jwt.payload
          |> Yojson.Safe.Util.member("username")
          |> Yojson.Safe.Util.to_string_option;
        switch (follower_username) {
        | None => Filters.unauthorized
        | Some(follower_username) =>
          let%lwt result_follower_id =
            Profile__Repository.Repository.find_user_id_by_username(
              ~username=follower_username,
            );
          switch (result_follower_id) {
          | Error(Database.Connection.Database_error(e)) =>
            prerr_endline("\n" ++ e);
            Response.of_status(`Internal_server_error) |> Lwt.return;
          | Ok(None) => Filters.unauthorized
          | Ok(Some(follower_id)) =>
            let%lwt followed_id =
              Profile__Repository.Repository.find_user_id_by_username(
                ~username,
              );
            switch (followed_id) {
            | Error(Database.Connection.Database_error(e)) =>
              prerr_endline("\n" ++ e);
              Response.of_status(`Internal_server_error) |> Lwt.return;
            | Ok(None) => Response.of_status(`Not_found) |> Lwt.return
            | Ok(Some(followed_id)) =>
              let%lwt follow =
                Profile__Repository.Repository.follow_one(
                  ~follower_id,
                  ~followed_id,
                );
              switch (follow) {
              | Error(Database.Connection.Database_error(e)) =>
                prerr_endline("\n" ++ e);
                Response.of_status(`Internal_server_error) |> Lwt.return;
              | Ok () =>
                return_by_follower_username(
                  ~follower_username=Some(follower_username),
                  ~followed_username=username,
                )
              };
            };
          };
        };
      }
    )
  | Unfollow =>
    Filters.bearer_auth @@
    (
      req => {
        let jwt = (req |> Request.context)#token;
        let follower_username =
          jwt.payload
          |> Yojson.Safe.Util.member("username")
          |> Yojson.Safe.Util.to_string_option;
        switch (follower_username) {
        | None => Filters.unauthorized
        | Some(follower_username) =>
          let%lwt result_follower_id =
            Profile__Repository.Repository.find_user_id_by_username(
              ~username=follower_username,
            );
          switch (result_follower_id) {
          | Error(Database.Connection.Database_error(e)) =>
            prerr_endline("\n" ++ e);
            Response.of_status(`Internal_server_error) |> Lwt.return;
          | Ok(None) => Filters.unauthorized
          | Ok(Some(follower_id)) =>
            let%lwt followed_id =
              Profile__Repository.Repository.find_user_id_by_username(
                ~username,
              );
            switch (followed_id) {
            | Error(Database.Connection.Database_error(e)) =>
              prerr_endline("\n" ++ e);
              Response.of_status(`Internal_server_error) |> Lwt.return;
            | Ok(None) => Response.of_status(`Not_found) |> Lwt.return
            | Ok(Some(followed_id)) =>
              let%lwt follow =
                Profile__Repository.Repository.unfollow_one(
                  ~follower_id,
                  ~followed_id,
                );
              switch (follow) {
              | Error(Database.Connection.Database_error(e)) =>
                prerr_endline("\n" ++ e);
                Response.of_status(`Internal_server_error) |> Lwt.return;
              | Ok () =>
                return_by_follower_username(
                  ~follower_username=Some(follower_username),
                  ~followed_username=username,
                )
              };
            };
          };
        };
      }
    );
