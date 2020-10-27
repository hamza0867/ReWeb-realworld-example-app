open ReWeb;
module D = Decoders_yojson.Safe.Decode;

type routes =
  | Get
  | Follow
  | Unfollow;

let server = username =>
  fun
  | Get =>
    Filters.bearer_auth_optional @@
    (
      req => {
        let jwt_opt = (req |> Request.context)#token;
        let return_by_follower_username = follower_username => {
          let%lwt profile =
            Profile__Repository.Repository.get_one(
              ~follower_username,
              ~followed_username=username,
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
        switch (jwt_opt) {
        | None => return_by_follower_username(None)
        | Some(jwt) =>
          let follower_username =
            jwt.payload
            |> Yojson.Safe.Util.member("username")
            |> Yojson.Safe.Util.to_string_option;
          switch (follower_username) {
          | None => Filters.unauthorized
          | Some(follower_usernameusername) =>
            return_by_follower_username(follower_username)
          };
        };
      }
    )
  | Follow => (_req => Response.of_status(`OK) |> Lwt.return)
  | Unfollow => (_req => Response.of_status(`OK) |> Lwt.return);
