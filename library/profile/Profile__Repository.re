module MakeRepository = (Database: Database.Connection) => {
  open Profile__Entity;
  open Database;

  /*let create_one = (unregistered: create_entity) => {*/
  /*let create_one_query = [%rapper*/
  /*execute(*/
  /*{sql| INSERT INTO users(email, username, password)*/
    /*VALUES (%string{email}, %string{username}, %string{password})|sql},*/
  /*record_in,*/
  /*)*/
  /*];*/

  /*Caqti_lwt.Pool.use(create_one_query(unregistered), pool) |> or_error;*/
  /*};*/

  let get_one = (~follower_username, ~followed_username) => {
    let get_one_query = [%rapper
      get_opt(
        {sql| SELECT
        @int{id},
          @string{username},
          @string{bio},
          @string?{image}
          FROM users WHERE username = %string{followed_username}|sql},
        record_out,
      )
    ];
    let profile_opt =
      Caqti_lwt.Pool.use(get_one_query(~followed_username), pool) |> or_error;
    let (>>=) = Lwt_result.bind;
    profile_opt
    >>= (
      fun
      | None => Ok(None) |> Lwt.return
      | Some(profile) => {
          switch (follower_username) {
          | None =>
            Ok(
              Some({
                username: profile.username,
                bio: profile.bio,
                image: profile.image,
                following: false,
              }),
            )
            |> Lwt.return
          | Some(follower_username) =>
            let following = {
              let find_following =
                [%rapper
                  get_opt(
                    {sql| SELECT
              @bool{active}
              FROM follows
              WHERE follower_id = (SELECT id from users WHERE username=%string{follower_username})
              AND followed_id = %int{followed_id}
              |sql},
                    function_out,
                  )
                ](
                  (~active) =>
                  active
                );
              let%lwt following_opt =
                Caqti_lwt.Pool.use(
                  find_following(~follower_username, ~followed_id=profile.id),
                  pool,
                )
                |> or_error;
              following_opt
              |> Result.map(~f=Option.value(~default=false))
              |> Lwt.return;
            };
            following
            >>= (
              following =>
                Ok(
                  Some({
                    username: profile.username,
                    bio: profile.bio,
                    image: profile.image,
                    following,
                  }),
                )
                |> Lwt.return
            );
          };
        }
    );
  };

  let find_user_id_by_username = (~username) => {
    let find_follower_id_query = [%rapper
      get_opt(
        {sql|
      SELECT @int{id }FROM users WHERE username = %string{username}
      |sql},
      )
    ];
    Caqti_lwt.Pool.use(find_follower_id_query(~username), pool) |> or_error;
  };

  let follow_one = (~follower_id, ~followed_id) => {
    let follow_one_query = [%rapper
      execute(
        {sql|
        INSERT INTO follows ( follower_id, followed_id, active )
          VALUES (%int{follower_id}, %int{followed_id}, 't')
          ON CONFLICT ON CONSTRAINT follows_pkey DO
        UPDATE SET active = 't'
        |sql},
      )
    ];
    Caqti_lwt.Pool.use(follow_one_query(~follower_id, ~followed_id), pool)
    |> or_error;
  };

  let unfollow_one = (~follower_id, ~followed_id) => {
    let unfollow_one_query = [%rapper
      execute(
        {sql|
        INSERT INTO follows ( follower_id, followed_id, active )
          VALUES (%int{follower_id}, %int{followed_id}, 'f')
          ON CONFLICT ON CONSTRAINT follows_pkey DO
        UPDATE SET active = 'f'
        |sql},
      )
    ];
    Caqti_lwt.Pool.use(unfollow_one_query(~follower_id, ~followed_id), pool)
    |> or_error;
  };
};

module Repository = MakeRepository(Database.Connection);
