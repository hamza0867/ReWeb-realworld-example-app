module MakeRepository = (Database: Database.Connection) => {
  open Profile__Entity;
  open Database;

  let or_error = m => {
    switch%lwt (m) {
    | Ok(a) => Ok(a) |> Lwt.return
    | Error(e) => Error(Database_error(Caqti_error.show(e))) |> Lwt.return
    };
  };

  let pool = Database.pool;

  let create_table = [%rapper
    execute(
      {sql|
      CREATE TABLE IF NOT EXISTS follows (
        follower_id integer references users (id),
        followed_id integer references users (id)
      );
      |sql},
    )
  ];

  Caqti_lwt.Pool.use(create_table(), pool);

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
              @int{followed_id},
              @int{follower_id}
              FROM follows
              WHERE follower_id = (SELECT id from users WHERE username=%string{follower_username})
              AND followed_id = %int{followed_id}
              |sql},
                    function_out,
                  )
                ](
                  (~follower_id, ~followed_id) =>
                  true
                );
              let%lwt following_opt =
                Caqti_lwt.Pool.use(
                  find_following(~follower_username, ~followed_id=profile.id),
                  pool,
                )
                |> or_error;
              following_opt
              |> Result.map(Option.value(~default=false))
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
  /*let update_one = (unregistered: User__Entity.t) => {*/
  /*print_endline(*/
  /*"\n" ++ (unregistered.image |> Option.value(~default="null")),*/
  /*);*/
  /*let update_one_query = [%rapper*/
  /*execute(*/
  /*{sql|*/
    /*UPDATE users*/
    /*SET email = %string{email},*/
    /*username = %string{username},*/
    /*bio = %string{bio},*/
    /*password = %string{password},*/
    /*image = %string?{image}*/
    /*WHERE id = %int{id}  |sql},*/
  /*record_in,*/
  /*)*/
  /*];*/
  /*Caqti_lwt.Pool.use(update_one_query(unregistered), pool) |> or_error;*/
  /*};*/
};

module Repository = MakeRepository(Database.Connection);
