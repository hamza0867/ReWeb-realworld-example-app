module MakeRepository = (Database: Database.Connection) => {
  open User__Entity;
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
      CREATE TABLE IF NOT EXISTS users (
        id serial PRIMARY KEY,
        email VARCHAR(128) UNIQUE NOT NULL,
        username VARCHAR(128) UNIQUE NOT NULL,
        bio VARCHAR,
        password VARCHAR,
        image VARCHAR
      );
      |sql},
    )
  ];

  Caqti_lwt.Pool.use(create_table(), pool);

  let create_one = (unregistered: create_entity) => {
    let create_one_query = [%rapper
      execute(
        {sql| INSERT INTO users(email, username, password)
          VALUES (%string{email}, %string{username}, %string{password})|sql},
        record_in,
      )
    ];

    Caqti_lwt.Pool.use(create_one_query(unregistered), pool) |> or_error;
  };

  let get_one_by_email = (~email) => {
    let get_one_query = [%rapper
      get_opt(
        {sql| SELECT
          @int{id},
          @string{email},
          @string{username},
          @string{bio},
          @string{password},
          @string?{image}
          FROM users WHERE email = %string{email}|sql},
        record_out,
      )
    ];
    Caqti_lwt.Pool.use(get_one_query(~email), pool) |> or_error;
  };
  let update_one = (unregistered: User__Entity.t) => {
    print_endline(
      "\n" ++ (unregistered.image |> Option.value(~default="null")),
    );
    let update_one_query = [%rapper
      execute(
        {sql|
        UPDATE users
        SET email = %string{email},
        username = %string{username},
        bio = %string{bio},
        password = %string{password},
        image = %string?{image}
        WHERE id = %int{id}  |sql},
        record_in,
      )
    ];
    Caqti_lwt.Pool.use(update_one_query(unregistered), pool) |> or_error;
  };
};

module Repository = MakeRepository(Database.Connection);
