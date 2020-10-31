let create_table_users = [%rapper
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

let create_table_follows = [%rapper
  execute(
    {sql|
      CREATE TABLE IF NOT EXISTS follows (
        follower_id integer references users (id),
        followed_id integer references users (id),
        active boolean not null,
        constraint pkey primary key (follower_id, followed_id)
      );
      |sql},
  )
];

let create_schema = () => {
  let pool = Database.Connection.pool;
  let (>>=) = Lwt_result.(>>=);
  Caqti_lwt.Pool.use(create_table_users(), pool)
  >>= (_ => Caqti_lwt.Pool.use(create_table_follows(), pool))
  |> Database.Connection.or_error;
};
