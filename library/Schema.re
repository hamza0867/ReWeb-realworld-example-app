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
        constraint follows_pkey primary key (follower_id, followed_id)
      );
      |sql},
  )
];

let create_table_articles = [%rapper
  execute(
    {sql|
      CREATE TABLE IF NOT EXISTS articles (
        id serial PRIMARY KEY,
        title VARCHAR(128) UNIQUE NOT NULL,
        slug VARCHAR(128) UNIQUE NOT NULL,
        description VARCHAR(128) NOT NULL,
        body VARCHAR NOT NULL,
        created_at VARCHAR(128),
        updated_at VARCHAR(128),
        author_id INTEGER REFERENCES users (id)
      );
      |sql},
  )
];

let create_table_user_favorites_article = [%rapper
  execute(
    {sql|
      CREATE TABLE IF NOT EXISTS user_favorites_article (
        user_id integer references users (id),
        article_id integer references articles (id),
        active boolean not null,
        constraint user_favorites_article_pkey primary key (user_id, article_id)
      )
      |sql},
  )
];

let create_table_tags = [%rapper
  execute(
    {sql|
      CREATE TABLE IF NOT EXISTS tags (
        tag VARCHAR(128) PRIMARY KEY
      );
      |sql},
  )
];

let create_table_article_tag = [%rapper
  execute(
    {sql|
      CREATE TABLE IF NOT EXISTS article_tag (
        article_id integer references articles (id),
        tag VARCHAR(128) references tags (tag),
        constraint article_tag_pkey primary key (article_id, tag)
      )
      |sql},
  )
];

let create_schema = () => {
  let pool = Database.Connection.pool;
  let (>>=) = Lwt_result.(>>=);
  Caqti_lwt.Pool.use(create_table_users(), pool)
  >>= (_ => Caqti_lwt.Pool.use(create_table_follows(), pool))
  >>= (_ => Caqti_lwt.Pool.use(create_table_articles(), pool))
  >>= (_ => Caqti_lwt.Pool.use(create_table_user_favorites_article(), pool))
  >>= (_ => Caqti_lwt.Pool.use(create_table_tags(), pool))
  >>= (_ => Caqti_lwt.Pool.use(create_table_article_tag(), pool))
  |> Database.Connection.or_error;
};
