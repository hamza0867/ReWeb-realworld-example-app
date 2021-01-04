module MakeRepository = (Database: Database.Connection) => {
  open Article__Entity;
  open Database;

  let create_one = (~unregistered: create_entity, ~author_id) => {
    let create_one_query = [%rapper
      get_one(
        {sql|
        INSERT INTO articles(title, slug, description, body, created_at, updated_at, author_id)
    VALUES (%string{title},
            %string{slug},
            %string{description},
            %string{body},
            %string{created_at},
            %string{updated_at},
            %int{author_id}
            )
    RETURNING @int{id}
    ;
    |sql},
      )
    ];

    let create_tag_query = [%rapper
      execute(
        {sql|
        INSERT INTO tags(tag)
          VALUES (%string{tag})
          ON CONFLICT (tag) DO NOTHING;
        |sql},
      )
    ];

    let link_tag_article = [%rapper
      execute(
        {sql|
      INSERT INTO article_tag(article_id, tag)
      VALUES (%int{article_id}, %string{tag});
      |sql},
      )
    ];

    let%lwt (module Connection) = connection;

    let call_db = f => f((module Connection): (module Caqti_lwt.CONNECTION));

    let (>>=) = Lwt_result.(>>=);

    let%lwt result =
      Connection.start()
      >>= (
        () => {
          unregistered.tagList
          |> List.fold(~init=Lwt.return_ok(), ~f=(acc, tag) => {
               switch%lwt (acc) {
               | Error(e) => acc
               | Ok () => call_db(create_tag_query(~tag))
               }
             });
        }
      )
      >>= (
        () =>
          call_db(
            create_one_query(
              ~title=unregistered.title,
              ~slug=unregistered.title |> Util.slugify,
              ~description=unregistered.description,
              ~body=unregistered.body,
              ~created_at=
                CalendarLib.Calendar.now() |> Article__Model.time_to_string,
              ~updated_at=
                CalendarLib.Calendar.now() |> Article__Model.time_to_string,
              ~author_id,
            ),
          )
      )
      >>= (
        article_id => {
          unregistered.tagList
          |> List.fold(~init=Lwt.return_ok(), ~f=(acc, tag) =>
               switch%lwt (acc) {
               | Error(e) => acc
               | Ok () => call_db(link_tag_article(~article_id, ~tag))
               }
             );
        }
      )
      >>= (() => Connection.commit())
      |> or_error;

    switch (result) {
    | Ok () => Lwt.return_ok()
    | Error(Database_error(e)) =>
      prerr_endline("\n" ++ e);
      Connection.rollback() |> or_error;
    };
  };

  let get_one_by_slug = (~slug, ~user_id, ~username) => {
    let get_one_query = [%rapper
      get_opt(
        {sql| SELECT
        @int{id},
        @string{slug},
        @string{title},
        @string{description},
        @string{body},
        @string{created_at},
        @string{updated_at},
        @int{author_id}
        FROM articles WHERE slug = %string{slug}
        |sql},
        record_out,
      )
    ];

    let get_tag_list_by_article_id = [%rapper
      get_many(
        {sql|
        SELECT @string{tags.tag} FROM tags JOIN article_tag
        ON article_tag.article_id = %int{article_id}
        |sql},
      )
    ];

    let (>>=) = Lwt_result.(>>=);

    let get_favorited_by_article_id_and_user_id = [%rapper
      get_opt(
        {sql|
      SELECT @bool{active} FROM user_favorites_article
      WHERE user_id = %int{user_id} AND article_id = %int{article_id}
      |sql},
      )
    ];

    let get_favoritesCount_by_article_id = [%rapper
      get_one(
        {sql|
    SELECT COUNT(active) @int{favoritesCount} FROM user_favorites_article
    WHERE article_id = %int{article_id}
    |sql},
      )
    ];

    let find_username_by_user_id = (~user_id) => {
      let find_follower_id_query = [%rapper
        get_opt(
          {sql|
      SELECT @string{username} FROM users WHERE id = %int{user_id}
      |sql},
        )
      ];
      Caqti_lwt.Pool.use(find_follower_id_query(~user_id), pool) |> or_error;
    };

    Caqti_lwt.Pool.use(get_one_query(~slug), pool)
    |> or_error
    >>= (
      entity_opt =>
        switch (entity_opt) {
        | None => Lwt.return(Ok(None))
        | Some(entity) =>
          let article_id = entity.id;
          let tagListPromise =
            Caqti_lwt.Pool.use(get_tag_list_by_article_id(~article_id), pool)
            |> or_error;
          let favoritesCountPromise =
            Caqti_lwt.Pool.use(
              get_favoritesCount_by_article_id(~article_id),
              pool,
            )
            |> or_error;
          let%lwt favorited =
            switch (user_id) {
            | None => Lwt.return(Ok(false))
            | Some(user_id) =>
              Caqti_lwt.Pool.use(
                get_favorited_by_article_id_and_user_id(
                  ~user_id,
                  ~article_id,
                ),
                pool,
              )
              |> or_error
              >>= (
                fun
                | None => Lwt.return(Ok(false))
                | Some(favorited) => Lwt.return(Ok(favorited))
              )
            };
          let%lwt (tagList_result, favoritesCount_result) =
            Lwt.both(tagListPromise, favoritesCountPromise);
          switch (tagList_result, favoritesCount_result) {
          | (Error(e), _)
          | (_, Error(e)) => Error(e) |> Lwt.return
          | (Ok(tagList), Ok(favoritesCount)) =>
            switch (favorited) {
            | Error(e) => Lwt.return(Error(e))
            | Ok(favorited) =>
              find_username_by_user_id(~user_id=entity.author_id)
              >>= (
                author_name_opt => {
                  switch (author_name_opt) {
                  | None => Lwt.return(Ok(None))
                  | Some(author_name) =>
                    module ProfileRepository =
                      Profile__Repository.MakeRepository(Database);
                    ProfileRepository.get_one(
                      ~follower_username=username,
                      ~followed_username=author_name,
                    )
                    >>= (
                      author_opt => {
                        switch (author_opt) {
                        | None => Lwt.return(Ok(None))
                        | Some(author) =>
                          let authorModel =
                            Profile.Model.make_from_entity(
                              ~username=author.username,
                              ~bio=author.bio,
                              ~image=author.image,
                              ~following=author.following,
                            );
                          switch (authorModel) {
                          | Error(e) =>
                            prerr_endline("\n" ++ e);
                            Lwt.return(Error(Database_error(e)));
                          | Ok(author) =>
                            let return_object: Article__Model.t = {
                              slug: entity.slug,
                              title: entity.title,
                              description: entity.description,
                              body: entity.body,
                              tagList,
                              createdAt:
                                entity.created_at
                                |> Article__Model.time_of_string,
                              updatedAt:
                                entity.updated_at
                                |> Article__Model.time_of_string,
                              favorited,
                              favoritesCount,
                              author,
                            };
                            Ok(Some(return_object)) |> Lwt.return;
                          };
                        };
                      }
                    );
                  };
                }
              )
            }
          };
        }
    );
  };

  let get_many = (~tag, ~author_name, ~favorited_by_username, ~limit, ~offset) => {
    let (>>=) = Lwt_result.(>>=);
    let articles_promise =
      (
        switch (tag) {
        | Some(tag) =>
          let get_article_ids_by_tag = [%rapper
            get_many(
              {sql|
      SELECT @int{article_id} FROM article_tag WHERE tag = %string{tag}
      |sql},
            )
          ];
          let all_articles_by_ids_query = [%rapper
            get_many(
              {sql| SELECT
            @int{articles.id},
            @string{articles.slug},
            @string{articles.title},
            @string{articles.description},
            @string{articles.body},
            @string{articles.created_at},
            @string{articles.updated_at},
            @bool{coalesce(user_favorites_article.active, 'f') AS favorited},
            COUNT(user_favorites_article.active) @int{favoritesCount},
            @string{users.username},
            @string{users.bio},
            @string?{users.image},
            @bool{coalesce(follows.active, 'f') AS following}
            FROM articles
            LEFT JOIN user_favorites_article ON (user_favorites_article.article_id = articles.id AND user_favorites_article.user_id = (SELECT users.id FROM users WHERE users.username = %string{favorited_by_username}) )
            JOIN users ON users.id = articles.author_id AND users.username = coalesce(%string?{author_name}, users.username)
            LEFT JOIN follows ON (follows.followed_id = articles.author_id AND follows.follower_id = (SELECT users.id FROM users WHERE users.username = %string{favorited_by_username}))
            WHERE articles.id IN (%list{%int{article_ids}})
            GROUP BY articles.id, user_favorites_article.active, users.id, follows.active
            ORDER BY articles.created_at
            LIMIT %int{limit} OFFSET %int{offset}
              |sql},
            )
          ];
          Caqti_lwt.Pool.use(get_article_ids_by_tag(~tag), Database.pool)
          >>= (
            article_ids => {
              switch (article_ids) {
              | [] => Lwt.return_ok([])
              | [x, ...y] =>
                Caqti_lwt.Pool.use(
                  all_articles_by_ids_query(
                    ~article_ids,
                    ~author_name,
                    ~favorited_by_username,
                    ~limit,
                    ~offset,
                  ),
                  Database.pool,
                )
              };
            }
          );
        | None =>
          let all_articles_query = [%rapper
            get_many(
              {sql| SELECT
            @int{articles.id},
            @string{articles.slug},
            @string{articles.title},
            @string{articles.description},
            @string{articles.body},
            @string{articles.created_at},
            @string{articles.updated_at},
            @bool{coalesce(user_favorites_article.active, 'f') AS favorited},
            COUNT(user_favorites_article.active) @int{favoritesCount},
            @string{users.username},
            @string{users.bio},
            @string?{users.image},
            @bool{coalesce(follows.active, 'f') AS following}
            FROM articles
            LEFT JOIN user_favorites_article ON (user_favorites_article.article_id = articles.id AND user_favorites_article.user_id = (SELECT users.id FROM users WHERE users.username = %string{favorited_by_username}) )
            JOIN users ON users.id = articles.author_id AND users.username = coalesce(%string?{author_name}, users.username)
            LEFT JOIN follows ON (follows.followed_id = articles.author_id AND follows.follower_id = (SELECT users.id FROM users WHERE users.username = %string{favorited_by_username}))
            GROUP BY articles.id, user_favorites_article.active, users.id, follows.active
            ORDER BY articles.created_at
            LIMIT %int{limit} OFFSET %int{offset}
              |sql},
            )
          ];
          Caqti_lwt.Pool.use(
            all_articles_query(
              ~limit,
              ~offset,
              ~author_name,
              ~favorited_by_username,
            ),
            Database.pool,
          );
        }
      )
      |> or_error;

    let get_tag_list_by_article_id = [%rapper
      get_many(
        {sql|
        SELECT @string{tags.tag} FROM tags JOIN article_tag
        ON article_tag.article_id = %int{article_id}
        |sql},
      )
    ];

    articles_promise
    >>= (
      articles => {
        articles
        |> List.fold(
             ~init=Lwt.return_ok([]),
             ~f=(
                  articles_list_result,
                  (
                    article_id,
                    slug,
                    title,
                    description,
                    body,
                    created_at,
                    updated_at,
                    favorited,
                    favoritesCount,
                    author_name,
                    author_bio,
                    author_image,
                    author_following,
                  ),
                ) => {
             switch%lwt (articles_list_result) {
             | Error(e) => articles_list_result
             | Ok(articles_list) =>
               Caqti_lwt.Pool.use(
                 get_tag_list_by_article_id(~article_id),
                 Database.pool,
               )
               >>= (
                 tags => {
                   let author_result =
                     Profile.Model.make_from_entity(
                       ~username=author_name,
                       ~bio=author_bio,
                       ~image=author_image,
                       ~following=author_following,
                     );
                   switch (author_result) {
                   | Ok(author) =>
                     Ok([
                       Article__Model.{
                         slug,
                         title,
                         description,
                         body,
                         tagList: tags,
                         createdAt: Article__Model.time_of_string(created_at),
                         updatedAt: Article__Model.time_of_string(updated_at),
                         favorited,
                         favoritesCount,
                         author,
                       },
                       ...articles_list,
                     ])
                     |> Lwt.return
                   };
                 }
               )
             }
           })
        |> or_error;
      }
    );
  };

  let update_one =
      (~original_slug, ~new_title, ~new_slug, ~new_description, ~new_body) => {
    let update_one_query = [%rapper
      execute(
        {sql|
    UPDATE articles
    SET title = %string{new_title},
    slug = %string{new_slug},
    description = %string{new_description},
    body = %string{new_body}
    WHERE slug = %string{original_slug}  |sql},
      )
    ];
    Caqti_lwt.Pool.use(
      update_one_query(
        ~new_title,
        ~new_slug,
        ~new_description,
        ~new_body,
        ~original_slug,
      ),
      pool,
    )
    |> or_error;
  };

  let delete_one_by_slug = (~slug) => {
    let delete_query = [%rapper
      execute(
        {sql|
        DELETE FROM articles WHERE slug = %string{slug}
       |sql},
      )
    ];
    Caqti_lwt.Pool.use(delete_query(~slug), pool) |> or_error;
  };
};

module Repository = MakeRepository(Database.Connection);
