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
              ~slug=
                unregistered.title
                |> String.lowercase
                |> String.Search_pattern.replace_all(
                     ~in_=_,
                     ~with_="-",
                     String.Search_pattern.create(" "),
                   ),
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
  /*let get_one_by_email = (~email) => {*/
  /*let get_one_query = [%rapper*/
  /*get_opt(*/
  /*{sql| SELECT*/
    /*@int{id},*/
    /*@string{email},*/
    /*@string{username},*/
    /*@string{bio},*/
    /*@string{password},*/
    /*@string?{image}*/
    /*FROM users WHERE email = %string{email}|sql},*/
  /*record_out,*/
  /*)*/
  /*];*/
  /*Caqti_lwt.Pool.use(get_one_query(~email), pool) |> or_error;*/
  /*};*/
  /*let update_one = (unregistered: Article__Entity.t) => {*/
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
