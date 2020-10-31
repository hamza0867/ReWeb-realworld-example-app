module type Connection = {
  let pool: Caqti_lwt.Pool.t(Caqti_lwt.connection, [> Caqti_error.connect]);

  type error =
    | Database_error(string);

  let or_error:
    Lwt_result.t('a, [< Caqti_error.t]) => Lwt_result.t('a, error);
};

module Connection = {
  let connection_url =
    try(Sys.getenv("CONN_STRING")) {
    | Not_found => failwith("CONN_STRING env variable not found")
    };

  let pool =
    switch (
      Caqti_lwt.connect_pool(~max_size=10, Uri.of_string(connection_url))
    ) {
    | Ok(pool) => pool
    | Error(err) => failwith(Caqti_error.show(err))
    };

  type error =
    | Database_error(string);

  let or_error = m => {
    switch%lwt (m) {
    | Ok(a) => Ok(a) |> Lwt.return
    | Error(e) => Error(Database_error(Caqti_error.show(e))) |> Lwt.return
    };
  };
};
