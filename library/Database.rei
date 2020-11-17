module type Connection = {
  let connection: Lwt.t(Caqti_lwt.connection);

  let pool: Caqti_lwt.Pool.t(Caqti_lwt.connection, [> Caqti_error.connect]);

  type error =
    | Database_error(string);

  let or_error:
    Lwt_result.t('a, [< Caqti_error.t]) => Lwt_result.t('a, error);
};

module Connection: Connection;
