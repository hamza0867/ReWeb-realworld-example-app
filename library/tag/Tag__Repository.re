module MakeRepository = (Database: Database.Connection) => {
  open Database;

  let get_all = () => {
    let query = [%rapper
      get_many({sql|
    SELECT @string{tag} FROM tags
    |sql})
    ];
    Caqti_lwt.Pool.use(query(), pool) |> or_error;
  };
};

module Repository = MakeRepository(Database.Connection);
