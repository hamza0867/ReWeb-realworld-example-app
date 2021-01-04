open ReWeb;

let server = _req => {
  let (>>=) = Lwt.(>>=);
  Tag__Repository.Repository.get_all()
  >>= (
    fun
    | Ok(tags) =>
      [%yojson {tags: [%y `List(tags |> List.map(~f=tag => `String(tag)))]}]
      |> Response.of_json(~status=`OK)
      |> Lwt.return
    | Error(Database.Connection.Database_error(e)) => {
        prerr_endline("\n" ++ e);
        Response.of_status(`Internal_server_error) |> Lwt.return;
      }
  );
};
