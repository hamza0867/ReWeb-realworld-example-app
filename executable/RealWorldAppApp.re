open ReWeb;

let _ = {
  let (>>=) = Lwt.(>>=);
  Lib.Schema.create_schema()
  >>= (
    fun
    | Ok () => print_endline("\n Schema created successfully") |> Lwt.return
    | Error(Lib.Database.Connection.Database_error(e)) =>
      prerr_endline("\n Error happened while creating schema: " ++ e)
      |> Lwt.return
  );
};

let apiServer =
  fun
  | (meth, ["users", ...path])
  | (meth, ["user", ...path]) => Lib.User.Server.server @@ (meth, path)
  | (`GET, ["profiles", username]) =>
    Lib.Profile.Server.server(username, Get)

  | (`POST, ["profiles", username, "follow"]) =>
    Lib.Profile.Server.server(username, Follow)

  | (`DELETE, ["profiles", username, "follow"]) =>
    Lib.Profile.Server.server(username, Unfollow)

  | (meth, ["articles", ...path]) =>
    Lib.Article.Server.server @@ (meth, path)

  | (_, path) => (
      _ =>
        Response.of_text(
          ~status=`Not_found,
          "unsupported url "
          ++ List.fold_left(
               (acc, curr) => acc ++ "/" ++ curr,
               "",
               ["api", ...path],
             ),
        )
        |> Lwt.return
    );

let server =
  fun
  | (meth, ["api", ...path]) =>
    ReWeb.Filter.cors(Header.AccessControlAllowOrigin.All) @@
    apiServer @@
    (meth, path)
  | (_, path) => (
      _ =>
        Response.of_text(
          ~status=`Not_found,
          "unsupported url "
          ++ List.fold_left((acc, curr) => acc ++ "/" ++ curr, "", path),
        )
        |> Lwt.return
    );

Server.serve(~port=8080, server);
