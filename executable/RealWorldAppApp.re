open ReWeb;

let apiServer =
  fun
  | (meth, ["users", ...path])
  | (meth, ["user", ...path]) => Lib.User.Server.server @@ (meth, path)

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