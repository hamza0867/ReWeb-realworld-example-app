module R = ReWeb.Request;
open ReWeb;

[@deriving to_yojson]
type errorsBody = {body: list(string)};

[@deriving to_yojson]
type errorJson = {errors: errorsBody};

let makeErrorJson = errors => {
  errors: {
    body: errors,
  },
};

let bad_request = message =>
  `Bad_request |> ReWeb.Response.of_status(~message) |> Lwt.return;

let body_json_bad = string =>
  bad_request("ReWeb.Filter.body_json: " ++ string);

let body_json = (next, request) => {
  let body = R.body(request);
  open Lwt.Syntax;
  let* body_string = Piaf.Body.to_string(body);
  switch (body_string) {
  | Ok("") =>
    makeErrorJson(["can't be empty"])
    |> errorJson_to_yojson
    |> ReWeb.Response.of_json(~status=`Code(422))
    |> Lwt.return
  | Ok(body_string) =>
    switch (Yojson.Safe.from_string(body_string)) {
    | ctx => request |> R.set_context(ctx) |> next
    | exception (Yojson.Json_error(string)) => body_json_bad(string)
    }
  | Error(_) => body_json_bad("could not read request body")
  };
};

let get_auth = request => {
  let ( let* ) = (o, f) =>
    switch (o) {
    | None => None
    | Some(x) => f(x)
    };
  let* value = R.header("Authorization", request);
  switch (String.split_on_char(' ', value)) {
  | [typ, credentials] => Some((typ, credentials))
  | _ => None
  };
};

let unauthorized = `Unauthorized |> ReWeb.Response.of_status |> Lwt.return;

let bearer_auth = (next, request) =>
  switch (get_auth(request)) {
  | Some(("Bearer", token))
  | Some(("Token", token)) =>
    let (>>=) = Result.bind;
    let jwt =
      token |> Jose.Jwt.of_string >>= Jose.Jwt.validate(~jwk=AppConfig.jwk);
    switch (jwt) {
    | Ok(jwt) =>
      request
      |> R.set_context({
           as _;
           pub token = jwt;
           pub prev = R.context(request)
         })
      |> next
    | Error(`Expired) =>
      Response.of_json(
        ~status=`Unauthorized,
        [%yojson {
                   errors: {
                     authentication: {
                       expired: "expired token",
                     },
                   },
                 }],
      )
      |> Lwt.return
    | Error(`Invalid_signature) =>
      Response.of_json(
        ~status=`Unauthorized,
        [%yojson {
                   errors: {
                     authentication: {
                       invalid: "invalid token",
                     },
                   },
                 }],
      )
      |> Lwt.return
    | Error(`Msg(msg)) =>
      prerr_endline(msg);
      Response.of_json(
        ~status=`Unauthorized,
        [%yojson {
                   errors: {
                     authentication: {
                       invalid: "invalid token",
                     },
                   },
                 }],
      )
      |> Lwt.return;
    };
  | _ => unauthorized
  };
