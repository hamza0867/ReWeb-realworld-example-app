let body_json: ReWeb.Filter.t(unit, Yojson.Safe.t, [> ReWeb.Response.http]);

[@deriving to_yojson]
type errorsBody = {body: list(string)};

[@deriving to_yojson]
type errorJson = {errors: errorsBody};

let makeErrorJson: list(string) => errorJson;

let bearer_auth:
  ReWeb.Filter.t(
    'ctx,
    {
      .
      token: Jose.Jwt.t,
      prev: 'ctx,
    },
    [> ReWeb.Response.http],
  );

let bearer_auth_optional:
  ReWeb.Filter.t(
    'ctx,
    {
      .
      token: option(Jose.Jwt.t),
      prev: 'ctx,
    },
    [> ReWeb.Response.http],
  );

let unauthorized: Lwt.t([> ReWeb.Response.http]);
