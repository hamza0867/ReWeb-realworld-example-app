module D = Decoders_yojson.Safe.Decode;

module type Validation = {
  type t;
  let fromString: string => option(t);
  let toString: t => string;
  let to_yojson: t => Yojson.Safe.t;
  let of_yojson: Yojson.Safe.t => result(t, string);
};

module Email: Validation = {
  type t = string;
  let emailRegex = Re.Pcre.regexp({|^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$|});
  let fromString = str => Re.execp(emailRegex, str) ? Some(str) : None;
  let toString = str => str;
  let to_yojson = t => `String(t);

  let of_yojson = json => {
    switch (json) {
    | `String(email) =>
      switch (fromString(email)) {
      | Some(s) => Ok(s)
      | None => Error("Invalid email")
      }
    | _ => Error("The email needs to be a string")
    };
  };
};

module Url: Validation = {
  type t = string;
  let urlRegex =
    Re.Pcre.regexp(
      {|[(http(s)?):\/\/(www\.)?a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)|},
    );
  let fromString = str => Re.execp(urlRegex, str) ? Some(str) : None;
  let toString = str => str;
  let to_yojson = t => `String(t);
  let of_yojson = json => {
    switch (json) {
    | `String(url) =>
      switch (fromString(url)) {
      | Some(s) => Ok(s)
      | None => Error("Invalid url")
      }
    | _ => Error("The url needs to be a string")
    };
  };
};
