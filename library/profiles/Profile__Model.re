[@deriving to_yojson]
type t = {
  username: string,
  bio: string,
  image: option(Validations.Url.t),
  following: bool,
};

let make_from_entity =
    (~username, ~bio, ~image, ~following): result(t, string) => {
  let image = image |> Option.map(Validations.Url.fromString);
  switch (image) {
  | Some(None) => Error("Invalid image url")
  | Some(image) => Ok({username, bio, image, following})
  | None => Ok({username, bio, image: None, following})
  };
};

