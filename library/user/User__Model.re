[@deriving to_yojson]
type t = {
  email: Validations.Email.t,
  token: string,
  username: string,
  bio: string,
  image: option(Validations.Url.t),
};

let make_from_entity =
    (~email, ~token, ~username, ~bio, ~image): result(t, string) => {
  let email = email |> Validations.Email.fromString;
  let image = image |> Option.map(~f=Validations.Url.fromString);
  switch (email, image) {
  | (None, _) => Error("Invalid email")
  | (_, Some(None)) => Error("Invalid image url")
  | (Some(email), Some(image)) => Ok({username, email, token, bio, image})
  | (Some(email), None) => Ok({username, email, token, bio, image: None})
  };
};

[@deriving (of_yojson, show)]
type no_token = {
  email: string,
  username: string,
  bio: string,
  image: option(string),
};

let make_from_token = (user: Yojson.Safe.t, token: string): result(t, string) => {
  let no_token_result = no_token_of_yojson(user);
  let (>>=) = Result.bind;
  no_token_result
  >>= (
    no_token =>
      make_from_entity(
        ~username=no_token.username,
        ~email=no_token.email,
        ~bio=no_token.bio,
        ~image=no_token.image,
        ~token,
      )
  );
};
