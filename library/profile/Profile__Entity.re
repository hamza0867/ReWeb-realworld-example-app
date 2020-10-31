type t = {
  username: string,
  bio: string,
  image: option(string),
  following: bool,
};

type no_following = {
  id: int,
  username: string,
  bio: string,
  image: option(string),
};
