type t = {
  id: int,
  email: string,
  username: string,
  bio: string,
  password: string,
  image: option(string),
};

type no_password = {
  id: int,
  email: string,
  username: string,
  bio: string,
  image: option(string),
};

type create_entity = {
  email: string,
  username: string,
  password: string,
};
