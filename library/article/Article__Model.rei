let time_to_string: CalendarLib.Calendar.t => string;

let time_of_string: string => CalendarLib.Calendar.t;

[@deriving to_yojson]
type t = {
  slug: string,
  title: string,
  description: string,
  body: string,
  tagList: list(string),
  createdAt: CalendarLib.Calendar.t,
  updatedAt: CalendarLib.Calendar.t,
  favorited: bool,
  favoritesCount: int,
  author: Profile.Model.t,
};
