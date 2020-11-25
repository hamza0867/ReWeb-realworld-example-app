let time_to_string = CalendarLib.Printer.Calendar.sprint("%FT%T.000Z");

let time_of_string = CalendarLib.Printer.Calendar.from_fstring("%FT%T.000Z");

let calendar_to_yojson = (time: CalendarLib.Calendar.t) => {
  let string_rep = time |> time_to_string;
  `String(string_rep);
};

[@deriving to_yojson]
type t = {
  slug: string,
  title: string,
  description: string,
  body: string,
  tagList: list(string),
  [@to_yojson calendar_to_yojson]
  createdAt: CalendarLib.Calendar.t,
  [@to_yojson calendar_to_yojson]
  updatedAt: CalendarLib.Calendar.t,
  favorited: bool,
  favoritesCount: int,
  author: Profile.Model.t,
};
