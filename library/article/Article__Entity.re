type create_entity = {
  title: string,
  description: string,
  body: string,
  tagList: list(string),
};

let t_of_create_entity = (~create_entity, ~author): Article__Model.t => {
  title: create_entity.title,
  slug:
    create_entity.title
    |> String.lowercase
    |> String.Search_pattern.replace_all(
         ~in_=_,
         ~with_="-",
         String.Search_pattern.create(" "),
       ),
  description: create_entity.description,
  body: create_entity.body,
  tagList: create_entity.tagList,
  createdAt: CalendarLib.Calendar.now(),
  updatedAt: CalendarLib.Calendar.now(),
  favorited: false,
  favoritesCount: 0,
  author,
};
