let foo = () => print_endline("Hello");

module Functions = {
  let (>>) = (f, g, x) => g(f(x));
};
let slugify = title =>
  title
  |> String.lowercase
  |> String.Search_pattern.replace_all(
       ~in_=_,
       ~with_="-",
       String.Search_pattern.create(" "),
     );
