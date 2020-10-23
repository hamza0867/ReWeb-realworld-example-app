let foo = () => print_endline("Hello");

module Functions = {
  let (>>) = (f, g, x) => g(f(x));
};
