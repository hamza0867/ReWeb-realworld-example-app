open ReWeb;
module D = Decoders_yojson.Safe.Decode;

let server =
  fun
  | (meth, path) => Article__Resource.resource @@ (meth, path);
