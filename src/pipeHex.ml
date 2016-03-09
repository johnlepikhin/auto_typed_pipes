
module Make (T : Pipe.TYPE) =
struct
  type record = T.record
  type meta = T.meta

  let escape s =
    let len = String.length s in
    let b = Buffer.create (len * 3) in
    String.iter (function
        | '!' .. '~' as c ->
          Buffer.add_char b ' ';
          Buffer.add_char b c
        | c ->
          Buffer.add_char b ' ';
          Buffer.add_string b @@ Printf.sprintf "%x" (Char.code c)
      ) s;
    Buffer.contents b

  let of_string s =
    failwith "Hex pipe format for input is not supported"
  
  let to_string = function
    | Pipe.Record r ->
      T.fields_of_record r
      |> List.map escape
      |> String.concat "\t"
    | Pipe.Meta r ->
      T.line_of_meta r
      |> escape
  
  let record_separator = '\n'

  let name = "ascii"
end
