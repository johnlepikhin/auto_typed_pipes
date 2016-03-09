

module Type =
struct
  type record = string list
  type meta = string

  let record_of_fields lst = lst
  let meta_of_line s = s
  let fields_of_record r = r
  let line_of_meta r = r
end

module Pipe = PipeLwt.Make (Type) (PipeShell.Make) (PipeShell.Make)

let pipe = Pipe.init Lwt_io.stdin Lwt_io.stdout

let printer r =
  Pipe.output pipe r

let main =
  Pipe.iter_input printer pipe

let () =
  Lwt_main.run main
