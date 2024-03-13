open Locksmith
open Cmdliner

type formats =
  | BitWarden
  | OnePassword
[@@deriving enumerate]

let format_to_string = function
  | BitWarden -> "bitWarden"
  | OnePassword -> "onePassword"
;;

let get_format format =
  let f =
    match format with
    | BitWarden -> Bitwarden.make
    | OnePassword -> OnePassword.make
  in
  f
;;

let debug_info input output file =
  print_endline
    (Format.sprintf
       "Converting %s from %s to %s\n"
       file
       (format_to_string input)
       (format_to_string output))
;;

let convert input_format output_format file =
  let read = (get_format input_format).read in
  let to_string = (get_format output_format).to_string in
  let out = read file |> to_string in
  debug_info input_format output_format file;
  print_endline out
;;

(* Cmdliner argument parsing below *)

let input_format =
  let doc = "Input format" in
  let my_enum = Arg.enum (List.map (fun f -> format_to_string f, f) all_of_formats) in
  Arg.(required & opt (some my_enum) None & info [ "i"; "f"; "from" ] ~doc)
;;

let output_format =
  let doc = "Input format" in
  let my_enum = Arg.enum (List.map (fun f -> format_to_string f, f) all_of_formats) in
  Arg.(required & opt (some my_enum) None & info [ "t"; "to" ] ~doc)
;;

let file = Arg.(value & pos 0 file "dune-project" & info [] ~docv:"FROM")

let cmd =
  let doc = "Transform between password manager export formats." in
  let info = Cmd.info "locksmith" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const convert $ input_format $ output_format $ file)
;;

let main () = exit (Cmd.eval cmd)
let () = main ()
