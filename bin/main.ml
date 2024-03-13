open Locksmith2
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

let convert (input_format : formats option) file =
  let output_format = OnePassword in
  let input_format =
    match input_format with
    | Some x -> x
    | _ -> failwith "error"
  in
  let read = (get_format input_format).read in
  let to_string = (get_format output_format).to_string in
  let out = read file |> to_string in
  print_endline out
;;

(* Cmdliner argument parsing below *)

let input_format =
  let doc = "Input format" in
  let my_enum = Arg.enum (List.map (fun f -> format_to_string f, f) all_of_formats) in
  Arg.(value & opt (some my_enum) None & info [ "i"; "f"; "from" ] ~doc)
;;

let file = Arg.(value & pos 0 file "dune-project" & info [] ~docv:"FROM")

let cmd =
  let doc = "Transform between password manager export formats." in
  let info = Cmd.info "locksmith" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const convert $ input_format $ file)
;;

let main () = exit (Cmd.eval cmd)
let () = main ()
