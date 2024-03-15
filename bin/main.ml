open Locksmith
open Cmdliner

type formats =
  | BitWarden
  | KeyChain
[@@deriving enumerate]

let format_to_string = function
  | BitWarden -> "bitWarden"
  | KeyChain -> "keyChain"
;;

let get_format = function
  | BitWarden -> Bitwarden.format
  | KeyChain -> Keychain.format
;;

let infer_format input =
  let ic = open_in input in
  let line = input_line ic in
  try
    let format =
      if String.starts_with ~prefix:"folder,favorite" line
      then BitWarden
      else if String.starts_with ~prefix:"Title,URL" line
      then KeyChain
      else failwith "Can't infer input format. Please specify with '--from' flag."
    in
    close_in ic;
    format
  with
  | e ->
    close_in_noerr ic;
    raise e
;;

let convert input_format output_format input_file output_file =
  let input_format =
    match input_format with
    | Some x -> x
    | None -> infer_format input_file
  in
  let read = (get_format input_format).read in
  let to_string = (get_format output_format).to_string in
  let out = read input_file |> to_string in
  match output_file with
  | None -> print_endline out
  | Some name ->
    let oc = open_out name in
    Printf.fprintf oc "%s" out;
    close_out oc
;;

(* Cmdliner argument parsing below *)

let formats_enum = Arg.enum (List.map (fun f -> format_to_string f, f) all_of_formats)

let input_format =
  let doc = "Input format" in
  Arg.(value & opt (some formats_enum) None & info [ "i"; "f"; "from" ] ~doc)
;;

let output_format =
  let doc = "Output format" in
  Arg.(required & opt (some formats_enum) None & info [ "t"; "to" ] ~doc)
;;

let output_file =
  let doc = "Output file" in
  Arg.(value & opt (some string) None & info [ "o"; "output" ] ~doc)
;;

let input_file = Arg.(value & pos 0 file "dune-project" & info [] ~docv:"FROM")

let cmd =
  let doc = "Transform between password manager export formats." in
  let info = Cmd.info "locksmith" ~version:"%%VERSION%%" ~doc in
  Cmd.v
    info
    Term.(const convert $ input_format $ output_format $ input_file $ output_file)
;;

let main () = exit (Cmd.eval cmd)
let () = main ()
