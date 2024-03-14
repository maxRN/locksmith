open SharedEntry

type t =
  { folder : string
  ; favorite : string
  ; entry_type : string
  ; name : string
  ; notes : string
  ; fields : string
  ; reprompt : string
  ; login_uri : string
  ; login_username : string
  ; login_password : string
  ; login_totp : string
  }
[@@deriving fields, csv, show]

let read file : SharedEntry.entry =
  let my_csv = csv_load file in
  List.iter (pp Format.std_formatter) my_csv;
  { username = "u"; password = "hello" }
;;

let shared_entry_of_bitwarden entry  =
  { username = entry.login_username; password = entry.login_password }
;;

let to_string _entry = "bitwarden output"
let format : SharedEntry.format = { read; to_string }
