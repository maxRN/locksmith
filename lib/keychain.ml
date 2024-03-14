open SharedEntry

type keychain =
  { title : string
  ; url : string
  ; username : string
  ; password : string
  ; notes : string option
  ; otpAuth : string option
  }

let header = "Title,URL,Username,Password,Notes,OTPAuth"

let keychain_of_array row =
  let x = function
    | "" -> None
    | y -> Some y
  in
  { title = x row.(0)
  ; url = x row.(1)
  ; username = x row.(2)
  ; password = x row.(3)
  ; notes = x row.(4)
  ; otpAuth = x row.(5)
  }
;;

let shared_entry_of_keychain entry =
  { username = Some entry.username; password = Some entry.password }
;;

let shared_entry_of_array array = shared_entry_of_keychain (keychain_of_array array)

let keychain_of_shared_entry entry =
  {
    title = "Title";

  }
;;

let list_of_entry entry =
  let x = function
    | None -> ""
    | Some y -> y
  in
  List.map
    x
    [ entry.folder
    ; entry.favorite
    ; entry.entry_type
    ; entry.name
    ; entry.notes
    ; entry.fields
    ; entry.reprompt
    ; entry.login_uri
    ; entry.login_username
    ; entry.login_password
    ; entry.login_totp
    ]
;;

let string_of_entry entry = String.concat "," (list_of_entry entry)

let to_string (entries : entry list) =
  let bw_entries = List.map keychain_of_shared_entry entries in
  let strings = List.map string_of_entry bw_entries in
  let output = String.concat "\n" strings in
  output
;;

let read file =
  Csv.load file |> Csv.to_array |> Array.map shared_entry_of_array |> Array.to_list
;;

let format = { read; to_string }
