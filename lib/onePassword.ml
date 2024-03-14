open SharedEntry

type onePassword =
  { folder : string option
  ; favorite : string option
  ; entry_type : string option
  ; name : string option
  ; notes : string option
  ; fields : string option
  ; reprompt : string option
  ; login_uri : string option
  ; login_username : string option
  ; login_password : string option
  ; login_totp : string option
  }

let onePassword_of_array row =
  let x = function
    | "" -> None
    | y -> Some y
  in
  { folder = x row.(0)
  ; favorite = x row.(1)
  ; entry_type = x row.(2)
  ; name = x row.(3)
  ; notes = x row.(4)
  ; fields = x row.(5)
  ; reprompt = x row.(6)
  ; login_uri = x row.(7)
  ; login_username = x row.(8)
  ; login_password = x row.(9)
  ; login_totp = x row.(10)
  }
;;

let shared_entry_of_onePassword entry =
  { username = entry.login_username; password = entry.login_password }
;;

let shared_entry_of_array array = shared_entry_of_onePassword (onePassword_of_array array)

let onePassword_of_shared_entry entry =
  { folder = None
  ; favorite = None
  ; entry_type = None
  ; name = None
  ; notes = None
  ; fields = None
  ; reprompt = None
  ; login_uri = None
  ; login_username = entry.username
  ; login_password = entry.password
  ; login_totp = None
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
  let bw_entries = List.map onePassword_of_shared_entry entries in
  let strings = List.map string_of_entry bw_entries in
  let output = String.concat "\n" strings in
  output
;;

let read file =
  Csv.load file |> Csv.to_array |> Array.map shared_entry_of_array |> Array.to_list
;;

let format = { read; to_string }
