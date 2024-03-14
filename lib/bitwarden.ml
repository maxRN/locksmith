open SharedEntry

let header =
  "folder,favorite,entry,name,notes,fields,reprompt,login_uri,login_username,login_password,login_totp"
;;

(* Find out what is really optional *)
type bitwarden =
  { folder : string option
  ; favorite : string option
  ; entry_type : string
  ; name : string
  ; notes : string option
  ; fields : string option
  ; reprompt : string option
  ; login_uri : string option
  ; login_username : string option
  ; login_password : string option
  ; login_totp : string option
  }
[@@deriving make]

let bitwarden_of_array (row : string array) : bitwarden =
  let x = function
    | "" -> None
    | y -> Some y
  in
  { folder = x row.(0)
  ; favorite = x row.(1)
  ; entry_type = Option.get (x row.(2)) (* TODO: check this *)
  ; name = Option.get (x row.(3))
  ; notes = x row.(4)
  ; fields = x row.(5)
  ; reprompt = x row.(6)
  ; login_uri = x row.(7)
  ; login_username = x row.(8)
  ; login_password = x row.(9)
  ; login_totp = x row.(10)
  }
;;

let password_of_bitwarden entry =
  { title = entry.name
  ; url = entry.login_uri
  ; notes = entry.notes
  ; username = Option.get entry.login_username
  ; password = Option.get entry.login_password
  ; otpAuth = entry.login_totp
  }
;;

let shared_entry_of_bitwarden (entry : bitwarden) =
  match entry.entry_type with
  | "login" -> SharedEntry.Password (password_of_bitwarden entry)
  | _ -> failwith "Only handling login types"
;;

let shared_entry_of_array array = shared_entry_of_bitwarden (bitwarden_of_array array)

let bitwarden_of_password (password : password) =
  make_bitwarden
    ~entry_type:"login"
    ~name:password.title
    ~login_username:password.username
    ~login_password:password.password
    ()
;;

let bitwarden_of_shared_entry entry =
  match entry with
  | SharedEntry.Password p -> bitwarden_of_password p
  | SharedEntry.Card _ -> failwith "Not handling this card"
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
    ; Some entry.entry_type
    ; Some entry.name
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
  let bw_entries = List.map bitwarden_of_shared_entry entries in
  let strings = List.map string_of_entry bw_entries in
  let strings = List.concat [ [ header ]; strings ] in
  let output = String.concat "\n" strings in
  output
;;

let read file =
  let array = Csv.load file |> Csv.to_array |> Array.map shared_entry_of_array in
  let array = Array.sub array 1 (Array.length array - 1) in
  Array.to_list array
;;

let format = { read; to_string }
