open SharedEntry

type keychain =
  { title : string option
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
  { title =
      x row.(0)
      (* No checks here, because we assume we get a proper keychain export file
         which should include the required values (url, username, password) *)
  ; url = row.(1)
  ; username = row.(2)
  ; password = row.(3)
  ; notes = x row.(4)
  ; otpAuth = x row.(5)
  }
;;

let shared_entry_of_keychain entry =
  Password
    { title = entry.title
    ; username = entry.username
    ; password = entry.password
    ; url = Some entry.url
    ; notes = entry.notes
    ; otpAuth = entry.otpAuth
    }
;;

let filter_shared_entries (entries : entry list) : password list =
  let filter = function
    | Password x -> Some x
    | _ -> None
  in
  List.filter_map filter entries
;;

let shared_entry_of_array array = shared_entry_of_keychain (keychain_of_array array)

let keychain_of_password (entry : SharedEntry.password) =
  { title = entry.title
  ; url =
      (match entry.url with
       | Some x -> x
       | None -> "import.example.com")
  ; username = entry.username
  ; password = entry.password
  ; notes = entry.notes
  ; otpAuth = entry.otpAuth
  }
;;

let list_of_entry entry =
  let x = function
    | None -> ""
    | Some y -> y
  in
  List.map
    x
    [ entry.title
    ; Some entry.url
    ; Some entry.username
    ; Some entry.password
    ; entry.notes
    ; entry.otpAuth
    ]
;;

let string_of_entry entry = String.concat "," (list_of_entry entry)

let to_string (entries : entry list) =
  let strings =
    filter_shared_entries entries
    |> List.map keychain_of_password
    |> List.map string_of_entry
  in
  String.concat "\n" (header :: strings)
;;

let read file = read_csv file |> Array.map shared_entry_of_array |> Array.to_list
let format = { read; to_string }
