type password =
  { title : string option
  ; url : string option
  ; username : string
  ; password : string
  ; notes : string option
  ; otpAuth : string option
  }
[@@deriving show]

type card =
  { title : string
  ; card_number : string
  }
[@@deriving show]

type entry =
  | Password of password
  | Card of card
[@@deriving show]

type format =
  { read : string -> entry list
  ; to_string : entry list -> string
  }

let read_csv file =
  let array = Csv.load file |> Csv.to_array in
  Array.sub array 1 (Array.length array - 1)
;;
