type entry =
  { username : string option
  ; password : string option
  }
[@@deriving show]

type format =
  { read : string -> entry list
  ; to_string : entry list -> string
  }
