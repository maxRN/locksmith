type entry =
  { username : string
  ; password : string
  }
[@@deriving show]

type format =
  { read : string -> entry
  ; to_string : entry -> string
  }
