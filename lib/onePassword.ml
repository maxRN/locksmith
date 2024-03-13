let default : SharedEntry.entry = { username = "u"; password = "123" }
let read file = if file == "123" then default else default

let to_string (entry : SharedEntry.entry) =
  if entry.username == "123" then "u are user" else "don't threaten me with a good time"
;;

let make : SharedEntry.format = { read; to_string }
