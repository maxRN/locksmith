let default : SharedEntry.entry = { username = "u"; password = "123" }
let read _file = default
let to_string (_entry : SharedEntry.entry) = "1Password output"
let make : SharedEntry.format = { read; to_string }
