let default : SharedEntry.entry = { username = "u"; password = "123" }
let read file = if file == "123" then default else default
let to_string (_entry : SharedEntry.entry) = "bitwarden output"
let make : SharedEntry.format = { read; to_string }
