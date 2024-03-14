open Locksmith

let test_parse_bitWarden_csv () =
  let f = Bitwarden.format in
  let expected =
    { username = "username"; password = "secretPassword" } |> SharedEntry.show_entry
  in
  let actual = f.read "bitWarden_export.csv" |> SharedEntry.show_entry in
  Alcotest.(check string) "correct CSV" expected actual
;;

let () =
  let open Alcotest in
  run
    "CSV parsing"
    [ "Bitwarden", [ test_case "Default" `Quick test_parse_bitWarden_csv ] ]
;;
