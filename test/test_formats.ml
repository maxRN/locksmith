open Locksmith

let test_parse_bitWarden_csv () =
  let f = Bitwarden.format in
  let expected =
    List.map
      SharedEntry.show_entry
      [ { username = Some "username"; password = Some "secret-password" } ]
  in
  let actual =
    List.map SharedEntry.show_entry (f.read "testFixtures/bitwarden_export.csv")
  in
  Alcotest.(check (list string)) "correct CSV" expected actual
;;

let () =
  let open Alcotest in
  run
    "CSV parsing"
    [ "Bitwarden", [ test_case "Default" `Quick test_parse_bitWarden_csv ] ]
;;
