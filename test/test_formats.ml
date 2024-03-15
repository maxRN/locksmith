open Locksmith

let test_parse_bitWarden_csv () =
  let f = Bitwarden.format in
  let pw =
    SharedEntry.Password
      { title = Some "Gmail"
      ; url = Some "gmail.com"
      ; username = "username"
      ; password = "secret-password"
      ; notes = None
      ; otpAuth = None
      }
  in
  let expected = List.map SharedEntry.show_entry [ pw ] in
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
