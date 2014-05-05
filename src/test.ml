open Core.Std
open Unix.Process_channels
open OUnit

let drop_carrage_return str =
  let buffer = Buffer.create (String.length str) in
  String.iter str ~f:(fun ch ->
      if ch <> '\r' then
        Buffer.add_char buffer ch
    );
  Buffer.contents buffer

let test_result expected output exit_status =
  let exit_message = Unix.Exit_or_signal.to_string_hum exit_status in
  assert_equal "exited normally" exit_message ~printer: Fn.id;
  assert_equal expected output ~printer: Fn.id

let test_bash name batsh expected =
  let bash = Bash.compile batsh in
  let code = (Bash.print bash) ^ "\n" in
  (* Code *)
  let inx = In_channel.create ("tests/bash/" ^ name ^ ".sh") in
  let code_expected = In_channel.input_all inx in
  In_channel.close inx;
  assert_equal code_expected code ~printer: Fn.id;
  (* Run result *)
  let stdout, stdin = Unix.open_process "bash" in
  Out_channel.output_string stdin code;
  Out_channel.close stdin;
  let output = In_channel.input_all stdout in
  let exit_status = Unix.close_process (stdout, stdin) in
  test_result expected output exit_status

let test_winbat name batsh expected =
  let winbat = Winbat.compile batsh in
  let code = (Winbat.print winbat) ^ "\n" in
  (* Code *)
  let inx = In_channel.create ("tests/batch/" ^ name ^ ".bat") in
  let code_expected = In_channel.input_all inx in
  In_channel.close inx;
  assert_equal code_expected code ~printer: Fn.id;
  (* Run result *)
  let filename = Filename.temp_file "batsh" ".bat" in
  let outx = Out_channel.create filename in
  Out_channel.output_string outx code;
  Out_channel.close outx;

  let cmd = "wine cmd /c " ^ filename in
  let channels = Unix.open_process_full cmd ~env:[||] in
  let output_raw = In_channel.input_all channels.stdout in
  let output = drop_carrage_return output_raw in
  let exit_status = Unix.close_process_full channels in
  test_result expected output exit_status

let get_expected name =
  let answer_filename = "tests/output/" ^ name ^ ".txt" in
  let inx = In_channel.create answer_filename in
  let expected = In_channel.input_all inx in
  In_channel.close inx;
  expected

let test name func _ =
  let expected = get_expected name in
  let filename = "tests/" ^ name ^ ".batsh" in
  let batsh = Parser.create_from_file filename in
  func name batsh expected

let test_cases = "Batsh Unit Tests" >::: [
    "[Bash]Comment"       >:: test "comment" test_bash;
    "[Bash]Block"         >:: test "block" test_bash;
    "[Bash]Arith"         >:: test "arith" test_bash;
    "[Bash]Assignment"    >:: test "assignment" test_bash;
    "[Bash]Array"         >:: test "array" test_bash;
    "[Bash]String"        >:: test "string" test_bash;
    "[Bash]If"            >:: test "if" test_bash;
    "[Bash]While"         >:: test "while" test_bash;
    "[Bash]Function"      >:: test "function" test_bash;
    "[Bash]Recursion"     >:: test "recursion" test_bash;
    "[Bash]Command"       >:: test "command" test_bash;
    "[Bash]Exists"        >:: test "exists" test_bash;
    "[Winbat]Comment"     >:: test "comment" test_winbat;
    "[Winbat]Block"       >:: test "block" test_winbat;
    "[Winbat]Arith"       >:: test "arith" test_winbat;
    "[Winbat]Assignment"  >:: test "assignment" test_winbat;
    (* "[Winbat]Array"       >:: test "array" test_winbat; *)
    "[Winbat]String"      >:: test "string" test_winbat;
    "[Winbat]If"          >:: test "if" test_winbat;
    "[Winbat]While"       >:: test "while" test_winbat;
    "[Winbat]Function"    >:: test "function" test_winbat;
    "[Winbat]Recursion"   >:: test "recursion" test_winbat;
    "[Winbat]Command"     >:: test "command" test_winbat;
    "[Winbat]Exists"      >:: test "exists" test_winbat;
  ]

let _ =
  run_test_tt_main test_cases
