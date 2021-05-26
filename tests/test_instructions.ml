open OUnit2
open Chip8

let hex_print hex =
  Printf.sprintf "%X" (Int32.to_int hex)

let test_instruction_get_nibble _ =
  assert_equal 0x02l (instruction_get_nibble 0xFBA2l 0);
  assert_equal 0x0Al (instruction_get_nibble 0xFBA2l 1);
  assert_equal 0x0Bl (instruction_get_nibble 0xFBA2l 2);
  assert_equal 0x0Fl (instruction_get_nibble 0xFBA2l 3)

let test_instruction_get_last_nibbles _ =
  assert_equal 0x000Al (instruction_get_last_nibbles 0xCDEAl 1) ~printer:hex_print;
  assert_equal 0x00EAl (instruction_get_last_nibbles 0xCDEAl 2) ~printer:hex_print;
  assert_equal 0x0DEAl (instruction_get_last_nibbles 0xCDEAl 3) ~printer:hex_print;
  assert_equal 0xCDEAl (instruction_get_last_nibbles 0xCDEAl 4) ~printer:hex_print

let tests = "test suite for instructions" >::: [
  "instruction_get_nibble"       >:: test_instruction_get_nibble;
  "instruction_get_last_nibbles" >:: test_instruction_get_last_nibbles;
]

let _ = run_test_tt_main tests
