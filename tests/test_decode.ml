open OUnit2
open Chip8

let test_decode_instruction_clear_screen _ =
  assert_equal (decode_instruction 0xE0l) ClearScreen ~printer:show_instruction

let test_decode_instruction_jump _ =
  assert_equal (decode_instruction 0x1000l) (Jump 0x000l) ~printer:show_instruction;
  assert_equal (decode_instruction 0x1EFEl) (Jump 0xEFEl) ~printer:show_instruction;
  assert_equal (decode_instruction 0x1346l) (Jump 0x346l) ~printer:show_instruction

let test_decode_instruction_set_reg _ =
  assert_equal (decode_instruction 0x6000l) (SetReg (0x0l, 0x00l)) ~printer:show_instruction;
  assert_equal (decode_instruction 0x6A34l) (SetReg (0xAl, 0x34l)) ~printer:show_instruction;
  assert_equal (decode_instruction 0x6FCDl) (SetReg (0xFl, 0xCDl)) ~printer:show_instruction

  let test_decode_instruction_add_reg _ =
    assert_equal (decode_instruction 0x7000l) (AddReg (0x0l, 0x00l)) ~printer:show_instruction;
    assert_equal (decode_instruction 0x7A34l) (AddReg (0xAl, 0x34l)) ~printer:show_instruction;
    assert_equal (decode_instruction 0x7FCDl) (AddReg (0xFl, 0xCDl)) ~printer:show_instruction

let test_decode_instruction_set_reg_index _ =
  assert_equal (decode_instruction 0xA000l) (SetRegIndex 0x0l) ~printer:show_instruction;
  assert_equal (decode_instruction 0xA123l) (SetRegIndex 0x123l) ~printer:show_instruction;
  assert_equal (decode_instruction 0xAEFCl) (SetRegIndex 0xEFCl) ~printer:show_instruction

let test_decode_instruction_draw _ =
    assert_equal (decode_instruction 0xD000l) (Draw (0x0l, 0x0l, 0x0l)) ~printer:show_instruction;
    assert_equal (decode_instruction 0xDABDl) (Draw (0xAl, 0xBl, 0xDl)) ~printer:show_instruction;
    assert_equal (decode_instruction 0xD25Fl) (Draw (0x2l, 0x5l, 0xFl)) ~printer:show_instruction

let test_decode_instruction_unknown _ =
  assert_equal (decode_instruction 0xFE83l) Unknown ~printer:show_instruction;
  assert_equal (decode_instruction 0x3945l) Unknown ~printer:show_instruction;
  assert_equal (decode_instruction 0x5E47l) Unknown ~printer:show_instruction

let tests = "test suite for decode"  >::: [
  "decode_instruction_clear_screen"  >:: test_decode_instruction_clear_screen;
  "decode_instruction_jump"          >:: test_decode_instruction_jump;
  "decode_instruction_set_reg"       >:: test_decode_instruction_set_reg;
  "decode_instruction_add_reg"       >:: test_decode_instruction_add_reg;
  "decode_instruction_set_reg_index" >:: test_decode_instruction_set_reg_index;
  "decode_instruction_draw"          >:: test_decode_instruction_draw;
  "decode_instruction_unknown"       >:: test_decode_instruction_unknown;
]

let _ = run_test_tt_main tests
