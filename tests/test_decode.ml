
open Chip8

let pprint_instruction ppf inst =
  Fmt.pf ppf "%s" @@ show_instruction inst
let instruction_eq a b =
  (a = b)
let instruction_testable =
  Alcotest.testable pprint_instruction instruction_eq

let test_decode_instruction_clear_screen _ =
  Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0xE0l) ClearScreen

let test_decode_instruction_jump _ =
  Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0x1000l) (Jump 0x000l);
  Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0x1EFEl) (Jump 0xEFEl);
  Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0x1346l) (Jump 0x346l)

let test_decode_instruction_set_reg _ =
  Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0x6000l) (SetReg (0x0l, 0x00l));
  Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0x6A34l) (SetReg (0xAl, 0x34l));
  Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0x6FCDl) (SetReg (0xFl, 0xCDl))

  let test_decode_instruction_add_reg _ =
    Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0x7000l) (AddReg (0x0l, 0x00l));
    Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0x7A34l) (AddReg (0xAl, 0x34l));
    Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0x7FCDl) (AddReg (0xFl, 0xCDl))

let test_decode_instruction_set_reg_index _ =
  Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0xA000l) (SetRegIndex 0x0l);
  Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0xA123l) (SetRegIndex 0x123l);
  Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0xAEFCl) (SetRegIndex 0xEFCl)

let test_decode_instruction_draw _ =
    Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0xD000l) (Draw (0x0l, 0x0l, 0x0l));
    Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0xDABDl) (Draw (0xAl, 0xBl, 0xDl));
    Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0xD25Fl) (Draw (0x2l, 0x5l, 0xFl))

let test_decode_instruction_unknown _ =
  Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0xFE83l) Unknown;
  Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0x3945l) Unknown;
  Alcotest.(check instruction_testable) "decode 0xE0 should ClearScreen" (decode_instruction 0x5E47l) Unknown

let () =
  let open Alcotest in
  run "Decode" [
    "decode", [
        test_case "Decode ClearScreen" `Quick test_decode_instruction_clear_screen;
        test_case "Decode Jump" `Quick test_decode_instruction_jump;
        test_case "Decode SetReg" `Quick test_decode_instruction_set_reg;
        test_case "Decode AddReg" `Quick test_decode_instruction_add_reg;
        test_case "Decode SetRegIndex" `Quick test_decode_instruction_set_reg_index;
        test_case "Decode Draw" `Quick test_decode_instruction_draw;
        test_case "Decode Unknown" `Quick test_decode_instruction_unknown;
      ];
  ]
