open Chip8

let test_instruction_get_nibble _ =
  Alcotest.(check int32) "get 0th nibble" 0x02l (instruction_get_nibble 0xFBA2l 0);
  Alcotest.(check int32) "get 1st nibble" 0x0Al (instruction_get_nibble 0xFBA2l 1);
  Alcotest.(check int32) "get 2nd nibble" 0x0Bl (instruction_get_nibble 0xFBA2l 2);
  Alcotest.(check int32) "get 3rd nibble" 0x0Fl (instruction_get_nibble 0xFBA2l 3)

let test_instruction_get_last_nibbles _ =
  Alcotest.(check int32) "get last 1 nibble" 0x000Al (instruction_get_last_nibbles 0xCDEAl 1);
  Alcotest.(check int32) "get last 2 nibbles" 0x00EAl (instruction_get_last_nibbles 0xCDEAl 2);
  Alcotest.(check int32) "get last 3 nibbles" 0x0DEAl (instruction_get_last_nibbles 0xCDEAl 3);
  Alcotest.(check int32) "get last 4 nibbles" 0xCDEAl (instruction_get_last_nibbles 0xCDEAl 4)

let () =
  let open Alcotest in
  run "Instructions" [
    "nibbles", [
        test_case "Get Nibble"     `Quick test_instruction_get_nibble;
        test_case "Capitalization" `Quick test_instruction_get_last_nibbles;
      ];
  ]
