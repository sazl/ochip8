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

let test_instruction_jump _ =
  let init_state = init () in
  let addr = 0xA0l in
  let jmp_state = { init_state with decoded = Jump addr } in
  let mod_state = execute jmp_state in
  Alcotest.(check int32) "jump to 0xA0" addr mod_state.pc

let test_instruction_jump_consecutive _ =
  let init_state = init () in
  let addr = 0xA0l in
  let jmp_state = { init_state with decoded = Jump addr } in
  let mod_state = execute jmp_state in
  let other_addr = 0xEFl in
  let jmp2_state = { mod_state with decoded = Jump other_addr } in
  let final_state = execute jmp2_state in
  Alcotest.(check int32) "jump to consecutive addresses" other_addr final_state.pc

let test_skip_if_regs_not_equal _ =
  let init_state = init () in
  let init_pc = 0x20l in
  let x = 0l in
  let y = 1l in
  let xi = Int32.to_int x in
  let yi = Int32.to_int y in
  let skip_state = { init_state with decoded = SkipIfRegsNotEqual (x, y); pc = init_pc } in
  let _ = skip_state.regs.(xi) <- 4l in
  let _ = skip_state.regs.(yi) <- 6l in
  let mod_state = execute skip_state in
  let expected_pc = Int32.add init_pc 2l in
  Alcotest.(check int32) "skip instruction if X != Y" expected_pc mod_state.pc

let () =
  let open Alcotest in
  run "Instructions" [
    "nibbles", [
      test_case "Get Nibble" `Quick test_instruction_get_nibble;
      test_case "Get Last Nibbles" `Quick test_instruction_get_last_nibbles;
    ];
    "jump", [
      test_case "Jump to address" `Quick test_instruction_jump;
      test_case "Jump to consecutive" `Quick test_instruction_jump_consecutive;
    ];
    "skip_if_regs_not_equal", [
      test_case "Skip if X != Y" `Quick test_skip_if_regs_not_equal
    ]
  ]
