open Chip8

let _ =
  let state = read_program "ibm.ch8" in
  let rec loop state =
    let fstate = fetch state in
    let dstate = decode fstate in
    let estate = execute dstate in
    let disp = estate.display in
      print_string @@ Printf.sprintf "inst: 0x%X\n" (Int32.to_int estate.inst);
      print_string @@ Printf.sprintf "i: 0x%X\n" (Int32.to_int estate.i);
      print_string @@ Printf.sprintf "pc: 0x%X\n" (Int32.to_int estate.pc);
      print_string @@ show_registers estate.regs;
      print_endline "\n---";
      display_show disp;
      loop estate
in
  loop state