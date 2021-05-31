open Chip8

let _ =
  let state = read_program "roms/maze.ch8" in
  let rec loop state =
    let fstate = fetch state in
    let dstate = decode fstate in
    let estate = execute dstate in
    let disp = estate.display in
      display_show disp;
      loop estate
in
  loop state