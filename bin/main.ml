open Chip8

let run rom =
  let state = read_program rom in
  let rec loop state =
    let fstate = fetch state in
    let dstate = decode fstate in
    let estate = execute dstate in
    let disp = estate.display in
      display_show disp;
      loop estate
in
  loop state

let restore_cursor _ =
  let _ = print_string "\x1b[?25h" in
  let _ = Sys.command "clear" in
  exit 0

let _ = begin
  let _ = Sys.signal Sys.sigint (Sys.Signal_handle restore_cursor) in
  let _ = Sys.command "clear" in
  if Array.length Sys.argv > 1 then
    let rom = Sys.argv.(1) in
    run rom
  else
    print_endline "no rom specified: ochip8 <rom>"
end