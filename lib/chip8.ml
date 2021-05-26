
type instruction =
| Unknown
| ClearScreen
| Jump of Int32.t
| SetReg of Int32.t * Int32.t
| AddReg of Int32.t * Int32.t
| SetRegIndex of Int32.t
| Draw of Int32.t * Int32.t * Int32.t
[@@deriving show, eq]

let register_count = 16
let memory_size = 0x1000
let program_start = 0x200
let program_max_size = memory_size - program_start
let display_width = 64
let display_height = 32

type registers = Int32.t array
[@@deriving show]

type chip8 = {
    regs: registers;
    i: Int32.t;
    display: int array array;
    inst: Int32.t;
    decoded: instruction;
    mem: Bytes.t;
    vdel: Int32.t;
    vsnd: Int32.t;
    pc: Int32.t;
    sp: Int32.t;
} [@@deriving show]

let init () = {
    regs = Array.make register_count 0l;
    i = 0l;
    display = Array.make_matrix display_height display_width 0;
    inst = 0l;
    decoded = Unknown;
    mem = Bytes.create memory_size;
    vdel = 0l;
    vsnd = 0l;
    pc = Int32.of_int program_start;
    sp = 0l;
}

let instruction_get_nibble inst nibble =
    let nshift = nibble * 4 in
    let res = Int32.shift_right inst nshift in
    Int32.logand res 0x0Fl

let instruction_get_last_nibbles inst nibbles =
    let mask = Int32.of_float ((16. ** (Float.of_int nibbles)) -. 1.) in
    Int32.logand inst mask

let read_program path =
    let inc = open_in_bin path in
    let state = init () in
    let _ = input inc state.mem program_start program_max_size in
    state

let fetch state =
    let position = state.pc in
    let inst = Bytes.get_int16_be state.mem (Int32.to_int position) in
    { state with inst = Int32.of_int (inst land 0xFFFF); pc = Int32.add state.pc 2l }

let decode_instruction inst =
    match Int32.logand inst 0xF000l with
    | 0x0000l -> ClearScreen
    | 0x1000l ->
        let addr = instruction_get_last_nibbles inst 3 in
        Jump addr
    | 0x6000l ->
        let reg = instruction_get_nibble inst 2 in
        let num = instruction_get_last_nibbles inst 2 in
        SetReg (reg, num)
    | 0x7000l ->
        let reg = instruction_get_nibble inst 2 in
        let num = instruction_get_last_nibbles inst 2 in
        AddReg (reg, num)
    | 0xA000l ->
        let num = instruction_get_last_nibbles inst 3 in
        SetRegIndex num
    | 0xD000l ->
        let x = instruction_get_nibble inst 2 in
        let y = instruction_get_nibble inst 1 in
        let num = instruction_get_nibble inst 0 in
        Draw (x, y, num)
    | _ ->
        Unknown

let decode state =
    let inst = decode_instruction state.inst in
    { state with decoded = inst }

let display_show display =
    for i = 0 to display_height - 1 do
        for j = 0 to display_width - 1 do
            if display.(i).(j) = 0 then
                print_char ' '
            else
                print_char '.';
        done;
        print_endline ""
    done;
    print_endline "--------------------------"

let display_clear state =
    let _ =
        for i = 0 to display_height - 1 do
            for j = 0 to display_width - 1 do
                state.display.(i).(j) <- 0
            done
        done
    in
        state

let display_draw state xv yv height =
    let xi = Int32.to_int xv in
    let yi = Int32.to_int yv in
    let vx = Int32.to_int @@ Int32.unsigned_rem state.regs.(xi) (Int32.of_int display_width) in
    let vy = Int32.to_int @@ Int32.unsigned_rem state.regs.(yi) (Int32.of_int display_height) in
    let _ = state.regs.(0xF) <- 0l in
    let i = Int32.to_int state.i in
    let h = Int32.to_int height in
    let _ = print_string @@ Printf.sprintf "xi: 0x%X yi: 0x%X vx: 0x%X vy: 0x%X i: 0x%X h: 0x%x\n" xi yi vx vy i h in
    let _ =
        for y = 0 to h-1 do
            let sprite = Int32.of_int @@ 0xFF land Bytes.get_int8 state.mem (i + y) in
            let _ = print_string @@ Printf.sprintf "0x%X," (Int32.to_int sprite) in
            for x = 0 to 7 do
                if (Int32.logand sprite (Int32.shift_left 0x80l x)) != 0l then
                    if state.display.(y + vy).(x + vx) = 1 then
                        state.regs.(0xF) <- 1l;
                    let curr_pix = state.display.(y + vy).(x + vx) in
                    state.display.(y + vy).(x + vx) <- curr_pix lxor 0x1;
            done
        done
    in
        state

let execute state =
    match state.decoded with
    | Unknown -> state
    | ClearScreen -> display_clear state
    | Jump addr -> { state with pc = addr }
    | SetReg (x, num) ->
        Array.set state.regs (Int32.to_int x) num;
        state
    | AddReg (x, num) ->
        let xi = Int32.to_int x in
        let curr = Array.get state.regs xi in
        let res = Int32.add curr num in
        Array.set state.regs xi res;
        state
    | SetRegIndex (num) -> { state with i = num }
    | Draw (x, y, num) -> display_draw state x y num
