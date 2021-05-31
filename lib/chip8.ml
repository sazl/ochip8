
type instruction =
| Unknown
| ClearScreen
| SubroutineReturn
| Jump of Int32.t
| SubroutineCall of Int32.t
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
    stack: Int32.t list;
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
    stack = [];
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
    | 0x0000l -> (
        let nibble = instruction_get_last_nibbles inst 2 in
        match nibble with
        | 0xE0l -> ClearScreen
        | 0xEEl -> SubroutineReturn
        | _ -> Unknown
    )
    | 0x1000l ->
        let addr = instruction_get_last_nibbles inst 3 in
        Jump addr
    | 0x2000l ->
        let addr = instruction_get_last_nibbles inst 3 in
        SubroutineCall addr
    | 0x5000l ->
        Unknown
    | 0x6000l ->
        let reg = instruction_get_nibble inst 2 in
        let num = instruction_get_last_nibbles inst 2 in
        SetReg (reg, num)
    | 0x7000l ->
        let reg = instruction_get_nibble inst 2 in
        let num = instruction_get_last_nibbles inst 2 in
        AddReg (reg, num)
    | 0x9000l ->
        Unknown
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
                print_string "█";
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
    let vx = Int32.to_int state.regs.(xi) in
    let vy = Int32.to_int state.regs.(yi) in
    let _ = state.regs.(0xF) <- 0l in
    let i = Int32.to_int state.i in
    let h = Int32.to_int height in
    let _ =
        for y = 0 to h-1 do
        begin
            let cy = (vy + y) mod display_height in
            let pixel = Bytes.get_int8 state.mem (i + y) in

            for x = 0 to 7 do
                if (pixel land (0x80 lsr x)) != 0 then
                begin
                    let cx = (vx + x) mod display_width in
                    if state.display.(cy).(cx) = 1 then
                    begin
                        state.regs.(0xF) <- 1l;
                    end;
                    let curr_pix = state.display.(cy).(cx) in
                    state.display.(cy).(cx) <- curr_pix lxor 0x1;
                end;
            done;
        end;
        done
    in
        state

let execute state =
    match state.decoded with
    | Unknown -> state
    | ClearScreen -> display_clear state
    | SubroutineReturn ->
        let addr = List.hd state.stack in
        { state with pc = addr; stack = List.tl state.stack }
    | Jump addr -> { state with pc = addr }
    | SubroutineCall addr ->
        { state with stack = state.pc :: state.stack; pc = addr }
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
