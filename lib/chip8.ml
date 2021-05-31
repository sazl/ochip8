
type instruction =
| Unknown
| ClearScreen
| SubroutineReturn
| Jump of Int32.t
| JumpWithIndex of Int32.t
| SubroutineCall of Int32.t
| SetReg of Int32.t * Int32.t
| AddReg of Int32.t * Int32.t
| SetRegIndex of Int32.t
| Draw of Int32.t * Int32.t * Int32.t
| SkipIfRegValueEqual of Int32.t * Int32.t
| SkipIfRegsEqual of Int32.t * Int32.t
| SkipIfRegsNotEqual of Int32.t * Int32.t
| RandomNumber of Int32.t * Int32.t
| SetRegXY of Int32.t * Int32.t
| OrRegXY of Int32.t * Int32.t
| AndRegXY of Int32.t * Int32.t
| XorRegXY of Int32.t * Int32.t
| AddRegXY of Int32.t * Int32.t
| SubRegXY of Int32.t * Int32.t
| ShiftRightRegXY of Int32.t * Int32.t
| SubRegYX of Int32.t * Int32.t
| ShiftLeftRegXY of Int32.t * Int32.t
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

let timers_update state =
    let del = if Int32.compare state.vdel 0l > 0 then Int32.pred state.vdel else 0l in
    let snd = if Int32.compare state.vsnd 0l > 0 then Int32.pred state.vsnd else 0l in
    {state with vdel = del; vsnd = snd }

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
    | 0x3000l ->
        let xi = instruction_get_nibble inst 2 in
        let num = instruction_get_last_nibbles inst 2 in
        SkipIfRegValueEqual (xi, num)
    | 0x5000l -> (
        let x = instruction_get_nibble inst 2 in
        let y = instruction_get_nibble inst 3 in
        SkipIfRegsEqual (x, y)
    )
    | 0x6000l ->
        let reg = instruction_get_nibble inst 2 in
        let num = instruction_get_last_nibbles inst 2 in
        SetReg (reg, num)
    | 0x7000l ->
        let reg = instruction_get_nibble inst 2 in
        let num = instruction_get_last_nibbles inst 2 in
        AddReg (reg, num)
    | 0X8000l -> (
        let sel = instruction_get_nibble inst 3 in
        let x = instruction_get_nibble inst 1 in
        let y = instruction_get_nibble inst 2 in
        match sel with
        | 0x0l -> SetRegXY (x, y)
        | 0x1l -> OrRegXY (x, y)
        | 0x2l -> AndRegXY (x, y)
        | 0x3l -> XorRegXY (x, y)
        | 0x4l -> AddRegXY (x, y)
        | 0x5l -> SubRegXY (x, y)
        | 0x6l -> ShiftRightRegXY (x, y)
        | 0x7l -> SubRegYX (x, y)
        | 0xEl -> ShiftLeftRegXY (x, y)
        | _ -> Unknown
    )
    | 0x9000l ->
        let x = instruction_get_nibble inst 2 in
        let y = instruction_get_nibble inst 3 in
        SkipIfRegsNotEqual (x, y)
    | 0xA000l ->
        let num = instruction_get_last_nibbles inst 3 in
        SetRegIndex num
    | 0xB000l ->
        let num = instruction_get_last_nibbles inst 3 in
        JumpWithIndex num
    | 0xC000l ->
        let x = instruction_get_nibble inst 2 in
        let num = instruction_get_last_nibbles inst 2 in
        RandomNumber (x, num)
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
    begin
        print_string "\x1b[?25l";
        for i = 0 to display_height - 1 do
            for j = 0 to display_width - 1 do
                if display.(i).(j) = 0 then
                    print_char ' '
                else
                    print_string "█";
            done;
            print_endline ""
        done;
        print_string "\x1B[64A"
    end

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


(* TODO: arith/log overflow and set VF *)

let execute_instruction state =
    match state.decoded with
    | Unknown -> state
    | ClearScreen -> display_clear state
    | SubroutineReturn ->
        let addr = List.hd state.stack in
        { state with pc = addr; stack = List.tl state.stack }
    | Jump addr -> { state with pc = addr }
    | JumpWithIndex num -> (
        let v0 = state.regs.(0x0) in
        let ind = Int32.add v0 num in
        { state with pc = ind }
    )
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
    | SkipIfRegValueEqual (x, num) -> (
        let xi = Int32.to_int x in
        let vx = state.regs.(xi) in
        if vx = num then
            { state with pc = Int32.add state.pc 2l }
        else
            state
    )
    | SkipIfRegsEqual (x, y) -> (
        let xi = Int32.to_int x in
        let yi = Int32.to_int y in
        let vx = state.regs.(xi) in
        let vy = state.regs.(yi) in
        if vx = vy then
            { state with pc = Int32.add state.pc 2l }
        else
            state
    )
    | SkipIfRegsNotEqual (x, y) -> (
        let xi = Int32.to_int x in
        let yi = Int32.to_int y in
        let vx = state.regs.(xi) in
        let vy = state.regs.(yi) in
        if vx != vy then
            { state with pc = Int32.add state.pc 2l }
        else
            state
    )
    | RandomNumber (x, num) -> (
        let xi = Int32.to_int x in
        let n = Int32.to_int num in
        let vx = state.regs.(xi) in
        let rnd = Int32.of_int @@ Random.int n in
        let res = Int32.logand rnd vx in
        let _ = state.regs.(xi) <- res in
        state
    )
    | SetRegXY (x, y) ->
        let xi = Int32.to_int x in
        let yi = Int32.to_int y in
        let vy = state.regs.(yi) in
        let _ = state.regs.(xi) <- vy in
        state
    | OrRegXY (x, y) ->
        let xi = Int32.to_int x in
        let yi = Int32.to_int y in
        let vx = state.regs.(xi) in
        let vy = state.regs.(yi) in
        let _ = state.regs.(xi) <- Int32.logor vx vy in
        state
    | AndRegXY (x, y) ->
        let xi = Int32.to_int x in
        let yi = Int32.to_int y in
        let vx = state.regs.(xi) in
        let vy = state.regs.(yi) in
        let _ = state.regs.(xi) <- Int32.logand vx vy in
        state
    | XorRegXY (x, y) ->
        let xi = Int32.to_int x in
        let yi = Int32.to_int y in
        let vx = state.regs.(xi) in
        let vy = state.regs.(yi) in
        let _ = state.regs.(xi) <- Int32.logxor vx vy in
        state
    | AddRegXY (x, y) ->
        let xi = Int32.to_int x in
        let yi = Int32.to_int y in
        let vx = state.regs.(xi) in
        let vy = state.regs.(yi) in
        let _ = state.regs.(xi) <- Int32.add vx vy in
        state
    | SubRegXY (x, y) ->
        let xi = Int32.to_int x in
        let yi = Int32.to_int y in
        let vx = state.regs.(xi) in
        let vy = state.regs.(yi) in
        let _ = state.regs.(xi) <- Int32.sub vx vy in
        state
    | ShiftRightRegXY (x, y) ->
        let xi = Int32.to_int x in
        let yi = Int32.to_int y in
        let vy = state.regs.(yi) in
        let _ = state.regs.(xi) <- Int32.shift_right vy 1 in
        state
    | SubRegYX (x, y) ->
        let xi = Int32.to_int x in
        let yi = Int32.to_int y in
        let vx = state.regs.(xi) in
        let vy = state.regs.(yi) in
        let _ = state.regs.(xi) <- Int32.sub vy vx in
        state
    | ShiftLeftRegXY (x, y) ->
        let xi = Int32.to_int x in
        let yi = Int32.to_int y in
        let vy = state.regs.(yi) in
        let _ = state.regs.(xi) <- Int32.shift_left vy 1 in
        state

let execute state =
    let mod_state = execute_instruction state in
    timers_update mod_state