(* Hardcaml Implementation of Day 6 Part 1
 *
 * stream input, parse numbers and operators, store per column,
 * compute each column result (multiply or add), sum everything
 *)

open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a [@bits 1]
    ; data_valid : 'a [@bits 1]
    ; data_byte : 'a [@bits 8]
    ; input_done : 'a [@bits 1]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a [@bits 1]
    ; done_ : 'a [@bits 1]
    ; grand_total : 'a [@bits 64]
    }
  [@@deriving sexp_of, hardcaml]
end

let create (i : Signal.t I.t) =
  let open Signal in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

  let ascii_space = 32 in
  let ascii_newline = 10 in
  let ascii_0 = 48 in
  let ascii_9 = 57 in
  let ascii_star = 42 in
  let ascii_plus = 43 in

  let state = wire 3 in

  let is_digit = (i.data_byte >=:. ascii_0) &: (i.data_byte <=:. ascii_9) in
  let is_space = i.data_byte ==:. ascii_space in
  let is_newline = i.data_byte ==:. ascii_newline in
  let is_star = i.data_byte ==:. ascii_star in
  let is_plus = i.data_byte ==:. ascii_plus in
  let is_operator = is_star |: is_plus in
  let parse_enable = i.data_valid &: (state ==:. 1) in

  let row = reg_fb spec ~width:3 ~enable:parse_enable ~f:(fun r ->
    mux2 is_newline
      (mux2 (r ==:. 4) (of_int ~width:3 0) (r +:. 1))
      r) in

  let col = wire 12 in

  let current_num = reg_fb spec ~width:32 ~enable:parse_enable ~f:(fun num ->
    let digit_val = uresize (i.data_byte -:. ascii_0) 32 in
    let times_ten = uresize (num *: of_int ~width:32 10) 32 in
    mux2 is_digit
      (times_ten +: digit_val)
      (of_int ~width:32 0)) in

  let num_done = parse_enable &: (current_num <>:. 0) &: (is_space |: is_newline) in
  let col_bits = 10 in
  let compute_enable = wire 1 in
  let compute_col = wire 12 in
  let read_addr = mux2 compute_enable compute_col col in

  let make_row_storage row_idx =
    multiport_memory (1 lsl col_bits)
      ~write_ports:[| { Write_port.
        write_clock = i.clock;
        write_address = uresize col col_bits;
        write_enable = num_done &: (row ==:. row_idx);
        write_data = current_num;
      } |]
      ~read_addresses:[| uresize read_addr col_bits |] in

  let row0_data = make_row_storage 0 in
  let row1_data = make_row_storage 1 in
  let row2_data = make_row_storage 2 in
  let row3_data = make_row_storage 3 in

  let ops_data = multiport_memory (1 lsl col_bits)
    ~write_ports:[| { Write_port.
      write_clock = i.clock;
      write_address = uresize col col_bits;
      write_enable = parse_enable &: (row ==:. 4) &: is_operator;
      write_data = uresize is_star 32;
    } |]
    ~read_addresses:[| uresize read_addr col_bits |] in

  let col_advance = num_done |: (parse_enable &: (row ==:. 4) &: is_operator) in
  let col_reg = reg_fb spec ~width:12 ~enable:parse_enable ~f:(fun c ->
    mux2 is_newline
      (of_int ~width:12 0)
      (mux2 col_advance (c +:. 1) c)) in
  let () = col <== col_reg in
  let next_col = col_reg in

  let max_col = reg_fb spec ~width:12 ~enable:parse_enable ~f:(fun mc ->
    mux2 (next_col >: mc) next_col mc) in

  let () = compute_enable <== (state ==:. 2) in
  let compute_col_reg = reg_fb spec ~width:12 ~enable:compute_enable ~f:(fun cc ->
    mux2 (cc <: max_col) (cc +:. 1) cc) in
  let () = compute_col <== compute_col_reg in

  let all_done = compute_enable &: (compute_col_reg >=: max_col) in
  let state_reg = reg_fb spec ~width:3 ~enable:vdd ~f:(fun st ->
    mux st [
      mux2 i.start (of_int ~width:3 1) (of_int ~width:3 0);
      mux2 i.input_done (of_int ~width:3 2) (of_int ~width:3 1);
      mux2 all_done (of_int ~width:3 3) (of_int ~width:3 2);
      of_int ~width:3 3;
    ]) in
  let () = state <== state_reg in

  let read_v0 = reg spec ~enable:compute_enable (uresize row0_data.(0) 64) in
  let read_v1 = reg spec ~enable:compute_enable (uresize row1_data.(0) 64) in
  let read_v2 = reg spec ~enable:compute_enable (uresize row2_data.(0) 64) in
  let read_v3 = reg spec ~enable:compute_enable (uresize row3_data.(0) 64) in
  let read_op = reg spec ~enable:compute_enable ops_data.(0) in

  let is_mult = bit read_op 0 in  (* 1 for *, 0 for + *)
  let mult_result =
    let m1 = uresize (read_v0 *: read_v1) 64 in
    let m2 = uresize (m1 *: read_v2) 64 in
    uresize (m2 *: read_v3) 64 in
  let add_result = read_v0 +: read_v1 +: read_v2 +: read_v3 in
  let col_result = mux2 is_mult mult_result add_result in

  let grand_total = reg_fb spec ~width:64 ~enable:compute_enable ~f:(fun total ->
    total +: col_result) in

  { O.
    ready = state ==:. 0;
    done_ = state ==:. 3;
    grand_total = grand_total;
  }

let generate_verilog () =
  let module Circuit = Circuit.With_interface(I)(O) in
  let circuit = Circuit.create_exn ~name:"day6_hw_part1" create in
  let _verilog = Rtl.output ~output_mode:(To_file "day6_hw/day6_hw_part1.v") Verilog circuit in
  printf "generated Verilog RTL: day6_hw/day6_hw_part1.v\n"

(* testbench *)
let () =
  let module Sim = Cyclesim.With_interface(I)(O) in
  let sim = Sim.create create in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let input_file = "../inputs/day6_in.txt" in
  let input_data = In_channel.read_all input_file in
  let bytes = String.to_array input_data in
  printf "processing %d bytes from %s\n" (Array.length bytes) input_file;

  (* reset *)
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;

  (* start *)
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;

  Array.iteri bytes ~f:(fun _idx byte ->
    inputs.data_valid := Bits.vdd;
    inputs.data_byte := Bits.of_int ~width:8 (Char.to_int byte);
    Cyclesim.cycle sim;
  );
  inputs.data_valid := Bits.gnd;
  inputs.input_done := Bits.vdd;
  Cyclesim.cycle sim;

  let max_cycles = 100000 in
  let rec wait_done cycle =
    if cycle > max_cycles then
      failwith "timeout waiting for done"
    else if Bits.to_bool !(outputs.done_) then
      cycle
    else begin
      Cyclesim.cycle sim;
      wait_done (cycle + 1)
    end in

  let cycles = wait_done 0 in

  let total = Bits.to_int64 !(outputs.grand_total) in
  printf "\n=== result ===\n";
  printf "total: %Ld\n" total;
  printf "completed in %d cycles\n" cycles;

  let _ = generate_verilog () in
  printf "verilog RTL successfully generated!\n"
