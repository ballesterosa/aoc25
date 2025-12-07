(* Hardcaml Implementation of Day 6 Part 2
 *
 * follows the Python implementation:
 * 1. load entire input into a single memory array
 * 2. find row_len by scanning for first newline
 * 3. calculate row offsets: [0, row_len+1, 2*(row_len+1), ...]
 * 4. for each column:
 *    - read operator from row 4
 *    - for each offset position, build number from digits in rows 0-3
 *    - apply operator to accumulate column result
 * 5. sum all column results
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

  let ascii_newline = 10 in
  let ascii_0 = 48 in
  let ascii_9 = 57 in
  let ascii_star = 42 in
  let ascii_plus = 43 in

  (* states: 0=idle, 1=loading, 2=find_row_len, 3=init_col, 4=read_op_addr, 5=wait_op, 6=read_op_data,
   *         7=check_op, 8=read_rows, 9=check_num, 10=accumulate, 11=next_col, 12=done *)
  let state = wire 4 in

  let max_input_size = 65536 in
  let input_addr_bits = 16 in

  let load_counter = reg_fb spec ~width:input_addr_bits
    ~enable:(i.data_valid &: (state ==:. 1))
    ~f:(fun c -> c +:. 1) in

  let read_addr = wire input_addr_bits in

  let input_memory = multiport_memory max_input_size
    ~write_ports:[| { Write_port.
      write_clock = i.clock;
      write_address = load_counter;
      write_enable = i.data_valid &: (state ==:. 1);
      write_data = uresize i.data_byte 8;
    } |]
    ~read_addresses:[| read_addr |] in

  let mem_data = uresize input_memory.(0) 8 in

  (* find row_len by scanning for first newline *)
  let scan_counter = reg_fb spec ~width:input_addr_bits
    ~enable:(state ==:. 2)
    ~f:(fun c -> c +:. 1) in

  let found_newline = (state ==:. 2) &: (mem_data ==:. ascii_newline) in

  let row_len = reg_fb spec ~width:16 ~enable:found_newline
    ~f:(fun _ -> uresize scan_counter 16) in

  (* calculate row offset helper - row_offsets[i] = i * (row_len + 1) *)
  let row_offset row_idx =
    let row_idx_sig = of_int ~width:16 row_idx in
    uresize (row_idx_sig *: (row_len +:. 1)) input_addr_bits in

  (* process columns *)
  let col = reg_fb spec ~width:12 ~enable:(state ==:. 11) ~f:(fun c ->
    mux2 (c <: (uresize row_len 12 -:. 1)) (c +:. 1) c) in

  let curr_off = reg_fb spec ~width:8 ~enable:vdd ~f:(fun off ->
    mux2 (state ==:. 3)
      (of_int ~width:8 0)
      (mux2 (state ==:. 10) (off +:. 1) off)) in

  let operator = reg spec ~enable:(state ==:. 6) mem_data in
  let is_mult = operator ==:. ascii_star in
  let is_plus = operator ==:. ascii_plus in
  let is_valid_op = is_mult |: is_plus in

  let parse_row = reg_fb spec ~width:3 ~enable:(state ==:. 8) ~f:(fun r ->
    mux2 (r ==:. 3) (of_int ~width:3 0) (r +:. 1)) in

  let current_num_wire = wire 64 in
  let current_num = reg_fb spec ~width:64 ~enable:vdd ~f:(fun _num ->
    current_num_wire) in

  let () = current_num_wire <==
    mux2 ((state ==:. 4) |: (state ==:. 10))
      (of_int ~width:64 0)
      (mux2 (state ==:. 8)
        (let is_digit = (mem_data >=:. ascii_0) &: (mem_data <=:. ascii_9) in
         let digit_val = uresize (mem_data -:. ascii_0) 64 in
         let times_ten = uresize (current_num *: of_int ~width:64 10) 64 in
         mux2 is_digit (times_ten +: digit_val) current_num)
        current_num) in

  let column_result = reg_fb spec ~width:64 ~enable:vdd ~f:(fun res ->
    mux2 (state ==:. 7)
      (mux2 is_mult (of_int ~width:64 1) (of_int ~width:64 0))
      (mux2 (state ==:. 10)
        (mux2 is_mult (uresize (res *: current_num) 64) (res +: current_num))
        res)) in

  let grand_total = reg_fb spec ~width:64 ~enable:(state ==:. 11) ~f:(fun total ->
    total +: column_result) in

  let all_cols_done = (state ==:. 11) &: (col >=: (uresize row_len 12 -:. 1)) in
  let num_is_zero = current_num ==:. 0 in
  let finished_4_rows = (parse_row ==:. 3) &: (state ==:. 8) in  (* After processing row 3 *)

  let state_reg = reg_fb spec ~width:4 ~enable:vdd ~f:(fun st ->
    mux st [
      mux2 i.start (of_int ~width:4 1) (of_int ~width:4 0);  (* 0: idle *)
      mux2 i.input_done (of_int ~width:4 2) (of_int ~width:4 1);  (* 1: loading *)
      mux2 found_newline (of_int ~width:4 3) (of_int ~width:4 2);  (* 2: find row_len *)
      of_int ~width:4 4;  (* 3: init column *)
      of_int ~width:4 5;  (* 4: set address to read operator *)
      of_int ~width:4 6;  (* 5: wait for memory *)
      of_int ~width:4 7;  (* 6: latch operator data *)
      mux2 is_valid_op (of_int ~width:4 8) (of_int ~width:4 11);  (* 7: check if operator is valid *)
      mux2 finished_4_rows (of_int ~width:4 9) (of_int ~width:4 8);  (* 8: read rows 0-3 *)
      mux2 num_is_zero (of_int ~width:4 11) (of_int ~width:4 10);  (* 9: check if num is zero *)
      of_int ~width:4 8;  (* 10: accumulate and loop back to read next number (state 8) *)
      mux2 all_cols_done (of_int ~width:4 12) (of_int ~width:4 3);  (* 11: next col *)
      of_int ~width:4 12;  (* 12: done *)
    ]) in
  let () = state <== state_reg in

  (* calculate memory read address based on state and parse_row *)
  let row_for_addr = mux parse_row [
    row_offset 0; row_offset 1; row_offset 2; row_offset 3;
  ] in

  let compute_addr =
    mux2 ((state ==:. 4) |: (state ==:. 5) |: (state ==:. 6) |: (state ==:. 7))
      (* read operator: row_offset[4] + col *)
      ((row_offset 4) +: uresize col input_addr_bits)
      (* read digit: row_offset[parse_row] + col + curr_off *)
      (row_for_addr +: uresize col input_addr_bits +: uresize curr_off input_addr_bits) in

  let () = read_addr <==
    mux2 (state ==:. 2)
      scan_counter
      compute_addr in

  { O.
    ready = state ==:. 0;
    done_ = state ==:. 12;
    grand_total = grand_total;
  }

let generate_verilog () =
  let module Circuit = Circuit.With_interface(I)(O) in
  let circuit = Circuit.create_exn ~name:"day6_hw_part2" create in
  let _verilog = Rtl.output ~output_mode:(To_file "day6_hw/day6_hw_part2.v") Verilog circuit in
  printf "generated Verilog RTL: day6_hw/day6_hw_part2.v\n"

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

  inputs.input_done := Bits.gnd;
  Array.iteri bytes ~f:(fun _idx byte ->
    inputs.data_valid := Bits.vdd;
    inputs.data_byte := Bits.of_int ~width:8 (Char.to_int byte);
    Cyclesim.cycle sim;
  );

  inputs.data_valid := Bits.gnd;
  inputs.input_done := Bits.vdd;
  Cyclesim.cycle sim;

  let max_cycles = 1000000 in
  let rec wait_done cycle =
    if cycle > max_cycles then
      failwith "timeout waiting for done"
    else if Bits.to_bool !(outputs.done_) then
      cycle
    else begin
      Cyclesim.cycle sim;
      wait_done (cycle + 1)
    end in  let cycles = wait_done 0 in

  let total = Bits.to_int64 !(outputs.grand_total) in
  printf "\n=== result ===\n";
  printf "total: %Ld\n" total;
  printf "completed in %d cycles\n" cycles;

  let _ = generate_verilog () in
  printf "verilog RTL successfully generated!\n"
