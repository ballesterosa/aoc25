(* Hardcaml Implementation of Day 1
 *
 * Division by 100 is implemented using reciprocal multiplication:
 * x / 100 ≈ (x * 0x028F5C29) >> 32
 * which is the standard hardware optimization for division by constants
 * found the magic number from this formula: ceil(2^32 / 100)
 *)

open! Core
open! Hardcaml
open! Signal

(* division by 100 using reciprocal multiplication
 * magic number from libdivide algorithm *)
let div_by_100 x =
  let open Signal in
  (* multiply by reciprocal for unsigned division by 100 *)
  let magic = of_int ~width:32 0x028F5C29 in
  let product = uresize (x *: magic) 64 in
  (* shift right by 32 to get quotient *)
  let shifted = srl product 32 in
  uresize shifted 32

(* modulo 100 using subtraction: x - (x/100)*100 *)
let mod_by_100 x =
  let open Signal in
  let quotient = div_by_100 x in
  let hundred = of_int ~width:32 100 in
  let mult_result = uresize (quotient *: hundred) 32 in
  x -: mult_result

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    valid : 'a;
    direction : 'a; (* 1 = right, 0 = left *)
    value : 'a [@bits 32];
  } [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = {
    curr_pos : 'a [@bits 32];
    count : 'a [@bits 32];
    part_2_count : 'a [@bits 32];
  } [@@deriving sexp_of, hardcaml]
end

let create (i : _ I.t) =
  let open Signal in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let spec_with_reset = Reg_spec.override spec ~clear_to:(of_int ~width:32 50) in

  (* helper function to compute next position *)
  let compute_next_pos curr_pos_val =
    let new_pos_right = curr_pos_val +: i.value in
    let result_right = mod_by_100 new_pos_right in

    (* left direction: handle negative wrapping *)
    let new_pos_left = curr_pos_val -: i.value in
    let is_negative = msb new_pos_left in

    (* for negative values: we need to compute ((new_pos_left % 100) + 100) % 100
     * due to lack of signed modulo, instead try: new_pos_left % 100 = new_pos_left - (new_pos_left/100)*100
     * for negative division truncating to zero: -127/100 = -1, so -127 % 100 = -127 - (-100) = -27
     * then (-27 + 100) % 100 = 73 % 100 = 73 *)

    (* Compute signed division: for negative numbers, negate, divide, negate result *)
    let is_neg = msb new_pos_left in
    let abs_val = mux2 is_neg (negate new_pos_left) new_pos_left in
    let abs_div = div_by_100 abs_val in
    let signed_div = mux2 is_neg (negate abs_div) abs_div in
    let hundred = of_int ~width:32 100 in
    let mult_result = uresize (signed_div *: hundred) 32 in
    let remainder = new_pos_left -: mult_result in
    (* remainder is now in range [-99, 99] for our inputs *)
    let adjusted = remainder +: hundred in
    let result_left_neg = mod_by_100 adjusted in

    let result_left = mux2 is_negative result_left_neg new_pos_left in
    mux2 i.direction result_right result_left
  in  (* State registers *)
  let curr_pos = reg_fb spec_with_reset ~enable:i.valid ~width:32 ~f:compute_next_pos in

  (* combinational logic for new position value (used for count and part_2_count) *)
  let next_pos = compute_next_pos curr_pos in

  let count = reg_fb spec ~enable:i.valid ~width:32 ~f:(fun count ->
    let should_increment = next_pos ==:. 0 in
    mux2 should_increment (count +:. 1) count
  ) in

  let part_2_count = reg_fb spec ~enable:i.valid ~width:32 ~f:(fun part_2_count ->
    let one = of_int ~width:32 1 in
    let zero = of_int ~width:32 0 in

    (* right: (curr_pos + value) / 100 *)
    let new_pos_right = curr_pos +: i.value in
    let increment_right = div_by_100 new_pos_right in

    (* left: more complex *)
    let new_pos_left = curr_pos -: i.value in
    (* check if new_pos_left <= 0: either negative (msb set) or equal to zero *)
    let is_negative = msb new_pos_left in
    let is_zero = new_pos_left ==:. 0 in
    let is_le_zero = is_negative |: is_zero in

    (* if new_pos <= 0 and curr_pos != 0: (value - curr_pos) / 100 + 1 *)
    (* if new_pos <= 0 and curr_pos == 0: value / 100 *)
    let diff = i.value -: curr_pos in
    let div_diff = div_by_100 diff in
    let increment_neg_nonzero = div_diff +: one in
    let increment_neg_zero = div_by_100 i.value in
    let curr_pos_is_zero = curr_pos ==:. 0 in
    let increment_neg = mux2 curr_pos_is_zero increment_neg_zero increment_neg_nonzero in
    let increment_left = mux2 is_le_zero increment_neg zero in

    let increment = mux2 i.direction increment_right increment_left in
    part_2_count +: increment
  ) in

  { O.curr_pos; count; part_2_count }

let simulate () =
  let module Sim = Cyclesim.With_interface(I)(O) in

  let sim = Sim.create create in

  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ O.t = Cyclesim.outputs sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  inputs.valid := Bits.gnd;
  Cyclesim.cycle sim;

  let lines = In_channel.read_lines "../day1_in.txt" in
  List.iter lines ~f:(fun line ->
    let direction = if Char.equal line.[0] 'R' then 1 else 0 in
    let value = Int.of_string (String.sub line ~pos:1 ~len:(String.length line - 1)) in

    inputs.valid := Bits.vdd;
    inputs.direction := Bits.of_int ~width:1 direction;
    inputs.value := Bits.of_int ~width:32 value;
    Cyclesim.cycle sim
  );  (* Deassert valid and cycle once more to let final values settle *)
  inputs.valid := Bits.gnd;
  Cyclesim.cycle sim;

  let curr_pos = Bits.to_int !(outputs.curr_pos) in
  let count = Bits.to_int !(outputs.count) in
  let part_2_count = Bits.to_int !(outputs.part_2_count) in

  (curr_pos, count, part_2_count, sim)

let generate_verilog () =
  let module Circuit = Circuit.With_interface(I)(O) in
  let circuit = Circuit.create_exn ~name:"day1_hw" create in
  let verilog = Rtl.output ~output_mode:(To_file "day1_hw/day1_hw.v") Verilog circuit in
  printf "Generated Verilog RTL: day1_hw/day1_hw.v\n";
  verilog

let () =
  let start_time = Time_ns.now () in

  let (curr_pos, count, part_2_count, _sim) = simulate () in

  let end_time = Time_ns.now () in
  let duration = Time_ns.diff end_time start_time in

  printf "curr pos:     %d\n" curr_pos;
  printf "count:        %d\n" count;
  printf "part_2_count: %d\n" part_2_count;
  printf "Time:         %s\n" (Time_ns.Span.to_string duration);
  printf "\n";

  let _ = generate_verilog () in
  printf "Verilog RTL successfully generated!\n"
