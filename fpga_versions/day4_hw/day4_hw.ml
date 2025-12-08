(* Hardcaml Implementation of Day 4 *)
open! Core
open! Hardcaml
open! Signal

(* parser - converts ASCII to binary grid *)
module Parser = struct
  module I = struct
    type 'a t = {
      clock : 'a;
      clear : 'a;
      valid : 'a;
      char_data : 'a [@bits 8]; (* ASCII char *)
    } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      is_at : 'a; (* 1 if '@' else 0 *)
      valid_out : 'a;
    } [@@deriving sexp_of, hardcaml]
  end

  let create (i : _ I.t) =
    (* '@' is ASCII 64 (0x40) *)
    let is_at = i.char_data ==:. 64 in
    let valid_out = reg_fb (Reg_spec.create ~clock:i.clock ~clear:i.clear ()) ~width:1 ~f:(fun _ -> i.valid) in
    { O.is_at; valid_out }
end

(* counts adjacent cells *)
module Core = struct
  module I = struct
    type 'a t = {
      clock : 'a;
      clear : 'a;
      valid : 'a;
      (* 3x3 window around current cell *)
      window : 'a [@bits 9]; (* bit order: [nw, n, ne, w, center, e, sw, s, se] *)
    } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      adjacent_count : 'a [@bits 4];
      has_less_than_4 : 'a;
    } [@@deriving sexp_of, hardcaml]
  end

  let create (i : _ I.t) =
    (* extract the 9 bits from window using bit select *)
    let nw = select i.window 0 0 in
    let n  = select i.window 1 1 in
    let ne = select i.window 2 2 in
    let w  = select i.window 3 3 in
    let center = select i.window 4 4 in
    let e  = select i.window 5 5 in
    let sw = select i.window 6 6 in
    let s  = select i.window 7 7 in
    let se = select i.window 8 8 in

    (* count adjacent '@'s *)
    let count = uresize nw 4 +: uresize n 4 +: uresize ne 4 +:
                uresize w 4 +: uresize e 4 +:
                uresize sw 4 +: uresize s 4 +: uresize se 4 in

    let count_lt_4 = count <:. 4 in
    let center_not_empty = center in
    let has_less_than_4 = count_lt_4 &: center_not_empty in

    { O.adjacent_count = count; has_less_than_4 }
end

(* part 1 module that processes entire grid with line buffers *)
module GridProcessor = struct
  module I = struct
    type 'a t = {
      clock : 'a;
      clear : 'a;
      valid : 'a;
      char_data : 'a [@bits 8]; (* ASCII character input stream *)
      grid_cols : 'a [@bits 8]; (* total cols for line buffer sizing *)
    } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      adjacent_count : 'a [@bits 4];
      has_less_than_4 : 'a;
      valid_out : 'a;
      window : 'a [@bits 9];
    } [@@deriving sexp_of, hardcaml]
  end

  let create (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in

    let parser_out = Parser.create { Parser.I.clock = i.clock; clear = i.clear; valid = i.valid; char_data = i.char_data } in
    let parsed_bit = parser_out.is_at in

    (* line buffers: we need 3 rows to form a 3x3 window
     * architecture: [row2] <- [row1] <- [row0] <- parsed_bit
     * when we have 3 full rows, we can start extracting windows *)

    (* using shift registers as line buffers *)
    let line_length = 139 in (* max grid width + 1 for safety *)

    (* curr row buffer (row 0) *)
    let row0 = Array.init line_length ~f:(fun _ ->
      wire 1
    ) in

    (* prev row buffer (row 1) *)
    let row1 = Array.init line_length ~f:(fun _ ->
      wire 1
    ) in

    (* two rows back (row 2) *)
    let row2 = Array.init line_length ~f:(fun _ ->
      wire 1
    ) in

    (* shift data through line buffers on valid cycles *)
    for idx = 0 to line_length - 1 do
      if idx = 0 then begin
        row0.(0) <== reg spec ~enable:i.valid parsed_bit;
        row1.(0) <== reg spec ~enable:i.valid row0.(line_length - 1);
        row2.(0) <== reg spec ~enable:i.valid row1.(line_length - 1);
      end else begin
        row0.(idx) <== reg spec ~enable:i.valid row0.(idx - 1);
        row1.(idx) <== reg spec ~enable:i.valid row1.(idx - 1);
        row2.(idx) <== reg spec ~enable:i.valid row2.(idx - 1);
      end
    done;

    (* extract 3x3 window from the buffers
     * window center is at position [1,1] relative to newest data
     *
     * row2[2] row2[1] row2[0]  (oldest)
     * row1[2] row1[1] row1[0]
     * row0[2] row0[1] row0[0]  (newest)
     *)

    (* build window bits: [nw, n, ne, w, center, e, sw, s, se] *)
    let nw = row2.(2) in
    let n  = row2.(1) in
    let ne = row2.(0) in
    let w  = row1.(2) in
    let center = row1.(1) in
    let e  = row1.(0) in
    let sw = row0.(2) in
    let s  = row0.(1) in
    let se = row0.(0) in

    let window = concat_msb [ se; s; sw; e; center; w; ne; n; nw ] in

    (* feed window to core logic *)
    let core_out = Core.create {
      Core.I.clock = i.clock;
      clear = i.clear;
      valid = i.valid;
      window = window;
    } in

    (* pipeline valid signal to match processing delay *)
    let valid_delayed = reg spec ~enable:vdd (reg spec ~enable:vdd (reg spec ~enable:vdd i.valid)) in

    { O.adjacent_count = core_out.adjacent_count;
      has_less_than_4 = core_out.has_less_than_4;
      valid_out = valid_delayed;
      window = window;
    }
end

let simulate () =
  let lines = In_channel.read_lines "../inputs/day4_in.txt" in
  let n = List.length lines in
  let m = String.length (List.hd_exn lines) in

  printf "grid size: %dx%d\n" n m;

  (* use GridProcessor hardware to process entire stream *)
  let module GridSim = Cyclesim.With_interface(GridProcessor.I)(GridProcessor.O) in
  let grid_sim = GridSim.create GridProcessor.create in
  let grid_inputs = Cyclesim.inputs grid_sim in
  let grid_outputs = Cyclesim.outputs grid_sim in

  grid_inputs.valid := Bits.vdd;
  grid_inputs.grid_cols := Bits.of_int ~width:8 m;

  (* stream all characters through the hardware pipeline *)
  let char_count = ref 0 in
  let part1_results = ref [] in

  (* Part 1 *)
  printf "streaming grid through hardware pipeline\n";
  List.iter lines ~f:(fun line ->
    String.iter line ~f:(fun c ->
      grid_inputs.char_data := Bits.of_int ~width:8 (Char.to_int c);
      Cyclesim.cycle grid_sim;
      char_count := !char_count + 1;

      (* get results when valid *)
      let valid_out = Bits.to_int !(grid_outputs.valid_out) in
      if valid_out = 1 then begin
        let has_lt4 = Bits.to_int !(grid_outputs.has_less_than_4) in
        part1_results := has_lt4 :: !part1_results
      end
    )
  );

  (* flush the pipeline *)
  grid_inputs.char_data := Bits.of_int ~width:8 0;
  for _ = 1 to (m * 3) do  (* flush 3 rows worth *)
    Cyclesim.cycle grid_sim;
    let valid_out = Bits.to_int !(grid_outputs.valid_out) in
    if valid_out = 1 then begin
      let has_lt4 = Bits.to_int !(grid_outputs.has_less_than_4) in
      part1_results := has_lt4 :: !part1_results
    end
  done;

  let part1_count = List.fold !part1_results ~init:0 ~f:(+) in
  printf "processed %d characters through hardware pipeline\n" !char_count;
  printf "part 1 (streaming hardware): %d cells with < 4 neighbors\n" part1_count;

  (* Part 2 *)
  printf "\npart 2 (iterative)\n";

  (* make initial grid by parsing through hardware *)
  let grid2 = Array.make_matrix ~dimx:n ~dimy:m 0 in
  let module ParserSim = Cyclesim.With_interface(Parser.I)(Parser.O) in
  let parser_sim = ParserSim.create Parser.create in
  let parser_inputs = Cyclesim.inputs parser_sim in
  let parser_outputs = Cyclesim.outputs parser_sim in

  parser_inputs.valid := Bits.vdd;
  List.iteri lines ~f:(fun i line ->
    String.iteri line ~f:(fun j c ->
      parser_inputs.char_data := Bits.of_int ~width:8 (Char.to_int c);
      Cyclesim.cycle parser_sim;
      grid2.(i).(j) <- Bits.to_int !(parser_outputs.is_at)
    )
  );

  (* uses Core for iterative removal *)
  let module CoreSim = Cyclesim.With_interface(Core.I)(Core.O) in
  let core_sim = CoreSim.create Core.create in
  let core_inputs = Cyclesim.inputs core_sim in
  let core_outputs = Cyclesim.outputs core_sim in

  let part2_count = ref 0 in
  let iteration = ref 0 in

  core_inputs.valid := Bits.vdd;

  let rec iterate_removal () =
    iteration := !iteration + 1;
    let removal_candidates = ref [] in

    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        if grid2.(i).(j) <> 0 then begin
          let get_cell di dj =
            let ni = i + di in
            let nj = j + dj in
            if ni >= 0 && ni < n && nj >= 0 && nj < m then grid2.(ni).(nj) else 0
          in
          let window_bits =
            get_cell (-1) (-1) lor (get_cell (-1) 0 lsl 1) lor (get_cell (-1) 1 lsl 2) lor
            (get_cell 0 (-1) lsl 3) lor (get_cell 0 0 lsl 4) lor (get_cell 0 1 lsl 5) lor
            (get_cell 1 (-1) lsl 6) lor (get_cell 1 0 lsl 7) lor (get_cell 1 1 lsl 8)
          in

          core_inputs.window := Bits.of_int ~width:9 window_bits;
          Cyclesim.cycle core_sim;

          if Bits.to_int !(core_outputs.has_less_than_4) = 1 then
            removal_candidates := (i, j) :: !removal_candidates
        end
      done
    done;

    let removed_count = List.length !removal_candidates in

    if removed_count > 0 then begin
      List.iter !removal_candidates ~f:(fun (i, j) ->
        grid2.(i).(j) <- 0
      );

      part2_count := !part2_count + removed_count;
      printf "iteration %d: identified and removed %d cells\n" !iteration removed_count;
      iterate_removal ()
    end else begin
      printf "iteration %d: no change, done\n" !iteration
    end
  in

  iterate_removal ();
  (part1_count, !part2_count)

let generate_verilog () =
  let module Circuit = Circuit.With_interface(Core.I)(Core.O) in
  let circuit = Circuit.create_exn ~name:"day4_hw" Core.create in
  let _verilog = Rtl.output ~output_mode:(To_file "day4_hw/day4_hw.v") Verilog circuit in
  printf "generated verilog RTL: day4_hw/day4_hw.v\n";

  let module ParserCircuit = Hardcaml.Circuit.With_interface(Parser.I)(Parser.O) in
  let parser_circuit = ParserCircuit.create_exn ~name:"day4_parser" Parser.create in
  let _ = Rtl.output ~output_mode:(To_file "day4_hw/day4_parser.v") Verilog parser_circuit in
  printf "generated verilog RTL: day4_hw/day4_parser.v\n";

  let module GridCircuit = Hardcaml.Circuit.With_interface(GridProcessor.I)(GridProcessor.O) in
  let grid_circuit = GridCircuit.create_exn ~name:"day4_grid_processor" GridProcessor.create in
  let _ = Rtl.output ~output_mode:(To_file "day4_hw/day4_grid_processor.v") Verilog grid_circuit in
  printf "generated verilog RTL: day4_hw/day4_grid_processor.v\n"

let () =
  let start_time = Time_ns.now () in

  let (part1, part2) = simulate () in

  let end_time = Time_ns.now () in
  let duration = Time_ns.diff end_time start_time in

  printf "part 1 sum:  %d\n" part1;
  printf "part 2 sum:  %d\n" part2;
  printf "time:        %s\n" (Time_ns.Span.to_string duration);
  printf "\n";

  let _ = generate_verilog () in
  printf "verilog RTL successfully generated!\n"
