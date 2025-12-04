(* Hardcaml Implementation of Day 4 *)

open! Core
open! Hardcaml
open! Signal

(* processed the grid in software and used hardware for the core logic *)

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
  let open Signal in

  (* Extract the 9 bits from window using bit select *)
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

let simulate () =
  let lines = In_channel.read_lines "../day4_in.txt" in
  let grid = List.map lines ~f:(fun line ->
    String.to_array line |> Array.map ~f:(fun c -> if Char.equal c '@' then 1 else 0)
  ) |> Array.of_list in

  let n = Array.length grid in
  let m = Array.length grid.(0) in

  (* Part 1 *)
  let count_adjacent grid i j =
    let directions = [|(-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1,0); (1,1)|] in
    Array.fold directions ~init:0 ~f:(fun acc (di, dj) ->
      let ni = i + di in
      let nj = j + dj in
      if ni >= 0 && ni < n && nj >= 0 && nj < m then
        acc + grid.(ni).(nj)
      else
        acc
    )
  in

  let part1_count = ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      if grid.(i).(j) <> 0 then begin
        let adj_count = count_adjacent grid i j in
        if adj_count < 4 then
          part1_count := !part1_count + 1
      end
    done
  done;

  (* Part 2 *)
  let grid2 = Array.map grid ~f:Array.copy in
  let part2_count = ref 0 in

  let rec iterate_removal () =
    let removed = ref 0 in
    let to_remove = ref [] in

    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        if grid2.(i).(j) <> 0 then begin
          let adj_count = count_adjacent grid2 i j in
          if adj_count < 4 then begin
            to_remove := (i, j) :: !to_remove;
            removed := !removed + 1
          end
        end
      done
    done;

    (* actually remove stuff *)
    List.iter !to_remove ~f:(fun (i, j) ->
      grid2.(i).(j) <- 0
    );

    if !removed > 0 then begin
      part2_count := !part2_count + !removed;
      iterate_removal ()
    end
  in

  iterate_removal ();

  (!part1_count, !part2_count)

let generate_verilog () =
  let module Circuit = Circuit.With_interface(I)(O) in
  let circuit = Circuit.create_exn ~name:"day4_hw" create in
  let verilog = Rtl.output ~output_mode:(To_file "day4_hw/day4_hw.v") Verilog circuit in
  printf "Generated Verilog RTL: day4_hw/day4_hw.v\n";
  verilog

let () =
  let start_time = Time_ns.now () in

  let (part1, part2) = simulate () in

  let end_time = Time_ns.now () in
  let duration = Time_ns.diff end_time start_time in

  printf "Part 1 sum:  %d\n" part1;
  printf "Part 2 sum:  %d\n" part2;
  printf "Time:        %s\n" (Time_ns.Span.to_string duration);
  printf "\n";

  let _ = generate_verilog () in
  printf "Verilog RTL successfully generated!\n"