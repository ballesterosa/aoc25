use std::fs;
use std::time::Instant;
use std::collections::HashMap;

#[inline]
fn part1(input: &str) -> u32 {
    let mut lines = input.lines().step_by(2);
    let first_line = lines.next().unwrap();

    let mut beams: Vec<bool> = first_line.bytes()
        .map(|b| b == b'S')
        .collect();

    let mut sum = 0;
    for line in lines {
        for (i, &b) in line.as_bytes().iter().enumerate() {
            if b == b'^' && beams[i] {
                sum += 1;
                beams[i] = false;
                // no need for bounds checking
                beams[i - 1] = true;
                beams[i + 1] = true;
            }
        }
    }
    sum
}

#[inline]
fn part2(input: &str) -> usize {
    let mut lines = input.lines().step_by(2);
    let first_line = lines.next().unwrap();

    let start_pos = first_line.bytes().position(|b| b == b'S').unwrap();
    let width = first_line.len();

    let splitter_rows: Vec<Vec<usize>> = lines
        .map(|line| line.bytes()
            .enumerate()
            .filter_map(|(i, b)| (b == b'^').then_some(i))
            .collect())
        .filter(|v: &Vec<usize>| !v.is_empty())
        .collect();

    let mut current_states = HashMap::new();
    let mut initial_state = vec![false; width];
    initial_state[start_pos] = true;
    current_states.insert(initial_state, 1);

    for splitters in splitter_rows {
        let mut next_states = HashMap::new();

        for (state, count) in current_states {
            let hits: Vec<usize> = splitters.iter()
                .copied()
                .filter(|&pos| state[pos])
                .collect();

            if hits.is_empty() {
                *next_states.entry(state).or_insert(0) += count;
            } else {
                for combo in 0..(1 << hits.len()) {
                    let mut new_state = state.clone();
                    for (i, &pos) in hits.iter().enumerate() {
                        new_state[pos] = false;
                        if (combo & (1 << i)) != 0 {
                            new_state[pos - 1] = true;
                        }
                        if (combo & (1 << i)) == 0 {
                            new_state[pos + 1] = true;
                        }
                    }
                    *next_states.entry(new_state).or_insert(0) += count;
                }
            }
        }
        current_states = next_states;
    }

    current_states.values().sum()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string("inputs/day7_in.txt")?;

    let start = Instant::now();
    let sum1 = part1(&input);
    println!("Part 1: {} ({:?})", sum1, start.elapsed());

    let start = Instant::now();
    let sum2 = part2(&input);
    println!("Part 2: {} ({:?})", sum2, start.elapsed());

    Ok(())
}