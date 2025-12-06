use std::fs;
use std::time::Instant;

struct Input {
    numbers: Vec<Vec<u64>>,
    operators: Vec<char>
}

fn part1_parse_input(input: &str) -> Input {
    let mut numbers = Vec::with_capacity(4);
    let mut operators = Vec::new();

    for line in input.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        if let Some(first_char) = line.chars().next() {
            if first_char == '*' || first_char == '+' {
                operators = line.as_bytes()
                    .iter()
                    .filter(|&&b| b == b'*' || b == b'+')
                    .map(|&b| b as char)
                    .collect();
            } else if first_char.is_ascii_digit() {
                let row: Vec<u64> = line.split_whitespace()
                    .filter_map(|s| s.parse().ok())
                    .collect();
                numbers.push(row);
            }
        }
    }

    let transposed = transpose(numbers);

    Input { numbers: transposed, operators }
}

fn part2_parse_input(input: &str) -> Input {
    let mut lines_vec: Vec<Vec<char>> = Vec::new();
    let mut operators = Vec::new();

    for line in input.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        if line.contains('*') || line.contains('+') {
            operators = line.split_whitespace()
                .map(|s| s.chars().next().unwrap())
                .collect();
        } else {
            lines_vec.push(line.chars().collect());
        }
    }

    // find columns where ALL rows have a space
    let max_len = lines_vec.iter().map(|l| l.len()).max().unwrap_or(0);
    let mut separator_positions = Vec::with_capacity(operators.len() + 1);
    separator_positions.push(0);

    for col in 0..max_len {
        let all_space = lines_vec.iter().all(|line| {
            col >= line.len() || line[col] == ' '
        });

        if all_space {
            if let Some(&last_sep) = separator_positions.last() {
                if col > last_sep {
                    separator_positions.push(col);
                }
            }
        }
    }
    separator_positions.push(max_len);

    // extract numbers from each equation block
    let mut numbers = Vec::with_capacity(operators.len());
    for i in 0..separator_positions.len() - 1 {
        let start = separator_positions[i];
        let end = separator_positions[i + 1];

        let mut equation_numbers = Vec::new();

        for col in start..end {
            let mut num_val: u64 = 0;
            let mut has_digit = false;

            for line in &lines_vec {
                if col < line.len() {
                    let ch = line[col];
                    if ch.is_ascii_digit() {
                        num_val = num_val * 10 + (ch as u64 - '0' as u64);
                        has_digit = true;
                    } else if has_digit {
                        break;
                    }
                }
            }

            if has_digit {
                equation_numbers.push(num_val);
            }
        }

        if !equation_numbers.is_empty() {
            numbers.push(equation_numbers);
        }
    }

    Input { numbers, operators }
}

fn transpose(matrix: Vec<Vec<u64>>) -> Vec<Vec<u64>> {
    if matrix.is_empty() {
        return vec![];
    }

    let rows = matrix.len();
    let cols = matrix[0].len();

    (0..cols).map(|col| {
        (0..rows).map(|row| matrix[row][col]).collect()
    }).collect()
}

#[inline]
fn solve(parsed: &Input) -> u64 {
    parsed.operators.iter().enumerate().map(|(i, &op)| {
        if op == '+' {
            parsed.numbers[i].iter().sum::<u64>()
        } else {
            parsed.numbers[i].iter().product::<u64>()
        }
    }).sum()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Part 1
    let start_time = Instant::now();
    let input = fs::read_to_string("day6_in.txt")?;
    let parsed = part1_parse_input(&input);
    let sum1 = solve(&parsed);
    let duration1 = start_time.elapsed();

    // Part 2
    let start_time = Instant::now();
    let parsed2 = part2_parse_input(&input);
    let sum2 = solve(&parsed2);
    let duration2 = start_time.elapsed();

    println!("Part 1 sum:  {}", sum1);
    println!("Part 1 time: {:?}", duration1);
    println!("Part 2 sum:  {}", sum2);
    println!("Part 2 time: {:?}", duration2);

    Ok(())
}