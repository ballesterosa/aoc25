use std::fs;
use std::time::Instant;

#[inline]
fn parse_direction_value(s: &str) -> (u64, u64) {
    let mut parts = s.split('-');
    (
        parts.next().unwrap().parse().unwrap(),
        parts.next().unwrap().parse().unwrap()
    )
}

#[inline]
fn part1(i: u64) -> u64 {
    let num_digits = i.ilog10() + 1;
    if num_digits % 2 == 0 {
        let half_digits = num_digits / 2;
        let divisor = 10u64.pow(half_digits);
        let left_half = i / divisor;
        let right_half = i % divisor;
        if left_half == right_half {
            return i;
        }
    }
    0
}

#[inline]
fn part2(i: u64) -> u64 {
    let num_digits = i.ilog10() + 1;
    let mut is_invalid = false;

    // Check all possible pattern lengths (from 1 to num_digits/2)
    for pattern_len in 1..=(num_digits / 2) {
        if num_digits % pattern_len == 0 {
            let repetitions = num_digits / pattern_len;
            if repetitions >= 2 {
                let divisor = 10u64.pow(pattern_len);
                let pattern = i % divisor;

                let mut temp = i;
                let mut matches = true;
                for _ in 0..repetitions {
                    if temp % divisor != pattern {
                        matches = false;
                        break;
                    }
                    temp /= divisor;
                }

                if matches {
                    is_invalid = true;
                    break;
                }
            }
        }
    }

    if is_invalid {
        return i;
    } else {
        return 0;
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Part 1
    let start_time = Instant::now();
    let input = fs::read_to_string("inputs/day2_in.txt")?;
    let mut sum1: u64 = 0;
    for range in input.split(',') {
        let (start, end) = parse_direction_value(range.trim());
        for i in start..=end {
            sum1 += part1(i);
        }
    }
    let duration1 = start_time.elapsed();

    // Part 2
    let start_time = Instant::now();
    let mut sum2: u64 = 0;
    for range in input.split(',') {
        let (start, end) = parse_direction_value(range.trim());
        for i in start..=end {
            sum2 += part2(i);
        }
    }
    let duration2 = start_time.elapsed();

    println!("Part 1 sum:  {}", sum1);
    println!("Part 1 time: {:?}", duration1);
    println!("Part 2 sum:  {}", sum2);
    println!("Part 2 time: {:?}", duration2);

    Ok(())
}