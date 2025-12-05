use std::fs;
use std::time::Instant;

struct Input {
    ranges: Vec<(u64, u64)>,
    numbers: Vec<u64>,
}

fn parse_input(input: &str) -> Input {
    let mut ranges = Vec::new();
    let mut numbers = Vec::new();
    let mut in_ranges = true;

    for line in input.lines() {
        let line = line.trim();
        if line.is_empty() {
            in_ranges = false;
            continue;
        }

        if in_ranges {
            if let Some((start, end)) = line.split_once('-') {
                ranges.push((start.parse().unwrap(), end.parse().unwrap()));
            }
        } else {
            numbers.push(line.parse().unwrap());
        }
    }

    Input { ranges, numbers }
}

fn coalesce_ranges(ranges: &mut Vec<(u64, u64)>) {
    if ranges.is_empty() {
        return;
    }

    // sort by start position
    ranges.sort_unstable_by_key(|&(start, _)| start);

    let mut write_idx = 0;
    for read_idx in 1..ranges.len() {
        let (start, end) = ranges[read_idx];
        if start <= ranges[write_idx].1 + 1 {
            // merge: extend the current range
            ranges[write_idx].1 = ranges[write_idx].1.max(end);
        } else {
            // no overlap, move to next
            write_idx += 1;
            ranges[write_idx] = (start, end);
        }
    }
    ranges.truncate(write_idx + 1);
}

#[inline]
fn part1(parsed: &Input) -> u32 {
    // binary search
    parsed.numbers.iter()
        .filter(|&&num| {
            parsed.ranges.binary_search_by(|&(start, end)| {
                if num < start {
                    std::cmp::Ordering::Greater
                } else if num > end {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Equal
                }
            }).is_ok()
        })
        .count() as u32
}

#[inline]
fn part2(parsed: &Input) -> u64 {
    parsed.ranges.iter()
        .map(|(start, end)| end - start + 1)
        .sum()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Part 1
    let start_time = Instant::now();
    let input: String = fs::read_to_string("day5_in.txt")?;
    let mut parsed: Input = parse_input(&input);
    coalesce_ranges(&mut parsed.ranges);
    let sum1 = part1(&parsed);
    let duration1 = start_time.elapsed();

    // Part 2
    let start_time = Instant::now();
    let sum2 = part2(&parsed);
    let duration2 = start_time.elapsed();

    println!("Part 1 sum:  {}", sum1);
    println!("Part 1 time: {:?}", duration1);
    println!("Part 2 sum:  {}", sum2);
    println!("Part 2 time: {:?}", duration2);

    Ok(())
}