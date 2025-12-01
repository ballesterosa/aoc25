use std::fs::File;
use std::io::{BufRead, BufReader};
use std::time::Instant;

fn parse_direction_value(s: &str) -> Option<(char, i32)> {
    let direction: char = s.chars().next()?;
    let number: i32 = s[1..].parse().ok()?;
    Some((direction, number))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();

    // part 1
    let input_file = "day1_in.txt";

    let file = File::open(input_file)?;
    let reader = BufReader::new(file);

    let mut curr_pos: i32 = 50;
    let mut count: i32 = 0;
    let mut part_2_count: i32 = 0;

    for line in reader.lines() {
        let line = line?;
        let (dir, mut num) = parse_direction_value(&line).ok_or("Failed to parse!")?;

        if dir == 'R' {
            let new_pos = curr_pos + num;
            part_2_count += new_pos / 100;
            curr_pos = new_pos % 100;
            if curr_pos == 0 {
                count += 1;
            }
        } else {  // dir is L
            while num > 0 {
                if num > curr_pos {
                    if curr_pos != 0 {
                        part_2_count += 1;
                    }
                    num = num - curr_pos - 1;
                    curr_pos = 99;
                } else {
                    curr_pos = curr_pos - num;
                    num = 0;
                }
            }
            if curr_pos == 0 {
                count += 1;
                part_2_count += 1;
            }
        }
        // println!("curr pos:     {}", curr_pos);
        // println!("count:        {}", count);
        // println!("part_2_count: {}", part_2_count);
    }

    let duration = start.elapsed();

    println!("curr pos:     {}", curr_pos);
    println!("count:        {}", count);
    println!("part_2_count: {}", part_2_count);
    println!("Time:         {:?}", duration);

    Ok(())
}