use std::fs;
use std::time::Instant;

#[inline]
fn parse_direction_value(s: &str) -> (u8, i32) {
    let direction = s.as_bytes()[0];
    let number = s[1..].parse().unwrap();
    (direction, number)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();

    let input = fs::read_to_string("day1_in.txt")?;

    let mut curr_pos: i32 = 50;
    let mut count: i32 = 0;
    let mut part_2_count: i32 = 0;

    for line in input.lines() {
        let (dir, num) = parse_direction_value(line);

        if dir == b'R' {
            let new_pos = curr_pos + num;
            part_2_count += new_pos / 100;
            curr_pos = new_pos % 100;
            count += (curr_pos == 0) as i32;
        } else {  // dir is L
            let new_pos = curr_pos - num;
            if new_pos <= 0 {
                part_2_count += if curr_pos != 0 {
                    (num - curr_pos) / 100 + 1
                } else {
                    num / 100
                };
                curr_pos = ((new_pos % 100) + 100) % 100;
            } else {
                curr_pos = new_pos;
            }
            count += (curr_pos == 0) as i32;
        }
    }

    let duration = start.elapsed();

    println!("curr pos:     {}", curr_pos);
    println!("count:        {}", count);
    println!("part_2_count: {}", part_2_count);
    println!("Time:         {:?}", duration);

    Ok(())
}