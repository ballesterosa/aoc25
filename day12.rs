use std::fs::read_to_string;
use std::time::Instant;

#[inline]
fn the_last_one(input: &str) -> usize {
	// split input into blocks
	let blocks: Vec<&str> = input.split("\n\n").collect();
	let shape_blocks = &blocks[..blocks.len().saturating_sub(1)];
	let region_block = blocks.last().unwrap_or(&"");

	// for every shape, get the number of occupied cells
	let mut shape_area = Vec::new();
	for shape_block in shape_blocks {
		shape_area.push(shape_block.chars().filter(|&c| c == '#').count());
	}

	let mut fit_regions = 0;
	for region_data in region_block.lines() {
		if region_data.trim().is_empty() { continue; }
		let mut parts = region_data.split(':');
		let size_data = parts.next().unwrap().trim();
		let shape_data = parts.next().unwrap().trim();
		let dims: Vec<_> = size_data.split('x').collect();
		let m: usize = dims[0].parse().unwrap();
		let n: usize = dims[1].parse().unwrap();
		let mut area_needed = 0;
		for (id, freq) in shape_data.split_whitespace().enumerate() {
			let freq: usize = freq.parse().unwrap();
			area_needed += shape_area[id] * freq;
		}
		if m * n >= area_needed {
			fit_regions += 1;
		}
	}
	return fit_regions;
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let t1 = Instant::now();
    let input = read_to_string("inputs/day12_in.txt")?;
    let ans = the_last_one(&input);
    let d1 = t1.elapsed();
    println!("Answer: {} (elapsed: {:?})", ans, d1);
    Ok(())
}
