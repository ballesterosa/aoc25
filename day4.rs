use std::fs;
use std::time::Instant;

const DIRECTIONS: [(i32, i32); 8] = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)];

#[inline]
fn count_adjacent(grid: &[Vec<char>], i: usize, j: usize, n: usize, m: usize) -> u32 {
    let mut count = 0;
    for &(di, dj) in &DIRECTIONS {
        let ni = i as i32 + di;
        let nj = j as i32 + dj;
        if ni >= 0 && ni < n as i32 && nj >= 0 && nj < m as i32 {
            count += (grid[ni as usize][nj as usize] == '@') as u32;
        }
    }
    count
}

#[inline]
fn part1(grid: &[Vec<char>]) -> u32 {
    let n = grid.len();
    let m = grid[0].len();
    let mut sum = 0;

    for i in 0..n {
        for j in 0..m {
            if grid[i][j] != '.' {
                sum += (count_adjacent(grid, i, j, n, m) < 4) as u32;
            }
        }
    }
    sum
}

#[inline]
fn part2(grid: &mut [Vec<char>]) -> u32 {
    let n = grid.len();
    let m = grid[0].len();
    let mut sum = 0;

    for i in 0..n {
        for j in 0..m {
            if grid[i][j] != '.' && count_adjacent(grid, i, j, n, m) < 4 {
                grid[i][j] = '.';
                sum += 1;
            }
        }
    }
    sum
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input: String = fs::read_to_string("day4_in.txt")?;
    let mut grid: Vec<Vec<char>> = input
        .lines()
        .map(|line| line.chars().collect())
        .collect();

    // Part 1
    let start_time = Instant::now();
    let sum1 = part1(&grid);
    let duration1 = start_time.elapsed();

    // Part 2
    let start_time = Instant::now();
    let mut sum2 = 0;
    let mut curr_sum2 = part2(&mut grid);
    while curr_sum2 != 0 {
        sum2 += curr_sum2;
        curr_sum2 = part2(&mut grid);
    }
    let duration2 = start_time.elapsed();

    println!("Part 1 sum:  {}", sum1);
    println!("Part 1 time: {:?}", duration1);
    println!("Part 2 sum:  {}", sum2);
    println!("Part 2 time: {:?}", duration2);

    Ok(())
}