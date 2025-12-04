use std::fs;
use std::time::Instant;

#[inline]
fn part1(input: String) -> u32 {
    let mut sum1 = 0;
    for nums in input.split('\n') {
        let digits: Vec<u32> = nums.chars()
            .filter_map(|c| c.to_digit(10))
            .collect();

        let mut best_pair = 0;
        for i in 0..digits.len() {
            if best_pair >= (digits[i] * 10) + 9 {
                continue;
            }
            for j in (i+1)..digits.len() {
                let curr = (digits[i] * 10) + digits[j];
                if curr > best_pair {
                    best_pair = curr;
                }
            }
            if best_pair == 99 {
                break;
            }
        }
        sum1 += best_pair;
    }
    return sum1;
}

#[inline]
fn part2(input: String) -> u64 {
    let mut sum2 = 0;
    for nums in input.split('\n') {
        let digits: Vec<u32> = nums.chars()
            .filter_map(|c| c.to_digit(10))
            .collect();

        if digits.len() < 12 {
            continue;
        }

        // dp[i][j] = maximum number using j digits from first i positions
        let n = digits.len();
        let mut dp = vec![vec![0u64; 13]; n + 1];

        for i in 1..=n {
            for j in 0..=12.min(i) {
                // Don't take digit i-1
                dp[i][j] = dp[i-1][j];

                // Take digit i-1 (if we have at least j-1 digits already)
                if j > 0 {
                    let take = dp[i-1][j-1] * 10 + digits[i-1] as u64;
                    dp[i][j] = dp[i][j].max(take);
                }
            }
        }

        sum2 += dp[n][12];
    }
    return sum2;
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input: String = fs::read_to_string("day3_in.txt")?;

    // Part 1
    let start_time = Instant::now();
    let sum1 = part1(input.clone());
    let duration1 = start_time.elapsed();

    // Part 2
    let start_time = Instant::now();
    let sum2 = part2(input);
    let duration2 = start_time.elapsed();

    println!("Part 1 sum:  {}", sum1);
    println!("Part 1 time: {:?}", duration1);
    println!("Part 2 sum:  {}", sum2);
    println!("Part 2 time: {:?}", duration2);

    Ok(())
}