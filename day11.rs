use std::fs;
use std::time::Instant;
use std::collections::HashMap;

fn dp_count_paths(
    graph: &HashMap<String, Vec<String>>,
    node: &str,
    target: &str,
    has_dac: bool,
    has_fft: bool,
    memo: &mut HashMap<(String, bool, bool), u64>,
    check_has: bool,
) -> u64 {
    let key = (node.to_string(), has_dac, has_fft);
    if let Some(&cached) = memo.get(&key) {
        return cached;
    }
    if node == target {
        let result = if (has_dac && has_fft) || !check_has { 1 } else { 0 };
        memo.insert(key, result);
        return result;
    }
    let mut total = 0;
    if let Some(neighbors) = graph.get(node) {
        for neighbor in neighbors {
            let next_has_dac = has_dac || neighbor == "dac";
            let next_has_fft = has_fft || neighbor == "fft";
            total += dp_count_paths(graph, neighbor, target, next_has_dac, next_has_fft, memo, check_has);
        }
    }
    memo.insert(key, total);
    return total;
}

fn part1_count_paths(graph: &HashMap<String, Vec<String>>) -> u64 {
    let mut memo = HashMap::new();
    return dp_count_paths(graph, "you", "out", false, false, &mut memo, false);
}

fn part2_count_paths_with_dac_fft(graph: &HashMap<String, Vec<String>>) -> u64 {
    let mut memo = HashMap::new();
    return dp_count_paths(graph, "svr", "out", false, false, &mut memo, true);
}

fn parse_input(input: &str) -> HashMap<String, Vec<String>> {
    let mut graph = HashMap::new();

    for line in input.lines() {
        if line.trim().is_empty() {
            continue;
        }

        let parts: Vec<&str> = line.split(':').collect();
        if parts.len() != 2 {
            continue;
        }

        let node = parts[0].trim().to_string();
        let neighbors: Vec<String> = parts[1]
            .trim()
            .split_whitespace()
            .map(|s| s.to_string())
            .collect();

        graph.insert(node, neighbors);
    }

    return graph;
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Part 1
    let t1 = Instant::now();
    let input = fs::read_to_string("inputs/day11_in.txt")?;
    let graph = parse_input(&input);
    let part1 = part1_count_paths(&graph);
    let d1 = t1.elapsed();
    println!("Part 1: Paths from 'you' to 'out': {} (elapsed: {:?})", part1, d1);

    // Part 2
    let t2 = Instant::now();
    let part2 = part2_count_paths_with_dac_fft(&graph);
    let d2 = t2.elapsed();
    println!("Part 2: Paths from 'svr' to 'out' visiting both 'dac' and 'fft': {} (elapsed: {:?})", part2, d2);

    Ok(())
}