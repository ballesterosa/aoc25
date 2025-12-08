use std::fs;
use std::time::Instant;

#[inline]
fn parse_input(input: &str) -> Vec<(u32, u32, u32)> {
    return input.lines()
        .filter_map(|line| {
            let mut parts = line.split(',');
            Some((
                parts.next()?.parse().ok()?,
                parts.next()?.parse().ok()?,
                parts.next()?.parse().ok()?
            ))
        })
        .collect();
}

#[inline]
fn sq_dst(coord1: (u32, u32, u32), coord2: (u32, u32, u32)) -> i64 {
    let dx = coord1.0 as i64 - coord2.0 as i64;
    let dy = coord1.1 as i64 - coord2.1 as i64;
    let dz = coord1.2 as i64 - coord2.2 as i64;
    return dx * dx + dy * dy + dz * dz;
}

#[inline]
fn compute_all_pairs(coords: &[(u32, u32, u32)]) -> Vec<(usize, usize, i64)> {
    let n = coords.len();
    let mut pairs = Vec::with_capacity((n * (n - 1)) / 2);

    for i in 0..n {
        for j in (i + 1)..n {
            pairs.push((i, j, sq_dst(coords[i], coords[j])));
        }
    }

    pairs.sort_unstable_by_key(|&(_, _, dist)| dist);
    return pairs;
}

#[inline]
fn find_components(graph: &[Vec<usize>]) -> Vec<usize> {
    let n = graph.len();
    let mut visited = vec![false; n];
    let mut component_sizes = Vec::new();
    let mut stack = Vec::with_capacity(n);

    for start in 0..n {
        if visited[start] {
            continue;
        }

        let mut size = 0;
        stack.push(start);
        visited[start] = true;

        while let Some(node) = stack.pop() {
            size += 1;
            for &neighbor in &graph[node] {
                if !visited[neighbor] {
                    visited[neighbor] = true;
                    stack.push(neighbor);
                }
            }
        }

        component_sizes.push(size);
        stack.clear();
    }

    return component_sizes;
}

#[inline]
fn is_connected(graph: &[Vec<usize>], visited: &mut [bool], stack: &mut Vec<usize>) -> bool {
    let n = graph.len();
    if n == 0 {
        return true;
    }

    visited.fill(false);
    stack.clear();
    stack.push(0);
    visited[0] = true;
    let mut count = 1;

    while let Some(node) = stack.pop() {
        if count == n {
            return true;
        }
        for &neighbor in &graph[node] {
            if !visited[neighbor] {
                visited[neighbor] = true;
                stack.push(neighbor);
                count += 1;
            }
        }
    }

    return count == n;
}

#[inline]
fn part1(pairs: &[(usize, usize, i64)], n: usize) -> usize {
    let mut graph = vec![Vec::with_capacity(10); n];

    for &(i, j, _) in pairs.iter().take(1000) {
        graph[i].push(j);
        graph[j].push(i);
    }

    let mut sizes = find_components(&graph);
    sizes.sort_unstable_by(|a, b| b.cmp(a));

    return sizes.iter().take(3).product();
}

#[inline]
fn part2(pairs: &[(usize, usize, i64)], coords: &[(u32, u32, u32)], n: usize) -> u64 {
    let mut graph = vec![Vec::with_capacity(10); n];
    let mut visited = vec![false; n];
    let mut stack = Vec::with_capacity(n);

    for &(i, j, _) in pairs {
        graph[i].push(j);
        graph[j].push(i);

        if is_connected(&graph, &mut visited, &mut stack) {
            let x1 = coords[i].0 as u64;
            let x2 = coords[j].0 as u64;
            return x1 * x2;
        }
    }

    return 0;
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();
    let input = fs::read_to_string("inputs/day8_in.txt")?;
    let coords = parse_input(&input);
    let pairs = compute_all_pairs(&coords);
    let n = coords.len();
    let sum1 = part1(&pairs, n);
    println!("Part 1: {} ({:?})", sum1, start.elapsed());

    let start = Instant::now();
    let sum2 = part2(&pairs, &coords, n);
    println!("Part 2: {} ({:?})", sum2, start.elapsed());

    Ok(())
}