use std::fs;
use std::time::Instant;

#[derive(Debug, Clone)]
struct Machine {
    target: Vec<bool>,
    buttons: Vec<Vec<usize>>,
    joltage: Vec<i64>,
}

fn parse_input(input: &str) -> Vec<Machine> {
    let mut machines = Vec::new();

    for line in input.lines() {
        if line.trim().is_empty() {
            continue;
        }

        // parse indicator lights [.##.]
        let start = line.find('[').unwrap();
        let end = line.find(']').unwrap();
        let lights_str = &line[start + 1..end];
        let target: Vec<bool> = lights_str.chars().map(|c| c == '#').collect();

        // parse buttons
        let mut buttons = Vec::new();
        let rest = &line[end + 1..];

        let mut i = 0;
        while i < rest.len() {
            if rest.chars().nth(i) == Some('(') {
                let mut j = i + 1;
                while j < rest.len() && rest.chars().nth(j) != Some(')') {
                    j += 1;
                }
                let button_str = &rest[i + 1..j];
                let button: Vec<usize> = button_str
                    .split(',')
                    .map(|s| s.trim().parse().unwrap())
                    .collect();
                buttons.push(button);
                i = j + 1;
            } else {
                i += 1;
            }
        }

        // parse joltage requirements {3,5,4,7}
        let mut joltage = Vec::new();
        if let Some(start_brace) = rest.rfind('{') {
            if let Some(end_brace) = rest.rfind('}') {
                let joltage_str = &rest[start_brace + 1..end_brace];
                joltage = joltage_str
                    .split(',')
                    .map(|s| s.trim().parse().unwrap())
                    .collect();
            }
        }

        machines.push(Machine { target, buttons, joltage });
    }

    return machines;
}

#[inline]
fn solve_machine(machine: &Machine) -> usize {
    let n_lights = machine.target.len();
    let n_buttons = machine.buttons.len();
    let mut matrix = vec![vec![false; n_buttons + 1]; n_lights];

    for (button_idx, button) in machine.buttons.iter().enumerate() {
        for &light_idx in button {
            if light_idx < n_lights {
                matrix[light_idx][button_idx] = true;
            }
        }
    }

    for i in 0..n_lights {
        matrix[i][n_buttons] = machine.target[i];
    }

    // gaussian elimination to row echelon form
    let mut pivot_row = 0;
    let mut pivot_cols = Vec::new();

    for col in 0..n_buttons {
        let mut found = false;
        for row in pivot_row..n_lights {
            if matrix[row][col] {
                matrix.swap(pivot_row, row);
                found = true;
                break;
            }
        }

        if !found {
            continue;
        }

        pivot_cols.push(col);

        for row in 0..n_lights {
            if row != pivot_row && matrix[row][col] {
                for c in 0..=n_buttons {
                    matrix[row][c] ^= matrix[pivot_row][c];
                }
            }
        }

        pivot_row += 1;
        if pivot_row >= n_lights {
            break;
        }
    }

    // find free variables (columns without pivots)
    let mut free_vars = Vec::new();
    for col in 0..n_buttons {
        if !pivot_cols.contains(&col) {
            free_vars.push(col);
        }
    }

    let num_free = free_vars.len();
    if num_free > 0 && num_free <= 15 {
        let mut min_presses = usize::MAX;
        let mut solution = vec![false; n_buttons];

        for mask in 0..(1 << num_free) {
            solution.fill(false);

            for (i, &var) in free_vars.iter().enumerate() {
                solution[var] = (mask >> i) & 1 == 1;
            }

            for i in (0..pivot_cols.len()).rev() {
                let col = pivot_cols[i];
                solution[col] = matrix[i][n_buttons];

                for c in (col + 1)..n_buttons {
                    if matrix[i][c] {
                        solution[col] ^= solution[c];
                    }
                }
            }

            let presses = solution.iter().filter(|&&x| x).count();
            min_presses = min_presses.min(presses);
        }

        return min_presses;
    }

    // no free variables or too many
    let mut solution = vec![false; n_buttons];
    for i in (0..pivot_cols.len()).rev() {
        let col = pivot_cols[i];
        solution[col] = matrix[i][n_buttons];

        for c in (col + 1)..n_buttons {
            if matrix[i][c] {
                solution[col] ^= solution[c];
            }
        }
    }

    return solution.iter().filter(|&&x| x).count();
}

#[inline]
fn solve_machine_joltage(machine: &Machine) -> i64 {
    let n_counters = machine.joltage.len();
    let n_buttons = machine.buttons.len();

    // build coefficient matrix A and target vector b
    let mut a = vec![vec![0i64; n_buttons]; n_counters];

    for (button_idx, button) in machine.buttons.iter().enumerate() {
        for &counter_idx in button {
            if counter_idx < n_counters {
                a[counter_idx][button_idx] = 1;
            }
        }
    }

    // use Gaussian elimination with better handling of free variables
    let mut aug = vec![vec![0i64; n_buttons + 1]; n_counters];
    for i in 0..n_counters {
        for j in 0..n_buttons {
            aug[i][j] = a[i][j];
        }
        aug[i][n_buttons] = machine.joltage[i];
    }

    // gaussian elimination to reduced row echelon form
    let mut pivot_row = 0;
    let mut pivot_cols = Vec::new();

    for col in 0..n_buttons {
        let mut best_row = None;
        let mut best_val = i64::MAX;

        for row in pivot_row..n_counters {
            if aug[row][col] != 0 && aug[row][col].abs() < best_val {
                best_row = Some(row);
                best_val = aug[row][col].abs();
            }
        }

        if best_row.is_none() {
            continue;
        }

        let best_row = best_row.unwrap();
        aug.swap(pivot_row, best_row);
        pivot_cols.push(col);

        let pivot_val = aug[pivot_row][col];

        for row in 0..n_counters {
            if row != pivot_row && aug[row][col] != 0 {
                let factor = aug[row][col];
                for c in 0..=n_buttons {
                    aug[row][c] = aug[row][c] * pivot_val - aug[pivot_row][c] * factor;
                }
            }
        }

        pivot_row += 1;
        if pivot_row >= n_counters {
            break;
        }
    }

    // simplify rows by GCD
    for row in 0..pivot_row {
        let mut g = 0i64;
        for col in 0..=n_buttons {
            g = gcd(g, aug[row][col].abs());
        }
        if g > 1 {
            for col in 0..=n_buttons {
                aug[row][col] /= g;
            }
        }
    }

    // find free variables
    let mut free_vars = Vec::new();
    for col in 0..n_buttons {
        if !pivot_cols.contains(&col) {
            free_vars.push(col);
        }
    }

    // try different values for free variables to minimize total presses
    let num_free = free_vars.len();

    if num_free == 0 {
        let mut solution = vec![0i64; n_buttons];

        for i in (0..pivot_cols.len()).rev() {
            let col = pivot_cols[i];
            let pivot_val = aug[i][col];

            if pivot_val == 0 {
                continue;
            }

            let mut val = aug[i][n_buttons];
            for c in (col + 1)..n_buttons {
                val -= aug[i][c] * solution[c];
            }

            if val % pivot_val == 0 {
                solution[col] = val / pivot_val;
            } else {
                return i64::MAX / 2;
            }
        }

        if solution.iter().all(|&x| x >= 0) {
            return solution.iter().sum();
        } else {
            return solution.iter().map(|&x| x.abs()).sum();
        }
    }

    // try a range of values for free variables
    let max_val = machine.joltage.iter().max().copied().unwrap_or(100);
    let search_range = if num_free <= 2 {
        (max_val * 3).min(200)
    } else {
        (max_val * 2).min(100)
    };

    let mut min_cost = i64::MAX;
    try_free_variables(&aug, &pivot_cols, &free_vars, n_buttons, 0, &mut vec![0; num_free], search_range, &mut min_cost);

    return min_cost;
}

fn try_free_variables(
    aug: &[Vec<i64>],
    pivot_cols: &[usize],
    free_vars: &[usize],
    n_buttons: usize,
    free_idx: usize,
    free_values: &mut Vec<i64>,
    max_val: i64,
    min_cost: &mut i64,
) {
    if free_idx == free_vars.len() {
        let mut solution = vec![0i64; n_buttons];
        for (i, &var) in free_vars.iter().enumerate() {
            solution[var] = free_values[i];
        }

        for i in (0..pivot_cols.len()).rev() {
            let col = pivot_cols[i];
            let pivot_val = aug[i][col];

            if pivot_val == 0 {
                continue;
            }

            let mut val = aug[i][n_buttons];
            for c in (col + 1)..n_buttons {
                val -= aug[i][c] * solution[c];
            }

            if val % pivot_val != 0 {
                return; // no valid solution with these free values
            }

            solution[col] = val / pivot_val;
        }

        if solution.iter().all(|&x| x >= 0) {
            let cost: i64 = solution.iter().sum();
            *min_cost = (*min_cost).min(cost);
        }

        return;
    }

    // if current free variable sum already exceeds min_cost, stop
    let current_sum: i64 = free_values[..free_idx].iter().sum();
    if current_sum >= *min_cost {
        return;
    }

    // try different values for this free variable
    for val in 0..=max_val {
        free_values[free_idx] = val;
        try_free_variables(aug, pivot_cols, free_vars, n_buttons, free_idx + 1, free_values, max_val, min_cost);
    }
}

fn gcd(a: i64, b: i64) -> i64 {
    if b == 0 {
        return a;
    } else {
        return gcd(b, a % b);
    }
}

#[inline]
fn part1(machines: &[Machine]) -> usize {
    return machines.iter().map(|m| solve_machine(m)).sum();
}

#[inline]
fn part2(machines: &[Machine]) -> i64 {
    return machines.iter().map(|m| solve_machine_joltage(m)).sum();
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();
    let input = fs::read_to_string("inputs/day10_in.txt")?;
    let machines = parse_input(&input);
    let part1_res = part1(&machines);
    println!("Part 1: {} ({:?})", part1_res, start.elapsed());

    let start = Instant::now();
    let part2_res = part2(&machines);
    println!("Part 2: {} ({:?})", part2_res, start.elapsed());

    Ok(())
}
