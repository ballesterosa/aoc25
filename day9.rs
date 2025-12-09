use std::fs;
use std::time::Instant;

fn parse_input(input: &str) -> Vec<(i32, i32)> {
    let mut points = Vec::new();
    for line in input.lines() {
        let (x, y) = line.split_once(',').unwrap();
        points.push((x.parse().unwrap(), y.parse().unwrap()));
    }
    return points;
}

#[inline]
fn part1(points: &[(i32, i32)]) -> u64 {
    let mut max_area = 0;
    for i in 0..points.len() {
        for j in (i + 1)..points.len() {
            let area = ((points[i].0 - points[j].0).abs() as i64 + 1) as u64
                * ((points[i].1 - points[j].1).abs() as i64 + 1) as u64;
            if area > max_area {
                max_area = area;
            }
        }
    }
    return max_area;
}

#[inline]
fn part2(polygon: &[(i32, i32)]) -> u64 {
    let mut max_area = 0;

    let poly_min_x = polygon.iter().map(|p| p.0).min().unwrap();
    let poly_max_x = polygon.iter().map(|p| p.0).max().unwrap();
    let poly_min_y = polygon.iter().map(|p| p.1).min().unwrap();
    let poly_max_y = polygon.iter().map(|p| p.1).max().unwrap();

    for i in 0..polygon.len() {
        let pi = polygon[i];
        for j in (i + 1)..polygon.len() {
            let pj = polygon[j];
            if pi.0 == pj.0 || pi.1 == pj.1 {
                continue;
            }

            let (x1, x2) = if pi.0 < pj.0 {
                (pi.0, pj.0)
            } else {
                (pj.0, pi.0)
            };
            let (y1, y2) = if pi.1 < pj.1 {
                (pi.1, pj.1)
            } else {
                (pj.1, pi.1)
            };

            if x1 < poly_min_x || x2 > poly_max_x || y1 < poly_min_y || y2 > poly_max_y {
                continue;
            }

            let area = ((x2 - x1 + 1) as u64) * ((y2 - y1 + 1) as u64);

            if area > max_area && is_rectangle_valid(polygon, x1, y1, x2, y2) {
                let area = ((x2 - x1 + 1) as u64) * ((y2 - y1 + 1) as u64);
                max_area = max_area.max(area);
            }
        }
    }

    return max_area;
}

#[inline]
fn is_rectangle_valid(polygon: &[(i32, i32)], x1: i32, y1: i32, x2: i32, y2: i32) -> bool {
    if !is_point_inside_polygon(polygon, x1, y1) || !is_point_inside_polygon(polygon, x1, y2) ||
       !is_point_inside_polygon(polygon, x2, y1) || !is_point_inside_polygon(polygon, x2, y2) {
        return false;
    }

    let rect_edges = [
        ((x1, y1), (x2, y1)),
        ((x2, y1), (x2, y2)),
        ((x2, y2), (x1, y2)),
        ((x1, y2), (x1, y1)),
    ];

    for i in 0..polygon.len() {
        let j = (i + 1) % polygon.len();
        let p1 = polygon[i];
        let p2 = polygon[j];

        let seg_min_x = p1.0.min(p2.0);
        let seg_max_x = p1.0.max(p2.0);
        let seg_min_y = p1.1.min(p2.1);
        let seg_max_y = p1.1.max(p2.1);

        if seg_max_x < x1 || seg_min_x > x2 || seg_max_y < y1 || seg_min_y > y2 {
            continue;
        }

        for &(r1, r2) in &rect_edges {
            if segments_intersect(p1, p2, r1, r2) {
                return false;
            }
        }
    }

    return true;
}

#[inline]
fn segments_intersect(p1: (i32, i32), p2: (i32, i32), q1: (i32, i32), q2: (i32, i32)) -> bool {
    let d = (p1.0 - p2.0) as i64 * (q1.1 - q2.1) as i64
        - (p1.1 - p2.1) as i64 * (q1.0 - q2.0) as i64;

    if d == 0 {
        return false;
    }

    let t = ((p1.0 - q1.0) as i64 * (q1.1 - q2.1) as i64
        - (p1.1 - q1.1) as i64 * (q1.0 - q2.0) as i64) as f64
        / d as f64;
    let u = -((p1.0 - p2.0) as i64 * (p1.1 - q1.1) as i64
        - (p1.1 - p2.1) as i64 * (p1.0 - q1.0) as i64) as f64
        / d as f64;

    return t > 0.0 && t < 1.0 && u > 0.0 && u < 1.0;
}

#[inline]
fn is_point_inside_polygon(polygon: &[(i32, i32)], px: i32, py: i32) -> bool {
    for i in 0..polygon.len() {
        let j = (i + 1) % polygon.len();
        if is_on_segment(polygon[i], polygon[j], px, py) {
            return true;
        }
    }

    let mut inside = false;
    for i in 0..polygon.len() {
        let j = (i + 1) % polygon.len();
        let (xi, yi) = polygon[i];
        let (xj, yj) = polygon[j];

        if ((yi > py) != (yj > py)) && (px < (xj - xi) * (py - yi) / (yj - yi) + xi) {
            inside = !inside;
        }
    }
    return inside;
}

#[inline]
fn is_on_segment(p1: (i32, i32), p2: (i32, i32), px: i32, py: i32) -> bool {
    let min_x = p1.0.min(p2.0);
    let max_x = p1.0.max(p2.0);
    let min_y = p1.1.min(p2.1);
    let max_y = p1.1.max(p2.1);

    if px < min_x || px > max_x || py < min_y || py > max_y {
        return false;
    }

    return ((py - p1.1) as i64 * (p2.0 - p1.0) as i64
            - (px - p1.0) as i64 * (p2.1 - p1.1) as i64) == 0;
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let start = Instant::now();
    let input = fs::read_to_string("inputs/day9_in.txt")?;
    let points = parse_input(&input);
    let part1_res = part1(&points);
    println!("Part 1: {} ({:?})", part1_res, start.elapsed());

    let start = Instant::now();
    let part2_res = part2(&points);
    println!("Part 2: {} ({:?})", part2_res, start.elapsed());

    Ok(())
}