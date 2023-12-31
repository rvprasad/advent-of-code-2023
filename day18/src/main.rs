use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::iter::zip;
use std::ops::Range;
use substring::Substring;

fn main() {
    let args: Vec<String> = env::args().collect();
    if let Ok(plan) = get_plan(&args[1]) {
        println!(
            "part 1 {:?}",
            process_for_part1(&plan.iter().map(|(dir, count, _)| (*dir, *count)).collect())
        );
        println!("part 2 {:?}", process_for_part2(&plan));
    }
}

fn process_for_part2(plan: &Vec<(char, i32, String)>) -> u64 {
    fn count_internal_points(
        row: &i32,
        cols: &Vec<(i32, char)>,
        boundary_points: &HashSet<(i32, i32)>,
    ) -> u64 {
        let mut cols = cols.clone();
        cols.sort();
        let ret = cols
            .iter()
            .fold(
                (None, -1, 0u64),
                |(first_flip_side, last_col, count), (col, side)| {
                    let ret = match first_flip_side {
                        None => (Some(side), *col, 1),
                        Some(v) if side == v => {
                            let inc = if boundary_points.contains(&(*row, *col - 1)) {
                                (*col - last_col) as u64
                            } else {
                                1
                            };
                            (first_flip_side, *col, count + inc)
                        }
                        _ => {
                            let inc = (*col - last_col) as u64;
                            (first_flip_side, *col, count + inc)
                        }
                    };
                    ret
                },
            )
            .2;
        ret
    }

    let new_plan = Vec::from_iter(plan.iter().map(|(_, _, color)| {
        let mut chars = color.chars();
        let dir = match chars.nth(7) {
            Some('0') => 'R',
            Some('1') => 'D',
            Some('2') => 'L',
            Some('3') => 'U',
            _ => panic!("Invalid state"),
        };
        let count = i32::from_str_radix(color.substring(2, 7), 16).unwrap();
        (dir, count)
    }));
    let trench_with_sides = dig_trench(&new_plan);
    let mut row_to_vert_boundary_points = HashMap::new();
    trench_with_sides.iter().for_each(|((r, c), s)| {
        if *s == 'L' || *s == 'R' {
            row_to_vert_boundary_points
                .entry(*r)
                .or_insert(Vec::new())
                .push((*c, *s));
        }
    });
    let boundary_points = HashSet::from_iter(trench_with_sides.iter().map(|x| x.0).into_iter());
    row_to_vert_boundary_points
        .keys()
        .map(|row| count_internal_points(row, &row_to_vert_boundary_points[row], &boundary_points))
        .sum::<u64>()
}

fn process_for_part1(plan: &Vec<(char, i32)>) -> i32 {
    fn find_internal_point(
        boundary: &HashSet<(i32, i32)>,
        num_rows: i32,
        num_cols: i32,
    ) -> (i32, i32) {
        fn is_pos_inside(
            row: &i32,
            col: &i32,
            num_rows: i32,
            num_cols: i32,
            boundary: &HashSet<(i32, i32)>,
        ) -> bool {
            let col_inc_edge_count = ((col + 1)..num_cols)
                .filter(|c| boundary.contains(&(*row, *c)))
                .count();
            let col_dec_edge_count = (0..*col).filter(|c| boundary.contains(&(*row, *c))).count();
            let row_inc_edge_count = ((row + 1)..num_rows)
                .filter(|r| boundary.contains(&(*r, *col)))
                .count();
            let row_dec_edge_count = (0..*row).filter(|r| boundary.contains(&(*r, *col))).count();
            col_inc_edge_count % 2 == 1
                && col_dec_edge_count % 2 == 1
                && row_inc_edge_count % 2 == 1
                && row_dec_edge_count % 2 == 1
        }
        boundary
            .iter()
            .flat_map(|(r, c)| vec![(r + 1, *c), (r - 1, *c), (*r, c + 1), (*r, c - 1)])
            .filter(|(r, c)| {
                0 <= *r
                    && *r < num_rows
                    && 0 <= *c
                    && *c < num_cols
                    && !boundary.contains(&(*r, *c))
            })
            .find(|(r, c)| is_pos_inside(r, c, num_rows, num_cols, boundary))
            .unwrap()
    }

    let trench_with_sides = dig_trench(plan);
    let trench = trench_with_sides.iter().map(|x| x.0);
    let rows_and_cols: (Vec<i32>, Vec<i32>) = trench.clone().unzip();
    let num_rows = rows_and_cols.0.iter().max().unwrap() + 1;
    let num_cols = rows_and_cols.1.iter().max().unwrap() + 1;
    let boundary = HashSet::from_iter(trench.map(|pos| pos.clone()));
    let mut work_set = HashSet::from([find_internal_point(&boundary, num_rows, num_cols)]);
    let mut dug_positions = boundary;
    while !work_set.is_empty() {
        let mut new_work_set = HashSet::new();
        for work in &work_set {
            dug_positions.insert(*work);
            let (r, c) = *work;
            if r + 1 < num_rows && !dug_positions.contains(&(r + 1, c)) {
                new_work_set.insert((r + 1, c));
            }
            if -1 < r - 1 && !dug_positions.contains(&(r - 1, c)) {
                new_work_set.insert((r - 1, c));
            }
            if c + 1 < num_cols && !dug_positions.contains(&(r, c + 1)) {
                new_work_set.insert((r, c + 1));
            }
            if -1 < c - 1 && !dug_positions.contains(&(r, c - 1)) {
                new_work_set.insert((r, c - 1));
            }
        }
        work_set.clear();
        work_set.extend(new_work_set);
    }
    dug_positions.len() as i32
}

fn dig_trench(plan: &Vec<(char, i32)>) -> Vec<((i32, i32), char)> {
    fn helper(range: Range<i32>, base: i32) -> impl Iterator<Item = i32> {
        range.into_iter().map(move |i| base + i)
    }
    fn get_curr_side(dir: &char, last_dir: Option<char>) -> char {
        match last_dir {
            None => match dir {
                'U' => 'R',
                'R' => 'B',
                'D' => 'L',
                'L' => 'T',
                _ => panic!("Impossible!"),
            },
            Some(ld) => match (ld, dir) {
                ('U', 'R') => 'B',
                ('U', 'L') => 'T',
                ('D', 'R') => 'B',
                ('D', 'L') => 'T',
                ('R', 'D') => 'L',
                ('R', 'U') => 'R',
                ('L', 'D') => 'L',
                ('L', 'U') => 'R',
                _ => panic!("What did I miss?"),
            },
        }
    }
    let trench_with_sides = plan
        .iter()
        .fold(
            (0, 0, None, Vec::new()),
            |(row, col, last_dir, mut path), (dir, count)| {
                let sides =
                    std::iter::repeat(get_curr_side(dir, last_dir)).take((count + 1) as usize);
                let dir_opt = Some(*dir);
                match dir {
                    'R' => {
                        let rows = std::iter::repeat(row).take(*count as usize);
                        let cols = helper(1..*count, col);
                        path.extend(zip(zip(rows, cols), sides));
                        (row, col + count, dir_opt, path)
                    }
                    'L' => {
                        let rows = std::iter::repeat(row).take(*count as usize);
                        let cols = helper(-(count - 1)..0, col);
                        path.extend(zip(zip(rows, cols), sides));
                        (row, col - count, dir_opt, path)
                    }
                    'D' => {
                        let rows = helper(0..(count + 1), row);
                        let cols = std::iter::repeat(col).take((count + 1) as usize);
                        path.extend(zip(zip(rows, cols), sides));
                        (row + count, col, dir_opt, path)
                    }
                    'U' => {
                        let rows = helper(-*count..1, row);
                        let cols = std::iter::repeat(col).take((count + 1) as usize);
                        path.extend(zip(zip(rows, cols), sides));
                        (row - count, col, dir_opt, path)
                    }
                    _ => panic!("Invalid state"),
                }
            },
        )
        .3;
    let trench: (Vec<i32>, Vec<i32>) = trench_with_sides.iter().map(|x| x.0).unzip();
    let row_shift = -trench.0.iter().min().unwrap();
    let col_shift = -trench.1.iter().min().unwrap();
    trench_with_sides
        .iter()
        .map(|((r, c), s)| ((r + row_shift, c + col_shift), *s))
        .collect()
}

fn get_plan(filename: &String) -> std::io::Result<Vec<(char, i32, String)>> {
    let mut plan = Vec::new();

    let f = File::open(filename)?;
    let reader = BufReader::new(f);

    for line in reader.lines().flatten() {
        let split: Vec<String> = line.split(' ').map(String::from).collect();
        plan.push((
            split[0].chars().next().unwrap(),
            split[1].parse::<i32>().unwrap(),
            split[2].clone(),
        ));
    }

    Ok(plan)
}
