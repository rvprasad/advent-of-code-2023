use std::cmp;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::iter;

type Position = (i32, i32);

fn main() {
    let args: Vec<String> = env::args().collect();
    if let Ok((map, slopes, begin, end)) = get_map(&args[1]) {
        println!("part1 {:?}", solve_part1(&map, &slopes, begin, end));
        println!("part2 {:?}", solve_part2(&map, begin, end));
    }
}

fn solve_part2(map: &HashMap<Position, Vec<Position>>, begin: Position, end: Position) -> usize {
    let mut distances = HashMap::new();

    for entry in map.iter().filter(|(_, v)| v.len() > 2) {
        let mut work_list = Vec::from_iter(entry.1.iter().map(|s| vec![entry.0, s]));
        while let Some(path) = work_list.pop() {
            if let Some((last_pos, Some(neighbors))) = path.last().map(|k| (k, map.get(k))) {
                if neighbors.len() == 2 {
                    neighbors
                        .iter()
                        .filter(|n| !path.contains(n))
                        .next()
                        .iter()
                        .for_each(|p| {
                            let mut new_path = path.clone();
                            new_path.push(p);
                            work_list.push(new_path)
                        });
                } else {
                    let len = path.len() - 1;
                    distances.insert((path[0], *last_pos), len);
                    distances.insert((*last_pos, path[0]), len);
                }
            }
        }
    }
    let mut new_map = HashMap::new();
    for (s, d) in distances.keys() {
        if s < d {
            new_map.entry(**s).or_insert(Vec::new()).push(**d);
            new_map.entry(**d).or_insert(Vec::new()).push(**s);
        }
    }

    let length_calculator = |path: &Vec<Position>| {
        iter::zip(path.iter().take(path.len() - 1), path.iter().skip(1))
            .filter_map(|pair| distances.get(&pair))
            .sum()
    };

    let successor_filter = |position: &Position| new_map.get(&position).map(|v| v.clone());
    solve(&new_map, successor_filter, length_calculator, begin, end)
}

fn solve_part1(
    map: &HashMap<Position, Vec<Position>>,
    slopes: &HashMap<Position, char>,
    begin: Position,
    end: Position,
) -> usize {
    let successor_filter = |position: &Position| {
        let (r, c) = position;
        let slope = slopes.get(&position);
        map.get(&position).map(|successors| {
            successors
                .iter()
                .filter(|(nr, nc)| match slope {
                    Some('>') => nc > c,
                    Some('<') => nc < c,
                    Some('v') => nr > r,
                    Some('^') => nr < r,
                    _ => true,
                })
                .copied()
                .collect::<Vec<Position>>()
        })
    };
    let length_calculator = |path: &Vec<Position>| path.len() - 1;

    solve(map, successor_filter, length_calculator, begin, end)
}

fn solve<S, L>(
    map: &HashMap<Position, Vec<Position>>,
    successor_filter: S,
    length_calculator: L,
    begin: Position,
    end: Position,
) -> usize
where
    S: Fn(&Position) -> Option<Vec<Position>>,
    L: Fn(&Vec<Position>) -> usize,
{
    let mut ret = 0;
    let mut position_2_unexplored_successors: HashMap<Position, Vec<Position>> = HashMap::new();
    let mut path: Vec<Position> = Vec::new();
    path.push(begin);
    position_2_unexplored_successors
        .insert(begin, map.get(&begin).map_or(Vec::new(), |v| (*v).clone()));
    while let Some(cur_pos) = path.pop() {
        if let Some(unexplored_successors) = position_2_unexplored_successors.get_mut(&cur_pos) {
            if let Some(successor) = unexplored_successors.pop() {
                path.push(cur_pos);
                if successor != end && !path.contains(&successor) {
                    path.push(successor);
                    successor_filter(&successor)
                        .iter()
                        .for_each(|next_successors| {
                            position_2_unexplored_successors
                                .insert(successor, next_successors.clone());
                        });
                }
                if successor == end {
                    path.push(successor);
                    ret = cmp::max(ret, length_calculator(&path));
                    path.pop();
                }
            }
        }
    }
    ret as usize
}

fn get_map(
    filename: &String,
) -> std::io::Result<(
    HashMap<Position, Vec<Position>>,
    HashMap<Position, char>,
    Position,
    Position,
)> {
    let mut slopes = HashMap::new();

    let f = File::open(filename)?;
    let reader = BufReader::new(f);
    let mut positions = Vec::new();

    for (r, line) in reader.lines().flatten().enumerate() {
        for (c, chr) in line.chars().enumerate() {
            if chr != '#' {
                if chr != '.' {
                    slopes.insert((r as i32, c as i32), chr);
                }
                positions.push((r as i32, c as i32));
            }
        }
    }

    let rows_and_cols: (Vec<i32>, Vec<i32>) = positions.iter().cloned().unzip();
    let num_rows = rows_and_cols.0.iter().max().unwrap() + 1;
    let num_cols = rows_and_cols.1.iter().max().unwrap() + 1;
    let deltas = vec![(-1, 0), (1, 0), (0, -1), (0, 1)];
    let map = HashMap::from_iter(positions.iter().map(|(r, c)| {
        (
            (*r, *c),
            deltas
                .iter()
                .map(|(rd, cd)| (r + rd, c + cd))
                .filter(|(r, c)| {
                    0 <= *r
                        && r < &num_rows
                        && 0 <= *c
                        && c < &num_cols
                        && positions.contains(&(*r, *c))
                })
                .collect(),
        )
    }));

    let begin = map.keys().filter(|(r, _)| *r == 0).next().unwrap().clone();
    let end = map
        .keys()
        .filter(|(r, _)| *r == num_rows - 1)
        .next()
        .unwrap()
        .clone();
    Ok((map, slopes, begin, end))
}
