use anyhow::Result;
use partial_application::partial;
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn main() {
    let args: Vec<String> = env::args().collect();
    let content = get_content(&args[1]);
    let symbol_and_affected_positions = get_symbol_affected_positions(&content).unwrap();
    let part_num_and_positions = get_part_num_to_positions(&content).unwrap();
    process1(&symbol_and_affected_positions, &part_num_and_positions);
    process2(&symbol_and_affected_positions, &part_num_and_positions);
}

fn get_content(filename: &String) -> Vec<Vec<char>> {
    let f = File::open(filename).unwrap();
    let reader = BufReader::new(f);
    let lines = reader.lines().map(|l| l.unwrap()).filter_map(|l| {
        if l.is_empty() {
            None
        } else {
            Some(l.chars().collect())
        }
    });
    return Vec::from_iter(lines);
}

fn process2(
    symbol_and_affected_positions: &Vec<(char, HashSet<(i32, i32)>)>,
    part_num_and_positions: &Vec<(i32, HashSet<(i32, i32)>)>,
) {
    fn calculate_gear_ratio(
        position_to_part_num: &HashMap<(i32, i32), i32>,
        gear_affected_positions: &HashSet<(i32, i32)>,
    ) -> Option<(i32, i32)> {
        let mut affected_parts = Vec::from_iter(
            gear_affected_positions
                .iter()
                .filter_map(|p| position_to_part_num.get(p)),
        );
        affected_parts.sort();
        affected_parts.dedup();
        return if affected_parts.len() == 2 {
            Some((*affected_parts[0], *affected_parts[1]))
        } else {
            None
        };
    }

    let position_to_part_num: HashMap<(i32, i32), i32> = HashMap::from_iter(
        part_num_and_positions
            .iter()
            .flat_map(|(part_num, positions)| positions.iter().map(|p| (*p, *part_num))),
    );

    let gear_affected_positions = symbol_and_affected_positions
        .iter()
        .filter_map(|(sym, positions)| if *sym == '*' { Some(positions) } else { None });

    let gear_ratios = gear_affected_positions
        .filter_map(partial!(calculate_gear_ratio => &position_to_part_num, _));
    println!("{}", gear_ratios.fold(0, |acc, (a, b)| acc + a * b));
}

fn process1(
    symbol_and_affected_positions: &Vec<(char, HashSet<(i32, i32)>)>,
    part_num_and_positions: &Vec<(i32, HashSet<(i32, i32)>)>,
) {
    fn filter_relevant_parts(
        symbol_affected_positions: &HashSet<(i32, i32)>,
        part_num_positions: &(i32, HashSet<(i32, i32)>),
    ) -> Option<i32> {
        let (part_num, positions) = part_num_positions;
        return positions
            .iter()
            .find(|p| symbol_affected_positions.contains(p))
            .map(|_| part_num)
            .copied();
    }

    let symbol_affected_positions =
        symbol_and_affected_positions
            .iter()
            .fold(HashSet::<(i32, i32)>::new(), |mut acc, e| {
                acc.extend(e.1.iter());
                acc
            });
    let relevant_part_nums = Vec::from_iter(
        part_num_and_positions
            .iter()
            .filter_map(partial!(filter_relevant_parts => &symbol_affected_positions, _)),
    );
    println!("{}", relevant_part_nums.iter().fold(0, |acc, x| acc + x));
}

fn get_symbol_affected_positions(
    content: &Vec<Vec<char>>,
) -> Result<Vec<(char, HashSet<(i32, i32)>)>> {
    let mut ret = Vec::new();
    let non_symbols = Vec::from_iter("0123456789.".chars());
    for (row_num, row) in content.iter().enumerate() {
        for (col_num, elem) in row.iter().enumerate() {
            if non_symbols.contains(elem) {
                continue;
            }
            let mut positions = HashSet::new();
            let row_i32 = i32::try_from(row_num)?;
            let col_i32 = i32::try_from(col_num)?;
            for r in -1..=1 {
                for c in -1..=1 {
                    positions.insert((row_i32 + r, col_i32 + c));
                }
            }
            ret.push((*elem, positions));
        }
    }
    return Ok(ret);
}

fn get_part_num_to_positions(content: &Vec<Vec<char>>) -> Result<Vec<(i32, HashSet<(i32, i32)>)>> {
    let mut ret = Vec::new();
    let digits = Vec::from_iter("0123456789".chars());
    for (row_num, row) in content.iter().enumerate() {
        let row_i32 = i32::try_from(row_num)?;
        let mut captured_digits = Vec::new();
        let last_col = row.len() - 1;
        for (col_num, elem) in row.iter().enumerate() {
            let mut col_i32 = i32::try_from(col_num)?;
            let process_captured_digits;
            if digits.contains(elem) {
                captured_digits.push(*elem);
                process_captured_digits = col_num == last_col;
            } else {
                process_captured_digits = !captured_digits.is_empty();
                col_i32 -= 1;
            }
            if process_captured_digits {
                let num_positions = HashSet::from_iter(
                    (0..i32::try_from(captured_digits.len())?).map(|i| (row_i32, col_i32 - i)),
                );
                let number = i32::from_str_radix(&String::from_iter(captured_digits.clone()), 10)?;
                ret.push((number, num_positions));
                captured_digits.clear();
            }
        }
    }
    return Ok(ret);
}
