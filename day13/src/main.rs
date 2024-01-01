use anyhow::Result;
use itertools::iproduct;
use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn main() {
    let args: Vec<String> = env::args().collect();
    if let Ok(patterns) = get_patterns(&args[1]) {
        println!("{:?}", process_for_part1(&patterns));
        println!("{:?}", process_for_part2(&patterns));
    }
}

fn process_for_part1(patterns: &Vec<Vec<String>>) -> usize {
    patterns
        .iter()
        .map(|pattern| {
            let tmp1 = get_mirroring_indices(pattern);
            let hor = tmp1.first().unwrap_or(&0);
            let transposed = transpose(pattern);
            let tmp1 = get_mirroring_indices(&transposed);
            let ver = tmp1.first().unwrap_or(&0);
            100 * hor + ver
        })
        .sum()
}

fn process_for_part2(patterns: &Vec<Vec<String>>) -> usize {
    fn calculate_summary(pattern: &Vec<String>) -> (Vec<usize>, Vec<usize>) {
        let hor = get_mirroring_indices(&pattern);
        let transposed = transpose(&pattern);
        let ver = get_mirroring_indices(&transposed);
        (hor, ver)
    }

    patterns
        .iter()
        .map(|pattern| {
            let (old_hor, old_ver) = calculate_summary(pattern);
            let ret = iproduct!(0..(pattern.len()), 0..(pattern[0].len()))
                .filter(|(i, j)| pattern[*i].chars().nth(*j).unwrap() == '#')
                .map(|(i, j)| {
                    let mut copy = pattern.clone();
                    copy[i].replace_range(j..(j + 1), ".");
                    copy
                })
                .find_map(|new_pattern| {
                    let (new_hor, new_ver) = calculate_summary(&new_pattern);
                    let tmp1 = new_hor.iter().filter(|e| !old_hor.contains(e)).next();
                    if tmp1.is_some() {
                        return tmp1.map(|x| x * 100);
                    }

                    let tmp2 = new_ver.iter().filter(|e| !old_ver.contains(e)).next();
                    if tmp2.is_some() {
                        tmp2.map(|x| x * 1)
                    } else {
                        None
                    }
                });
            ret.unwrap()
        })
        .sum()
}

fn get_mirroring_indices(pattern: &Vec<String>) -> Vec<usize> {
    fn is_mirroring(i: usize, pattern: &Vec<String>) -> bool {
        let len = pattern.len();
        let m = if 2 * i < len { i } else { len - i };
        (0..m)
            .into_iter()
            .all(|j| pattern[i - j - 1] == pattern[i + j])
    }

    (1..(pattern.len()))
        .filter(|i| is_mirroring(*i, pattern))
        .collect()
}

fn transpose(pattern: &Vec<String>) -> Vec<String> {
    let range = 0..(pattern.first().unwrap().len());
    let mut transpose = Vec::new();
    range.clone().for_each(|_| transpose.push(String::from("")));
    for row in pattern {
        for (i, ch) in row.char_indices() {
            transpose[i].push(ch);
        }
    }
    transpose
}

fn get_patterns(filename: &String) -> Result<Vec<Vec<String>>> {
    let mut patterns = Vec::new();

    let f = File::open(filename)?;
    let reader = BufReader::new(f);
    let mut cur_pattern = Vec::new();

    for line in reader.lines().flatten() {
        if line.is_empty() {
            if !cur_pattern.is_empty() {
                patterns.push(cur_pattern.clone());
            }
            cur_pattern.clear();
        } else {
            cur_pattern.push(line);
        }
    }
    if !cur_pattern.is_empty() {
        patterns.push(cur_pattern);
    }

    Ok(patterns)
}
