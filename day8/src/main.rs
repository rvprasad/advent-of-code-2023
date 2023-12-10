use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

#[derive(Debug)]
struct Data {
    movements: Vec<char>,
    node_to_left_node: HashMap<String, String>,
    node_to_right_node: HashMap<String, String>,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let data = get_data(&args[1]);
    println!("{:?}", process1(&data));
    println!("{:?}", process2(&data));
}

fn process2(data: &Data) -> usize {
    fn calculate_gcd(numbers: &[usize]) -> usize {
        fn calculate_gcd2(a: usize, b: usize) -> usize {
            return if b == 0 { a } else { calculate_gcd2(b, a % b) };
        }
        let b = if numbers.len() == 2 {
            numbers[1]
        } else {
            calculate_gcd(&numbers[1..])
        };
        calculate_gcd2(numbers[0], b)
    }

    let strides = data
        .node_to_left_node
        .keys()
        .filter(|k| k.ends_with("A"))
        .map(|k| calculate_steps(k, |x| x.ends_with("Z"), data, 0, 0))
        .collect::<Vec<usize>>();
    let gcd = calculate_gcd(&strides[0..]);
    return strides
        .iter()
        .fold(1usize, |acc, e| acc.checked_mul(*e / gcd).unwrap())
        / gcd;
}

fn process1(data: &Data) -> usize {
    return calculate_steps(&"AAA".to_string(), |x| x == "ZZZ", &data, 0, 0);
}

fn calculate_steps(
    node: &String,
    is_end_node: fn(&String) -> bool,
    data: &Data,
    index: usize,
    len: usize,
) -> usize {
    let new_node = match data.movements[index] {
        'R' => &data.node_to_right_node[node],
        'L' => &data.node_to_left_node[node],
        _ => panic!("Invalid state"),
    };
    if is_end_node(new_node) {
        return index + len + 1;
    }
    if index + 1 < data.movements.len() {
        return calculate_steps(&new_node, is_end_node, data, index + 1, len);
    }
    return calculate_steps(&new_node, is_end_node, data, 0, len + data.movements.len());
}

fn get_data(filename: &String) -> Data {
    let f = File::open(filename).unwrap();
    let reader = BufReader::new(f);
    let mut lines = reader.lines().map(|l| l.unwrap());
    let movements = Vec::from_iter(lines.next().unwrap().chars());
    lines.next();

    fn process_line(line: String) -> (String, String, String) {
        let tmp1: Vec<String> = line
            .replace(' ', "")
            .split(|x| "(),= ".contains(x))
            .filter(|x| !x.is_empty())
            .map(String::from)
            .collect();
        (tmp1[0].to_owned(), tmp1[1].to_owned(), tmp1[2].to_owned())
    }

    let triples = lines.map(process_line);
    let tmp1: (Vec<(String, String)>, Vec<(String, String)>) =
        triples.map(|t| ((t.0.clone(), t.1), (t.0, t.2))).unzip();
    Data {
        movements,
        node_to_left_node: HashMap::from_iter(tmp1.0),
        node_to_right_node: HashMap::from_iter(tmp1.1),
    }
}
