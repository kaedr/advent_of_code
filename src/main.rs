#![allow(dead_code)]

mod aoc2024;

use std::{
    cell::{LazyCell, RefCell},
    cmp::Ordering,
    collections::{HashMap, HashSet},
    env,
    fs::{File, read_to_string},
    io::{self, BufRead},
    thread,
    time::Instant,
};

use regex::{Captures, Regex};

fn main() {
    let args: Vec<String> = env::args().collect();
    let now = Instant::now();
    match (args[1].as_str(), args[2].as_str(), args[3].as_str()) {
        ("2025", "1", file_name) => day_one(file_name),
        ("2025", "2", file_name) => day_two(file_name),
        ("2025", "3", file_name) => day_three(file_name),
        ("2025", "3b", file_name) => day_three_part_two(file_name),
        ("2025", "4", file_name) => day_four(file_name),
        ("2025", "4b", file_name) => day_four_part_two(file_name),
        ("2025", "5", file_name) => day_five(file_name),
        ("2025", "6", file_name) => day_six(file_name),
        ("2025", "6b", file_name) => day_six_part_two(file_name),
        ("2025", "7", file_name) => day_seven(file_name),
        ("2025", "8", file_name) => day_eight(file_name),
        ("2025", "9", file_name) => day_nine(file_name),
        ("2025", "10", file_name) => day_ten(file_name),
        ("2025", "11", file_name) => day_eleven(file_name),
        ("2025", "12", file_name) => day_twelve(file_name),
        ("2024", "1", file_name) => aoc2024::day_one(file_name),
        ("2024", "2", file_name) => aoc2024::day_two(file_name),
        ("2024", "2b", file_name) => aoc2024::day_two_b(file_name),
        ("2024", "3", file_name) => aoc2024::day_three(file_name),
        ("2024", "3b", file_name) => aoc2024::day_three_b(file_name),
        ("2024", "4", file_name) => aoc2024::day_four(file_name),
        ("2024", "5", file_name) => aoc2024::day_five(file_name),
        ("2024", "6", file_name) => aoc2024::day_six(file_name),
        (_, _, _) => panic!("WAT?! {:?}", args),
    }

    let elapsed = now.elapsed();
    println!("Execution took: {:.4?}", elapsed);
}

fn read_lines(path: &str) -> Vec<String> {
    if let Ok(file) = File::open(path) {
        let lines = io::BufReader::new(file).lines();
        lines.filter_map(|item| item.ok()).collect()
    } else {
        Vec::new()
    }
}

const COMBO_REGEX: LazyCell<Regex> =
    LazyCell::new(|| Regex::new(r"([LR])(\d+)").expect("Failed to compile regex"));

fn day_one(file_name: &str) {
    let line_strings = read_lines(file_name);
    let mut current_position = 50;
    let mut zero_count = 0;
    let mut pass_count = 0;

    for line in line_strings {
        match COMBO_REGEX
            .captures(&line)
            .expect(&format!("Failed to parse entry from: {line}"))
            .extract()
        {
            (_, [direction, number]) => {
                let mut number: isize = number
                    .parse()
                    .expect(&format!("Failed to parse usize from: {number}"));
                let mut passes = 0;
                // Handle big numbers
                passes += number / 100;
                number = number % 100;
                // println!("After Big numbers: {passes}");

                if direction == "L" {
                    if number >= current_position && current_position != 0 {
                        passes += 1;
                        // println!("After left handle: {passes}");
                    }
                    number = 100 - number
                } else {
                    passes += (current_position + number) / 100;
                    // println!("After right handle: {passes}");
                }
                current_position = (current_position + number) % 100;

                zero_count += if current_position == 0 { 1 } else { 0 };
                pass_count += passes;
                println!(
                    "Line: {line}, cur_pos: {current_position} (num: {number}, passed: {passes})"
                );
            }
        }
    }

    println!("Final zero_count is: {zero_count}");
    println!("Final pass_count is: {pass_count}");
}

const RANGE_REGEX: LazyCell<Regex> =
    LazyCell::new(|| Regex::new(r"(\d+)\-(\d+)").expect("Failed to compile regex"));

fn day_two(file_name: &str) {
    let source = read_to_string(file_name).expect("Failed to read source file");
    let mut invalid_total = 0;

    for item in RANGE_REGEX.captures_iter(&source) {
        match item.extract() {
            (_, [start, end]) => {
                let start: usize = start
                    .parse()
                    .expect(&format!("Failed to parse usize from: {start}"));
                let end: usize = end
                    .parse()
                    .expect(&format!("Failed to parse usize from: {end}"));
                for id in start..=end {
                    let id_str = id.to_string();
                    if day_two_part_two(&id_str) {
                        println!("Found: {id_str}");
                        invalid_total += id;
                    }
                }
            }
        }
    }
    println!("total of invalid ids found: {invalid_total}");
}

fn day_two_part_two(id_str: &str) -> bool {
    (2..=id_str.len()).any(|num_chunks| {
        if id_str.len() % num_chunks == 0 {
            let char_vec = id_str.chars().collect::<Vec<_>>();
            let chunk_size = id_str.len() / num_chunks;
            let check_chars = &char_vec[0..chunk_size];
            char_vec
                .chunks(chunk_size)
                .all(|chunk| chunk == check_chars)
        } else {
            false
        }
    })
}

fn day_three(file_name: &str) {
    let mut total_joltage = 0;

    for line in read_lines(file_name) {
        let digits = line
            .chars()
            .collect::<Vec<_>>()
            .windows(2)
            .fold(('0', '0'), |prev, curr| {
                if curr[0] > prev.0 {
                    (curr[0], curr[1])
                } else if curr[1] > prev.1 {
                    (prev.0, curr[1])
                } else {
                    prev
                }
            });
        let value = format!("{}{}", digits.0, digits.1)
            .parse::<usize>()
            .expect(&format!("Failed to parse digits: {:?}", digits));
        total_joltage += value;
    }

    println!("Total joltage: {total_joltage}");
}

fn day_three_part_two(file_name: &str) {
    let mut total_joltage = 0;

    for line in read_lines(file_name) {
        let digits =
            line.chars()
                .collect::<Vec<_>>()
                .windows(12)
                .fold(['0'; 12], |store, current| {
                    for idx in 0..12 {
                        if current[idx] > store[idx] {
                            return [&store[..idx], &current[idx..]].concat().try_into().expect(
                                &format!(
                                    "Couldn't combine slices: {:?} + {:?}",
                                    &store[..idx],
                                    &current[idx..],
                                ),
                            );
                        }
                    }
                    store
                });
        // println!("digits: {:?}", digits);
        let value = digits
            .iter()
            .collect::<String>()
            .parse::<usize>()
            .expect(&format!("Failed to parse digits: {:?}", digits));
        total_joltage += value;
    }

    println!("Total joltage: {total_joltage}");
}

fn day_four(file_name: &str) {
    let mut accessible_rolls = 0;
    let grid = read_lines(file_name)
        .iter()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let y_limit = grid.len();
    let x_limit = grid
        .first()
        .expect("Must have at least one row in grid")
        .len();

    for y in 0..y_limit {
        for x in 0..x_limit {
            let mut adjacent_roll_count = 0;
            let neighbors = neighbors(x as isize, y as isize, x_limit as isize, y_limit as isize);
            for (neighbor_x, neighbor_y) in neighbors {
                if grid[neighbor_y][neighbor_x] == '@' {
                    adjacent_roll_count += 1;
                }
            }
            if grid[y][x] == '@' && adjacent_roll_count < 4 {
                accessible_rolls += 1;
            }
        }
    }

    println!("Accessible rolls: {accessible_rolls}");
}

fn day_four_part_two(file_name: &str) {
    let mut number_rolls_removed = 0;
    let mut grid = read_lines(file_name)
        .iter()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let y_limit = grid.len();
    let x_limit = grid
        .first()
        .expect("Must have at least one row in grid")
        .len();

    let mut rolls_removed = true;
    while rolls_removed {
        rolls_removed = false;
        let mut rolls_to_remove = Vec::new();
        for y in 0..y_limit {
            for x in 0..x_limit {
                let mut adjacent_roll_count = 0;
                let neighbors =
                    neighbors(x as isize, y as isize, x_limit as isize, y_limit as isize);
                for (neighbor_x, neighbor_y) in neighbors {
                    if grid[neighbor_y][neighbor_x] == '@' {
                        adjacent_roll_count += 1;
                    }
                }
                if grid[y][x] == '@' && adjacent_roll_count < 4 {
                    rolls_to_remove.push((x, y));
                    number_rolls_removed += 1;
                }
            }
        }
        if !rolls_to_remove.is_empty() {
            for (x, y) in rolls_to_remove {
                grid[y][x] = 'X';
            }
            // println!("{:?}", grid);
            rolls_removed = true;
        }
    }

    println!("Removed rolls: {number_rolls_removed}");
}

fn neighbors(x: isize, y: isize, x_limit: isize, y_limit: isize) -> Vec<(usize, usize)> {
    let mut neighbors = Vec::new();
    for x_offset in -1..=1 {
        for y_offset in -1..=1 {
            let (x_coord, y_coord) = (x + x_offset, y + y_offset);
            if (x_offset != 0 || y_offset != 0)
                && x_coord >= 0
                && x_coord < x_limit
                && y_coord >= 0
                && y_coord < y_limit
            {
                neighbors.push((x_coord as usize, y_coord as usize));
            }
        }
    }
    neighbors
}

fn day_five(file_name: &str) {
    let source = read_to_string(file_name).expect("Failed to read source file");
    let lone_number_regex = Regex::new(r"(?m)^(\d+)$").expect("Failed to compile regex");
    let mut fresh_ingredients = 0;

    let mut ranges = RANGE_REGEX
        .captures_iter(&source)
        .map(extract_range)
        .collect::<Vec<_>>();
    ranges.sort();
    let ranges = ranges;

    let values = lone_number_regex
        .captures_iter(&source)
        .map(|number_capture| {
            number_capture
                .get(1)
                .expect("Y no can haz digits?")
                .as_str()
                .parse()
                .expect(&format!("Failed to parse usize from: {:?}", number_capture))
        })
        .collect::<Vec<usize>>();

    // println!("Ranges: {:?}", &ranges);

    for value in values {
        for (start, end) in &ranges {
            if (start..=end).contains(&&value) {
                // println!("{value} is in {start}..={end}");
                fresh_ingredients += 1;
                break;
            }
        }
    }

    let mut final_ranges: Vec<(usize, usize)> = Vec::new();
    for (start, end) in &ranges {
        let mut changes_made = false;

        for (front, back) in final_ranges.iter_mut() {
            match (
                (*start).cmp(back),
                (*start).cmp(front),
                (*end).cmp(back),
                (*end).cmp(front),
            ) {
                // Completely within
                (_, Ordering::Equal | Ordering::Greater, Ordering::Less | Ordering::Equal, _) => {
                    changes_made = true
                }
                // Completely above or Completely below
                (Ordering::Greater, _, _, _) | (_, _, _, Ordering::Less) => (),
                // Completely overlaps
                (_, Ordering::Less, Ordering::Greater, _) => {
                    // println!("Replacing {front}..{back} with {start}..{end}");
                    *front = *start;
                    *back = *end;
                    changes_made = true;
                }
                // Extends top
                (Ordering::Less | Ordering::Equal, _, Ordering::Greater, _) => {
                    // println!("Replacing {front}..{back} with {front}..{end}");
                    *back = *end;
                    changes_made = true;
                }
                // Extends bottom
                (_, Ordering::Less, _, Ordering::Equal | Ordering::Greater) => {
                    // println!("Replacing {front}..{back} with {start}..{back}");
                    *front = *start;
                    changes_made = true;
                } // WAT?!
                  // (start_back, start_front, end_back, end_front) => {
                  //     unimplemented!(
                  //         "{start}..{end} ({:?}, {:?}, {:?}, {:?}) {front}..{back}",
                  //         start_back,
                  //         start_front,
                  //         end_back,
                  //         end_front
                  //     )
                  // }
            }
        }
        if !changes_made {
            final_ranges.push((*start, *end));
        }
    }

    let mut fresh_id_count = 0;

    for (start, end) in &final_ranges {
        fresh_id_count += (end - start) + 1;
    }

    println!("Fresh ingredients: {fresh_ingredients}");
    // println!("Fresh ids: {:?}", &final_ranges);
    println!("Fresh ids: {fresh_id_count}");
}

fn extract_range(range: Captures<'_>) -> (usize, usize) {
    let (_, [start, end]) = range.extract();
    let start: usize = start
        .parse()
        .expect(&format!("Failed to parse usize from: {start}"));
    let end: usize = end
        .parse()
        .expect(&format!("Failed to parse usize from: {end}"));
    (start, end)
}

#[derive(Debug)]
enum Op {
    Add,
    Mul,
}

#[derive(Debug)]
struct Problem {
    op: Op,
    terms: Vec<usize>,
}

impl Problem {
    fn from_symbol(symbol: &str) -> Self {
        Self {
            op: match symbol {
                "+" => Op::Add,
                "*" => Op::Mul,
                _ => unreachable!(),
            },
            terms: Vec::new(),
        }
    }

    fn ingest_term(&mut self, term_str: &str) {
        self.terms.push(
            term_str
                .trim()
                .parse()
                .expect(&format!("Somebody formatted something wrong: '{term_str}'")),
        );
    }

    fn evaluate(&self) -> usize {
        self.terms
            .iter()
            .cloned()
            .reduce(|acc, item| match self.op {
                Op::Add => acc + item,
                Op::Mul => acc * item,
            })
            .expect("Y u no have terms?!")
    }
}

fn day_six(file_name: &str) {
    let lines = read_lines(file_name);
    let mut split_lines = lines
        .iter()
        .map(|line| line.split_ascii_whitespace().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let mut problems: Vec<Problem> = split_lines
        .pop()
        .expect("Must have lines!")
        .into_iter()
        .map(Problem::from_symbol)
        .collect();

    for line in split_lines {
        for (idx, item) in line.iter().enumerate() {
            problems[idx].ingest_term(item);
        }
    }

    let mut total_output = 0;
    for problem in problems {
        println!("SubTotal: {}", problem.evaluate());
        total_output += problem.evaluate();
    }

    println!("Total: {total_output}");
}

fn day_six_part_two(file_name: &str) {
    let lines = read_lines(file_name);
    let mut line_iters = lines.iter().map(|line| line.chars()).collect::<Vec<_>>();

    let mut problems: Vec<Problem> = Vec::new();
    let mut terms: Vec<String> = Vec::new();

    let mut not_finished = true;
    while not_finished {
        let mut term = String::new();
        for line_iter in &mut line_iters {
            match line_iter.next_back() {
                symbol @ Some('+') | symbol @ Some('*') => {
                    terms.push(term.clone());
                    let mut problem = Problem::from_symbol(
                        symbol
                            .expect("This should literally be impossible")
                            .to_string()
                            .as_str(),
                    );
                    for term_str in &terms {
                        if !term_str.trim().is_empty() {
                            problem.ingest_term(term_str);
                        }
                    }
                    terms.clear();
                    problems.push(problem);
                }
                Some(ch) => {
                    term.push(ch);
                    // println!("{}, {}", terms.len(), lines.len());
                    if term.len() >= lines.len() {
                        terms.push(term.clone());
                    }
                }
                None => not_finished = false,
            }
        }
    }

    let mut total_output = 0;
    for problem in problems {
        // println!("{:?}", problem);
        println!("SubTotal: {}", problem.evaluate());
        total_output += problem.evaluate();
    }

    println!("Total: {total_output}");
}

fn day_seven(file_name: &str) {
    let mut num_splits = 0;
    let mut beams = HashSet::new();
    let mut quantum_beams: Vec<u64> = Vec::new();
    let lines = read_lines(file_name);
    for line in &lines {
        if let Some(idx) = line.find("S") {
            quantum_beams = vec![0; line.len()];
            beams.insert(idx);
            quantum_beams[idx] += 1;
        } else {
            for (splindex, _) in line.match_indices("^") {
                if beams.contains(&splindex) {
                    beams.remove(&splindex);
                    num_splits += 1;
                    beams.insert(splindex - 1);
                    beams.insert(splindex + 1);
                }

                if quantum_beams[splindex] > 0 {
                    quantum_beams[splindex - 1] += quantum_beams[splindex];
                    quantum_beams[splindex + 1] += quantum_beams[splindex];
                    quantum_beams[splindex] = 0;
                }
            }
        }
    }
    println!("Split {num_splits} times.");
    println!(
        "Created {:?} realities",
        quantum_beams.into_iter().reduce(|l, r| l + r)
    );
}

/// This is really bad, like Trillions of function calls bad
fn day_seven_part_two(lines: &Vec<String>, current_line: usize, tachyon_path: usize) -> usize {
    // println!("Line: {current_line}, path: {tachyon_path}");
    if current_line == 0 {
        if let Some(tachyon_path) = lines[0].find("S") {
            day_seven_part_two(lines, current_line + 1, tachyon_path)
        } else {
            unreachable!("BROKEN INPUT!")
        }
    } else if current_line >= lines.len() {
        1
    } else if lines[current_line].as_bytes()[tachyon_path] == b'^' {
        day_seven_part_two(lines, current_line + 1, tachyon_path - 1)
            + day_seven_part_two(lines, current_line + 1, tachyon_path + 1)
    } else {
        day_seven_part_two(lines, current_line + 1, tachyon_path)
    }
}

#[derive(Debug)]
struct JBox {
    x: usize,
    y: usize,
    z: usize,
    circuit: RefCell<usize>,
}

impl From<&str> for JBox {
    fn from(value: &str) -> Self {
        let v = splinteger(value);
        let (x, y, z) = (v[0], v[1], v[2]);
        Self {
            x,
            y,
            z,
            circuit: RefCell::new(0),
        }
    }
}

impl JBox {
    fn distance_from(&self, other: &Self) -> f32 {
        ((self.x as f32 - other.x as f32).powi(2)
            + (self.y as f32 - other.y as f32).powi(2)
            + (self.z as f32 - other.z as f32).powi(2))
        .sqrt()
    }

    fn connect_to(&self, other: &Self, next_circuit: usize) -> Outcome {
        let (l_circ, r_circ) = (
            self.circuit.borrow().clone(),
            other.circuit.borrow().clone(),
        );
        match (l_circ, r_circ) {
            (0, 0) => {
                *self.circuit.borrow_mut() = next_circuit;
                *other.circuit.borrow_mut() = next_circuit;
                Outcome::IncNextCircuit
            }
            (0, circ) | (circ, 0) => {
                *self.circuit.borrow_mut() = circ;
                *other.circuit.borrow_mut() = circ;
                Outcome::AddedToExisting(circ)
            }
            (l_circ, r_circ) => {
                if l_circ != r_circ {
                    *other.circuit.borrow_mut() = l_circ;
                    Outcome::Connect(l_circ, r_circ)
                } else {
                    Outcome::AlreadyConnected
                }
            }
        }
    }
}

enum Outcome {
    AddedToExisting(usize),
    IncNextCircuit,
    Connect(usize, usize),
    AlreadyConnected,
}

#[derive(Debug)]
struct Distance {
    gap: f32,
    a: usize,
    b: usize,
}

impl PartialOrd for Distance {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.gap.partial_cmp(&other.gap)
    }
}

impl Ord for Distance {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other)
            .expect("The existance of invalid floats is a plague on mankind")
    }
}

impl PartialEq for Distance {
    fn eq(&self, other: &Self) -> bool {
        self.gap == other.gap
    }
}

impl Eq for Distance {}

fn day_eight(file_name: &str) {
    let mut j_boxes = Vec::new();
    let mut distances = Vec::new();
    for line in read_lines(file_name) {
        let jbox = JBox::from(line.as_str());
        let pos = j_boxes.len();
        for (i, item) in j_boxes.iter().enumerate() {
            distances.push(Distance {
                gap: jbox.distance_from(item),
                a: pos,
                b: i,
            });
        }
        j_boxes.push(jbox);
    }

    distances.sort();
    let mut next_circuit = 1;
    let mut circuits: HashMap<usize, HashSet<usize>> = HashMap::new();

    let mut last_two = (0, 0);

    let mut distances_iter = distances.iter();
    while let Some(distance) = distances_iter.next()
        && !(circuits.len() == 1
            && circuits
                .values()
                .next()
                .expect("I hope this works...")
                .len()
                == 1000)
    {
        let left = &j_boxes[distance.a];
        let right = &j_boxes[distance.b];
        last_two = (distance.a, distance.b);

        match left.connect_to(right, next_circuit) {
            Outcome::AddedToExisting(circ) => {
                circuits
                    .get_mut(&circ)
                    .expect("I know it's here")
                    .insert(distance.a);
                circuits
                    .get_mut(&circ)
                    .expect("I know it's here")
                    .insert(distance.b);
            }
            Outcome::IncNextCircuit => {
                circuits.insert(
                    next_circuit,
                    vec![distance.a, distance.b].into_iter().collect(),
                );
                next_circuit += 1;
            }
            Outcome::Connect(l_circ, r_circ) => {
                let members = circuits.remove(&r_circ).expect("I know it's here");
                for member in &members {
                    *(j_boxes[*member].circuit.borrow_mut()) = l_circ;
                }
                circuits
                    .get_mut(&l_circ)
                    .expect("I know it's here")
                    .extend(members);
            }
            Outcome::AlreadyConnected => (),
        }
    }

    println!("{} * {}", j_boxes[last_two.0].x, j_boxes[last_two.1].x);
    let total = j_boxes[last_two.0].x * j_boxes[last_two.1].x;
    println!("Total: {total}");
}

fn day_nine(file_name: &str) {
    let mut red_tiles: Vec<(usize, usize)> = Vec::new();
    let mut rectangles: Vec<(usize, (usize, usize), (usize, usize))> = Vec::new();
    for line in read_lines(file_name) {
        let (x, y) = line.split_once(',').expect("BAD DATA!");
        let (x, y) = (
            x.parse::<usize>().expect("Y U NO INT"),
            y.parse::<usize>().expect("Y U NO INT"),
        );
        for (x_2, y_2) in red_tiles.iter() {
            rectangles.push((
                (x.abs_diff(*x_2) + 1) * (y.abs_diff(*y_2) + 1),
                (*x_2, *y_2),
                (x, y),
            ));
        }
        red_tiles.push((x, y));
    }

    rectangles.sort();

    println!(
        "Bigun: {:?}",
        rectangles.last().expect("How is it not here?")
    );

    // Walk it back
    for (area, corner_a, corner_b) in rectangles.iter().rev() {
        let (top_left, bottom_right) = tl_br(*corner_a, *corner_b);
        if red_tiles
            .windows(2)
            .filter(|items| {
                let (tl, br) = tl_br(items[0], items[1]);
                tl.0 < bottom_right.0
                    && br.0 > top_left.0
                    && tl.1 < bottom_right.1
                    && br.1 > top_left.1
            })
            .count()
            == 0
        {
            println!("{area}");
            break;
        }
    }

    // println!("{:?}", rectangles);
}

fn tl_br(corner_a: (usize, usize), corner_b: (usize, usize)) -> ((usize, usize), (usize, usize)) {
    match (corner_a.0 < corner_b.0, corner_a.1 < corner_b.1) {
        (true, true) => ((corner_a), (corner_b)),
        (true, false) => ((corner_a.0, corner_b.1), (corner_b.0, corner_a.1)),
        (false, true) => ((corner_b.0, corner_a.1), (corner_a.0, corner_b.1)),
        (false, false) => ((corner_b), (corner_a)),
    }
}

fn splinteger(input: &str) -> Vec<usize> {
    input
        .split(',')
        .map(|val| val.parse().expect(&format!("BAD DATA! {val} from {input}")))
        .collect::<Vec<_>>()
}

const TARGET_REGEX: LazyCell<Regex> =
    LazyCell::new(|| Regex::new(r"\[([\.#]+)\]").expect("Failed to compile regex"));
const BUTTONS_REGEX: LazyCell<Regex> =
    LazyCell::new(|| Regex::new(r"\((.+?)\)").expect("Failed to compile regex"));
const JOLTAGE_REGEX: LazyCell<Regex> =
    LazyCell::new(|| Regex::new(r"\{(.+?)\}").expect("Failed to compile regex"));

fn day_ten(file_name: &str) {
    struct Machine {
        target_state: usize,
        buttons: Vec<usize>,
        joltages: Vec<u8>,
    }

    impl Machine {
        fn from_str(input: &str) -> Self {
            let target_state: usize = TARGET_REGEX
                .captures(input)
                .expect("Captured Nothing!")
                .get(1)
                .expect("With no subgroups?!")
                .as_str()
                .chars()
                .enumerate()
                .fold(0, |acc, (i, ch)| {
                    if ch == '#' {
                        acc + 2usize.pow(i as u32)
                    } else {
                        acc
                    }
                });

            let buttons = BUTTONS_REGEX
                .captures_iter(input)
                .map(|button_text| {
                    splinteger(button_text.get(1).expect("Faulty regex?").as_str())
                        .into_iter()
                        .fold(0, |acc, val| acc + 2usize.pow(val as u32))
                })
                .collect::<Vec<_>>();

            let joltages = splinteger(
                JOLTAGE_REGEX
                    .captures(input)
                    .expect("Captured Nothing!")
                    .get(1)
                    .expect("With no subgroups?!")
                    .as_str(),
            )
            .iter()
            .map(|i| *i as u8)
            .collect::<Vec<_>>();

            Self {
                target_state,
                buttons,
                joltages,
            }
        }

        fn part_one_presses(&self) -> usize {
            let mut layers_deep = 0;
            let mut states = vec![0];
            let mut new_states = Vec::new();
            loop {
                for state in states {
                    for button in &self.buttons {
                        if state == self.target_state {
                            println!("{:8b} took {layers_deep} presses!", &self.target_state);
                            return layers_deep;
                        } else {
                            // println!("Pushing {:8b} on {:8b}", button, state);
                            new_states.push(state ^ button);
                        }
                    }
                }

                states = new_states;
                new_states = Vec::new();
                layers_deep += 1;
            }
        }

        fn part_two_presses(&self) -> usize {
            if let Some(presses) =
                self.part_two_solver(1, self.buttons.clone(), vec![0; self.joltages.len()])
            {
                presses
            } else {
                unreachable!("We done borked it!");
            }
        }

        fn part_two_solver(
            &self,
            depth: usize,
            buttons: Vec<usize>,
            state: Vec<u8>,
        ) -> Option<usize> {
            buttons
                .iter()
                .filter_map(|button| {
                    let new_state = button_on_joltage_state(*button, &state);
                    if new_state == self.joltages {
                        Some(depth)
                    } else if new_state
                        .iter()
                        .zip(self.joltages.iter())
                        .any(|(new, jolt)| new > jolt)
                    {
                        None
                    } else {
                        let still_valid_buttons = buttons
                            .iter()
                            .filter(|button| {
                                new_state.iter().zip(self.joltages.iter()).enumerate().all(
                                    |(position, (new, jolt))| {
                                        !(new == jolt && affects_position(**button, position))
                                    },
                                )
                            })
                            .cloned()
                            .collect::<Vec<_>>();
                        println!("{:?}", still_valid_buttons);
                        self.part_two_solver(depth + 1, still_valid_buttons, new_state)
                    }
                })
                .min()
        }
    }

    fn affects_position(button: usize, position: usize) -> bool {
        let place_value = 2usize.pow(position as u32);
        button & place_value == place_value
    }

    fn button_on_joltage_state(button: usize, state: &Vec<u8>) -> Vec<u8> {
        let mut new_state = Vec::new();
        for (position, val) in state.iter().enumerate() {
            if affects_position(button, position) {
                new_state.push(val + 1);
            } else {
                new_state.push(*val);
            }
        }
        new_state
    }

    let presses = read_lines(file_name)
        .iter()
        .map(|input| Machine::from_str(input))
        .fold((0, 0), |(p1, p2), problem| {
            (
                p1 + problem.part_one_presses(),
                p2 + problem.part_two_presses(),
            )
        });

    println!("Part 1 Presses: {}", presses.0);
    println!("Part 1 Presses: {}", presses.1);
}

fn day_eleven(file_name: &str) {
    fn nodewalker<'a>(
        node: &'a str,
        destination: &'a str,
        nodes: &HashMap<String, Vec<String>>,
        memo: &mut HashMap<String, usize>,
    ) -> Option<usize> {
        // This works for two reasons, first: .all() returns true on an empty iter
        // see below for second reason.
        if node == destination {
            Some(1)
        } else {
            // since nodes.get returns None if that node isn't a key
            // such as our friend 'out', this handles
            nodes.get(node).map(|paths| {
                paths
                    .iter()
                    .filter_map(|next| {
                        if let Some(visit_count) = memo.get(next) {
                            Some(*visit_count)
                        } else {
                            if let Some(paths) = nodewalker(next, destination, nodes, memo) {
                                memo.insert(next.clone(), paths);
                                Some(paths)
                            } else {
                                None
                            }
                        }
                    })
                    .sum()
            })
        }
    }

    let lines = read_lines(file_name);
    let nodes: HashMap<String, Vec<String>> = lines
        .iter()
        .map(|line| {
            let (key, tail) = line.split_once(':').expect("MUST HAS COLON!");
            let values = tail
                .split_whitespace()
                .map(|s| s.to_string())
                .collect::<Vec<_>>();
            (key.to_string(), values)
        })
        .collect();

    println!(
        "Paths (you -> out): {:?}",
        nodewalker("you", "out", &nodes, &mut HashMap::new())
    );

    let order_a = ["svr", "dac", "fft", "out"];
    let order_b = ["svr", "fft", "dac", "out"];

    let nodes_a = nodes.clone();
    let handle_a = thread::spawn(move || {
        let total: Option<usize> = order_a
            .windows(2)
            .map(|window| {
                println!("(A) trying {} -> {}", window[0], window[1]);
                nodewalker(window[0], window[1], &nodes_a, &mut HashMap::new())
            })
            .product();

        println!("(A) {:?} paths (svr -> dac -> fft -> out )", total);
    });

    let total: Option<usize> = order_b
        .windows(2)
        .map(|window| {
            println!("(B) trying {} -> {}", window[0], window[1]);
            nodewalker(window[0], window[1], &nodes, &mut HashMap::new())
        })
        .product();

    println!("(B) {:?} paths (svr -> fft -> dac -> out )", total);

    handle_a.join().expect("WAT");
}

fn day_twelve(file_name: &str) {
    struct PackageShape {
        matrix: [[bool; 3]; 3],
    }

    impl PackageShape {
        fn slots_filled(&self) -> usize {
            self.matrix.iter().flatten().filter(|i| **i).count()
        }
    }

    struct Problem {
        x: usize,
        y: usize,
        quantities: Vec<usize>,
    }

    impl Problem {
        fn area(&self) -> usize {
            self.x * self.y
        }

        fn simple_fit(&self, pkgs: &Vec<PackageShape>) -> bool {
            self.area()
                >= self
                    .quantities
                    .iter()
                    .zip(pkgs.iter())
                    .fold(0, |acc, (q, pkg)| acc + (q * pkg.slots_filled()))
        }
    }

    let mut package_types: Vec<PackageShape> = Vec::new();
    let mut problems: Vec<Problem> = Vec::new();

    let lines = read_lines(file_name);
    let mut line_iter = lines.iter();

    while let Some(line) = line_iter.next() {
        if let Some(second_char) = line.chars().nth(1)
            && second_char == ':'
        {
            let (one, two, three) = (
                line_iter
                    .next()
                    .expect("Bad present!")
                    .chars()
                    .collect::<Vec<_>>(),
                line_iter
                    .next()
                    .expect("Bad present!")
                    .chars()
                    .collect::<Vec<_>>(),
                line_iter
                    .next()
                    .expect("Bad present!")
                    .chars()
                    .collect::<Vec<_>>(),
            );
            package_types.push(PackageShape {
                matrix: [
                    [one[0] == '#', one[1] == '#', one[2] == '#'],
                    [two[0] == '#', two[1] == '#', two[2] == '#'],
                    [three[0] == '#', three[1] == '#', three[2] == '#'],
                ],
            });
        } else if let Some((x, rest)) = line.split_once('x')
            && let Some((y, quants)) = rest.split_once(':')
        {
            problems.push(Problem {
                x: x.parse().expect("BOOO"),
                y: y.parse().expect("YYYY?!"),
                quantities: quants
                    .split_whitespace()
                    .map(|qt| qt.parse().expect("BADNESS"))
                    .collect(),
            });
        }
    }

    println!(
        "{} packages, {} problems",
        package_types.len(),
        problems.len()
    );

    let tricky = problems
        .iter()
        .filter(|problem| problem.simple_fit(&package_types))
        .collect::<Vec<_>>();
    println!("{} survived easy elim", tricky.len());
}
