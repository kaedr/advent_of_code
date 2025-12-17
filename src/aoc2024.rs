use std::{
    cell::LazyCell,
    cmp::Ordering,
    collections::HashMap,
    fs::{File, read_to_string},
    io::{self, BufRead},
    isize,
};

use regex::Regex;

use crate::read_lines;

pub fn day_one(file_path: &str) {
    let (mut nums_a, mut nums_b): (Vec<isize>, Vec<isize>) = (Vec::new(), Vec::new());

    if let Ok(file) = File::open(file_path) {
        let lines = io::BufReader::new(file).lines();

        for (left, right) in lines.filter_map(|line| {
            line.ok()?.split_once("   ").map(|(left, right)| {
                (
                    left.parse::<isize>().unwrap(),
                    right.parse::<isize>().unwrap(),
                )
            })
        }) {
            nums_a.push(left);
            nums_b.push(right);
        }

        nums_a.sort();
        nums_b.sort();

        let total = nums_a
            .iter()
            .zip(&nums_b)
            .fold(0, |accum, (left, right)| accum + (left - right).abs());

        println!("{}", total);

        let sim_score = nums_a.iter().fold(0, |accum, a| {
            accum + a * nums_b.iter().filter(|b| &a == b).count() as isize
        });
        println!("{}", sim_score);
    }
}

pub fn day_two(file_path: &str) {
    let lines = read_lines(file_path);
    let count = lines
        .iter()
        .filter(|item| {
            let differences: Vec<_> = item
                .split_whitespace()
                .map(|item| item.parse::<isize>().unwrap())
                .collect::<Vec<_>>()
                .windows(2)
                .map(|pair| pair[0] - pair[1])
                .collect();

            differences.iter().all(|item| *item > 0 && *item < 4)
                || differences.iter().all(|item| *item < 0 && *item > -4)
        })
        .count();
    println!("{}", count)
}

fn check_sequence(seq: &[isize]) -> bool {
    let differences = seq
        .windows(2)
        .map(|pair| pair[0] - pair[1])
        .collect::<Vec<_>>();

    differences.iter().all(|item| *item > 0 && *item < 4)
        || differences.iter().all(|item| *item < 0 && *item > -4)
}

pub fn day_two_b(file_path: &str) {
    let lines = read_lines(file_path);
    let mut count = 0;

    for line in lines {
        let values: Vec<_> = line
            .split_whitespace()
            .map(|item| item.parse::<isize>().unwrap())
            .collect();

        if check_sequence(&values) {
            count += 1;
        } else {
            for idx in 0..values.len() {
                let one_out: Vec<_> = [&values[..idx], &values[idx + 1..]]
                    .concat()
                    .try_into()
                    .expect(&format!(
                        "Couldn't combine slices: {:?} + {:?}",
                        &values[..idx],
                        &values[idx + 1..],
                    ));
                if check_sequence(&one_out) {
                    count += 1;
                    println!("{:?}", one_out);
                    break;
                }
            }
        }
    }

    println!("{}", count)
}

const MUL_REGEX: LazyCell<Regex> =
    LazyCell::new(|| Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").expect("Failed to compile regex"));

pub fn day_three(file_path: &str) {
    let source_text = read_to_string(file_path).expect("Failed to read source file");
    let mut total = 0;

    for items in MUL_REGEX.captures_iter(&source_text) {
        let (_, [left, right]) = items.extract();
        let left: usize = left
            .parse()
            .expect(&format!("Failed to parse usize from: {left}"));
        let right: usize = right
            .parse()
            .expect(&format!("Failed to parse usize from: {right}"));
        total += left * right;
    }

    println!("Total: {total}");
}

pub fn day_three_b(file_path: &str) {
    let source_text = read_to_string(file_path).expect("Failed to read source file");
    let mut total = 0;

    let segments = source_text.split("do");
    for segment in segments {
        if !segment.starts_with("n't()") {
            for items in MUL_REGEX.captures_iter(segment) {
                let (_, [left, right]) = items.extract();
                let left: usize = left
                    .parse()
                    .expect(&format!("Failed to parse usize from: {left}"));
                let right: usize = right
                    .parse()
                    .expect(&format!("Failed to parse usize from: {right}"));
                total += left * right;
            }
        }
    }

    println!("Total: {total}");
}

pub fn day_four(file_path: &str) {
    let directions: [(isize, isize); 8] = [
        (-1, 0),
        (-1, 1),
        (0, 1),
        (1, 1),
        (1, 0),
        (1, -1),
        (0, -1),
        (-1, -1),
    ];

    let grid = read_lines(file_path)
        .iter()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut xmas_found_count = 0;
    let mut two_mas_found_count = 0;

    for (y, row) in grid.iter().enumerate() {
        for (x, item) in row.iter().enumerate() {
            if *item == 'X' {
                for (y_offset, x_offset) in directions {
                    if is_right(&grid, y as isize + y_offset, x as isize + x_offset, 'M')
                        && is_right(
                            &grid,
                            y as isize + y_offset * 2,
                            x as isize + x_offset * 2,
                            'A',
                        )
                        && is_right(
                            &grid,
                            y as isize + y_offset * 3,
                            x as isize + x_offset * 3,
                            'S',
                        )
                    {
                        xmas_found_count += 1;
                    }
                }
            } else if *item == 'A' && x_mas(&grid, y as isize, x as isize) {
                two_mas_found_count += 1;
            }
        }
    }

    println!("Found (XMAS): {xmas_found_count}");
    println!("Found (MAS): {two_mas_found_count}");
}

fn is_right(grid: &Vec<Vec<char>>, y: isize, x: isize, ch: char) -> bool {
    if let Some(row) = grid.get(y as usize) {
        if let Some(item) = row.get(x as usize) {
            return *item == ch;
        }
    }
    false
}

fn x_mas(grid: &Vec<Vec<char>>, y: isize, x: isize) -> bool {
    if (is_right(grid, y + 1, x + 1, 'M') && is_right(grid, y - 1, x - 1, 'S')
        || is_right(grid, y + 1, x + 1, 'S') && is_right(grid, y - 1, x - 1, 'M'))
        && (is_right(grid, y - 1, x + 1, 'M') && is_right(grid, y + 1, x - 1, 'S')
            || is_right(grid, y - 1, x + 1, 'S') && is_right(grid, y + 1, x - 1, 'M'))
    {
        true
    } else {
        false
    }
}

pub fn day_five(file_path: &str) {
    let lines = read_lines(file_path);
    let mut rules: HashMap<&str, Vec<&str>> = HashMap::new();

    let mut center_vals = Vec::new();
    let mut incorrect_central_vals = Vec::new();

    for line in &lines {
        if let Some((l, r)) = line.split_once('|') {
            rules.entry(l).or_default().push(r);
        } else {
            let mut dirty = false;
            let mut invalids: Vec<&str> = Vec::new();
            let mut line_items = line.split(',').collect::<Vec<_>>();
            for line_item in line_items.iter().rev() {
                if invalids.contains(line_item) {
                    dirty = true;
                    break;
                } else {
                    if let Some(items) = rules.get(line_item) {
                        invalids.extend_from_slice(items)
                    }
                }
            }
            if dirty {
                line_items.sort_by(|l, r| {
                    // println!("{l} <=> {r}");
                    if let Some(left) = rules.get(l)
                        && left.contains(r)
                    {
                        std::cmp::Ordering::Less
                    } else if let Some(right) = rules.get(r)
                        && right.contains(l)
                    {
                        std::cmp::Ordering::Greater
                    } else {
                        std::cmp::Ordering::Equal
                    }
                });
                incorrect_central_vals.push(line_items[line_items.len() / 2]);
            }
            if !dirty && line_items.len() > 1 {
                center_vals.push(line_items[line_items.len() / 2]);
            }
        }
    }

    println!(
        "Total of correct centers: {}",
        center_vals.iter().fold(0, |prev, val| prev
            + val.parse::<usize>().expect("Bad parse!"))
    );

    println!(
        "Total of incorrect centers: {}",
        incorrect_central_vals.iter().fold(0, |prev, val| prev
            + val.parse::<usize>().expect("Bad parse!"))
    );
}

#[derive(Debug)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    fn apply(&self, input: (isize, isize)) -> (isize, isize) {
        let (y, x) = input;
        match self {
            Self::Up => (y - 1, x),
            Self::Right => (y, x + 1),
            Self::Down => (y + 1, x),
            Self::Left => (y, x - 1),
        }
    }

    fn turn(&mut self) {
        match self {
            Self::Up => *self = Self::Right,
            Self::Right => *self = Self::Down,
            Self::Down => *self = Self::Left,
            Self::Left => *self = Self::Up,
        }
    }
}

fn get_from_grid(grid: &Vec<Vec<char>>, coords: (isize, isize)) -> Option<char> {
    let (y, x) = coords;
    grid.get(y as usize)
        .map(|row| row.get(x as usize))
        .flatten()
        .copied()
}

fn can_has_box(
    grid: &Vec<Vec<char>>,
    corners: &Vec<(isize, isize)>,
    dir: &Direction,
    coords: (isize, isize),
) -> bool {
    for corner in corners {
        match (dir, coords.0.cmp(&corner.0), coords.1.cmp(&corner.1)) {
            (Direction::Up, Ordering::Equal, Ordering::Less) => {
                if let Some(item) = grid
                    .get(corner.0 as usize)
                    .map(|row| row.get((corner.1 + 1) as usize))
                    .flatten()
                    && *item == '#'
                {
                    return true;
                }
            }
            (Direction::Right, Ordering::Less, Ordering::Equal) => {
                if let Some(item) = grid
                    .get((corner.0 + 1) as usize)
                    .map(|row| row.get(corner.1 as usize))
                    .flatten()
                    && *item == '#'
                {
                    return true;
                }
            }
            (Direction::Down, Ordering::Equal, Ordering::Greater) => {
                if let Some(item) = grid
                    .get(corner.0 as usize)
                    .map(|row| row.get((corner.1 - 1) as usize))
                    .flatten()
                    && *item == '#'
                {
                    return true;
                }
            }
            (Direction::Left, Ordering::Greater, Ordering::Equal) => {
                if let Some(item) = grid
                    .get((corner.0 - 1) as usize)
                    .map(|row| row.get(corner.1 as usize))
                    .flatten()
                    && *item == '#'
                {
                    return true;
                }
            }
            (_dir, _y_ord, _x_ord) => {
                // println!("Current: {:?}, Corner: {:?}", coords, corner);
                // println!("Moving: {:?}, y_ord: {:?}, x_ord: {:?}", dir, _y_ord, _x_ord);
                ()
            }
        }
    }
    false
}

pub fn day_six(file_path: &str) {
    let mut grid = read_lines(file_path)
        .iter()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let start_point = grid.iter().enumerate().find_map(|(y, row)| {
        row.iter().enumerate().find_map(|(x, val)| {
            if *val == '^' {
                Some((y as isize, x as isize))
            } else {
                None
            }
        })
    });

    if let Some((mut y, mut x)) = start_point {
        let mut previous_turns = Vec::new();
        let mut direction = Direction::Up;
        while y >= 0 && y < grid.len() as isize && x >= 0 && x < grid.len() as isize {
            if grid[y as usize][x as usize] != 'O' {
                grid[y as usize][x as usize] = 'X';
            }
            while let Some(val) = get_from_grid(&grid, direction.apply((y, x)))
                && val == '#'
            {
                direction.turn();
                previous_turns.push((y, x));
            }
            let mark_next = can_has_box(&grid, &previous_turns, &direction, (y, x));
            (y, x) = direction.apply((y, x));
            if mark_next {
                grid[y as usize][x as usize] = 'O';
            }
        }
    } else {
        unreachable!("There must always be a Lich King");
    }

    let mut loop_point_count = 0;
    let mut tile_count = 0;
    for row in grid {
        for item in row {
            if item == 'X' {
                tile_count += 1;
            } else if item == 'O' {
                loop_point_count += 1;
                tile_count += 1;
            }
            print!("{item}");
        }
        println!();
    }

    println!("{tile_count} Tiles visited.");
    println!("{loop_point_count} Loops possible.");
}
