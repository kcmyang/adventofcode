use crate::lib::util;

enum Direction {
    Forward,
    Down,
    Up,
}

struct Movement {
    direction: Direction,
    magnitude: i32,
}

fn get_movements() -> Vec<Movement> {
    let mut result = Vec::<Movement>::new();

    if let Ok(lines) = util::read_lines("src/lib/day02/input.txt") {
        for line in lines {
            if let Ok(value) = line {
                let tokens: Vec<&str> = value.split(" ").collect();

                if tokens.len() < 2 {
                    panic!("expected at least 2 tokens")
                }

                let direction = match tokens[0] {
                    "forward" => Direction::Forward,
                    "down" => Direction::Down,
                    "up" => Direction::Up,
                    _ => panic!("{}", format!("invalid direction {d}", d = tokens[0])),
                };

                let magnitude = tokens[1].parse::<i32>().unwrap();

                result.push(Movement {
                    direction,
                    magnitude,
                });
            }
        }
    }

    result
}

pub fn sol() {
    let data = get_movements();

    // part 1
    let part1 = || -> i32 {
        let mut hpos: i32 = 0;
        let mut depth: i32 = 0;

        for movement in &data {
            match movement.direction {
                Direction::Forward => hpos += movement.magnitude,
                Direction::Down => depth += movement.magnitude,
                Direction::Up => depth -= movement.magnitude,
            }
        }

        hpos * depth
    };

    // part 2
    let part2 = || -> i32 {
        let mut hpos: i32 = 0;
        let mut depth: i32 = 0;
        let mut aim: i32 = 0;

        for movement in &data {
            match movement.direction {
                Direction::Forward => {
                    hpos += movement.magnitude;
                    depth += aim * movement.magnitude;
                }
                Direction::Down => aim += movement.magnitude,
                Direction::Up => aim -= movement.magnitude,
            };
        }

        hpos * depth
    };

    println!("{}", part1());
    println!("{}", part2());
}
