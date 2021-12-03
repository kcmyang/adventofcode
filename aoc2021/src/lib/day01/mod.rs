use crate::lib::util;

fn get_measurements() -> Vec<i32> {
    let mut result = Vec::<i32>::new();

    if let Ok(lines) = util::read_lines("src/lib/day01/input.txt") {
        for line in lines {
            if let Ok(value) = line {
                if let Ok(depth) = value.parse::<i32>() {
                    result.push(depth);
                }
            }
        }
    }

    result
}

pub fn sol() {
    let data = get_measurements();

    // part 1
    let part1 = || -> i32 {
        let mut result = 0;

        for i in 1..data.len() {
            if data[i] > data[i - 1] {
                result += 1;
            }
        }

        result
    };

    // part 2
    let part2 = || -> i32 {
        let mut result = 0;

        for i in 3..data.len() {
            if data[i] > data[i - 3] {
                result += 1;
            }
        }

        result
    };

    println!("{}", part1());
    println!("{}", part2());
}
