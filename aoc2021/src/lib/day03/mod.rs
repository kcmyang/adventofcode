use crate::lib::util;
use counter::Counter;

fn get_reports() -> Vec<String> {
    let mut result = Vec::<String>::new();

    if let Ok(lines) = util::read_lines("src/lib/day03/input.txt") {
        for line in lines {
            if let Ok(value) = line {
                result.push(value);
            }
        }
    }

    result
}

pub fn sol() {
    let data = get_reports();

    // part 1
    let part1 = || {
        let mut gamma: u32 = 0;
        let mut epsilon: u32 = 0;

        let str_len = data[0].chars().count();
        let mut counters = vec![Counter::<char>::new(); str_len];

        for s in &data {
            let mut chars = s.chars();

            for i in 0..str_len {
                if let Some(c) = chars.next() {
                    counters[i][&c] += 1
                }
            }
        }

        for c in &counters {
            gamma <<= 1;
            epsilon <<= 1;

            match c[&'1'] >= c[&'0'] {
                true => gamma += 1,
                false => epsilon += 1,
            }
        }

        gamma * epsilon
    };

    // part 2
    let part2 = || {
        let str_len = data[0].chars().count();

        let oxygen_rating = {
            let mut oxygen_pool = data.clone();
            let mut str_pos = 0;

            while oxygen_pool.len() > 1 && str_pos < str_len {
                let mut ones = Vec::<String>::new();
                let mut i = 0;

                while i < oxygen_pool.len() {
                    if oxygen_pool[i].chars().nth(str_pos).unwrap() == '1' {
                        ones.push(oxygen_pool.remove(i));
                    } else {
                        i += 1;
                    }
                }

                if ones.len() >= oxygen_pool.len() {
                    oxygen_pool = ones;
                }

                str_pos += 1;
            }

            u32::from_str_radix(&oxygen_pool[0], 2).unwrap()
        };

        let co2_rating = {
            let mut co2_pool = data.clone();
            let mut str_pos = 0;

            while co2_pool.len() > 1 && str_pos < str_len {
                let mut zeros = Vec::<String>::new();
                let mut i = 0;

                while i < co2_pool.len() {
                    if co2_pool[i].chars().nth(str_pos).unwrap() == '0' {
                        zeros.push(co2_pool.remove(i));
                    } else {
                        i += 1;
                    }
                }

                if zeros.len() <= co2_pool.len() {
                    co2_pool = zeros;
                }

                str_pos += 1;
            }

            u32::from_str_radix(&co2_pool[0], 2).unwrap()
        };

        oxygen_rating * co2_rating
    };

    println!("{}", part1());
    println!("{}", part2());
}
