use std::io;

mod lib;

fn main() {
    let solutions = vec![lib::day01::sol];

    println!("Enter day number:");

    let mut buf = String::new();

    while buf.is_empty() {
        io::stdin().read_line(&mut buf).unwrap();
    }

    let day_num = match buf.trim().parse::<usize>() {
        Ok(num) => {
            if num < 1 {
                panic!("Day number is too small")
            }

            if num > solutions.len() {
                panic!("Day number is too large")
            }

            num
        }
        Err(..) => panic!("Could not convert to number"),
    };

    println!();

    solutions[day_num - 1]();
}
