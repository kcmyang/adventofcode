use std::env;

mod lib;

fn main() {
    let args: Vec<String> = env::args().collect();

    let day = args[1].parse::<usize>().unwrap();

    if day < 1 || day > lib::SOLS.len() {
        panic!("invalid day number");
    }

    println!("Running day {}", day);
    println!();
    lib::SOLS[day - 1]();
}
