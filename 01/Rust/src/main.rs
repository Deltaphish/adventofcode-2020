#![feature(asm)]

use std::fs;
use std::time::{Duration, Instant};


fn read_input(file_name : &str) -> Vec<usize> {
    return fs::read_to_string(file_name)
        .expect("Failed to read input")
        .split('\n')
        .filter_map(|x| x.parse::<usize>().ok())
        .collect();
}

fn find_solution_naive(rows : &[usize]) -> Option<usize> {
    for x in rows {
        for y in rows {
            for z in rows{
                if x != y && x != z && y != z && z + x + y == 2020 {
                    return Some(x*y*z);
                }
            }
        }
    }
    None
}
/*
fn find_solution_binary(rows_const : &[usize]) -> Option<usize>{
    let mut rows = rows_const.to_owned();
    rows.sort_unstable();

    for i in 0..rows.len(){
        let val = rows[i];
        let mut high = rows.len();
        let mut low = i;

        let mut pivot:usize = (high - low) / 2;

        while high > (low+1) {
            let pivot_val = rows[pivot];

            if val + pivot_val > 2020 {
                high = pivot;
            }
            else if val + pivot_val < 2020 {
                low = pivot;
            }
            else {
                return Some(val * pivot_val);
            }
            pivot = low + (high - low) / 2;
        }
    }

    None
} 
*/

fn find_solution_linear(rows_const : &[usize]) -> Option<usize>{
    let mut rows = rows_const.to_owned();
    rows.sort_unstable();

    for i in 0..rows.len() {
        let fst = rows[i];
        for j in i..rows.len() {
            let snd = rows[j] + fst;
            for u in j..rows.len(){
                if snd + rows[u] == 2020 {
                    return Some(rows[i]*rows[j]*rows[u])
                }
            }
        }
    }

    None
}

fn find_solution_asm(rows_const : &[usize]) -> Option<usize>{
    let mut rows = rows_const.to_owned();
    rows.sort_unstable();

    unsafe {
        asm!("nop");
    }

    for i in 0..rows.len() {
        let fst = rows[i];
        for j in i..rows.len() {
            let snd = rows[j] + fst;
            for u in j..rows.len(){
                if snd + rows[u] == 2020 {
                    return Some(rows[i]*rows[j]*rows[u])
                }
            }
        }
    }

    None
}


fn benchmark(name : &str, solver : fn(&[usize]) -> Option<usize>, rows : &[usize]){

    let mut avg = 0;

    let mut solution = 0;

    for i in 0..2000 {
        let mut new_rows = rows.clone();

        let now = Instant::now();
        solution = solver(&mut new_rows).expect("No solution found");
        let duration = now.elapsed().as_nanos();
        if avg == 0{
            avg = duration;
        }else{
            avg = (avg + duration) / 2;
        }
    }


    println!("Solver {} found: {}, in {} ns",name,solution,avg);
}

fn main() {
    let mut rows = read_input("../input");

    //benchmark("naive", find_solution_naive, &rows);
    //benchmark("binary", find_solution_binary, &rows);
    benchmark("linear", find_solution_linear, &rows);
}
