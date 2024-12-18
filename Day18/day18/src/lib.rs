use pathfinding::prelude::bfs;
use std::str::FromStr;

fn parse_input(input: &str) -> Vec<(i32,i32)> {
    let mut corrupted : Vec<(i32,i32)> = Vec::new();
    for location in input.lines() {
        let mut coords = location.split(",");
        let x = i32::from_str(coords.next().unwrap()).unwrap();
        let y = i32::from_str(coords.next().unwrap()).unwrap();
        corrupted.push((x,y));
    }
    return corrupted;
}

pub fn find_shortest_path(width: i32, height: i32, corrupted: &Vec<(i32,i32)>) -> Option<Vec<(i32,i32)>> {
    let result = bfs(&(0, 0),
        |&(x, y)| {
            let mut v : Vec<(i32,i32)> = vec![];
            let mut add_if_not_corrupted = |&(xa,ya)| {
                if xa >= 0 && xa <= width && ya >= 0 && ya <= height && !corrupted.contains(&(xa,ya)) {
                    v.push((xa,ya))
                } 
            };
            let n = (x+1,y);
            add_if_not_corrupted(&n);
            let n = (x-1,y);
            add_if_not_corrupted(&n);
            let n = (x,y-1);
            add_if_not_corrupted(&n);
            let n = (x,y+1);
            add_if_not_corrupted(&n);

            return v;
        },
        |&p| p == (width,height));

    return result;
}

pub fn shortest_path(width: i32, height: i32, input: &str, byte_limit: usize) -> usize {
    let corrupted = parse_input(input).into_iter().take(byte_limit).collect();
    
    let result = find_shortest_path(width, height, &corrupted);
    return result.expect("no path found").len()-1;
}

pub fn find_when_impassable(width: i32, height: i32, input: &str, byte_limit: usize, max_byte_limit: usize) -> String {
    let all_corrupted = parse_input(input);
    let mut byte_limit_experiment = byte_limit;
    loop {
        let corrupted = all_corrupted.clone().into_iter().take(byte_limit_experiment).collect();
        let result = find_shortest_path(width, height, &corrupted);
        let len = match result {
            Some(path) => path.len(),
            None => 0
        };
        if byte_limit_experiment > max_byte_limit {
            panic!("Exceeded max byte limit without finding a blocker");
        }
        if len == 0 {
            let blocker = all_corrupted[byte_limit_experiment-1];
            dbg!(byte_limit_experiment, blocker);
            return format!("{},{}", blocker.0, blocker.1);
        }
        byte_limit_experiment += 1;
    } 
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::str::FromStr;

    #[test]
    fn empty_grid() {
        let result = shortest_path(6, 6, "", 0);
        assert_eq!(result, 12);
    }

    #[test]
    fn aoc_sample() {
        let result = shortest_path(6, 6, &fs::read_to_string("../sampledata.txt").unwrap(), 12);
        assert_eq!(result, usize::from_str(&fs::read_to_string("../sampledata.answer.txt").unwrap()).unwrap());
    }

    #[test]
    fn aoc_test() {
        let result = shortest_path(70, 70, &fs::read_to_string("../testdata.txt").unwrap(), 1024);
        assert_eq!(result, usize::from_str(&fs::read_to_string("../testdata.answer.txt").unwrap()).unwrap());
    }

    #[test]
    fn aoc_sample_part2() {
        let input = fs::read_to_string("../sampledata.txt").unwrap();
        let result = find_when_impassable(6, 6, &input, 12, 25);
        assert_eq!(result, fs::read_to_string("../sampledata.answer2.txt").unwrap());
    }

    #[test]
    fn aoc_test_part2() {
        let input = fs::read_to_string("../testdata.txt").unwrap();
        let result = find_when_impassable(70, 70, &input, 1024, 3450);
        assert_eq!(result, fs::read_to_string("../testdata.answer2.txt").unwrap());
    }
}
