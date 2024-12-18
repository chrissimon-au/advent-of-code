use pathfinding::prelude::bfs;
use std::str::FromStr;

fn parse_input(input: &str, byte_limit: usize) -> Vec<(i32,i32)> {
    let mut corrupted : Vec<(i32,i32)> = Vec::new();
    for location in input.lines().take(byte_limit) {
        let mut coords = location.split(",");
        let x = i32::from_str(coords.next().unwrap()).unwrap();
        let y = i32::from_str(coords.next().unwrap()).unwrap();
        corrupted.push((x,y));
    }
    return corrupted;
}

pub fn find_shortest_path(width: i32, height: i32, input: &str, byte_limit: usize) -> Option<Vec<(i32,i32)>> {
    let corrupted = parse_input(input, byte_limit);

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
    let result = find_shortest_path(width, height, input, byte_limit);
    return result.expect("no path found").len()-1;
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
}
