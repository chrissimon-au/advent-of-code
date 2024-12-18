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

pub fn shortest_path(width: i32, height: i32, input: &str) -> usize {
    let corrupted = parse_input(input);

    let result = bfs(&(0, 0),
        |&(x, y)| {
            let mut v : Vec<(i32,i32)> = vec![];

            let mut add_if_not_corrupted = |p| {if !corrupted.contains(p) { v.push(*p) } };
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
        |&p| p == (width-1,height-1));
    return result.expect("no path found").len()+1;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::str::FromStr;

    #[test]
    fn empty_gred() {
        let result = shortest_path(6, 6, "");
        assert_eq!(result, 12);
    }

    #[test]
    fn sample() {
        let result = shortest_path(6, 6, &fs::read_to_string("../sampledata.txt").unwrap());
        assert_eq!(result, usize::from_str(&fs::read_to_string("../sampledata.answer.txt").unwrap()).unwrap());
    }
}
