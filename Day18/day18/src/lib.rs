pub fn shortest_path(width: i32, height: i32, _input: &str) -> i32 {
    return width + height;
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
        assert_eq!(result, i32::from_str(&fs::read_to_string("../sampledata.answer.txt").unwrap()).unwrap());
    }
}
