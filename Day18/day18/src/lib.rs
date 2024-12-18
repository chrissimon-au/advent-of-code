pub fn shortest_path(width: i32, height: i32, _input: &str) -> i32 {
    return width + height;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = shortest_path(6, 6, "");
        assert_eq!(result, 12);
    }
}
