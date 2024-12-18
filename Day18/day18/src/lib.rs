pub fn shortest_path(_width: i32, _height: i32, _input: &str) -> i32 {
    return 0;
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
