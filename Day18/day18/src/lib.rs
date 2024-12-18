pub fn check() -> bool {
    return true;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = check();
        assert_eq!(result, true);
    }
}
