#[macro_export]
macro_rules! min {
    ($a:expr) => {
        $a
    };
    ($a:expr, $b:expr $(,)?) => {
        core::cmp::min($a, $b)
    };
    ($a:expr, $b:expr, $( $rest:expr ),+ $(,)?) => {
        min!(core::cmp::min($a, $b), $( $rest ),+)
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_min() {
        assert_eq!(3, min!(3));
        assert_eq!(1, min!(1, 2));
        assert_eq!(1, min!(3, 1, 2));
        assert_eq!(1, min!(1, 1, 1));
    }
}
