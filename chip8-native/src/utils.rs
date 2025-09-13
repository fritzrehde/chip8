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

pub(crate) fn min_opt<T: Ord>(opt_a: Option<T>, opt_b: Option<T>, c: T) -> T {
    match (opt_a, opt_b) {
        (None, None) => c,
        (None, Some(b)) => min!(b, c),
        (Some(a), None) => min!(a, c),
        (Some(a), Some(b)) => min!(a, b, c),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_min() {
        assert_eq!(3, min!(3));
        assert_eq!(1, min!(1, 2));
        assert_eq!(1, min!(3, 1, 2));
        assert_eq!(1, min!(1, 1, 1));
    }

    #[test]
    fn test_min_opt() {
        assert_eq!(3, min_opt(Some(4), None, 3));
        assert_eq!(2, min_opt(None, None, 2));
        assert_eq!(1, min_opt(Some(1), Some(2), 3));
    }
}
