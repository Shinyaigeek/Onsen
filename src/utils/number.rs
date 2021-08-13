pub enum Number {
    float(f64),
    int(i64),
}

impl Number {
    pub fn from_float(f: f64) -> Self {
        Self::float(f)
    }

    pub fn from_int(i: i64) -> Self {
        Self::int(i)
    }

    pub fn from_str(s: String) -> Self {
        if s.contains(".") {
            let parsed_f = s.parse::<f64>();
            match parsed_f {
                Ok(f) => Self::float(f),
                Err(_) => panic!("Invalid float: {}", s),
            }
        } else {
            let parsed_i = s.parse::<i64>();
            match parsed_i {
                Ok(i) => Self::int(i),
                Err(_) => panic!("Invalid int: {}", s),
            }
        }
    }

    pub fn to_string(self: Self) -> String {
        match self {
            Self::float(f) => format!("{}", f),
            Self::int(i) => format!("{}", i),
        }
    }

    pub fn to_int(self: Self) -> i64 {
        match self {
            Self::float(f) => f as i64,
            Self::int(i) => i,
        }
    }

    pub fn to_float(self: Self) -> f64 {
        match self {
            Self::float(f) => f,
            Self::int(i) => i as f64,
        }
    }

    pub fn is_float(self: Self) -> bool {
        match self {
            Self::float(f) => true,
            Self::int(i) => false,
        }
    }
}

#[cfg(test)]
mod test {
    use super::Number;
    #[test]
    fn test_from_float() {
        let f = 1.23;
        let n = Number::from_float(f);
        assert_eq!(n.to_string(), f.to_string());
    }

    #[test]
    fn test_from_int() {
        let i = 123;
        let n = Number::from_int(i);
        assert_eq!(n.to_string(), i.to_string());
    }

    #[test]
    fn test_from_str_int() {
        let s = "123".to_string();
        let n = Number::from_str(s);
        assert_eq!(n.to_int(), 123);
    }

    #[test]
    fn test_from_str_float() {
        let s = "1.23".to_string();
        let n = Number::from_str(s);
        assert_eq!(n.to_float(), 1.23);
    }

    #[test]
    fn test_to_string() {
        let n = Number::float(1.23);
        assert_eq!(n.to_string(), "1.23");
    }

    #[test]
    fn test_to_int() {
        let n = Number::int(123);
        assert_eq!(n.to_int(), 123);
    }

    #[test]
    fn test_to_float() {
        let n = Number::float(1.23);
        assert_eq!(n.to_float(), 1.23);
    }

    #[test]
    fn test_is_float() {
        let n = Number::float(1.23);
        assert!(n.is_float());

        let n = Number::int(123);
        assert!(!n.is_float());
    }
}
