use std::ops::{Add, BitOr};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Regex<T> {
    Epsilon,
    Atom(T),
    Union(Vec<Regex<T>>),
    Concat(Vec<Regex<T>>),
    Star(Box<Regex<T>>),
}

impl<T> Regex<T> {
    pub fn star(self) -> Self {
        Regex::Star(Box::new(self))
    }
}
impl<T: Clone> Regex<T> {
    pub fn plus(self) -> Self {
        self.clone() + self.star()
    }
}

impl<T> Add<Regex<T>> for Regex<T> {
    type Output = Self;

    fn add(self, rhs: Regex<T>) -> Self::Output {
        use Regex::Concat;
        match (self, rhs) {
            (Concat(mut v1), Concat(v2)) => {
                v1.extend(v2);
                Concat(v1)
            }
            (Concat(mut v1), r) => {
                v1.push(r);
                Concat(v1)
            }
            (r, Concat(v2)) => {
                let mut v = vec![r];
                v.extend(v2);
                Concat(v)
            }
            (lhs, rhs) => Concat(vec![lhs, rhs]),
        }
    }
}

impl<T> BitOr<Regex<T>> for Regex<T> {
    type Output = Self;

    fn bitor(self, rhs: Regex<T>) -> Self::Output {
        use Regex::Union;
        match (self, rhs) {
            (Union(mut v1), Union(v2)) => {
                v1.extend(v2);
                Union(v1)
            }
            (Union(mut v1), r) => {
                v1.push(r);
                Union(v1)
            }
            (r, Union(mut v2)) => {
                v2.push(r);
                Union(v2)
            }
            (lhs, rhs) => Union(vec![lhs, rhs]),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Regex;
    #[test]
    fn creating_regex() {
        use Regex::*;
        let re: Regex<i32> = Atom(1) | (Atom(2) + Atom(3) + Atom(4)) | Atom(5).star();
        assert_eq!(
            re,
            Union(vec![
                Atom(1),
                Concat(vec![Atom(2), Atom(3), Atom(4)]),
                Star(Box::new(Atom(5)))
            ])
        )
    }
}
