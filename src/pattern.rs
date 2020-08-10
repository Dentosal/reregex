use std::collections::HashSet;
use std::fmt;

use super::char_set::{self, CharSet};

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Concat(Vec<Pattern>),
    Alternation(Vec<Pattern>),
    Repetition {
        pattern: Box<Pattern>,
        kind: RepetitionKind,
    },
    CharSet(CharSet),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Concat(items) => {
                for item in items {
                    if item.fmt_is_atom() {
                        write!(f, "{}", item)?;
                    } else {
                        write!(f, "({})", item)?;
                    }
                }
                Ok(())
            }
            Self::Alternation(items) => {
                let v: Vec<_> = items
                    .iter()
                    .map(|item| {
                        if item.fmt_is_atom() {
                            format!("{}", item)
                        } else {
                            format!("({})", item)
                        }
                    })
                    .collect();
                write!(f, "{}", v.join("|"))
            }
            Self::Repetition { pattern, kind } => {
                if pattern.fmt_is_atom() {
                    write!(f, "{}", pattern)?;
                } else {
                    write!(f, "({})", pattern)?;
                }

                write!(f, "{}", kind)?;

                Ok(())
            }
            Self::CharSet(CharSet::Any) => write!(f, "."),
            Self::CharSet(CharSet::AnyOf(set)) => {
                if set.len() == 1 {
                    write!(f, "{}", set.iter().collect::<String>())
                } else {
                    write!(f, "[{}]", char_set::fmt(set))
                }
            }
            Self::CharSet(CharSet::AnyExcept(set)) => write!(f, "[^{}]", char_set::fmt(set)),
        }
    }
}

impl Pattern {
    pub fn simplify(self) -> Self {
        match self {
            Self::Concat(mut items) => {
                if items.len() == 1 {
                    items.pop().unwrap().simplify()
                } else {
                    Self::Concat(items.into_iter().map(Pattern::simplify).collect())
                }
            }
            Self::Alternation(mut items) => {
                if items.len() == 1 {
                    items.pop().unwrap().simplify()
                } else {
                    Self::Alternation(items.into_iter().map(Pattern::simplify).collect())
                }
            }
            Self::Repetition { pattern, kind } => Self::Repetition {
                pattern: Box::new((*pattern).simplify()),
                kind,
            },
            other => other,
        }
    }

    fn exact_char(c: char) -> Self {
        let mut hs = HashSet::new();
        hs.insert(c);
        Self::CharSet(CharSet::AnyOf(hs))
    }

    /// Is an atom, i.e. doesn't need parens around
    fn fmt_is_atom(&self) -> bool {
        match self {
            Self::Concat(items) => items.len() < 2,
            Self::Alternation(items) => items.len() < 2,
            Self::Repetition { .. } => true,
            Self::CharSet(_) => true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RepetitionKind {
    /// Greedy repetitions are sorted after non-greedy ones
    pub greedy: bool,
    /// Min number of rounds
    pub min: usize,
    /// At least min if Some
    pub max: Option<usize>,
}
impl fmt::Display for RepetitionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.max {
            None => match self.min {
                0 => write!(f, "*")?,
                1 => write!(f, "+")?,
                _ => write!(f, "{{{},}}", self.min)?,
            },
            Some(1) if self.min == 0 => write!(f, "?")?,
            Some(x) => write!(f, "{{{}, {}}}", self.min, x)?,
        }

        if !self.greedy {
            write!(f, "?")?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseMode {
    Normal,
    Escaped,
    CharSet { buffer: String, positive: bool },
    SubGroup { buffer: String, nesting: usize },
}

/// Parse a regex into a Pattern
pub fn parse(s: &str) -> Result<Pattern, String> {
    let mut alternatives = vec![];
    let mut current_alternative = vec![];

    let mut mode = ParseMode::Normal;

    for c in s.chars() {
        match mode {
            ParseMode::Normal => match c {
                '\\' => {
                    mode = ParseMode::Escaped;
                }
                '(' => {
                    mode = ParseMode::SubGroup {
                        buffer: String::new(),
                        nesting: 0,
                    };
                }
                ')' => {
                    return Err("Mismatched closing paren ')'".to_string());
                }
                '[' => {
                    mode = ParseMode::CharSet {
                        buffer: String::new(),
                        positive: true,
                    };
                }
                ']' => {
                    return Err("Mismatched closing bracket ']'".to_string());
                }
                '*' | '+' | '?' => {
                    if let Some(prev) = current_alternative.pop() {
                        current_alternative.push(if c == '?' {
                            match prev {
                                Pattern::Repetition { pattern, kind } => {
                                    if !kind.greedy {
                                        return Err(format!("Cannot specify non-greedyness twice"));
                                    }
                                    Pattern::Repetition {
                                        pattern,
                                        kind: RepetitionKind {
                                            greedy: false,
                                            ..kind
                                        },
                                    }
                                }
                                pattern => Pattern::Repetition {
                                    pattern: Box::new(pattern),
                                    kind: RepetitionKind {
                                        min: 0,
                                        max: Some(1),
                                        greedy: true,
                                    },
                                },
                            }
                        } else {
                            if matches!(prev, Pattern::Repetition {..}) {
                                return Err(format!("Cannot specify repetition twice"));
                            }

                            Pattern::Repetition {
                                pattern: Box::new(prev),
                                kind: RepetitionKind {
                                    min: if c == '*' { 0 } else { 1 },
                                    max: None,
                                    greedy: true,
                                },
                            }
                        });
                    } else {
                        return Err(format!("Cannot repeat an empty pattern"));
                    }
                }
                '{' => todo!("Support repetition ranges"),
                '}' => {
                    return Err("Mismatched closing curly '}'".to_string());
                }
                '|' => {
                    alternatives.push(Pattern::Concat(current_alternative.clone()));
                    current_alternative.clear();
                }
                '.' => {
                    current_alternative.push(Pattern::CharSet(CharSet::Any));
                }
                _ => {
                    current_alternative.push(Pattern::exact_char(c));
                }
            },
            ParseMode::Escaped => {
                mode = ParseMode::Normal;
                current_alternative.push(Pattern::exact_char(match c {
                    '\\' => c,
                    '|' => c,
                    '+' | '*' | '?' => c,
                    '(' | ')' | '[' | ']' | '{' | '}' => c,
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    _ => {
                        return Err(format!("Unknown escape \\{{{}}}", c));
                    }
                }));
            }
            ParseMode::CharSet {
                ref mut buffer,
                ref mut positive,
            } => match c {
                ']' => {
                    if buffer.is_empty() {
                        buffer.push(c);
                    } else {
                        let set = char_set::parse(buffer)?;
                        current_alternative.push(Pattern::CharSet(match positive {
                            true => CharSet::AnyOf(set),
                            false => CharSet::AnyExcept(set),
                        }));
                        mode = ParseMode::Normal;
                    }
                }
                '^' => {
                    if !(buffer.is_empty() && *positive) {
                        buffer.push(c);
                    } else {
                        *positive = false;
                    }
                }
                _ => {
                    buffer.push(c);
                }
            },
            ParseMode::SubGroup {
                ref mut buffer,
                ref mut nesting,
            } => match c {
                '(' => {
                    *nesting += 1;
                    buffer.push(c);
                }
                ')' => {
                    if *nesting == 0 {
                        current_alternative.push(parse(&buffer)?);
                        mode = ParseMode::Normal;
                    } else {
                        *nesting -= 1;
                        buffer.push(c);
                    }
                }
                _ => buffer.push(c),
            },
        }
    }

    if mode != ParseMode::Normal {
        return Err(format!("Unexpected end of input (in {:?})", mode));
    }

    alternatives.push(Pattern::Concat(current_alternative.clone()));
    Ok(Pattern::Alternation(alternatives).simplify())
}

#[cfg(test)]
mod test_parse {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(
            parse("cat"),
            Ok(Pattern::Concat(vec![
                Pattern::exact_char('c'),
                Pattern::exact_char('a'),
                Pattern::exact_char('t'),
            ]))
        );

        assert_eq!(
            parse("a|b"),
            Ok(Pattern::Alternation(vec![
                Pattern::exact_char('a'),
                Pattern::exact_char('b'),
            ]))
        );

        assert_eq!(
            parse("ab|cd"),
            Ok(Pattern::Alternation(vec![
                Pattern::Concat(vec![Pattern::exact_char('a'), Pattern::exact_char('b')]),
                Pattern::Concat(vec![Pattern::exact_char('c'), Pattern::exact_char('d')])
            ]))
        );

        assert_eq!(
            parse("a+b"),
            Ok(Pattern::Concat(vec![
                Pattern::Repetition {
                    pattern: Box::new(Pattern::exact_char('a')),
                    kind: RepetitionKind {
                        min: 1,
                        max: None,
                        greedy: true
                    }
                },
                Pattern::exact_char('b')
            ]))
        );

        assert_eq!(
            parse("a+?b"),
            Ok(Pattern::Concat(vec![
                Pattern::Repetition {
                    pattern: Box::new(Pattern::exact_char('a')),
                    kind: RepetitionKind {
                        min: 1,
                        max: None,
                        greedy: false
                    }
                },
                Pattern::exact_char('b')
            ]))
        );

        assert_eq!(
            parse("(a|b)+c"),
            Ok(Pattern::Concat(vec![
                Pattern::Repetition {
                    pattern: Box::new(Pattern::Alternation(vec![
                        Pattern::exact_char('a'),
                        Pattern::exact_char('b'),
                    ])),
                    kind: RepetitionKind {
                        min: 1,
                        max: None,
                        greedy: true
                    }
                },
                Pattern::exact_char('c')
            ]))
        );

        assert_eq!(
            parse("a|b+c"),
            Ok(Pattern::Alternation(vec![
                Pattern::exact_char('a'),
                Pattern::Concat(vec![
                    Pattern::Repetition {
                        pattern: Box::new(Pattern::exact_char('b')),
                        kind: RepetitionKind {
                            min: 1,
                            max: None,
                            greedy: true
                        }
                    },
                    Pattern::exact_char('c')
                ])
            ]))
        );

        let mut abcd = HashSet::new();
        abcd.insert('a');
        abcd.insert('b');
        abcd.insert('c');
        abcd.insert('d');

        assert_eq!(
            parse("q|[a-d]+e"),
            Ok(Pattern::Alternation(vec![
                Pattern::exact_char('q'),
                Pattern::Concat(vec![
                    Pattern::Repetition {
                        pattern: Box::new(Pattern::CharSet(CharSet::AnyOf(abcd))),
                        kind: RepetitionKind {
                            min: 1,
                            max: None,
                            greedy: true
                        }
                    },
                    Pattern::exact_char('e')
                ])
            ]))
        );
    }
}
