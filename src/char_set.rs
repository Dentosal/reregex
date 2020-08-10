use std::collections::HashSet;

/// Parse char set string, i.e. string that can contain dashes like "a-z"
///
/// Some special characters:
/// * "]" must be the first character to be included (in regex, "[]]" or "[^]]")
/// * "-" must be the first or last character (in regex "[-]", "[^-]" or "[ab-]")
/// * Combined: "[^]a-z-]"
///
pub fn parse(s: &str) -> Result<HashSet<char>, String> {
    let mut result = HashSet::new();
    let mut previous = None;
    let mut range_to_next = false;

    for c in s.chars() {
        if c == '-' {
            if range_to_next {
                return Err("Two adjacent dashes not allowed".to_owned());
            }
            range_to_next = true;
        } else if range_to_next {
            range_to_next = false;
            if let Some(p) = previous {
                for r in p..=c {
                    result.insert(r);
                }
            } else {
                result.insert('-');
            }
        } else {
            result.insert(c);
            previous = Some(c);
        }
    }

    if range_to_next {
        result.insert('-');
    }

    Ok(result)
}

/// Formats set of characters using dash notation "a-z" to minify
pub fn fmt(chars: &HashSet<char>) -> String {
    let mut chars = chars.clone();
    let has_dash = chars.remove(&'-');
    let has_closing_bracket = chars.remove(&']');
    let mut sorted: Vec<_> = chars.iter().collect();
    sorted.sort();

    let mut string: String = sorted.into_iter().collect();

    // TODO: dash substitutions

    if has_closing_bracket {
        string.insert(0, ']');
    }

    if has_dash {
        string.push('-');
    }

    string
}
