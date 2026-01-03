use xxhash_rust::xxh32::xxh32;

const ALPHABET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

pub fn encode_hash(mut value: u32) -> String {
    let base = ALPHABET.len() as u32;
    if value == 0 {
        return (ALPHABET[0] as char).to_string();
    }

    let mut buffer = Vec::new();
    while value > 0 {
        buffer.push(ALPHABET[(value % base) as usize] as char);
        value /= base;
    }

    buffer.iter().rev().collect()
}

pub fn canonical_segments(body: &str) -> Vec<String> {
    body.split(';')
        .filter_map(|segment| {
            let trimmed = segment.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(normalize_segment(trimmed))
            }
        })
        .collect()
}

pub fn hash_css_body(body: &str) -> String {
    let canonical = canonical_segments(body).join(";");
    let hash = xxh32(canonical.as_bytes(), 0);
    encode_hash(hash)
}

fn normalize_segment(segment: &str) -> String {
    let mut result = String::new();
    let mut chars = segment.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            ':' => {
                while result.ends_with(' ') {
                    result.pop();
                }
                if !result.is_empty() {
                    result.push(' ');
                }
                result.push(':');
                result.push(' ');
                while matches!(chars.peek(), Some(next) if next.is_whitespace()) {
                    chars.next();
                }
            }
            c if c.is_whitespace() => {
                if !result.ends_with(' ') {
                    result.push(' ');
                }
                while matches!(chars.peek(), Some(next) if next.is_whitespace()) {
                    chars.next();
                }
            }
            _ => result.push(ch),
        }
    }

    result.trim().to_string()
}
