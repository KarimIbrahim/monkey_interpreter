use once_cell::sync::Lazy;
use std::collections::HashMap;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}
impl Token {
    pub fn new_byte(token_type: TokenType, byte: u8) -> Self {
        Self::new(token_type, &(byte as char).to_string())
    }

    pub fn new_eof() -> Self {
        Self::new(TokenType::EOF, "")
    }

    pub fn new_ident(ident: &str) -> Self {
        Self::new(TokenType::lookup_ident(ident), ident)
    }

    pub fn new_string(val: &str) -> Self {
        Self::new(TokenType::STRING, val)
    }

    pub fn new(token_type: TokenType, literal: &str) -> Self {
        Token {
            token_type,
            literal: literal.to_owned(),
        }
    }
}

static KEYWORDS: Lazy<HashMap<&str, TokenType>> = Lazy::new(|| {
    let mut map = HashMap::new();

    map.insert("fn", TokenType::FUNCTION);
    map.insert("let", TokenType::LET);
    map.insert("if", TokenType::IF);
    map.insert("else", TokenType::ELSE);
    map.insert("true", TokenType::TRUE);
    map.insert("false", TokenType::FALSE);
    map.insert("return", TokenType::RETURN);

    map
});

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, Hash)]
pub enum TokenType {
    ILLEGAL,
    #[default]
    EOF,

    // Identifiers and literals
    IDENT,
    INT,
    STRING,

    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    EQ,
    #[allow(non_camel_case_types)]
    NOT_EQ,

    LT,
    GT,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
}

impl TokenType {
    pub fn value(&self) -> String {
        match self {
            Self::ILLEGAL => "ILLEGAL".to_string(),
            Self::EOF => "EOF".to_string(),
            Self::IDENT => "IDENT".to_string(),
            Self::INT => "INT".to_string(),
            Self::ASSIGN => "=".to_string(),
            Self::PLUS => "+".to_string(),
            Self::MINUS => "-".to_string(),
            Self::ASTERISK => "*".to_string(),
            Self::SLASH => "/".to_string(),
            Self::BANG => "!".to_string(),
            Self::LT => "<".to_string(),
            Self::GT => ">".to_string(),
            Self::EQ => "==".to_string(),
            Self::NOT_EQ => "!=".to_string(),
            Self::COMMA => ",".to_string(),
            Self::SEMICOLON => ";".to_string(),
            Self::LPAREN => "(".to_string(),
            Self::RPAREN => ")".to_string(),
            Self::LBRACE => "{".to_string(),
            Self::RBRACE => "}".to_string(),
            Self::FUNCTION => "FUNCTION".to_string(),
            Self::LET => "LET".to_string(),
            Self::IF => "IF".to_string(),
            Self::ELSE => "ELSE".to_string(),
            Self::TRUE => "TRUE".to_string(),
            Self::FALSE => "FALSE".to_string(),
            Self::RETURN => "RETURN".to_string(),
            Self::STRING => "STRING".to_string(),
            Self::LBRACKET => "[".to_string(),
            Self::RBRACKET => "]".to_string(),
        }
    }

    pub fn lookup_ident(ident: &str) -> Self {
        match KEYWORDS.get(ident) {
            Some(token_type) => token_type.clone(),
            None => Self::IDENT,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::TokenType;

    #[test]
    fn token_value_test() {
        assert_eq!(TokenType::IDENT.value(), "IDENT");
    }
}
