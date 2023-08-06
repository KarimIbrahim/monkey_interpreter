use crate::token::{Token, TokenType};

#[derive(Default)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input,
            ..Default::default()
        };
        l.read_char();
        l
    }

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap() as u8;
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch as char {
            '=' if self.peek_char() == '=' => {
                let ch = self.ch as char;
                self.read_char();
                Token::new(TokenType::EQ, &format!("{}{}", ch, self.ch as char))
            }
            '=' => Token::new_byte(TokenType::ASSIGN, self.ch),
            '+' => Token::new_byte(TokenType::PLUS, self.ch),
            '-' => Token::new_byte(TokenType::MINUS, self.ch),
            '!' if self.peek_char() == '=' => {
                let ch = self.ch as char;
                self.read_char();
                Token::new(TokenType::NOT_EQ, &format!("{}{}", ch, self.ch as char))
            }
            '!' => Token::new_byte(TokenType::BANG, self.ch),
            '/' => Token::new_byte(TokenType::SLASH, self.ch),
            '*' => Token::new_byte(TokenType::ASTERISK, self.ch),
            '<' => Token::new_byte(TokenType::LT, self.ch),
            '>' => Token::new_byte(TokenType::GT, self.ch),
            ';' => Token::new_byte(TokenType::SEMICOLON, self.ch),
            '(' => Token::new_byte(TokenType::LPAREN, self.ch),
            ')' => Token::new_byte(TokenType::RPAREN, self.ch),
            ',' => Token::new_byte(TokenType::COMMA, self.ch),
            '{' => Token::new_byte(TokenType::LBRACE, self.ch),
            '}' => Token::new_byte(TokenType::RBRACE, self.ch),
            '"' => Token::new_string(self.read_string()),
            '\0' => Token::new_eof(),
            _ if Self::is_letter(self.ch) => return Token::new_ident(&self.read_identifier()),
            _ if Self::is_digit(self.ch) => return Token::new(TokenType::INT, &self.read_number()),
            _ => Token::new_byte(TokenType::ILLEGAL, self.ch),
        };

        self.read_char();

        return token;
    }

    pub fn read_identifier(&mut self) -> String {
        let position = self.position;

        while Self::is_letter(self.ch) || Self::is_digit(self.ch) {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    pub fn is_letter(ch: u8) -> bool {
        let ch = ch as char;
        ch.is_ascii_alphabetic() || ch == '_'
    }

    pub fn skip_whitespace(&mut self) {
        while (self.ch as char).is_ascii_whitespace() {
            self.read_char();
        }
    }

    pub fn is_digit(ch: u8) -> bool {
        (ch as char).is_ascii_digit()
    }

    pub fn read_number(&mut self) -> String {
        let position = self.position;

        while Self::is_digit(self.ch) {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    pub fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }

    fn read_string(&mut self) -> &str {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch as char == '"' || self.ch == 0 {
                break;
            }
        }

        &self.input[position..self.position]
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            token if token.token_type == TokenType::EOF => None,
            token => Some(token),
        }
    }
}
