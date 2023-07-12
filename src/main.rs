use std::str::FromStr;

use expression::ExpressionFromStrError;
use inquire::Text;

use crate::{expression::Expression, token::Token};

pub(crate) mod token {
    use std::{fmt::Display, str::FromStr};

    use crate::op::Op;

    #[derive(Debug, Copy, Clone, PartialEq)]
    pub(crate) enum Token {
        Op(Op),
        Num(f64),
        LeftParen,
        RightParen,
    }

    impl Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let s: String = match self {
                Token::Op(op) => op.to_string(),
                Token::LeftParen => '('.to_string(),
                Token::RightParen => ')'.to_string(),
                Token::Num(f) => f.to_string(),
            };
            write!(f, "{s}")
        }
    }

    #[allow(dead_code)]
    impl Token {
        pub(crate) fn is_paren(&self) -> bool {
            self.is_left_paren() || self.is_right_paren()
        }

        /// Returns `true` if the token is [`Num`].
        ///
        /// [`Num`]: Token::Num    
        pub(crate) fn is_num(&self) -> bool {
            matches!(self, Self::Num(..))
        }

        /// Returns `true` if the token is [`Op`].
        ///
        /// [`Op`]: Token::Op   
        pub(crate) fn is_op(&self) -> bool {
            matches!(self, Self::Op(..))
        }

        /// Returns `true` if the token is [`LeftParen`].
        ///
        /// [`LeftParen`]: Token::LeftParen
        pub(crate) fn is_left_paren(&self) -> bool {
            matches!(self, Self::LeftParen)
        }

        /// Returns `true` if the token is [`RightParen`].
        ///
        /// [`RightParen`]: Token::RightParen    
        pub(crate) fn is_right_paren(&self) -> bool {
            matches!(self, Self::RightParen)
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub(crate) enum TokenFromStrError {
        InvalidToken(String),
    }

    impl Display for TokenFromStrError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                TokenFromStrError::InvalidToken(t) => {
                    write!(f, "{t} couldn't be parsed as a Number or Operation.")
                }
            }
        }
    }

    impl FromStr for Token {
        type Err = TokenFromStrError;
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            if let Ok(op) = Op::from_str(s) {
                return Ok(Self::Op(op));
            }

            match s {
                "(" => Ok(Self::LeftParen),
                ")" => Ok(Self::RightParen),
                _ => match s.parse::<f64>() {
                    Ok(f) => Ok(Self::Num(f)),
                    _ => Err(TokenFromStrError::InvalidToken(s.to_string())),
                },
            }
        }
    }
}

pub(crate) mod op {
    use std::fmt::Display;
    use std::str::FromStr;

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub(crate) enum Op {
        Add,
        Sub,
        Mul,
        Div,
    }

    impl Display for Op {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let c: char = (*self).into();
            write!(f, "{c}")
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub(crate) enum OpFromStrError {
        IsNotAnOp(String),
    }

    impl Display for OpFromStrError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                OpFromStrError::IsNotAnOp(s) => {
                    write!(f, "{s} couldn't be parsed as an operation like + or -")
                }
            }
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub(crate) enum OpFromCharError {
        IsNotAnOp(char),
    }

    impl Display for OpFromCharError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                OpFromCharError::IsNotAnOp(c) => {
                    write!(f, "{c} couldn't be parsed as an operation like + or -")
                }
            }
        }
    }

    impl TryFrom<char> for Op {
        type Error = OpFromCharError;

        fn try_from(value: char) -> Result<Self, Self::Error> {
            match value {
                '+' => Ok(Self::Add),
                '-' => Ok(Self::Sub),
                '*' => Ok(Self::Mul),
                '/' => Ok(Self::Div),
                _ => Err(OpFromCharError::IsNotAnOp(value)),
            }
        }
    }

    impl Into<char> for Op {
        fn into(self) -> char {
            unsafe { self.try_into().unwrap_unchecked() }
        }
    }

    impl FromStr for Op {
        type Err = OpFromStrError;
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                "+" => Ok(Self::Add),
                "-" => Ok(Self::Sub),
                "*" => Ok(Self::Mul),
                "/" => Ok(Self::Div),
                _ => Err(OpFromStrError::IsNotAnOp(s.to_string())),
            }
        }
    }
}

pub(crate) mod expression {
    use std::collections::HashMap;
    use std::{fmt::Display, str::FromStr};

    use crate::{
        op::{Op, OpFromStrError},
        token::Token,
        NumberBuffer,
    };

    #[derive(Debug, Clone, PartialEq)]
    pub(crate) struct Expression {
        pub tokens: Vec<Token>,
        pub brackets: HashMap<usize, usize>,
    }

    impl Display for Expression {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let s: String = self.tokens.iter().map(|t| t.to_string()).collect();
            write!(f, "{s}")
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub(crate) enum CalculateBracketsError {
        /// Every left bracket must have a corresponding right bracket, and vice versa.
        RightBracketMissingLeftBracket(usize),

        /// The number of Left and Right brackets is unequal.
        NumberOfLeftAndRightBracketsAreUnequal,
    }

    impl Display for CalculateBracketsError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                CalculateBracketsError::RightBracketMissingLeftBracket(_)
                | CalculateBracketsError::NumberOfLeftAndRightBracketsAreUnequal => {
                    write!(f, "Mismatched or missing brackets in expression.")
                }
            }
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub(crate) enum NewExpressionError {
        /// Brackets must be valid to allow parsing later
        CalculateBracketsError(CalculateBracketsError),
        /// An expression must contain tokens to be created and parsed
        TokensIsEmpty,
    }

    impl Display for NewExpressionError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                NewExpressionError::CalculateBracketsError(err) => write!(f, "{err}"),
                NewExpressionError::TokensIsEmpty => write!(f, ""),
            }
        }
    }

    impl From<CalculateBracketsError> for NewExpressionError {
        fn from(value: CalculateBracketsError) -> Self {
            Self::CalculateBracketsError(value)
        }
    }

    impl Expression {
        // Creates a new expression.
        // If there is a leading left paren and trailing right paren as in '(1 + 5 + 2)',
        // this also removes those, since they just cause issues parsing later, and add no valuable information
        pub(crate) fn new(tokens: Vec<Token>) -> Result<Self, NewExpressionError> {
            if tokens.is_empty() {
                return Err(NewExpressionError::TokensIsEmpty);
            }
            let mut brackets = Expression::calculate_brackets(&tokens)?;
            brackets.shrink_to_fit();
            let mut expr = Self { tokens, brackets };
            expr.remove_surrounding_brackets_if_they_match()?;
            expr.prepend_zero_if_begins_with_minus();
            // expr.remove_all_leading_left_parens();
            expr.brackets = Expression::calculate_brackets(&expr.tokens)?;
            Ok(expr)
        }

        /// If the first character is a Left paren, and the last character is a Right paren,
        /// and they match, this removes both characters.
        /// If an error occurs, this returns that to be handled.
        pub(crate) fn remove_surrounding_brackets_if_they_match(
            &mut self,
        ) -> Result<(), NewExpressionError> {
            while !self.tokens.is_empty()
                && (self.brackets.get(&0) == Some(&(self.tokens.len() - 1)))
            {
                self.tokens.remove(0);
                self.tokens.pop();
                self.brackets = Expression::calculate_brackets(&self.tokens)?;
            }
            Ok(())
        }

        // If this is an expression like '-1 + 5', we prepend a zero,
        // making it 0 - 1 + 5,
        // This just makes it parse correctly later.
        pub(crate) fn prepend_zero_if_begins_with_minus(&mut self) {
            if let Some(Token::Op(Op::Sub)) = self.tokens.first() {
                self.tokens.insert(0, Token::Num(0.));
            }
        }

        pub(crate) fn tokens_to_readable_string(&self) -> String {
            self.tokens.iter().map(|t| format!("[{t}], ")).collect()
        }

        // Parses the expression as a number
        pub(crate) fn parse(&self) -> f64 {
            let mut accumulator: f64 = 0.0;
            let mut current_op: Option<Op> = None;
            let mut index: usize = 0;

            // println!("Parsing expr or subexpr {self}");

            // println!("brackets of {self} are {:?}", self.brackets);

            while index < self.tokens.len() {
                let token = &self.tokens[index];
                // println!("CURRENT TOKEN => {token} @ INDEX {index} in expression {self}");
                match token {
                    Token::LeftParen => {
                        let left_bracket_index = index;
                        let right_bracket_index = self.brackets[&index];
                        let slice = &self.tokens[left_bracket_index..=right_bracket_index];

                        let sub_expr = Expression::new(slice.to_vec()).unwrap();
                        // println!(
                        //     "New sub expression has tokens => {}",
                        //     sub_expr.tokens_to_readable_string()
                        // );
                        let num = sub_expr.parse();
                        // println!("{sub_expr} evaluated to {num}");

                        match current_op {
                            Some(Op::Add) => {
                                accumulator += num;
                            }
                            Some(Op::Sub) => {
                                accumulator -= num;
                            }
                            Some(Op::Mul) => {
                                accumulator *= num;
                            }
                            Some(Op::Div) => {
                                accumulator /= num;
                            }
                            // If we haven't seen an op yet,
                            // set the accumulator to this number
                            None => {
                                accumulator = num;
                            }
                        }

                        index = right_bracket_index;
                        continue;
                    }
                    Token::RightParen => {}
                    Token::Op(op) => {
                        current_op = Some(*op);
                    }
                    Token::Num(num) => match current_op {
                        Some(Op::Add) => {
                            accumulator += num;
                        }
                        Some(Op::Sub) => {
                            accumulator -= num;
                        }
                        Some(Op::Mul) => {
                            accumulator *= num;
                        }
                        Some(Op::Div) => {
                            accumulator /= num;
                        }
                        // If we haven't seen an op yet,
                        // set the accumulator to this number
                        None => {
                            accumulator = *num;
                        }
                    },
                }

                index += 1;
            }

            accumulator
        }

        pub(crate) fn calculate_brackets(
            tokens: &[Token],
        ) -> Result<HashMap<usize, usize>, CalculateBracketsError> {
            // If the number of left and right parens are unequal, return an error
            let number_of_left_parens = tokens.iter().filter(|x| x.is_left_paren()).count();
            let number_of_right_parens = tokens.iter().filter(|x| x.is_right_paren()).count();
            if number_of_left_parens != number_of_right_parens {
                return Err(CalculateBracketsError::NumberOfLeftAndRightBracketsAreUnequal);
            }

            let mut brackets: HashMap<usize, usize> = HashMap::new();
            let mut unpaired_left_brackets: Vec<usize> = vec![];
            for (index, token) in tokens.iter().enumerate() {
                if token.is_left_paren() {
                    unpaired_left_brackets.push(index);
                } else if token.is_right_paren() {
                    let Some(left_bracket_index) = unpaired_left_brackets.pop() else {
                        return Err(CalculateBracketsError::RightBracketMissingLeftBracket(index));
                    };
                    let right_bracket_index = index;
                    brackets.insert(left_bracket_index, right_bracket_index);
                }
            }
            Ok(brackets)
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub(crate) enum ExpressionFromStrError {
        NumberHadMultipleDots(String),
        NumberCouldNotBeParsed(String),
        OpFromStrError(OpFromStrError),
        NewExpressionError(NewExpressionError),
    }

    impl Display for ExpressionFromStrError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                ExpressionFromStrError::NumberHadMultipleDots(number) => {
                    write!(f, "{number} has multiple decimal points")
                }
                ExpressionFromStrError::NumberCouldNotBeParsed(number) => {
                    write!(f, "{number} could not be parsed as a number")
                }
                ExpressionFromStrError::OpFromStrError(err) => write!(f, "{err}"),
                ExpressionFromStrError::NewExpressionError(err) => write!(f, "{err}"),
            }
        }
    }

    impl From<NewExpressionError> for ExpressionFromStrError {
        fn from(v: NewExpressionError) -> Self {
            Self::NewExpressionError(v)
        }
    }

    impl FromStr for Expression {
        type Err = ExpressionFromStrError;

        fn from_str(s: &str) -> Result<Self, ExpressionFromStrError> {
            let mut tokens: Vec<Token> = Vec::new();
            let mut number_buffer = NumberBuffer::new();

            for c in s.chars().filter(|c| !c.is_whitespace()) {
                if c == '(' {
                    tokens.push(Token::LeftParen);
                } else if c == ')' {
                    if !number_buffer.is_empty() {
                        tokens.push(number_buffer.parse_then_clear()?);
                    }
                    tokens.push(Token::RightParen);
                } else if c == '.' {
                    // This is the second decimal point in the number,
                    // which is weird... so return an error.
                    if number_buffer.seen_decimal_point {
                        return Err(ExpressionFromStrError::NumberHadMultipleDots(
                            number_buffer.digits.clone(),
                        ));
                    }
                    // Otherwise, this is the first decimal point in the number
                    number_buffer.push(c);
                    number_buffer.seen_decimal_point = true;
                }
                // Digits in an f64
                else if "0123456789".contains(c) {
                    number_buffer.push(c);
                }
                // Op
                else {
                    match Op::from_str(&c.to_string()) {
                        Err(e) => {
                            return Err(ExpressionFromStrError::OpFromStrError(e));
                        }
                        Ok(op) => {
                            number_buffer.seen_decimal_point = false;
                            if !number_buffer.is_empty() {
                                tokens.push(number_buffer.parse_then_clear()?);
                            }
                            tokens.push(Token::Op(op));
                        }
                    }
                }
            }

            // If number buffer has a number in it still, push it to tokens
            if !number_buffer.is_empty() {
                tokens.push(number_buffer.parse_then_clear()?);
            }

            let mut expr = Self::new(tokens)?;
            expr.prepend_zero_if_begins_with_minus();
            Ok(expr)
        }
    }
}

// Accumulates characters to be converted into a number (an f64)
#[derive(Debug, Clone)]
pub(crate) struct NumberBuffer {
    digits: String,
    seen_decimal_point: bool,
}

impl NumberBuffer {
    pub(crate) fn new() -> Self {
        Self {
            digits: String::new(),
            seen_decimal_point: false,
        }
    }

    // Tries casting the String in the buffer as an f64.
    // If it fails, a descriptive error is returned
    pub(crate) fn as_f64(&self) -> Result<Token, ExpressionFromStrError> {
        self.digits
            .parse::<f64>()
            .map(Token::Num)
            .map_err(|_| ExpressionFromStrError::NumberCouldNotBeParsed(self.digits.clone()))
    }

    // calls `self.as_f64`, returning the result,
    // but also clears the buffer of digits
    pub(crate) fn parse_then_clear(&mut self) -> Result<Token, ExpressionFromStrError> {
        let num = self.as_f64()?;
        self.clear();
        Ok(num)
    }

    pub(crate) fn push(&mut self, c: char) {
        self.digits.push(c)
    }

    // Clears the buffer of digits
    pub(crate) fn clear(&mut self) {
        self.digits.clear()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.digits.is_empty()
    }
}

fn the_order_of_operations_is_not_implemented() {
    println!("Note: The order of operations (PEMDAS) is not yet implemented, so expressions are evaluated left to right. Please add parenthesis as needed.");
}

fn main() {
    println!("Hello! Welcome to Beetle's Calculator :D");
    the_order_of_operations_is_not_implemented();
    println!("Type HELP for help using the program.");
    println!("Type QUIT or EXIT to close the program.");

    loop {
        let user_input = Text::new("")
            .prompt()
            .expect("Failed to get user input")
            .to_lowercase();

        if user_input == "quit" || user_input == "exit" {
            std::process::exit(0);
        } else if user_input == "help" {
            println!("Try entering an expression like `1 + (2 + 3)`, it should evaluate to `6`.");
            the_order_of_operations_is_not_implemented();
        } else {
            match Expression::from_str(&user_input) {
                Ok(expr) => println!("{}", expr.parse()),
                Err(err) => println!("{err}"),
            }
        }
    }
}
#[test]
fn evaluation_works() {
    let user_input = "(0-1) + ( (2 + 3) * (5 + 4) ) / (2 * 3)";
    // user_input.retain(|c| !c.is_whitespace());
    let expr = Expression::from_str(&user_input).unwrap();
    let ans = expr.parse();
    assert!((ans - (7. + (1. / 3.))).abs() < 0.00001);
}
