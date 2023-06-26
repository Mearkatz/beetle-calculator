use inquire::{error::InquireResult, Text};
use std::{
    collections::{HashMap, VecDeque},
    rc::Rc,
    str::FromStr,
};
use thiserror::Error;
use types::Token;

use crate::types::{Expression, Op};

mod types;

/// Unwraps a Result within a loop without panicking if the Result is an Error.
/// Err's are simply printed, followed by a `continue`.
/// Ok's  are just returned.
/// I've tested this very briefly and it works fine.
macro_rules! graceful_unwrap_in_loop {
    ($e: expr) => {{
        match $e {
            Ok(ok) => ok,
            Err(e) => {
                eprintln!("{e}");
                continue;
            }
        }
    }};
}

/// Prints out help messages that teach the user how to use the program
pub fn help() {
    println!("-- Beetle Calculator Help --
What is an Expression (in the context of this program) ?:
    - EXPRESSIONS ARE An alternating sequence of Numbers (like '-1' or '1.5') and Operations (like '+' or '-')
        - By 'alternating sequence' I mean Numbers must be followed by Operations, and vice versa.
        - Example: '5 + 5' is legal, whereas '5 5 + 5' or '+ + 3' are not.
    - Expressions must end in a Number.        
    - If your expression starts with an operation it must either be '+' or '-', since starting an expression with '*5' or '/2' wouldn't make much sense.
        - Examples: '-15 * 3', '+5 + 5'

    - Expression nesting is achieved with parenthesis
        - Example: '1 + (2 + 3)'. Here '1 +' is the main Expression, and '(2 + 3)' is another Expression.
            - This simplifies to 1 + 5, and then to 6.");
}

fn main() {
    println!(
        "Beetle\'s Calculator\ntype an expression like \'1 + (2 + 3)\' below to evaluate it\nor type QUIT when you want the program to terminate"
    );

    let get_user_input = || -> InquireResult<String> {
        let mut user_input: String = Text::new("").prompt()?;
        user_input.retain(|c| !c.is_whitespace());
        Ok(user_input)
    };

    while let Ok(user_input) = get_user_input() {
        if "quit" == user_input.to_lowercase() {
            break;
        } else if "help" == user_input.to_lowercase() {
            help();
            continue;
        }

        // let brackets = match calculate_brackets(&user_input) {
        //     Ok(x) => x,
        //     Err(e) => {
        //         println!("{e}");
        //         continue;
        //     }
        // };

        // TOKENIZE USER INPUT
        // If there's an error, just output it to the user and prompt them again.
        let expr = graceful_unwrap_in_loop!(Expression::from_str(&user_input));

        println!("Expression Tokens: {expr}");

        // Evaluate expression or print an error message.
        if expr.validate().is_err() {
            eprintln!("That isn't a valid expression");
            continue;
        }
        let evaluated = graceful_unwrap_in_loop!(expr.evaluate());
        println!("{evaluated}");
    }
}

#[derive(Error, Debug)]
pub enum CalculateBracketsError {
    #[error("A right bracket in the expression is missing a matching left bracket")]
    RightBracketMissingLeftBracket { index: usize },
}

/// Returns matching brackets within a &str in the form of a HashMap,
/// keys are indices of left brackets, values are indices of the matching right bracket
fn calculate_brackets(user_input: &str) -> Result<HashMap<usize, usize>, CalculateBracketsError> {
    let mut brackets: HashMap<usize, usize> = HashMap::new();
    let mut unpaired_left_brackets: Vec<usize> = vec![];
    for (index, ins) in user_input.chars().enumerate() {
        if ins == '(' {
            unpaired_left_brackets.push(index);
        } else if ins == ')' {
            let Some(left_bracket_index) = unpaired_left_brackets.pop() else {
                return Err(CalculateBracketsError::RightBracketMissingLeftBracket { index });
            };
            let right_bracket_index = index;
            brackets.insert(left_bracket_index, right_bracket_index);
        }
    }
    Ok(brackets)
}

#[derive(Error, Debug)]
pub enum TokenizationError {
    #[error("Empty expressions like `()` cannot be evaluated")]
    EmptyExpression,

    #[error("{0} is an invalid character for use in an expression")]
    IllegalCharacter(char),

    #[error("{0} was expected to be a number, but couldn't be parsed as one")]
    CouldntParseNumber(String),

    #[error("There was an error finding the bracket pairs in the expression, due to the following error:\n `{0:?}`")]
    CalculateBracketsError(CalculateBracketsError),
}

/// Tries turning an &str into a VecDeque of Tokens.
fn tokenize_str(
    original_user_input: &String,
    mut absolute_start: usize,
    mut absolute_end: usize,
) -> Result<VecDeque<Token>, TokenizationError> {
    // let brackets = match calculate_brackets(original_user_input) {
    //     Ok(ok) => ok,
    //     Err(e) => {
    //         return Err(TokenizationError::CalculateBracketsError(e));
    //     }
    // };
    
    // Equivalent to the above commented code
    let brackets = calculate_brackets(original_user_input)
        .map_err(|e| TokenizationError::CalculateBracketsError(e))?;

    if original_user_input.chars().nth(absolute_start).unwrap() == '(' {
        absolute_start += 1;
    }

    if original_user_input.chars().nth(absolute_end - 1).unwrap() == ')' {
        // absolute_end -= 1;
    }

    // Portion of the user's expression we're tokenizing
    let substr = &original_user_input[absolute_start..absolute_end];
    println!("Tokenizing String (expr or subexpr) => {substr}");

    if substr.is_empty() {
        return Err(TokenizationError::EmptyExpression);
    }

    let mut number_buffer = String::new();
    let mut tokens = VecDeque::new();

    for (index, c) in substr.chars().enumerate() {
        // let s = c.to_string();
        println!("Looking at character {} @ index {} in substr {}", c, index, substr);

        // Character is an operator (+, -, *, /, etc)
        if let Some(op) = Op::try_from_char(c) {
            let Ok(f) = number_buffer.parse::<f64>() else {
                return Err(TokenizationError::CouldntParseNumber(number_buffer));
            };
            number_buffer.clear();
            // Note: VecDeque::push_back behaves the same as Vec::push
            tokens.push_back(Token::Expression(Expression::new_number(f)));
            tokens.push_back(Token::Op(op));

        // Character is part of a number
        } else if "1234567890.".contains(c) {
            number_buffer.push(c);
        }    
        // Character is a left bracket
        else if c == '(' {
            absolute_start = index;
            absolute_end = brackets[&absolute_start];
            let sub_expr_tokens: VecDeque<Token> =
                tokenize_str(original_user_input, absolute_start, absolute_end)?;

            let e = Expression::new_mutli_token_expression(sub_expr_tokens);
            tokens.push_back(Token::Expression(e));
        }
    }

    // If the number buffer is non-empty, convert it into a float and push it into tokens
    if !number_buffer.is_empty() {
        println!("The number buffer isn't empty, it still contains {}", number_buffer);
        let Ok(f) = number_buffer.parse::<f64>() else {
            return Err(TokenizationError::CouldntParseNumber(number_buffer));
        };
        tokens.push_back(Token::Expression(Expression::new_number(f)));
    }

    Ok(tokens)
}
