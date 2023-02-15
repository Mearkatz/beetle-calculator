use inquire::Text;
use std::collections::HashMap;
use std::fmt::Display;
use std::num::ParseFloatError;
use std::str::FromStr;
fn main() {
    println!(
        "Beetle\'s Calculator\ntype an expression like \'1 + (2 + 3)\' below to evaluate it\nor type QUIT when you want the program to terminate"
    );
    interactive_prompt();
}

/// Returns matching brackets within a &str in the form of a HashMap,
/// keys are indices of left brackets, values are indices of the matching right bracket
pub fn get_brackets(user_input: &str) -> HashMap<usize, usize> {
    let mut brackets: HashMap<usize, usize> = HashMap::new();
    let mut unpaired_left_brackets: Vec<usize> = vec![];
    for (index, ins) in user_input.chars().enumerate() {
        if ins == '(' {
            unpaired_left_brackets.push(index);
        } else if ins == ')' {
            let left_bracket_index = unpaired_left_brackets.pop().unwrap_or_else(|| {
                panic!("Right bracket at index {index} missing a corresponding Left Bracket")
            });
            let right_bracket_index = index;
            brackets.insert(left_bracket_index, right_bracket_index);
        }
    }
    brackets
}

#[derive(Clone)]
/// A maths expression in the form of a Vector of ExpressionTokens
pub struct Expression(Vec<ExpressionToken>);

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut stringified = String::new();
        for num_or_op in self.0.iter().cloned() {
            let s: String = num_or_op.into();
            stringified.push_str(&s);
        }

        write!(f, "{stringified}")
    }
}

#[derive(Clone)]
/// represents either an f64 or a maths operation
pub enum ExpressionToken {
    Number(Num),
    Operator(Op),
}

// ExpressionToken -> String
impl From<ExpressionToken> for String {
    fn from(val: ExpressionToken) -> Self {
        match val {
            ExpressionToken::Operator(Op::Add) => "+".to_string(),
            ExpressionToken::Operator(Op::Sub) => "-".to_string(),
            ExpressionToken::Operator(Op::Mul) => "*".to_string(),
            ExpressionToken::Operator(Op::Div) => "/".to_string(),
            ExpressionToken::Number(Num(n)) => n.to_string(),
        }
    }
}

#[derive(Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

// try Char -> Op
impl TryFrom<char> for Op {
    type Error = ();
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '+' => Ok(Self::Add),
            '-' => Ok(Self::Sub),
            '*' => Ok(Self::Mul),
            '/' => Ok(Self::Div),
            _ => Err(()),
        }
    }
}

#[derive(Copy, Clone)]
pub struct Num(f64);

impl FromStr for Num {
    type Err = ParseFloatError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.parse::<f64>() {
            Err(e) => Err(e),
            Ok(x) => Ok(Self(x)),
        }
    }
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

pub fn get_user_input() -> String {
    // Get user input.
    // Also remove any whitespace characters
    let user_input: String = Text::new("")
        .prompt()
        .unwrap_or(" ".to_string())
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();

    // If the user's expression starts with '-', prepend the expression with a zero to prevent errors later.
    // There are other ways of preventing errors, but this was the simplest way for me.
    if user_input.starts_with('-') {
        format!("0{user_input}")
    } else {
        user_input
    }
}

/// Prompts the user repeatedly to enter an expression,
/// or type QUIT or HELP to quit the program or print a help message
pub fn interactive_prompt() {
    loop {
        // GET USER INPUT
        let user_input = get_user_input();

        // QUIT IF USER TYPES 'QUIT'
        if "quit" == user_input.to_lowercase() {
            std::process::exit(0);
        } else if "help" == user_input.to_lowercase() {
            help();
            continue;
        }

        let brackets = get_brackets(&user_input);

        // TOKENIZE USER INPUT
        let tokenized_user_expression: Result<Expression, TokenizationError> =
            tokenize_expression(&brackets, 0, user_input.len(), &user_input);

        // Evaluate expression or print an error message.
        match tokenized_user_expression {
            Ok(expr) => {
                if let Ok(ans) = evaluate_expression(expr) {
                    println!("{ans}");
                } else {
                    println!();
                }
            },
            Err(TokenizationError::EmptyExpression) => {},

            Err(TokenizationError::FloatParsingError) => {
                println!("[ERROR]: A number in your expression could not be parsed as an f64.")
            },

            Err(TokenizationError::SubExprEvalError(sub_expr_err)) => {
                match sub_expr_err {
                        ExpressionEvaluationError::BeginsWithoutAddOrSub => println!("[ERROR]: A sub expression began with an Operation other than '+' or '-' "),
                        ExpressionEvaluationError::ExpectedOperator => println!("[ERROR]: A sub expression had a Number where an Operator was expected"),
                        ExpressionEvaluationError::ExpectedNumber => println!("[ERROR]: A sub expression had an Operator where a Number was expected"),
                        ExpressionEvaluationError::ExpectedToken => println!("[ERROR]: A sub expression ran out of tokens before it was expected to."),
                        ExpressionEvaluationError::EndsWithOperator => println!("[ERROR]: A sub expression ended with an Operator. Expressions must end with Numbers"),
                        ExpressionEvaluationError::BeginsWithOperator => println!("[ERROR]: A sub expression began with an operator (other than '+' or '-') "),
                    }
            },
            Err(TokenizationError::InvalidCharacter(c)) => println!("[ERROR]: {c} is not a valid character in an expression"),
        }
    }
}

/// Error that occurs when an expression passed to `evaluate_expression` is invalid
pub enum ExpressionEvaluationError {
    /// When an expression starts with a character other than '+' or '-'.
    /// My reasoning: '+5' or '-(1+3)' make some sense, whereas '*5' does not.
    BeginsWithoutAddOrSub,

    /// When a Number is the next ExpressionToken in an Expression, but an Operator was expected
    ExpectedOperator,

    /// When an Operator is the next ExpressionToken in an Expression, but a Number was expected
    ExpectedNumber,

    /// When a Number or Operator was expected to be the next ExpressionToken, but there are no more ExpressionTokens in the Expression
    ExpectedToken,

    /// When an expression ends with an Operator instead of a Number
    EndsWithOperator,

    /// When an expression begins with an Operator (other than + or -)
    BeginsWithOperator,
}

// Tries evaluating an Expression like '1 + (2 + 3)' into an f64 (in this case '6').
// If this fails the function will panic with the reason why it failed.
pub fn evaluate_expression(expr: Expression) -> Result<f64, ExpressionEvaluationError> {
    // If expression starts with '+' or '-', prepend it with a zero to prevent errors
    // Also handles if the Expression is empty
    let mut tokens: Vec<ExpressionToken> = match (expr.0).get(0) {
        // Add or Sub
        Some(ExpressionToken::Operator(Op::Add)) | Some(ExpressionToken::Operator(Op::Sub)) => {
            // let mut v: Vec<ExpressionToken> = vec![ExpressionToken::Number(Num(0.))];
            // v.extend((expr.0).into_iter());
            // v
            [ExpressionToken::Number(Num(0.))]
                .into_iter()
                .chain((expr.0).into_iter())
                .collect()
        }
        // Some other Operator
        Some(ExpressionToken::Operator(_)) => {
            return Err(ExpressionEvaluationError::BeginsWithoutAddOrSub);
        }
        // Empty expression, which is an error :)
        None => {
            return Err(ExpressionEvaluationError::ExpectedToken);
        }
        // A Number, which is fine
        Some(ExpressionToken::Number(_)) => expr.0,
    };

    // Because we evaluate expressions left to right, and popping gives us the rightmost element, we need to reverse the expression
    tokens.reverse();

    let mut answer: f64 = match tokens.pop() {
        Some(ExpressionToken::Number(Num(x))) => x,
        _ => return Err(ExpressionEvaluationError::ExpectedNumber),
    };

    // Continue evaluating tokens until there aren't any left
    while !tokens.is_empty() {
        // ======================================================================
        // Try getting an Operator followed by a Number from the remaining Tokens.
        // ======================================================================

        // Get Operator, if there's no more tokens or the token is a Number, return an error
        let Some(ExpressionToken::Operator(op)) = tokens.pop() else {
            return Err(ExpressionEvaluationError::ExpectedOperator);
        };

        // Get Number, if there's no more tokens, or the token is an Operator, return an error
        let Some(ExpressionToken::Number(Num(number))) = tokens.pop() else {
            return Err(ExpressionEvaluationError::ExpectedNumber);
        };

        // Perform operation
        match op {
            Op::Add => answer += number,
            Op::Sub => answer -= number,
            Op::Mul => answer *= number,
            Op::Div => answer /= number,
        }

        // Get next token (should be an Operator)
        // match tokens.pop() {
        //     Some(ExpressionToken::Operator(op)) => {
        //         let number = match tokens.pop() {
        //             Some(ExpressionToken::Number(Num(n))) => n,
        //             Some(ExpressionToken::Operator(op)) => return Err(ExpressionEvaluationError::),
        //             None => return Err(ExpressionEvaluationError::ExpectedSomething),
        //         };
        //         match op {
        //             Op::Add => answer += number,
        //             Op::Sub => answer -= number,
        //             Op::Mul => answer *= number,
        //             Op::Div => answer /= number,
        //         }
        //     }
        //     _ => return Err(ExpressionEvaluationError::ExpectedOperator),
        // }
    }
    Ok(answer)
}

/// Happens when an expression cannot be tokenized
pub enum TokenizationError {
    /// When an empty expression (one with no tokens) is passed to `tokenize expressison`
    EmptyExpression,

    /// when an Expression being tokenized or parsed begings with something other than the Add or
    /// ExpressionBeganWithoutAddOrSub,
    /// When a Number in the expression is parsed, but the parsing fails for some reason    
    FloatParsingError,

    /// When a sub-expression during tokenization could not be Evaluated.
    /// Contains the ExpressionEvaluationError that occurred
    SubExprEvalError(ExpressionEvaluationError),

    /// When some character that cannot be tokenized is seen.
    /// Also stores which character that was
    InvalidCharacter(char),
}

// Tries tokenizing an expression,
// If it succeeds, it returns the tokens as a Vector, which can then be evaluated with evaluate_expression
// If it fails, it will panic (printing the reason it failed)
pub fn tokenize_expression(
    brackets: &HashMap<usize, usize>, // pairs of brackets making up sub-expressions of `master_expression`
    mut absolute_start: usize, // start of the sub-expression in `master_expression` we're tokenizing
    mut absolute_end: usize,   // end of the sub-expression in `master_expression` we're tokenizing
    master_expression: &str,   // entire expression the user entered
) -> Result<Expression, TokenizationError> {
    // ====================================
    //  CREATE EXPR BY SLICING MASTER_EXPR
    // ====================================
    let expr: &str = &master_expression[absolute_start..absolute_end];
    if expr.starts_with('(') {
        absolute_start += 1;
    }

    if expr.starts_with('(') {
        absolute_end -= 1;
    }

    let expr: &str = &master_expression[absolute_start..absolute_end];

    if expr.is_empty() {
        return Err(TokenizationError::EmptyExpression);
    }

    // ============================================================
    //                      LOOP VARIABLES
    //          (variables used in the while loop)
    // ============================================================
    let mut expression_tokens: Vec<ExpressionToken> = Vec::new();

    // Accumulates digits of an f64 until a non-digit is encountered, at which point this empties itself and this is parsed as an f64 and pushed into expression_tokens
    let mut number_buffer = String::new();

    // index of character in expr being looked at currently
    let mut index: usize = 0;

    // character in expr currently being looked at
    let mut ch: char;

    // All the characters in the expression currently being parsed
    let chars: Vec<char> = expr.chars().collect();

    while index < expr.len() {
        ch = chars[index];

        // PART OF A NUMBER (f64)
        // (or '-' at the beginning of a negative number)
        if ".0123456789".contains(ch) || number_buffer.is_empty() && ch == '-' {
            number_buffer.push(ch);
        }
        // OPERATION (add, sub, mul, div, etc.)
        else if let Ok(op) = Op::try_from(ch) {
            // Parse number buffer as an f64 (if the buffer isn't empty)
            if !number_buffer.is_empty() {
                let Ok(number) = number_buffer.parse()
                else {
                    return Err(TokenizationError::FloatParsingError);
                };

                expression_tokens.push(ExpressionToken::Number(Num(number)));
            }
            expression_tokens.push(ExpressionToken::Operator(op));
            number_buffer.clear();
        }
        // Sub-Expressions are tokenized using tokenize_expression (this function)
        else if ch == '(' {
            let sub_expression_start: usize = index + absolute_start;
            let sub_expression_end = brackets[&sub_expression_start];

            // TOKENIZE SUB-EXPRESSION
            let sub_expression_tokenized: Expression = tokenize_expression(
                brackets,
                sub_expression_start,
                sub_expression_end + 1,
                master_expression,
            )?;

            // EVALUATE SUB-EXPRESSION
            let sub_expression_evaluated = match evaluate_expression(sub_expression_tokenized) {
                Ok(x) => x,
                Err(err) => return Err(TokenizationError::SubExprEvalError(err)),
            };

            expression_tokens.push(ExpressionToken::Number(Num(sub_expression_evaluated)));

            // Get next operator if there is one,
            // and push it into expression_tokens
            if let Some(c) = chars.get(index + 1) {
                if let Ok(op) = Op::try_from(*c) {
                    expression_tokens.push(ExpressionToken::Operator(op));
                }
            }

            // Set INDEX to the position AFTER the right bracket.
            // This is because we've just parsed the sub-expression, so we there's no reason to parse it here.
            index = sub_expression_end - 1;
        }
        // Right brackets are fine.
        // If a character appears that is not a legal character, print it out.
        else if ch != ')' {
            // panic!("{ch} is not a valid character to include in an expression");
            return Err(TokenizationError::InvalidCharacter(ch));
        }

        index += 1;
    }

    // If there's a number still in the buffer, add it to the expression_tokens
    if !number_buffer.is_empty() {
        let x = Num::from_str(number_buffer.as_str())
            .unwrap_or_else(|_| panic!("{number_buffer} couldn't be parsed as an f64"));
        expression_tokens.push(ExpressionToken::Number(x));
    }
    Ok(Expression(expression_tokens))
}
