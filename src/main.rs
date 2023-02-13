use inquire::Text;
use std::collections::HashMap;
use std::fmt::Display;

fn main() {
    println!("Beetle's Calculator");

    // Get user input.
    // Also remove any whitespace characters
    let user_input = Text::new("Enter an expression:")
        .prompt()
        .unwrap()
        .trim()
        .to_string();

    // If the user's expression starts with '-', prepend the expression with a zero to prevent errors later.
    // There are other ways of preventing errors, but this was the simplest way for me.
    let user_input: String = if user_input.starts_with('-') {
        format!("0{user_input}")
    } else {
        user_input
    };

    let brackets: HashMap<usize, usize> = get_brackets(&user_input);

    let tokenized_user_expression = Expression(tokenize_expression(
        &brackets,
        0,
        user_input.len(),
        &user_input,
    ));

    let ans = evaluate_expression(tokenized_user_expression);
    println!("ans: {ans}");
}

fn get_brackets(user_input: &str) -> HashMap<usize, usize> {
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
struct Expression(Vec<ExpressionToken>);

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
enum ExpressionToken {
    Number(Num),
    Operator(Op),
}

impl From<ExpressionToken> for String {
    fn from(val: ExpressionToken) -> Self {
        match val {
            ExpressionToken::Number(Num(n)) => n.to_string(),
            ExpressionToken::Operator(op) => match op {
                Op::Add => "+".to_string(),
                Op::Sub => "-".to_string(),
                Op::Mul => "*".to_string(),
                Op::Div => "/".to_string(),
            },
        }
    }
}

#[derive(Clone, Copy)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

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
struct Num(f64);

impl TryFrom<&str> for Num {
    type Error = ();
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Ok(x) = value.parse::<f64>() {
            Ok(Self(x))
        } else {
            Err(())
        }
    }
}

// Tries evaluating an Expression like '1 + (2 + 3)' into an f64 (in this case '6').
// If this fails the function will panic with the reason why it failed.
fn evaluate_expression(expr: Expression) -> f64 {
    // println!("Evaluating Expression {expr} ");
    // let expr_copy = expr.clone();
    let mut tokens = expr.0;
    // tokens.reverse();
    let mut answer: f64 = if let ExpressionToken::Number(Num(x)) = tokens.pop().unwrap() {
        x
    } else {
        panic!("Expressions must start with numbers");
    };

    // println!("REVERSED TOKENS: {}", expr_copy);

    while !tokens.is_empty() {
        if let ExpressionToken::Operator(op) = tokens.pop().unwrap() {
            let number = match tokens.pop() {
                Some(ExpressionToken::Number(Num(n))) => n,
                _ => panic!(
                    "Expected a number when evaluating expression, but found an operator instead"
                ),
            };
            match op {
                Op::Add => answer += number,
                Op::Sub => answer -= number,
                Op::Mul => answer *= number,
                Op::Div => answer /= number,
            }
        } else {
            panic!("found a number when evaluating expression that was unexpected");
        }
    }

    // println!("{expr_copy} evaluated to {answer}");
    answer
}

// Tries tokenizing an expression
// If it fails, it will panic (printing the reason it failed)
fn tokenize_expression(
    brackets: &HashMap<usize, usize>,
    absolute_start: usize,
    absolute_end: usize,
    master_expression: &str,
) -> Vec<ExpressionToken> {
    // ====================================
    //  CREATE EXPR BY SLICING MASTER_EXPR
    // ====================================
    let expr: &str = &master_expression[absolute_start..absolute_end];
    let absolute_start = if expr.starts_with('(') {
        absolute_start + 1
    } else {
        absolute_start
    };

    let absolute_end = if expr.starts_with('(') {
        absolute_end - 1
    } else {
        absolute_end
    };
    let expr: &str = &master_expression[absolute_start..absolute_end];

    if expr.is_empty() {
        panic!("cannot tokenize an empty expression");
    }

    // ============================================================
    //                      LOOP VARIABLES
    //          (variables used in the while loop)
    // ============================================================
    let mut expression_tokens: Vec<ExpressionToken> = Vec::new(); // This is what we return at the end of the function

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
            // println!("Pushing {ch} to number buffer");
            number_buffer.push(ch);
        }
        // OPERATION (add, sub, mul, div, etc.)
        else if let Ok(op) = Op::try_from(ch) {
            // println!("Attempting to parse {number_buffer} as an f64");
            if !number_buffer.is_empty() {
                let number: f64 = number_buffer
                    .parse()
                    .unwrap_or_else(|_| panic!("{number_buffer} couldn't be parsed as an f64"));
                expression_tokens.push(ExpressionToken::Number(Num(number)));
            }
            expression_tokens.push(ExpressionToken::Operator(op));
            number_buffer.clear();
        }
        // Sub-Expressions are tokenized using tokenize_expression (this function)
        else if ch == '(' {
            let sub_expression_start: usize = index + absolute_start;
            let sub_expression_end = brackets[&sub_expression_start];

            let sub_expression_tokenized = tokenize_expression(
                brackets,
                sub_expression_start,
                sub_expression_end + 1,
                master_expression,
            );

            let sub_expression_evaluated: f64 =
                evaluate_expression(Expression(sub_expression_tokenized));

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
            panic!("{ch} is not a valid character to include in an expression");
        }

        index += 1;
    }

    // If there's a number still in the buffer, add it to the expression_tokens
    if !number_buffer.is_empty() {
        let x = if let Ok(x) = Num::try_from(number_buffer.as_str()) {
            ExpressionToken::Number(x)
        } else {
            panic!("{number_buffer} couldn't be parsed as an f64")
        };
        expression_tokens.push(x);
    }
    expression_tokens
}
