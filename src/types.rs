use std::{collections::VecDeque, str::FromStr};
use strum_macros::Display;
use thiserror::Error;

use crate::{tokenize_str, TokenizationError};

#[derive(Clone, Debug)]
/// Represents an expression that has been / will be evaluated.
/// Expressions that WILL BE evaluated are represented as a series of Tokens,
/// Whereas an Expression that already has been evaluated is just a Number (an f64)
pub enum Expression {
    Number { value: f64 },
    MultiTokenExpression { tokens: VecDeque<Token> },
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Number { value } => write!(f, "{value}"),

            Expression::MultiTokenExpression { tokens } => {
                let stringified_sub_expr: String = tokens
                    .iter()
                    .map(|t| match t {
                        Token::Op(op) => match op {
                            Op::Add => "+".to_string(),
                            Op::Sub => "-".to_string(),
                            Op::Mul => "*".to_string(),
                            Op::Div => "/".to_string(),
                        },
                        Token::Expression(expr) => expr.to_string(),
                    })
                    .collect();
                write!(f, "{stringified_sub_expr}")
            }
        }
    }
}

impl Expression {
    pub fn new_number(value: f64) -> Self {
        Self::Number { value }
    }

    pub fn new_mutli_token_expression(tokens: VecDeque<Token>) -> Self {
        Self::MultiTokenExpression { tokens }
    }

    pub fn is_number(&self) -> bool {
        matches!(*self, Self::Number { value: _ })
    }

    pub fn is_multi_token_expression(&self) -> bool {
        !self.is_number()
    }

    /// Returns a mutable reference to the tokens of this expression (if there are any).
    /// This returns None, if this is a Number
    pub fn get_tokens(&self) -> Option<&VecDeque<Token>> {
        match self {
            Expression::Number { value: _ } => None,
            Expression::MultiTokenExpression { tokens } => Some(tokens),
        }
    }

    /// Attempts to evaluate an Expression, and all of its Sub-Expressions.
    pub fn evaluate(&self) -> Result<f64, EvaluationError> {
        match self {
            Expression::Number { value } => Ok(*value),
            Expression::MultiTokenExpression { tokens } => {
                let mut running_total: f64 = 0.0;

                let mut current_op: Option<Op> = None;

                for token in tokens.iter() {
                    // React depending on which token appears:
                    // An Operation like Add or Subtract set that mode as the current one.
                    // An Expression sigals that it's time to do math with the current_op
                    match token {
                        Token::Op(op) => {
                            current_op = Some(*op);
                        }
                        Token::Expression(exp) => {
                            let evaluated = exp.evaluate()?;
                            running_total = match current_op {
                                None => evaluated,
                                Some(Op::Add) => running_total + evaluated,
                                Some(Op::Sub) => running_total - evaluated,
                                Some(Op::Mul) => running_total * evaluated,
                                Some(Op::Div) => {
                                    if evaluated < f64::EPSILON {
                                        return Err(EvaluationError::DivisionByZero);
                                    }
                                    running_total / evaluated
                                }
                            };
                            current_op = None; // Resets after an operation is applied, like now.
                        }
                    }
                }

                Ok(running_total)
            }
        }
    }

    // Checks if an expression is parsable to prevent errors during evaluation.
    // An example of an un-parse-able expression would be one with no tokens, since an empty expression has no inherent value.
    // If this succeeds it returns `Ok(())`, otherwise a descriptive error of what went wrong.
    pub fn validate(&self) -> Result<(), ValidationError> {
        match self.get_tokens() {
            // A Number. All numbers are valid expressions.
            None => Ok(()),

            Some(tokens) => {
                // Expressions must be non-empty
                if tokens.is_empty() {
                    return Err(ValidationError::NoTokensPlusSkillIssue);
                }

                // Expressions' tokens must:
                // - Start with an Expression (index 0 must be an Expression Token)
                // - Every Expression Token (but the last) must be followed by an Operator
                let mut is_op = true;

                for t in tokens.iter() {
                    if t.is_op() == is_op {
                        return Err(ValidationError::ExpressionsAndOpsDontAlternate);
                    }
                    is_op = !is_op;
                }

                // If we make it to the end, then the Expression is (hopefully) fine to evaluate.
                Ok(())
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum EvaluationError {
    #[error("Division by zero is undefined")]
    DivisionByZero,
}

#[derive(Debug)]
pub enum ValidationError {
    /// Expressions should always contain tokens.
    ///
    /// Skill issue.
    NoTokensPlusSkillIssue,

    /// Either two expressions come one after another,
    /// or two operators come one after another,
    /// which makes no sense.
    ExpressionsAndOpsDontAlternate,
}

#[derive(Error, Debug)]
pub enum ExpressionFromStrError {
    #[error("{0}")]
    TokenizationError(TokenizationError),
}

impl FromStr for Expression {
    type Err = ExpressionFromStrError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // If the expression can be parsed as a Number, return a Number
        if let Ok(f) = s.parse::<f64>() {
            Ok(Expression::new_number(f))
        } else {
            let for_fuck_sake = s.to_string();
            let t = match tokenize_str(&for_fuck_sake, 0, s.len()) {
                Ok(ok) => ok,
                Err(e) => return Err(ExpressionFromStrError::TokenizationError(e)),
            };
            Ok(Expression::new_mutli_token_expression(t))
        }
    }
}

#[derive(Debug, Clone, Display)]
/// represents either an f64 or a maths operation
pub enum Token {
    Op(Op),
    Expression(Expression),
}

impl Token {
    pub fn is_op(&self) -> bool {
        matches!(self, Self::Op(_))
    }

    pub fn is_expression(&self) -> bool {
        !self.is_op()
    }
}

/// Maths operations like Addition, Subtraction, Multiplication, and Division
#[derive(Clone, Copy, Display, Debug)]
pub enum Op {
    #[strum(serialize = "+")]
    Add,
    #[strum(serialize = "-")]
    Sub,
    #[strum(serialize = "*")]
    Mul,
    #[strum(serialize = "/")]
    Div,
}

impl Op {
    pub fn try_from_char(c: char) -> Option<Self> {
        match c {
            '+' => Some(Self::Add),
            '-' => Some(Self::Sub),
            '*' => Some(Self::Mul),
            '/' => Some(Self::Div),
            _ => None,
        }
    }
}
