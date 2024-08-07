use regex::Regex;
use std::f64::consts::E;
use std::f64::consts::PI;
use std::io;
use std::io::prelude::*;

#[derive(Debug, PartialEq, Clone, Copy)]
enum Token {
    Op(Operation),
    Num(f64),
    Func(Func),
    Sep(char),
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
    Fac,
    Xor,
    And,
    Or,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Func {
    Sqrt,
    Log,
    Abs,
}

#[derive(Debug, PartialEq)]
enum Error {
    BadCharacter(char),
    BadBrackets,
    BadOperationOrder,
    NonExistentFunc(String),
    DivideByZero,
    NegativeFactorial,
    FractionalFactorial,
    TooManyDots,
    Indefinite,
    WhatDoYouMeanImaginary,
    BuggedAppCallDev(&'static str),
}

fn main() {
    let stdin = io::stdin();

    let mut answer = 0.;
    for line in stdin.lock().lines() {
        let result = complex_compute(&line.unwrap(), answer);
        match result {
            Ok(res) => {
                answer = res;
                println!(">> {res}")
            }
            Err(error) => println!("!! Error: {error:?}"),
        }
    }
}

// NOTE: Mother function, recursivelly dispatches in-bracket operations to compute()
fn complex_compute(input: &str, last_answer: f64) -> Result<f64, Error> {
    if !validate_brackets(input) {
        return Err(Error::BadBrackets);
    };

    // We now know that there are as many '(' as there are ')', and that they are positioned in a
    // mathematically valid manner.
    let first_cb = input.find(')');
    let last_ob = input.find('(');
    return {
        if let (Some(ind), Some(ind2)) = (last_ob, first_cb) {
            complex_compute(
                &input.replace(
                    &input[ind..=ind2],
                    &complex_compute(&input[(ind + 1)..ind2], last_answer)?.to_string(),
                ),
                last_answer,
            )
        } else {
            compute(input, last_answer)
        }
    };
}

fn compute(input: &str, last_answer: f64) -> Result<f64, Error> {
    let tokens = tokenise(input, last_answer)?;
    parse(tokens)
}

// Turns a string into a vector of tokens.
// NOTE: Turns the input lowercase and removes any spaces before iterating over the string.
// Puts numbers and letters into their separate buffer to assemble later once c isn't a
// number/letter.
fn tokenise(input: &str, last_answer: f64) -> Result<Vec<Token>, Error> {
    let mut tokenised: Vec<Token> = vec![];
    let mut num_buffer: Vec<char> = vec![];
    let mut word_buffer: Vec<char> = vec![];

    let re = Regex::new(r"\++").unwrap();
    let formatted = input
        .replace(" ", "")
        .to_lowercase()
        .replace("ans", &last_answer.to_string())
        .replace("e", &E.to_string())
        .replace("pi", &PI.to_string())
        .replace("!", "!0") // NOTE: Factorial is the only operation that only has one parameter, this
        //                  // avoids it having too many edge cases by making it use a mock value.
        //
        .replace("--", "+") // Removes any negative negative to make input more predictible.
        // NOTE: As there could then be many subsequent plusses, they are then all replaced by a
        // regex
        .replace("**", "$") // Important step, handling operators is way easier if they're 1 char.
        ;

    for c in re.replace_all(&formatted, "+").chars() {
        // Handling of the number buffer.  A hyphen that follows an operation/function will be
        // included in the number's sign.
        if c.is_numeric() || c == '.' || (c == '-' && num_buffer.is_empty()) {
            num_buffer.push(c);
        } else if !num_buffer.is_empty() {
            let num_as_str: String = num_buffer.iter().collect();

            if let Ok(val) = num_as_str.parse::<f64>() {
                tokenised.push(Token::Num(val));
                num_buffer = vec![];
                if c == '-' {
                    // NOTE: This condition triggers if a hyphen is between two numbers, in
                    // which case it is a substraction operation.
                    tokenised.push(Token::Op(Operation::Sub));
                }
            } else {
                return Err(Error::TooManyDots); // The only reason an array of digits and
                                                // dots could not parse to a string is if there is more than one dot.
            };
        };

        // Handling of the word buffer.
        if c.is_ascii_alphabetic() {
            word_buffer.push(c);
        } else if !word_buffer.is_empty() {
            let word: String = word_buffer.iter().collect();
            tokenised.push(match &word as &str {
                "sqrt" => Token::Func(Func::Sqrt),
                "log" => Token::Func(Func::Log),
                "abs" => Token::Func(Func::Abs),
                _ => return Err(Error::NonExistentFunc(word)),
            });
            word_buffer = vec![];
        };

        // We first check that there is no special behaviour to be had, as this is the only context
        if !(c == '-' || c == '.' || c.is_ascii_alphabetic() || c.is_numeric()) {
            tokenised.push(match c {
                '+' => Token::Op(Operation::Add),
                '*' => Token::Op(Operation::Mul),
                '/' => Token::Op(Operation::Div),
                '$' => Token::Op(Operation::Exp),
                '!' => Token::Op(Operation::Fac),
                '^' => Token::Op(Operation::Xor),
                '&' => Token::Op(Operation::And),
                '|' => Token::Op(Operation::Or),
                ',' | '[' | ']' | '(' | ')' => Token::Sep(c),
                _ => return Err(Error::BadCharacter(c)),
            });
        };
    }

    // Need a last check on the buffers, just in case they weren't empty at the last iteration.
    // NOTE: It is unlikely that a computation ending in an empty function won't return an error at
    // some point.
    if !num_buffer.is_empty() {
        let num_as_str: String = num_buffer.iter().collect();
        if num_as_str == "-" {
            return Err(Error::BadOperationOrder);
        };
        if let Ok(val) = num_as_str.parse::<f64>() {
            tokenised.push(Token::Num(val));
        } else {
            return Err(Error::TooManyDots);
        };
    }
    if !word_buffer.is_empty() {
        let word: String = word_buffer.iter().collect();
        tokenised.push(match &word as &str {
            "sqrt" => Token::Func(Func::Sqrt),
            "log" => Token::Func(Func::Log),
            _ => return Err(Error::NonExistentFunc(word)),
        });
    };

    Ok(tokenised)
}

fn parse(input: Vec<Token>) -> Result<f64, Error> {
    let mut to_return: f64 = 0.;
    if input.is_empty() {
        return Ok(0.);
    };
    if input.len() == 1 {
        if let Token::Num(a) = input[0] {
            to_return += a;
        } else {
            return Err(Error::BadOperationOrder);
        };
    };

    for (pos, token) in input.iter().enumerate() {
        // We get the special cases out of the way as to be able to ensure input[pos-1] and
        // input[pos+1] are Nums afterward.
        if let Token::Num(_) = token {
            continue;
        };
        if let Token::Sep(_) = token {
            continue;
        };

        // Check for functions
        if *token == Token::Func(Func::Sqrt) {
            if let Some(&Token::Num(a)) = input.get(pos + 1) {
                if a < 0. {
                    return Err(Error::WhatDoYouMeanImaginary);
                };
                to_return += a.sqrt();
                continue;
            } else {
                return Err(Error::BadOperationOrder);
            };
        };
        if *token == Token::Func(Func::Abs) {
            if let Some(&Token::Num(a)) = input.get(pos + 1) {
                to_return += a.abs();
                continue;
            } else {
                return Err(Error::BadOperationOrder);
            };
        };

        // Everything is now an operation
        if let (Some(&Token::Num(a)), Some(&Token::Num(b))) =
            (input.get(pos - 1), input.get(pos + 1))
        {
            match *token {
                Token::Op(Operation::Add) => to_return += a + b,
                Token::Op(Operation::Sub) => to_return += a - b,
                Token::Op(Operation::Mul) => to_return += a * b,
                Token::Op(Operation::Div) => {
                    if b != 0. {
                        to_return += a / b
                    } else {
                        return Err(Error::DivideByZero);
                    }
                }
                Token::Op(Operation::Exp) => {
                    if a != 0. || b != 0. {
                        to_return += a.powf(b)
                    } else {
                        return Err(Error::Indefinite);
                    }
                }
                Token::Op(Operation::Fac) => to_return += fac(a)?,
                Token::Op(Operation::And) => to_return += (a as i64 & b as i64) as f64,
                Token::Op(Operation::Or) => to_return += (a as i64 | b as i64) as f64,
                Token::Op(Operation::Xor) => to_return += (a as i64 ^ b as i64) as f64,
                _ => {
                    return Err(Error::BuggedAppCallDev(
                        "Unexpected token while parsing operations!",
                    ))
                }
            }
        } else {
            return Err(Error::BadOperationOrder);
        };
    }
    Ok(to_return)
}

fn validate_brackets(input: &str) -> bool {
    let mut brack_count = 0;
    for c in input.chars() {
        if brack_count < 0 {
            return false;
        }
        if c == '(' {
            brack_count += 1;
        };
        if c == ')' {
            brack_count -= 1;
        };
    }
    brack_count == 0
}

fn fac(n: f64) -> Result<f64, Error> {
    if n.fract() != 0. {
        return Err(Error::FractionalFactorial);
    };
    if n < 0. {
        return Err(Error::NegativeFactorial);
    };
    return match n {
        0. | 1. => Ok(1.),
        _ => Ok(fac(n - 1.)? * n),
    };
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_ok() {
        assert_eq!(complex_compute("", 0.), Ok(0.));
        assert_eq!(complex_compute("2", 0.), Ok(2.));
        assert_eq!(complex_compute("1+1", 0.), Ok(2.));
        assert_eq!(complex_compute("sqrt(9)", 0.), Ok(3.));
        assert_eq!(complex_compute("abs(-9)", 0.), Ok(9.));
        assert_eq!(complex_compute("ans", 2.), Ok(2.));
    }

    #[test]
    fn test_err() {
        assert_eq!(complex_compute("-", 0.), Err(Error::BadOperationOrder));
        assert_eq!(complex_compute("°", 0.), Err(Error::BadCharacter('°')));
        assert_eq!(
            complex_compute("a", 0.),
            Err(Error::NonExistentFunc("a".to_string()))
        );

        assert_eq!(
            complex_compute("sqrt-2", 0.),
            Err(Error::WhatDoYouMeanImaginary)
        );
        assert_eq!(complex_compute(")(", 0.), Err(Error::BadBrackets));
        assert_eq!(complex_compute("°", 0.), Err(Error::BadCharacter('°')));

        assert_eq!(complex_compute("0**0", 0.), Err(Error::Indefinite));
    }
}
