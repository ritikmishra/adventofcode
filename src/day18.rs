use std::collections::HashMap;
use regex::Regex;

#[derive(Debug)]
enum Operation {
    Add,
    Mult,
}

#[derive(Debug)]
enum Expression {
    Num(i64),
    Operation(Box<Expression>, Operation, Box<Expression>),
    Variable(String)
}

/// Evaluate an expression tree
/// 
/// The order of operations is implicit in the structure of the tree. 
fn evaluate_expression(expr: &Expression, variable_defs: &HashMap<String, i64>) -> i64 {
    match expr {
        Expression::Num(num) => *num,
        Expression::Operation(left_expr, op, right_expr) => match op {
            Operation::Add => evaluate_expression(left_expr, variable_defs) + evaluate_expression(right_expr, variable_defs),
            Operation::Mult => evaluate_expression(left_expr, variable_defs) * evaluate_expression(right_expr, variable_defs),
        },
        Expression::Variable(name) => *variable_defs.get(name).unwrap(),
    }
}

/// For an expression that has parentheses as the first and last character,
/// return true if you could remove the first and last parentheses without
/// changing the meaning of the expression
/// 
/// Just because the first + last characters are an open + close paren respectively
/// does not mean that removing them would result in a valid expression
/// 
/// For example: `(1 + 2) * (3 + 4)` could become the invalid expression `1 + 2) * (3 + 4`
fn parentheses_balanced_expression(expr: &str) -> bool {
    let mut paren_depth = 0;
    for (i, letter) in expr.chars().enumerate() {
        if letter == '(' {
            paren_depth += 1;
        } else if letter == ')' {
            paren_depth -= 1;
        }
        if paren_depth == 0 && i < expr.len() - 1 {
            return false;
        }
    }
    paren_depth == 0
}

/// Finds the index that is the boundary between the last token (number or paren group) 
/// 
/// Examples
/// ```
/// let boundary = index_of_last_number_or_paren("2 + 3 + 5")
/// assert_eq!(("2 + 3 +", " 5"), "2 + 3 + 5".split(boundary.unwrap()))
/// 
/// let boundary = index_of_last_number_or_paren("2 + (3 + 5)")
/// assert_eq!(("2 +", " (3 + 5)"), "2 + (3 + 5)".split(boundary.unwrap()))
/// ```
fn index_of_last_number_or_paren(expr: &str) -> Option<usize> {
    let mut paren_depth = 0;
    for (from_back, letter) in expr.chars().rev().enumerate() {
        let i = expr.len() - from_back - 1;
        if letter == ')' {
            paren_depth += 1;
        } else if letter == '(' {
            paren_depth -= 1;
        } else if paren_depth == 0 && (letter == '+' || letter == '*') {
            return Some(i + 1);
        }
    }
    None
}

/// Finds the index that is the boundary between the last token (number or paren group), while 
/// acknowledging the fact that addition has higher precedence over multiplication
/// 
/// Leaf nodes in the Expression tree get executed first
/// 
/// Examples
/// ```
/// let boundary = index_of_last_number_or_paren("2 * 3 + 5")
/// assert_eq!(("2 *", "3 + 5"), "2 + 3 + 5".split(boundary.unwrap()))
/// 
/// let boundary = index_of_last_number_or_paren("2 + (3 * 5)")
/// assert_eq!(("2 +", " (3 * 5)"), "2 + (3 * 5)".split(boundary.unwrap()))
/// 
/// let boundary = index_of_last_number_or_paren("2 + 3 + 5")
/// assert_eq!(("2 + 3 +", " 5"), "2 + 3 + 5".split(boundary.unwrap()))
/// 
/// let boundary = index_of_last_number_or_paren("2 + (3 + 5)")
/// assert_eq!(("2 +", " (3 + 5)"), "2 + (3 + 5)".split(boundary.unwrap()))
/// ```
fn index_of_last_number_or_paren_operator_precedence(expr: &str) -> Option<usize> {
    let mut paren_depth = 0;
    let mut maybe_ret: Option<usize> = None;
    for (from_back, letter) in expr.chars().rev().enumerate() {
        let i = expr.len() - from_back - 1;
        if letter == ')' {
            paren_depth += 1;
        } else if letter == '(' {
            paren_depth -= 1;
        } else if paren_depth == 0 {
            if letter == '*' {
                return Some(i + 1);
            } else if letter == '+' && maybe_ret == None {
                maybe_ret = Some(i+1);
            }
        }
    }
    maybe_ret
}

fn index_of_last_number_or_paren_pemdas(expr: &str) -> Option<usize> {
    let mut paren_depth = 0;
    let mut maybe_ret: Option<usize> = None;
    for (from_back, letter) in expr.chars().rev().enumerate() {
        let i = expr.len() - from_back - 1;
        if letter == ')' {
            paren_depth += 1;
        } else if letter == '(' {
            paren_depth -= 1;
        } else if paren_depth == 0 {
            if letter == '+' {
                return Some(i + 1);
            } else if letter == '*' && maybe_ret == None {
                maybe_ret = Some(i+1);
            }
        }
    }
    maybe_ret
}

fn parse_expression(untrimmed_expr: &str, find_index_of_last_token: &dyn Fn(&str) -> Option<usize>) -> Expression {
    lazy_static! {
        static ref LEFT_HAND_FRAGMENT_REGEX: Regex = Regex::new(r"^(.+) ([\+\*])").unwrap();
        static ref OUTER_PARENS_REGEX: Regex = Regex::new(r"^\((.*)\)$").unwrap();
        static ref VALID_VAR_NAME: Regex = Regex::new(r"^\w[\w\d]*$").unwrap();
    }

    let expr = untrimmed_expr.trim();
    match expr.trim().parse::<i64>() {
        Ok(num) => Expression::Num(num),
        Err(_) => {
            let actual_expr: &str;
            if let Some(unwrap_parens) = OUTER_PARENS_REGEX.captures(expr) {
                if parentheses_balanced_expression(expr) {
                    actual_expr = unwrap_parens.get(1).unwrap().as_str();
                } else {
                    actual_expr = expr;
                }
            } else {
                actual_expr = expr;
            }
            if VALID_VAR_NAME.is_match(actual_expr) {
                return Expression::Variable(String::from(actual_expr));
            }


            let split_at = find_index_of_last_token(actual_expr);
            let (left, right_hand_expr_str) = actual_expr.split_at(split_at.unwrap());

            let expr_captures = LEFT_HAND_FRAGMENT_REGEX.captures(left).unwrap();
            let left_hand_expr_str = expr_captures.get(1).unwrap().as_str();
            let operator = expr_captures
                .get(2)
                .unwrap()
                .as_str()
                .chars()
                .next()
                .unwrap();
            let left_hand_expr = parse_expression(left_hand_expr_str, find_index_of_last_token);
            let right_hand_expr = parse_expression(right_hand_expr_str, find_index_of_last_token);

            Expression::Operation(
                Box::new(left_hand_expr),
                if operator == '+' {
                    Operation::Add
                } else if operator == '*' {
                    Operation::Mult
                } else {
                    panic!()
                },
                Box::new(right_hand_expr),
            )
        }
    }
}


pub fn day18_main() {
    let MATH_EQUATIONS = include_str!("../inputs/day18.txt");

    // part 1
    let mut sum = 0;
    for expr_line in MATH_EQUATIONS.split("\n") {
        sum += evaluate_expression(&parse_expression(expr_line, &index_of_last_number_or_paren), &HashMap::new());
    }
    println!("part 1 sum of equation results, no precedence: {}", sum);

    // part 2
    let mut sum = 0;
    for expr_line in MATH_EQUATIONS.split("\n") {
        sum += evaluate_expression(&parse_expression(expr_line, &index_of_last_number_or_paren_operator_precedence), &HashMap::new());
    }
    println!("part 2 sum of equation results, + precedes *: {}", sum);

    // fun idea
    let expr_with_variable = "(5 * x * x) + 6 * x * y + (y * y)"; // (5x + y)^2
    let expr_tree = parse_expression(expr_with_variable, &index_of_last_number_or_paren_pemdas);
    println!("here is the expression tree\n---\n{:?}\n---", expr_tree);
    let mut var_defs: HashMap<String, i64> = HashMap::new();
    var_defs.insert(String::from("x"), 1);
    var_defs.insert(String::from("y"), -5);
    println!("it evaluates to {}", evaluate_expression(&expr_tree, &var_defs));

}
