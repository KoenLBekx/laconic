// TODO: resolve TODO's in code.
// TODO: implement all intended operators.
// TODO: implement Debug for Expression, using get_representation.
// TODO: if struct Interpreter ends up having no internal state (no properties), simply delete it
// and make its associated methods crate-level functions.
// TODO: implement different variable arrays - see the 'u' operator.
//      Default array: 0.
//      This means that the key for HashMap Shuttle.nums has to be a (i64, i64) tuple.

use std::collections::HashMap;
use std::str::FromStr;

// Static lifetime: justified, because the functions referenced
// are compiled into the application and live as long as it runs.
// The first parameter should refer to an Expression's value property,
// the second one to a Expression's operands property (operands are Expression objects),
// the third one to the Shuttle containing other state.
type OperatorFunc = &'static dyn Fn(&mut Option<f64>, &mut [Expression], &mut Shuttle);

#[derive(Debug, PartialEq)]
enum Atom {
    Operator(char),
    Number(f64),
    String(String),
    Comment(String),
}

#[derive(Debug, PartialEq)]
pub enum ProgramError {
    DigitParsingFailure{position: usize, reason: String},
    UnexpectedClosingBracket{position: usize},
    UnknownOperator{position: usize, operator: char},
    UnclosedBracketsAtEnd,
    UnknownBracketContentTypeMarker{position: usize, marker: char},
}

struct Expression
{
    operator: OperatorFunc,
    operands: Vec<Expression>,
    value: Option<f64>,
    opr_mark: char,
    is_last_of_override: bool,
    is_assignment_op: bool,
}

impl Expression {
    pub fn new(opr_mark: char) -> Self {
        let opr: OperatorFunc = match opr_mark {
            '0' => &opr_funcs::nop,
            '~' => &opr_funcs::unaryminus,
            '+' => &opr_funcs::add,
            '-' => &opr_funcs::minus,
            '*' => &opr_funcs::multiply,
            '/' => &opr_funcs::divide,
            '%' => &opr_funcs::modulo,
            ';' => &opr_funcs::combine,
            'W' => &opr_funcs::exec_while,
            '$' => &opr_funcs::assign_number_register,
            'v' => &opr_funcs::get_number_register,
            ':' => &opr_funcs::assignment_maker,
            '=' => &opr_funcs::equals,
            '<' => &opr_funcs::less,
            '>' => &opr_funcs::greater,
            '!' => &opr_funcs::not,
            'Z' => &opr_funcs::setting,
            _ => &opr_funcs::nop,
        };

        Expression {
            operator: opr,
            operands: Vec::<Expression>::new(),
            value: None,
            opr_mark,
            is_last_of_override: false,
            is_assignment_op: false,
        }
    }

    pub fn new_number(num: f64) -> Self {
        Expression {
            operator: &opr_funcs::nop,
            operands: Vec::<Expression>::new(),
            value: Some(num),
            opr_mark: '0',
            is_last_of_override: false,
            is_assignment_op: false,
        }
    }

    pub fn push_operand(&mut self, op: Expression) {
        self.operands.push(op);
    }

    pub fn get_value(&self, default: f64) -> f64 {
        // If Some, returns a clone of the contained value,
        // so the Expression's value isn't consumed.
        // If None, returns the provided default value.

        if self.value.is_some() {
            // As f64 implements the Copy trait,
            // the Option's value isn't consumed here.
            // See unit test unwrap_some_copy_implementor.
            self.value.unwrap()
        } else {
            default
        }
    }

    pub fn operate(&mut self, shuttle: &mut Shuttle) {
        let is_loop = "WF".contains(self.opr_mark);

        if !is_loop {
            for op in &mut self.operands {
                op.operate(shuttle);
            }
        }

        (self.operator)(&mut self.value, &mut self.operands, shuttle);

        if self.is_assignment_op {
            // Assign the resulting value to the variable
            // referenced by the first operand and stored by it in the shuttle.
            shuttle.nums.insert(shuttle.assignment_index, self.get_value(0f64));
        }
    }

    pub fn get_representation(&self) -> String {
        let mut exp_rep = match self.opr_mark {
            '0' => "num".to_string(),
            oth => oth.to_string(),
        };

        if self.is_assignment_op {
            exp_rep.push(':');
        }

        let mut ops = String::new();
        for op in &self.operands {
            ops.push('\n');
            ops.push_str(op.get_representation().as_str());
        }

        // (The underscore in the unicode escape sequence
        // precludes Vim folding using foldmarker={,}
        // from folding incorrectly.)

        // Vim folding fix brace: {
        ops = ops.replace('\n', "\n\u{0_2502}\t");
        exp_rep.push_str(ops.as_str());
        // Vim folding fix braces: {{
        exp_rep.push_str("\n\u{0_2514}\u{0_2500}>");
        exp_rep.push_str(format!("{:?}", self.value).as_str());

        exp_rep
    }
}

// Shuttle objects are used to be passed to every operator function
// to provide state other than the operands.
struct Shuttle {
    nums: HashMap<i64, f64>,
    strings: HashMap<i64, String>,
    routines: HashMap<i64, Expression>,
    assignment_index: i64,
    max_iterations: f64,
    orb: f64,
}

impl Shuttle {
    fn new() -> Self {
        Shuttle {
            nums: HashMap::<i64, f64>::new(),
            strings: HashMap::<i64, String>::new(),
            routines: HashMap::<i64, Expression>::new(),
            assignment_index: 0i64,
            max_iterations: 0f64,
            orb: 0.00000001f64,
        }
    }
}

pub struct Interpreter {
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter{}
    }

    pub fn execute(program: String) -> Result<f64, ProgramError> {
        let atoms = Self::split_atoms(&program);
        let atoms = atoms?;
        let mut tree: Expression = Self::make_tree(atoms);

        // Debug
        // println!("\nTree before operate() :\n{}", tree.get_representation());

        let mut shuttle = Shuttle::new();
        tree.operate(&mut shuttle);

        // Debug
        println!("\nTree after operate() :\n{}", tree.get_representation());

        Ok(tree.get_value(0f64))
    }

    // Numeric overflows will cause number atoms to be
    // f64::INFINITY of f64::NEG_INFINITY.
    fn split_atoms(program: &str) -> Result<Vec<Atom>, ProgramError> {
        let mut result = Vec::<Atom>::new();
        let mut reading_number = false;
        let mut bracket_nesting = 0u8;
        let mut opening_bracket_pos = Vec::<usize>::new();
        let mut current_num = 0f64;
        let mut current_bracket_content = String::new();
        let mut periods_found = 0u8;
        let mut frac_pos = 0i32;
        let mut pos = 0usize;
        let mut digit_value: f64;

        for c in program.chars() {
            pos += 1;

            if bracket_nesting == 0 {
                if c == '_' {
                    // Ignore
                } else if c.is_whitespace() {
                    if reading_number {
                        result.push(Atom::Number(current_num));
                    }

                    reading_number = false;
                    current_num = 0f64;
                    periods_found = 0;
                    frac_pos = 0;
                } else if c.is_ascii_digit() {
                    reading_number = true;

                    digit_value = match f64::from_str(&c.to_string()) {
                        Err(err) => return Err(ProgramError::DigitParsingFailure{position: pos, reason: err.to_string()}),
                        Ok(val) => val,
                    };

                    match periods_found {
                        0 => current_num = (current_num * 10f64) + digit_value,
                        // TODO : treat second period as the start of the repeated numbers sequence.
                        1 | 2 => {
                            frac_pos += 1;
                            current_num += digit_value / 10f64.powi(frac_pos);
                        },
                        _ => {
                            result.push(Atom::Number(current_num));
                            current_num = digit_value;
                            periods_found = 1;
                            frac_pos = 0;
                        },
                    }
                } else if c == '.' {
                    reading_number = true;
                    periods_found += 1;
                } else if c == '[' {
                    if reading_number {
                        result.push(Atom::Number(current_num));
                        reading_number = false;
                        current_num = 0f64;
                        periods_found = 0;
                        frac_pos = 0;
                    }

                    bracket_nesting += 1;
                    opening_bracket_pos.push(pos);
                } else if c == ']' {
                    return Err(ProgramError::UnexpectedClosingBracket{position: pos});
                } else {
                    if reading_number {
                        result.push(Atom::Number(current_num));
                        reading_number = false;
                        current_num = 0f64;
                        periods_found = 0;
                        frac_pos = 0;
                    }

                    if Self::is_known_operator(c) {
                        result.push(Atom::Operator(c));
                    } else {
                        return Err(ProgramError::UnknownOperator{position: pos, operator: c});
                    }
                }
            } else {
                match c {
                    '[' => {
                        bracket_nesting += 1;
                        current_bracket_content.push(c);
                        opening_bracket_pos.push(pos);
                    },
                    ']' => {
                        bracket_nesting -= 1;

                        if bracket_nesting == 0 {
                            // An empty bracket content is ignored.
                            if !current_bracket_content.is_empty() {
                                match current_bracket_content.chars().next().unwrap() {
                                    first if "sc".contains(first) => {
                                        let rest = match current_bracket_content.get(1..) {
                                            None => String::new(),
                                            Some(r) => r.to_string(),
                                        };

                                        match first {
                                            's' => result.push(Atom::String(rest)),
                                            'c' => result.push(Atom::Comment(rest)),
                                            _ => (),
                                        }
                                    },
                                    other => {
                                        let marker_pos = match opening_bracket_pos.pop() {
                                            None => 0,
                                            Some(ps) => ps + 1,
                                        };

                                        return Err(ProgramError::UnknownBracketContentTypeMarker{position: marker_pos, marker: other});
                                    },
                                }
                            }

                            current_bracket_content = String::new();
                        } else {
                            current_bracket_content.push(c);
                        }

                        opening_bracket_pos.pop();
                    },
                    _ => current_bracket_content.push(c),
                }
            }
        }

        if bracket_nesting > 0 {
            return Err(ProgramError::UnclosedBracketsAtEnd);
        }

        if reading_number {
            result.push(Atom::Number(current_num));
        }

        Ok(result)
    }

    fn make_tree(atoms: Vec<Atom>) -> Expression {
        let mut exp_stack = Vec::<Expression>::new();
        let mut override_start_found = false;
        let mut override_end_found = false;
        let mut found_last_op = false;
        let mut needed_ops = 0usize;
        let mut make_assignment_operator = false;

        // Preprocess atoms to replace the ':' operator with extra atoms.

        for exp in atoms.iter().rev() {
            match exp {
                Atom::Number(n) => {
                    let mut new_exp = Expression::new_number(*n);
                    
                    if override_end_found {
                        new_exp.is_last_of_override = true;
                        override_end_found = false;
                    }

                    exp_stack.push(new_exp);
                },
                Atom::Operator(c) => {
                    if !override_start_found {
                        needed_ops = match *c {
                            chr if "~v:!"         .contains(chr) => 1,
                            chr if "+-*/^%$W;=<>Z".contains(chr) => 2,
                            chr if "?"           .contains(chr) => 3,
                            _                                  => 0,
                        };
                    }

                    match *c {
                        '(' => override_start_found = true,
                        ')' => override_end_found = true,
                        op_for_stack => {
                            let mut new_exp = Expression::new(op_for_stack);
                            new_exp.is_assignment_op = make_assignment_operator;
                            make_assignment_operator = false;

                            if override_start_found {
                                override_start_found = false;

                                while let Some(e) = exp_stack.pop() {
                                    if e.is_last_of_override {
                                        found_last_op = true;
                                    }

                                    new_exp.push_operand(e);

                                    if found_last_op {
                                        found_last_op = false;
                                        break;
                                    }
                                }
                            } else {
                                for _i in 0..needed_ops {
                                    if let Some(e) = exp_stack.pop() {
                                        new_exp.push_operand(e);
                                    }
                                }
                            }

                            exp_stack.push(new_exp);

                            if op_for_stack == ':' {
                                make_assignment_operator = true;
                            }
                        },
                    }
                },
                Atom::Comment(_) => (),
                _ => (), // TODO: process strings when related operators are implemented.
            }
        }

        // If the stack has more than one expression, put whatever remains on the stack as operands in a ; - expression.
        match exp_stack.len() {
            0 => Expression::new_number(0f64),
            1 => exp_stack.pop().expect("Is should be possible to pop the single Expression from the expression stack."),
            _ => {
                let mut result = Expression::new(';');

                for exp in exp_stack.into_iter().rev() {
                    result.push_operand(exp);
                }

                result
            },
        }
    }

    fn is_known_operator(op: char) -> bool {
        "~+-*/^%$v:?W;()=<>!Z".contains(op)
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

fn are_near(num1: f64, num2: f64, precision: f64) -> bool {
    (num1 - num2).abs() <= precision
}

#[cfg(test)]
fn are_very_near(num1: f64, num2: f64) -> bool {
    are_near(num1, num2, 0.00000001f64)
}

pub(crate) mod opr_funcs {
    use super::{Expression, Shuttle, are_near};
    
    pub fn nop(_result_value: &mut Option<f64>, _operands: &mut [Expression], _shuttle: &mut Shuttle) {
        // Don't do anything.
        // Meant for Expressions that contain a fixed numerical value from creation.
    }
    
    pub fn add(result_value: &mut Option<f64>, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = 0f64;

        for op in operands {
            outcome += op.get_value(0f64);
        }

        *result_value = Some(outcome);
    }
    
    pub fn multiply(result_value: &mut Option<f64>, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_value(0f64),
                _ => outcome *= op.get_value(1f64),
            }
        }

        *result_value = Some(outcome);
    }
    
    pub fn minus(result_value: &mut Option<f64>, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_value(0f64),
                _ => outcome -= op.get_value(0f64),
            }
        }

        *result_value = Some(outcome);
    }
    
    pub fn divide(result_value: &mut Option<f64>, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_value(0f64),
                _ => outcome /= op.get_value(1f64),
            }
        }

        *result_value = Some(outcome);
    }
    
    pub fn modulo(result_value: &mut Option<f64>, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_value(0f64),
                _ => outcome %= op.get_value(1f64),
            }
        }

        *result_value = Some(outcome);
    }

    pub fn combine(result_value: &mut Option<f64>, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        *result_value = match operands.last() {
            Some(e) => Some(e.get_value(0f64)),
            None => Some(0f64),
        };
    }

    pub fn unaryminus(result_value: &mut Option<f64>, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        *result_value = match operands.first() {
            None => Some(0f64),
            Some(e) => Some(- e.get_value(0f64)),
        };
    }

    pub fn assign_number_register(result_value: &mut Option<f64>, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let index = if !operands.is_empty() {
            operands[0].get_value(0f64)
        } else {
            0f64
        } as i64;

        let reg_val = if operands.len() >= 2 {
            operands[1].get_value(0f64)
        } else {
            0f64
        };

        shuttle.nums.insert(index, reg_val);
        *result_value = Some(reg_val);
    }

    pub fn get_number_register(result_value: &mut Option<f64>, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let index = if !operands.is_empty() {
            operands[0].get_value(0f64)
        } else {
            0f64
        } as i64;

        let found = match shuttle.nums.get(&index) {
            None => 0f64,
            Some(n) => *n,
        };

        *result_value = Some(found);
    }

    pub fn assignment_maker(result_value: &mut Option<f64>, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let index = if !operands.is_empty() {
            operands[0].get_value(0f64)
        } else {
            0f64
        } as i64;

        shuttle.assignment_index = index;

        let found = match shuttle.nums.get(&index) {
            None => 0f64,
            Some(n) => *n,
        };

        *result_value = Some(found);
    }
    
    pub fn equals(result_value: &mut Option<f64>, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let mut are_equal = true;
        let mut first = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => first = op.get_value(0f64),
                _ => are_equal = are_near(first, op.get_value(0f64), shuttle.orb),
            }
        }

        *result_value = Some(if are_equal {
            1f64
        } else {
            0f64
        });
    }
    
    pub fn less(result_value: &mut Option<f64>, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = true;
        let mut first = 0f64;
        let mut second: f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => first = op.get_value(0f64),
                _ => {
                    second = op.get_value(0f64);
                    outcome = outcome && (first < second);
                    first = second;
                },
            }
        }

        *result_value = Some(if outcome {
            1f64
        } else {
            0f64
        });
    }
    
    pub fn greater(result_value: &mut Option<f64>, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = true;
        let mut first = 0f64;
        let mut second: f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => first = op.get_value(0f64),
                _ => {
                    second = op.get_value(0f64);
                    outcome = outcome && (first > second);
                    first = second;
                },
            }
        }

        *result_value = Some(if outcome {
            1f64
        } else {
            0f64
        });
    }

    pub fn not(result_value: &mut Option<f64>, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let mut outcome = true;

        for op in operands {
            outcome &= are_near(0f64, op.get_value(0f64), shuttle.orb);
        }

        *result_value = Some(if outcome {
            1f64
        } else {
            0f64
        });
    }
    
    pub fn setting(result_value: &mut Option<f64>, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let mut setting_nr = -1i64;
        let mut setting_value = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => setting_nr = op.get_value(-1f64) as i64,
                1 => setting_value = op.get_value(0f64),
                _ => (),
            }
        }

        match setting_nr {
            0i64 => shuttle.orb = setting_value,
            1i64 => shuttle.max_iterations = setting_value,
            _ => (),
        }

        *result_value = Some(setting_value);
    }
    
    pub fn exec_while(result_value: &mut Option<f64>, operands: &mut [Expression], shuttle: &mut Shuttle) {
        if operands.is_empty() {
            *result_value = Some(0f64);

            return;
        }

        let mut outcome = 0f64;
        let mut op_count: usize;
        let op_len = operands.len();
        let mut iter_count = 0f64;
        let mut op: &mut Expression;

        'outer: loop {
            iter_count += 1f64;

            if (shuttle.max_iterations > 0f64) && (iter_count > shuttle.max_iterations) {
                break;
            }

            op_count = 0;

            while op_count < op_len {
                op = &mut operands[op_count];

                match op_count {
                    0 =>  {
                        op.operate(shuttle);

                        if are_near(0f64, op.get_value(0f64), shuttle.orb) {
                                    break 'outer;
                        }
                    },
                    _ => {
                        op.operate(shuttle);
                        outcome = op.get_value(0f64);
                    },
                }

                op_count += 1;
            }
        }

        *result_value = Some(outcome);
    }
}

#[cfg(test)]
mod tests {
    mod split {
        use crate::*;

        #[test]
        fn split_positive_integer() {
            let result = Interpreter::split_atoms("138");
            assert_eq!(Ok(vec![Atom::Number(138f64)]), result);
        }

        #[test]
        fn split_positive_integer_contains_underscore() {
            let result = Interpreter::split_atoms("1_38");
            assert_eq!(Ok(vec![Atom::Number(138f64)]), result);
        }

        #[test]
        fn split_positive_integer_starts_with_underscore() {
            let result = Interpreter::split_atoms("_138");
            assert_eq!(Ok(vec![Atom::Number(138f64)]), result);
        }

        #[test]
        fn split_positive_integer_ends_with_underscore() {
            let result = Interpreter::split_atoms("138_");
            assert_eq!(Ok(vec![Atom::Number(138f64)]), result);
        }

        #[test]
        fn split_positive_fractal_greater_than_1() {
            let result = Interpreter::split_atoms("22.6");
            assert_eq!(Ok(vec![Atom::Number(22.6f64)]), result);
        }

        #[test]
        fn split_positive_fractal_smaller_than_1_starting_with_dot() {
            let result = Interpreter::split_atoms(".922");
            assert_eq!(Ok(vec![Atom::Number(0.922f64)]), result);
        }

        #[test]
        fn split_positive_fractal_smaller_than_1_starting_with_0() {
            let result = Interpreter::split_atoms("0.922");
            assert_eq!(Ok(vec![Atom::Number(0.922f64)]), result);
        }

        #[test]
        fn split_positive_fractal_smaller_than_1_starting_with_00() {
            let result = Interpreter::split_atoms("00.922");
            assert_eq!(Ok(vec![Atom::Number(0.922f64)]), result);
        }

        #[test]
        fn split_positive_fractal_greater_than_1_contains_underscores() {
            let result = Interpreter::split_atoms("_22_._6_");
            assert_eq!(Ok(vec![Atom::Number(22.6f64)]), result);
        }

        #[test]
        fn split_positive_integer_ends_with_dot() {
            let result = Interpreter::split_atoms("22.");
            assert_eq!(Ok(vec![Atom::Number(22f64)]), result);
        }

        #[test]
        fn split_positive_integer_dot() {
            let result = Interpreter::split_atoms(".");
            assert_eq!(Ok(vec![Atom::Number(0f64)]), result);
        }

        #[test]
        fn split_two_numbers() {
            let result = Interpreter::split_atoms("741 _.60");
            assert_eq!(Ok(vec![Atom::Number(741f64), Atom::Number(0.60f64)]), result);
        }

        #[test]
        fn split_known_operator() {
            let result = Interpreter::split_atoms("*");
            assert_eq!(Ok(vec![Atom::Operator('*')]), result);
        }

        #[test]
        fn split_unknown_operator() {
            let result = Interpreter::split_atoms("'");
            assert_eq!(Err(ProgramError::UnknownOperator{position: 1, operator: '\''}), result);
        }

        #[test]
        fn split_mixed() {
            let result = Interpreter::split_atoms(";.11:+_5.2[cAnd now the second operand]9119 ~ 45 + 3 2");

            assert_eq!(Ok(vec![
                Atom::Operator(';'),
                Atom::Number(0.11f64),
                Atom::Operator(':'),
                Atom::Operator('+'),
                Atom::Number(5.2f64),
                Atom::Comment("And now the second operand".to_string()),
                Atom::Number(9119f64),
                Atom::Operator('~'),
                Atom::Number(45f64),
                Atom::Operator('+'),
                Atom::Number(3f64),
                Atom::Number(2f64),
            ]), result);
        }

        #[test]
        fn split_string() {
            let result = Interpreter::split_atoms("[sVoltaire _]");
            assert_eq!(Ok(vec![Atom::String("Voltaire _".to_string())]), result);
        }

        #[test]
        fn split_nested_string() {
            let result = Interpreter::split_atoms("[sDescartes: [sJe pense, donc je suis.], disait-il.]");
            assert_eq!(Ok(vec![Atom::String("Descartes: [sJe pense, donc je suis.], disait-il.".to_string())]), result);
        }

        #[test]
        fn split_nested_comment() {
            let result = Interpreter::split_atoms("[cDescartes: [sJe pense, donc je suis.], disait-il.]");
            assert_eq!(Ok(vec![Atom::Comment("Descartes: [sJe pense, donc je suis.], disait-il.".to_string())]), result);
        }

        #[test]
        fn split_comment() {
            let result = Interpreter::split_atoms("[cLaconic]");
            assert_eq!(Ok(vec![Atom::Comment("Laconic".to_string())]), result);
        }

        #[test]
        fn split_unknown_bracket_type() {
            let result = Interpreter::split_atoms("[xLaconic]");
            assert_eq!(Err(ProgramError::UnknownBracketContentTypeMarker{position: 2, marker: 'x'}), result);
        }

        #[test]
        fn split_unexpected_closing_bracket() {
            let result = Interpreter::split_atoms("[cLaconic]$2 174]v2");
            assert_eq!(Err(ProgramError::UnexpectedClosingBracket{position: 17}), result);
        }

        #[test]
        fn split_unclosed_bracket() {
            let result = Interpreter::split_atoms("[cLaconic] [s $2 174 v2");
            assert_eq!(Err(ProgramError::UnclosedBracketsAtEnd), result);
        }

        #[test]
        fn split_unclosed_bracket_nested() {
            let result = Interpreter::split_atoms("[c [cLaconic] $2 174 v2");
            assert_eq!(Err(ProgramError::UnclosedBracketsAtEnd), result);
        }
    }

    mod expr {
        use crate::*;

        #[test]
        fn expr_new_number_get_value() {
            let exp = Expression::new_number(47.11f64);
            assert_eq!(47.11f64, exp.get_value(0f64));

            // Check if we can get the value a second time.
            assert_eq!(47.11f64, exp.get_value(0f64));
        }

        #[test]
        fn expr_add_numbers() {
            let mut exp = Expression::new('+');
            exp.push_operand(Expression::new_number(4000.3f64));
            exp.push_operand(Expression::new_number( 500.1f64));

            let mut shuttle = Shuttle::new();
            exp.operate(&mut shuttle);

            // Fails due to precision error: right value is 4500.400000000001.
            // assert_eq!(4500.4f64, exp.get_value(0f64));

            assert!(are_very_near(4500.4f64, exp.get_value(0f64)));
        }

        #[test]
        fn expr_add_expressions() {
            let mut exp = Expression::new('+');

            let mut op1 = Expression::new('+');
            op1.push_operand(Expression::new_number( 43f64));
            op1.push_operand(Expression::new_number(500f64));

            let mut op2 = Expression::new('+');
            op2.push_operand(Expression::new_number( 9.03f64));
            op2.push_operand(Expression::new_number(22f64));

            exp.push_operand(op1);
            exp.push_operand(op2);

            let mut shuttle = Shuttle::new();
            exp.operate(&mut shuttle);

            assert_eq!(574.03f64, exp.get_value(0f64));
        }

        #[test]
        fn expr_assign_num_reg_dot3() {
            let mut exp = Expression::new('$');
            exp.push_operand(Expression::new_number(4000.3f64));
            exp.push_operand(Expression::new_number( 500.1f64));

            let mut shuttle = Shuttle::new();
            exp.operate(&mut shuttle);

            match shuttle.nums.get(&4000i64) {
                None => panic!("The numerical register assigned to has not been found back."),
                Some(n) => assert_eq!(500.1f64, *n),
            }
        }

        #[test]
        fn expr_assign_num_reg_dot7() {
            let mut exp = Expression::new('$');
            exp.push_operand(Expression::new_number(4000.7f64));
            exp.push_operand(Expression::new_number( 500.1f64));

            let mut shuttle = Shuttle::new();
            exp.operate(&mut shuttle);

            match shuttle.nums.get(&4000i64) {
                None => panic!("The numerical register assigned to has not been found back."),
                Some(n) => assert_eq!(500.1f64, *n),
            }
        }

        #[test]
        fn expr_assign_num_reg_neg_dot3() {
            let mut exp = Expression::new('$');
            exp.push_operand(Expression::new_number(-4000.3f64));
            exp.push_operand(Expression::new_number( 500.1f64));

            let mut shuttle = Shuttle::new();
            exp.operate(&mut shuttle);

            match shuttle.nums.get(&-4000i64) {
                None => panic!("The numerical register assigned to has not been found back."),
                Some(n) => assert_eq!(500.1f64, *n),
            }
        }

        #[test]
        fn expr_assign_num_reg_neg_dot7() {
            let mut exp = Expression::new('$');
            exp.push_operand(Expression::new_number(-4000.7f64));
            exp.push_operand(Expression::new_number( 500.1f64));

            let mut shuttle = Shuttle::new();
            exp.operate(&mut shuttle);

            match shuttle.nums.get(&-4000i64) {
                None => panic!("The numerical register assigned to has not been found back."),
                Some(n) => assert_eq!(500.1f64, *n),
            }
        }

        #[test]
        fn expr_set_orb() {
            let mut exp = Expression::new('Z');
            exp.push_operand(Expression::new_number(0f64));
            exp.push_operand(Expression::new_number(0.001f64));
            let mut shuttle = Shuttle::new();

            exp.operate(&mut shuttle);

            assert_eq!(0.001f64, shuttle.orb);
        }

        #[test]
        fn expr_set_max_iterations() {
            let mut exp = Expression::new('Z');
            exp.push_operand(Expression::new_number(1f64));
            exp.push_operand(Expression::new_number(500f64));
            let mut shuttle = Shuttle::new();

            exp.operate(&mut shuttle);

            assert_eq!(500f64, shuttle.max_iterations);
        }
    }

    mod ops {
        use crate::{Expression, Shuttle};
        use crate::opr_funcs::*;

        #[test]
        fn nop_doesnt_change_value() {
            let mut the_value = Some(500f64);
            let mut ops = Vec::<Expression>::new();
            let mut shuttle = Shuttle::new();

            nop(&mut the_value, &mut ops, &mut shuttle);
            assert!(the_value.is_some());
            assert_eq!(500f64, the_value.unwrap());
            assert!(ops.is_empty());
        }

        #[test]
        fn op_add() {
            let mut the_value = None::<f64>;
            let mut ops = vec![Expression::new_number(12f64), Expression::new_number(68f64)];
            let mut shuttle = Shuttle::new();

            add(&mut the_value, &mut ops, &mut shuttle);

            // Check the value.
            assert!(the_value.is_some());
            assert_eq!(80f64, the_value.unwrap());

            // Verify that the operands didn't change.
            assert_eq!(Some(12f64), ops[0].value);
            assert_eq!(Some(68f64), ops[1].value);
        }
    }

    mod exec {
        use crate::{Interpreter, are_very_near};

        #[test]
        fn x_remaining_stack_items_while_making_tree() {
            assert_eq!(Ok(16f64), Interpreter::execute("+ 44 1 * 8 2".to_string()));
        }

        #[test]
        fn x_nested_ops() {
            assert_eq!(Ok(97f64), Interpreter::execute("+4+90 3".to_string()));
        }

        #[test]
        fn x_add_3_op() {
            assert_eq!(Ok(229f64), Interpreter::execute("+(3 111 115)".to_string()));
        }

        #[test]
        fn x_add_2_op() {
            assert_eq!(Ok(226f64), Interpreter::execute("+111 115".to_string()));
        }

        #[test]
        fn x_add_1_op() {
            assert_eq!(Ok(111f64), Interpreter::execute("+111".to_string()));
        }

        #[test]
        fn x_add_0_op() {
            assert_eq!(Ok(0f64), Interpreter::execute("+".to_string()));
        }

        #[test]
        fn x_minus_3_op() {
            assert_eq!(Ok(-9f64), Interpreter::execute("-(111 115 +3 2)".to_string()));
        }

        #[test]
        fn x_minus_2_op() {
            assert_eq!(Ok(-4f64), Interpreter::execute("-111 115".to_string()));
        }

        #[test]
        fn x_minus_1_op() {
            assert_eq!(Ok(111f64), Interpreter::execute("-111".to_string()));
        }

        #[test]
        fn x_minus_0_op() {
            assert_eq!(Ok(0f64), Interpreter::execute("-".to_string()));
        }

        #[test]
        fn x_combine_3_op() {
            assert_eq!(Ok(54f64), Interpreter::execute(";(1 +7 3 54)".to_string()));
        }

        #[test]
        fn x_combine_3_op_bis() {
            assert_eq!(Ok(10f64), Interpreter::execute(";(1 54 +7 3)".to_string()));
        }

        #[test]
        fn x_combine_2_op() {
            assert_eq!(Ok(54f64), Interpreter::execute("; 1 54".to_string()));
        }

        #[test]
        fn x_combine_1_op() {
            assert_eq!(Ok(54f64), Interpreter::execute(";54".to_string()));
        }

        #[test]
        fn x_combine_0_op() {
            assert_eq!(Ok(0f64), Interpreter::execute(";".to_string()));
        }

        #[test]
        fn x_override_to_1() {
            assert_eq!(Ok(51f64), Interpreter::execute("+- 50 +(2) 3".to_string()));
        }

        #[test]
        fn x_nested_override() {
            assert_eq!(Ok(59f64), Interpreter::execute("+(- 50 +(2) 3 7 1)".to_string()));
        }

        #[test]
        fn x_missing_end_of_override() {
            assert_eq!(Ok(41f64), Interpreter::execute("+ 50 -(2 3 7 1".to_string()));
        }

        #[test]
        fn x_missing_start_of_override() {
            assert_eq!(Ok(48f64), Interpreter::execute("+-50)2".to_string()));
        }

        #[test]
        fn x_multiply_3_op() {
            assert_eq!(Ok(210f64), Interpreter::execute("*(3 14 5)".to_string()));
        }

        #[test]
        fn x_multiply_2_op() {
            assert_eq!(Ok(11100f64), Interpreter::execute("*111 100".to_string()));
        }

        #[test]
        fn x_multiply_1_op() {
            assert_eq!(Ok(111f64), Interpreter::execute("*111".to_string()));
        }

        #[test]
        fn x_multiply_0_op() {
            assert_eq!(Ok(10f64), Interpreter::execute("+ 10 *".to_string()));
        }

        #[test]
        fn x_divide_3_op() {
            assert_eq!(Ok(0.5f64), Interpreter::execute("/(70 20 7)".to_string()));
        }

        #[test]
        fn x_divide_2_op() {
            assert_eq!(Ok(3.5f64), Interpreter::execute("/70 20".to_string()));
        }

        #[test]
        fn x_divide_1_op() {
            assert_eq!(Ok(70f64), Interpreter::execute("/70".to_string()));
        }

        #[test]
        fn x_divide_0_op() {
            assert_eq!(Ok(5f64), Interpreter::execute("+5 /".to_string()));
        }

        #[test]
        fn x_divide_by_zero() {
            assert_eq!(Ok(f64::INFINITY), Interpreter::execute("/8 0".to_string()));
        }

        #[test]
        fn x_modulo_3_op_integer() {
            assert_eq!(Ok(1f64), Interpreter::execute("%(70 20 3)".to_string()));
        }

        #[test]
        fn x_modulo_2_op_integer() {
            assert_eq!(Ok(10f64), Interpreter::execute("%70 20".to_string()));
        }

        #[test]
        fn x_modulo_2_op_fractal() {
            assert!(are_very_near(0.7f64, Interpreter::execute("%70 3.3".to_string()).unwrap()));
        }

        #[test]
        fn x_modulo_1_op() {
            assert_eq!(Ok(70f64), Interpreter::execute("%70".to_string()));
        }

        #[test]
        fn x_modulo_0_op() {
            assert_eq!(Ok(0f64), Interpreter::execute("%".to_string()));
        }

        #[test]
        fn x_unaryminus_3_op() {
            assert_eq!(Ok(-5.02f64), Interpreter::execute("~(5.02 8 3)".to_string()));
        }

        #[test]
        fn x_unaryminus_1_op() {
            assert_eq!(Ok(-5.02f64), Interpreter::execute("~5.02".to_string()));
        }

        #[test]
        fn x_unaryminus_0_op() {
            assert_eq!(Ok(2f64), Interpreter::execute("+ 2 ~".to_string()));
        }

        #[test]
        fn x_assign_num_return_value() {
            assert_eq!(Ok(111f64), Interpreter::execute("$4 111".to_string()));
        }

        #[test]
        fn x_get_num_reg() {
            assert_eq!(Ok(90f64), Interpreter::execute("$/21 2 90 $(4) v/21 2".to_string()));
        }

        #[test]
        fn x_add_assign() {
            assert_eq!(Ok(90f64), Interpreter::execute("$18 88 +:-21 3 2 v18".to_string()));
        }

        #[test]
        fn x_unaryminus_assign() {
            assert_eq!(Ok(-88f64), Interpreter::execute("$18 88 ~:18 v18".to_string()));
        }

        #[test]
        fn x_equality_2_exact() {
            assert_eq!(Ok(1f64), Interpreter::execute("=21.3 21.3".to_string()));
        }

        #[test]
        fn x_equality_2_in_orb() {
            assert_eq!(Ok(1f64), Interpreter::execute("Z0 .1 =21.3 21.35".to_string()));
        }

        #[test]
        fn x_equality_2_outside_orb() {
            assert_eq!(Ok(0f64), Interpreter::execute("Z0 .01 =21.3 21.35".to_string()));
        }

        #[test]
        fn x_less_true() {
            assert_eq!(Ok(1f64), Interpreter::execute("< 5.000001 5.000002".to_string()));
        }

        #[test]
        fn x_less_false() {
            assert_eq!(Ok(0f64), Interpreter::execute("< 5.000002 5.000002".to_string()));
        }

        #[test]
        fn x_less_3_true() {
            assert_eq!(Ok(1f64), Interpreter::execute("<(5.000001 5.000002 6)".to_string()));
        }

        #[test]
        fn x_less_3_false() {
            assert_eq!(Ok(0f64), Interpreter::execute("<(5.000001 5.000002 3)".to_string()));
        }

        #[test]
        fn x_greater_true() {
            assert_eq!(Ok(1f64), Interpreter::execute("> 5.000002 5.000001".to_string()));
        }

        #[test]
        fn x_greater_false() {
            assert_eq!(Ok(0f64), Interpreter::execute("> 5.000001 5.000002".to_string()));
        }

        #[test]
        fn x_greater_3_false() {
            assert_eq!(Ok(0f64), Interpreter::execute(">(5.000003 5.000002 6)".to_string()));
        }

        #[test]
        fn x_greater_3_true() {
            assert_eq!(Ok(1f64), Interpreter::execute(">(5.000002 5.000001 3)".to_string()));
        }

        #[test]
        fn x_not_1() {
            assert_eq!(Ok(0f64), Interpreter::execute("!1".to_string()));
        }

        #[test]
        fn x_not_0() {
            assert_eq!(Ok(1f64), Interpreter::execute("!0".to_string()));
        }

        #[test]
        fn x_not_other() {
            assert_eq!(Ok(0f64), Interpreter::execute("!~145".to_string()));
        }

        #[test]
        fn x_not_3_true() {
            assert_eq!(Ok(1f64), Interpreter::execute("!(0 0 0)".to_string()));
        }

        #[test]
        fn x_not_3_false() {
            assert_eq!(Ok(0f64), Interpreter::execute("!(~4 8 +90 1)".to_string()));
        }

        #[test]
        fn x_while_simple() {
            assert_eq!(Ok(1f64), Interpreter::execute("$1 4 W>v1 2 $1 1".to_string()));
        }

        #[test]
        fn x_while_executed() {
            assert_eq!(Ok(10f64), Interpreter::execute("$0 1 $1 0 W!>v0 4 ;+:1 v0 +:0 1 v1".to_string()));
        }

        #[test]
        fn x_while_executed_op_nr_overridden() {
            assert_eq!(Ok(10f64), Interpreter::execute("$0 1 $1 0 W(!>v0 4 +:1 v0 +:0 1) v1".to_string()));
        }

        #[test]
        fn x_while_never_executed() {
            assert_eq!(Ok(0f64), Interpreter::execute("$0 1 $1 0 W!>v0 ~1 ;+:1 v0 +:0 1 v1".to_string()));
        }
    }

    /*
    #[test]
    fn unwrap_some_copy_implementor() {
        let sm = Some(27f64);

        let mut val = sm.unwrap();
        assert_eq!(27f64, val);

        val = sm.unwrap();
        assert_eq!(27f64, val);
    }
    */
}
