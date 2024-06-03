// TODO: resolve TODO's in code.
// TODO: write unit tests on expressions having string content (ValueType::Text).
// TODO: implement all intended operators.
// TODO: implement Debug for Expression, using get_representation.
// TODO: if struct Interpreter ends up having no internal state (no properties), simply delete it
// and make its associated methods crate-level functions.
// TODO: implement different variable arrays - see the 'u' operator.
//      Default array: 0.
//      This means that the key for HashMap Shuttle.nums has to be a (i64, i64) tuple.
// TODO: have all operators have an acceptable behavior with less or more operands than standard.
//      (Special attention for ':' !)
// TODO: make private whatever can remain private.
// TODO: fn split_atoms should not construct f64 numbers,
//      but create uninitialized number expressions having only a number's string representation.
//      Upon their first call of operate(), they should calculate their number based on the
//      current number base.
//      (split_atoms can't be asked to handle calculated base specifications like Z 4 *n n .)

use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;

// Static lifetime: justified, because the functions referenced
// are compiled into the application and live as long as it runs.
// The first parameter should refer to an Expression's value property,
// the second one to a Expression's operands property (operands are Expression objects),
// the third one to the Shuttle containing other state.
type OperatorFunc = &'static dyn Fn(&mut ValueType, &mut [Expression], &mut Shuttle);

#[derive(PartialEq)]
enum Atom {
    Operator(char),
    Number(f64),
    String(String),
    Comment(String),
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Operator(c) => write!(f, "{}", c),
            Atom::Number(n) => write!(f, "{} ", n),
            Atom::String(_) => write!(f, "[s...]"),
            Atom::Comment(_) => write!(f, "[c...]"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ProgramError {
    DigitParsingFailure{position: usize, reason: String},
    UnexpectedClosingBracket{position: usize},
    UnknownOperator{position: usize, operator: char},
    UnclosedBracketsAtEnd,
    UnknownBracketContentTypeMarker{position: usize, marker: char},
}

#[derive(Clone, Debug)]
enum ValueType {
    Empty,
    Number(f64),
    Text(String),
}

struct Expression
{
    operator: OperatorFunc,
    operands: Vec<Expression>,
    value: ValueType,
    opr_mark: char,
    is_last_of_override: bool,
    has_overridden_nr_of_ops: bool,
}

impl Expression {
    pub fn new(opr_mark: char) -> Self {
        let opr: OperatorFunc = match opr_mark {
            '0' => &opr_funcs::nop,
            '"' => &opr_funcs::string_expr,
            '~' => &opr_funcs::unaryminus,
            '+' => &opr_funcs::add,
            '-' => &opr_funcs::minus,
            '*' => &opr_funcs::multiply,
            '/' => &opr_funcs::divide,
            '%' => &opr_funcs::modulo,
            '^' => &opr_funcs::power,
            'i' => &opr_funcs::intgr,
            'a' => &opr_funcs::abs,
            ';' => &opr_funcs::combine,
            'm' => &opr_funcs::min,
            'M' => &opr_funcs::max,
            'W' => &opr_funcs::exec_while,
            'F' => &opr_funcs::exec_for,
            '$' => &opr_funcs::assign_number_register,
            'v' => &opr_funcs::get_number_register,
            ':' => &opr_funcs::assignment_maker,
            '=' => &opr_funcs::equals,
            '<' => &opr_funcs::less,
            '>' => &opr_funcs::greater,
            '!' => &opr_funcs::not,
            '&' => &opr_funcs::and,
            '|' => &opr_funcs::or,
            'x' => &opr_funcs::xor,
            '?' => &opr_funcs::exec_if,
            'Z' => &opr_funcs::setting,
            'o' => &opr_funcs::enumerated_opr,
            'O' => &opr_funcs::enumerated_opr,
            _ => &opr_funcs::nop,
        };

        Expression {
            operator: opr,
            operands: Vec::<Expression>::new(),
            value: ValueType::Empty,
            opr_mark,
            is_last_of_override: false,
            has_overridden_nr_of_ops: false,
        }
    }

    pub fn new_number(num: f64) -> Self {
        Expression {
            operator: &opr_funcs::nop,
            operands: Vec::<Expression>::new(),
            value: ValueType::Number(num),
            opr_mark: '0',
            is_last_of_override: false,
            has_overridden_nr_of_ops: false,
        }
    }

    pub fn new_text(txt: String) -> Self {
        Expression {
            operator: &opr_funcs::string_expr,
            operands: Vec::<Expression>::new(),
            value: ValueType::Text(txt),
            opr_mark: '"',
            is_last_of_override: false,
            has_overridden_nr_of_ops: false,
        }
    }

    pub fn push_operand(&mut self, op: Expression) {
        self.operands.push(op);
    }

    pub fn get_value(&self) -> ValueType {
        self.value.clone()
    }

    pub fn get_num_value(&self, default: f64) -> f64 {
        match self.value {
            ValueType::Number(num) => num,
            _ => default,
        }
    }

    pub fn get_string_value(&self, default: String) -> String {
        match self.value {
            ValueType::Text(ref txt) => txt.clone(),
            _ => default,
        }
    }

    pub fn operate(&mut self, shuttle: &mut Shuttle) {
        /*
        #[cfg(test)]
        println!("operate {}", self.opr_mark);
        */

        shuttle.assignment_indexes_stack.push(Vec::<i64>::new());

        let defer_opd_evaluation = "WF?".contains(self.opr_mark);

        if !defer_opd_evaluation {
            for op in &mut self.operands {
                op.operate(shuttle);
            }
        }

        (self.operator)(&mut self.value, &mut self.operands, shuttle);

        let assignment_indexes = shuttle.assignment_indexes_stack.pop()
            .expect("fn operate should always find elements in shuttle.assignment_indexes_stack after execution of the operator.");

        /*
        #[cfg(test)]
        println!("operate {}, assignment_indexes: {:?}", self.opr_mark, assignment_indexes);
        */

        for index in assignment_indexes {
            // Assign the resulting value to the variable
            // referenced by the first operand and stored by it in the shuttle.
            shuttle.nums.insert(index, self.get_value());
        }

        /*
        #[cfg(test)]
        println!("operate {} ==> {:?}", self.opr_mark, self.get_value());
        */
    }

    pub fn get_representation(&self) -> String {
        let mut exp_rep = match self.opr_mark {
            '0' => match self.value {
                ValueType::Empty => "empty_num".to_string(),
                ValueType::Number(num) => num.to_string(),
                _ => panic!("An expression with opr_mark '0' should either have a ValueType::Number or ValueType::Empty"),
            },
            '"' => match self.value {
                ValueType::Empty => "no_value".to_string(),
                ValueType::Text(ref txt) => txt.clone(),
                _ => panic!("An expression with opr_mark '\"' should either have a ValueType::Text or ValueType::Empty"),
            },
            oth => oth.to_string(),
        };

        if self.has_overridden_nr_of_ops {
            exp_rep.push('(');
        }

        if self.is_last_of_override {
            exp_rep.push(')');

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

        if !("0\"".contains(self.opr_mark)) {
            // Vim folding fix braces: {{
            exp_rep.push_str("\n\u{0_2514}\u{0_2500}> ");
            // exp_rep.push_str(format!("{:?}", self.value).as_str());

            let val_rep = match self.value {
                ValueType::Empty => "no_value".to_string(),
                ValueType::Number(num) => num.to_string(),
                ValueType::Text(ref txt) => txt.clone(),
            };

            exp_rep.push_str(val_rep.as_str());
        }

        exp_rep
    }
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let representation = match self.opr_mark  {
            '0' => format!("{}", self.get_num_value(0f64)),
            '"' => self.get_string_value("(no_value)".to_string()),
            _   => String::new(),
        };

        let out_string = format!("{}{}", self.opr_mark, representation);
        write!(f, "{}", out_string)
    }
}

// Shuttle objects are used to be passed to every operator function
// to provide state other than the operands.
struct Shuttle {
    nums: HashMap<i64, ValueType>,
    routines: HashMap<i64, Expression>,
    assignment_indexes_stack: Vec<Vec<i64>>,
    max_iterations: f64,
    orb: f64,
}

impl Shuttle {
    fn new() -> Self {
        Shuttle {
            nums: HashMap::<i64, ValueType>::new(),
            routines: HashMap::<i64, Expression>::new(),
            assignment_indexes_stack: Vec::<Vec<i64>>::new(),
            max_iterations: 10_000f64,
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
        Self::execute_opts(program, true, false, false)
    }

    pub fn execute_opts(program: String, do_execute: bool, show_before: bool, show_after: bool) -> Result<f64, ProgramError> {
        let atoms = Self::split_atoms(&program)?;
        let mut tree: Expression = Self::make_tree(atoms);

        if show_before {
            println!("\nTree before operate() :\n{}", tree.get_representation());
        }

        if do_execute {
            let mut shuttle = Shuttle::new();
            tree.operate(&mut shuttle);
        }

        if show_after {
            println!("\nTree after operate() :\n{}", tree.get_representation());
        }

        Ok(tree.get_num_value(0f64))
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
        let mut needed_ops: usize;

        // Preprocess atoms to replace the ':' operator with extra atoms.

        for exp in atoms.iter().rev() {

            /*
            #[cfg(test)]
            println!("exp_stack: {:?}", exp_stack);
            */

            match exp {
                Atom::Number(n) => {
                    let new_exp = Expression::new_number(*n);
                    exp_stack.push(new_exp);
                },
                Atom::String(s) => {
                    let new_exp = Expression::new_text(s.to_string());
                    exp_stack.push(new_exp);
                },
                Atom::Operator(c) => {
                    needed_ops = match *c {
                        chr if "~iav:!"             .contains(chr) => 1,
                        chr if "+-*/^%&|x$W;mM=<>Zo".contains(chr) => 2,
                        chr if "?O"                 .contains(chr) => 3,
                        chr if "F"                  .contains(chr) => 5,
                        _                                          => 0,
                    };

                    match *c {
                        '(' => override_start_found = true,
                        // ')' => override_end_found = true,
                        op_for_stack => {
                            let mut new_exp = Expression::new(op_for_stack);

                            // The number of arguments for the : operator can't be overridden.
                            // The override_start marker will be applied to the previous operator
                            // instead.
                            if override_start_found && (op_for_stack != ':') {
                                new_exp.has_overridden_nr_of_ops = true;
                                override_start_found = false;

                                while let Some(e) = exp_stack.pop() {
                                    if e.opr_mark == ')' {
                                        match new_exp.operands.last_mut() {
                                            None =>  (),
                                            Some(ref mut op) => op.is_last_of_override = true,
                                        }

                                        break;
                                    }

                                    new_exp.push_operand(e);
                                }
                            } else {
                                let mut i = 0;

                                while i < needed_ops {
                                    i += 1;

                                    if let Some(e) = exp_stack.pop() {
                                        if e.opr_mark == ')' {
                                            // Unexpected, but we can't ignore it.
                                            match new_exp.operands.last_mut() {
                                                None =>  (),
                                                Some(ref mut op) => op.is_last_of_override = true,
                                            }

                                            break;
                                        }

                                        new_exp.push_operand(e);
                                    }
                                }
                            }

                            exp_stack.push(new_exp);
                        },
                    }
                },
                Atom::Comment(_) => (),
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
        "~+-*/^ia%$v:?WF;mM()=<>!&|xZoO".contains(op)
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
    use super::{Expression, Shuttle, ValueType, are_near};
    
    pub fn nop(_result_value: &mut ValueType, _operands: &mut [Expression], _shuttle: &mut Shuttle) {
        // Don't do anything.
        // Meant for Expressions that contain a fixed numerical value from creation.
    }

    pub fn string_expr(_result_value: &mut ValueType, _operands: &mut [Expression], _shuttle: &mut Shuttle) {
        // Don't do anything.
        // Meant for Expressions that contain a fixed string value from creation.
    }
    
    pub fn add(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = 0f64;

        for op in operands {
            outcome += op.get_num_value(0f64);
        }

        *result_value = ValueType::Number(outcome);
    }
    
    pub fn multiply(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_num_value(0f64),
                _ => outcome *= op.get_num_value(1f64),
            }
        }

        *result_value = ValueType::Number(outcome);
    }
    
    pub fn power(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_num_value(0f64),
                _ => outcome = outcome.powf(op.get_num_value(1f64)),
            }
        }

        *result_value = ValueType::Number(outcome);
    }
    
    pub fn minus(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_num_value(0f64),
                _ => outcome -= op.get_num_value(0f64),
            }
        }

        *result_value = ValueType::Number(outcome);
    }
    
    pub fn divide(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_num_value(0f64),
                _ => outcome /= op.get_num_value(1f64),
            }
        }

        *result_value = ValueType::Number(outcome);
    }
    
    pub fn modulo(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_num_value(0f64),
                _ => outcome %= op.get_num_value(1f64),
            }
        }

        *result_value = ValueType::Number(outcome);
    }

    pub fn combine(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        *result_value = match operands.last() {
            Some(e) => e.get_value(),
            None => ValueType::Number(0f64),
        };
    }
    
    pub fn min(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = f64::MAX;
        let mut current: f64;

        for op in operands {
            current = op.get_num_value(f64::MAX);

            if outcome > current {
                outcome = current;
            }
        }

        *result_value = ValueType::Number(outcome);
    }
    
    pub fn max(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = f64::MIN;
        let mut current: f64;

        for op in operands {
            current = op.get_num_value(f64::MIN);

            if outcome < current {
                outcome = current;
            }
        }

        *result_value = ValueType::Number(outcome);
    }

    pub fn unaryminus(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        *result_value = match operands.first() {
            None => ValueType::Number(0f64),
            Some(e) => ValueType::Number(- e.get_num_value(0f64)),
        };
    }

    pub fn intgr(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        *result_value = match operands.first() {
            None => ValueType::Number(0f64),
            Some(e) => ValueType::Number(e.get_num_value(0f64).trunc()),
        };
    }

    pub fn abs(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        *result_value = match operands.first() {
            None => ValueType::Number(0f64),
            Some(e) => ValueType::Number(e.get_num_value(0f64).abs()),
        };
    }

    pub fn assign_number_register(result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) {
        if operands.is_empty() {
            *result_value = ValueType::Number(0f64);
            return;
        }

        let mut index = operands[0].get_num_value(0f64) as i64;
        let mut reg_val = ValueType::Number(0f64);

        for opd in &operands[1..operands.len()] {
            reg_val = opd.get_value();
            shuttle.nums.insert(index, reg_val.clone());
            index += 1;
        }

        *result_value = reg_val;
    }

    pub fn get_number_register(result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let index = if operands.is_empty() {
            0f64
        } else {
            operands[0].get_num_value(0f64)
        } as i64;

        let found = match shuttle.nums.get(&index) {
            None => ValueType::Number(0f64),
            Some(v) => v.clone(),
        };

        *result_value = found;
    }

    pub fn assignment_maker(result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let index = if !operands.is_empty() {
            operands[0].get_num_value(0f64)
        } else {
            0f64
        } as i64;

        let stack_len = shuttle.assignment_indexes_stack.len();

        // Add the variable's index to the list of variables to be assigned to
        // for the operator that's the parent of the : operator, if any.
        if stack_len >= 2 {
            shuttle
                .assignment_indexes_stack
                [stack_len - 2]
                .push(index);
        }

        let found = match shuttle.nums.get(&index) {
            None => ValueType::Number(0f64),
            Some(v) => v.clone(),
        };

        *result_value = found;
    }
    
    pub fn equals(result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let mut are_equal = true;
        let mut first = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => first = op.get_num_value(0f64),
                _ => are_equal = are_near(first, op.get_num_value(0f64), shuttle.orb),
            }
        }

        *result_value = ValueType::Number(if are_equal {
            1f64
        } else {
            0f64
        });
    }
    
    pub fn less(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = true;
        let mut first = 0f64;
        let mut second: f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => first = op.get_num_value(0f64),
                _ => {
                    second = op.get_num_value(0f64);
                    outcome = outcome && (first < second);
                    first = second;
                },
            }
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });
    }
    
    pub fn greater(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut outcome = true;
        let mut first = 0f64;
        let mut second: f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => first = op.get_num_value(0f64),
                _ => {
                    second = op.get_num_value(0f64);
                    outcome = outcome && (first > second);
                    first = second;
                },
            }
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });
    }

    pub fn not(result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let mut outcome = true;

        for op in operands {
            outcome &= are_near(0f64, op.get_num_value(0f64), shuttle.orb);
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });
    }

    pub fn and(result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let mut outcome = true;

        for op in operands {
            outcome &= !are_near(0f64, op.get_num_value(0f64), shuttle.orb);
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });
    }

    pub fn or(result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let mut outcome = false;

        for op in operands {
            outcome |= !are_near(0f64, op.get_num_value(0f64), shuttle.orb);
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });
    }

    pub fn xor(result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let mut trues = 0usize;

        for op in operands {
              if !are_near(0f64, op.get_num_value(0f64), shuttle.orb) {
                  trues += 1;
              }
        }

        *result_value = ValueType::Number(if trues == 1 {
            1f64
        } else {
            0f64
        });
    }
    
    pub fn setting(result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let mut setting_nr = -1i64;
        let mut setting_value = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => setting_nr = op.get_num_value(-1f64) as i64,
                1 => setting_value = op.get_num_value(0f64),
                _ => (),
            }
        }

        match setting_nr {
            0i64 => shuttle.orb = setting_value,
            1i64 => shuttle.max_iterations = setting_value,
            _ => (),
        }

        *result_value = ValueType::Number(setting_value);
    }

    pub fn enumerated_opr(result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let default_outcome = ValueType::Number(0f64);

        if operands.is_empty() {
            *result_value = default_outcome;

            return;
        }

        let opr_func = match operands[0].get_num_value(-1f64) {
            0f64 => enum_opr_sign,
            _ =>  {
                *result_value = default_outcome;

                return;
            },
        };

        (opr_func)(result_value, &mut operands[1..], shuttle);
    }

    pub fn enum_opr_sign(result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) {
        let mut all_positive = true;
        let mut all_negative = true;

        let mut opd_val: f64;

        for opd in operands {
            opd_val = opd.get_num_value(0f64);

            all_positive = all_positive && (opd_val > 0f64);
            all_negative = all_negative && (opd_val < 0f64);
        }

        *result_value = ValueType::Number(
            if all_positive && all_negative {
                0f64
            } else if all_positive {
                1f64
            } else if all_negative {
                -1f64
            } else {
                0f64
            }
        );
    }
    
    pub fn exec_while(result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) {
        if operands.is_empty() {
            *result_value = ValueType::Number(0f64);

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

                        if are_near(0f64, op.get_num_value(0f64), shuttle.orb) {
                                    break 'outer;
                        }
                    },
                    _ => {
                        op.operate(shuttle);
                        outcome = op.get_num_value(0f64);
                    },
                }

                op_count += 1;
            }
        }

        *result_value = ValueType::Number(outcome);
    }
    
    pub fn exec_for(result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let mut outcome = 0f64;
        let mut counter_val = 0f64;
        let mut end_val = 0f64;
        let mut increment = 1f64;
        let mut counter_var = 0i64;
        let mut iter_count = 0f64;

        if operands.len() < 5 {
            *result_value = ValueType::Number(outcome);

            return;
        }

        for (op_count, op) in operands.iter_mut().enumerate() {
            if op_count <= 3 {
                op.operate(shuttle);
            }

            match op_count {
                0 => counter_val = op.get_num_value(0f64),
                1 => end_val = op.get_num_value(0f64),
                2 => increment = op.get_num_value(1f64),
                3 => counter_var = op.get_num_value(0f64) as i64,
                _ => (),
            }
        }

        let ascending = counter_val <= end_val;
        let op_len = operands.len();
        let mut op_count: usize;
        let mut op: &mut Expression;

        loop {
            iter_count += 1f64;

            if (shuttle.max_iterations > 0f64) && (iter_count > shuttle.max_iterations) {
                break;
            }

            if  (ascending && (counter_val > end_val)) ||
                ((!ascending) && (counter_val < end_val)) {
                    break;
            }

            shuttle.nums.insert(counter_var, ValueType::Number(counter_val));
            op_count = 4;

            while op_count < op_len {
                op = &mut operands[op_count];
                op.operate(shuttle);

                if op_count + 1 == op_len {
                    outcome = op.get_num_value(0f64);
                }

                op_count += 1;
            }

            counter_val += increment;
        }

        *result_value = ValueType::Number(outcome);
    }

    pub fn exec_if(result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) {
        let mut outcome = 0f64;
        let mut use_second = true;

        for op_tuple in operands.iter_mut().enumerate() {
            match op_tuple {
                (0, op) => {
                    op.operate(shuttle);
                    use_second = !are_near(0f64, op.get_num_value(0f64), shuttle.orb);
                },
                (1, op) if use_second => {
                    op.operate(shuttle);
                    outcome = op.get_num_value(0f64);
                },
                (1, _) => (),
                (_, op) if !use_second => {
                    op.operate(shuttle);
                    outcome = op.get_num_value(0f64);
                }
                _ => (),
            }
        }

        *result_value = ValueType::Number(outcome);
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
            assert_eq!(47.11f64, exp.get_num_value(0f64));

            // Check if we can get the value a second time.
            assert_eq!(47.11f64, exp.get_num_value(0f64));
        }

        #[test]
        fn expr_add_numbers() {
            let mut exp = Expression::new('+');
            exp.push_operand(Expression::new_number(4000.3f64));
            exp.push_operand(Expression::new_number( 500.1f64));

            let mut shuttle = Shuttle::new();
            exp.operate(&mut shuttle);

            // Fails due to precision error: right value is 4500.400000000001.
            // assert_eq!(4500.4f64, exp.get_num_value(0f64));

            assert!(are_very_near(4500.4f64, exp.get_num_value(0f64)));
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

            assert_eq!(574.03f64, exp.get_num_value(0f64));
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
                Some(n) => {
                    match n {
                        ValueType::Number(num) => assert_eq!(500.1f64, *num),
                        _ => panic!("The value found in shuttle.nums should be ValueType::Number."),
                    }
                },
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
                Some(n) => {
                    match n {
                        ValueType::Number(num) => assert_eq!(500.1f64, *num),
                        _ => panic!("The value found in shuttle.nums should be ValueType::Number."),
                    }
                },
            }
        }

        #[test]
        fn expr_assign_num_reg_neg_dot3() {
            let mut exp = Expression::new('$');
            exp.push_operand(Expression::new_number(-4000.3f64));
            exp.push_operand(Expression::new_number(500.1f64));

            let mut shuttle = Shuttle::new();
            exp.operate(&mut shuttle);

            match shuttle.nums.get(&-4000i64) {
                None => panic!("The numerical register assigned to has not been found back."),
                Some(n) => {
                    match n {
                        ValueType::Number(num) => assert_eq!(500.1f64, *num),
                        _ => panic!("The value found in shuttle.nums should be ValueType::Number."),
                    }
                },
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
                Some(n) => {
                    match n {
                        ValueType::Number(num) => assert_eq!(500.1f64, *num),
                        _ => panic!("The value found in shuttle.nums should be ValueType::Number."),
                    }
                },
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
        use crate::{Expression, Shuttle, ValueType};
        use crate::opr_funcs::*;

        #[test]
        fn nop_doesnt_change_value() {
            let mut the_value = ValueType::Number(500f64);
            let mut ops = Vec::<Expression>::new();
            let mut shuttle = Shuttle::new();

            nop(&mut the_value, &mut ops, &mut shuttle);

            assert!(ops.is_empty());

            match the_value {
                ValueType::Number(n) => assert_eq!(500f64, n),
                _ => panic!("A ValueType::Number was expected, other variant was found."),
            }
        }

        #[test]
        fn op_add() {
            let mut the_value = ValueType::Empty;
            let mut ops = vec![Expression::new_number(12f64), Expression::new_number(68f64)];
            let mut shuttle = Shuttle::new();

            add(&mut the_value, &mut ops, &mut shuttle);

            // Check the value.
            match the_value {
                ValueType::Number(n) => assert_eq!(80f64, n),
                _ => panic!("A ValueType::Number was expected, other variant was found."),
            }

            // Verify that the operands didn't change.
            assert_eq!(12f64, ops[0].get_num_value(0f64));
            assert_eq!(68f64, ops[1].get_num_value(0f64));
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
        fn x_override_with_operator_operands() {
            assert_eq!(Ok(13f64), Interpreter::execute("$0 10?(=%8 6 2 +:0 3 +:0 4 57)v0".to_string()));
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
            assert_eq!(Ok(52f64), Interpreter::execute("+-50)2".to_string()));
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
        fn x_power_0_op() {
            assert_eq!(Ok(0f64), Interpreter::execute("^".to_string()));
        }

        #[test]
        fn x_power_1_op() {
            assert_eq!(Ok(49f64), Interpreter::execute("^49".to_string()));
        }

        #[test]
        fn x_power_2_op_int_int() {
            assert_eq!(Ok(36f64), Interpreter::execute("^6 2".to_string()));
        }

        #[test]
        fn x_power_2_op_int_fract() {
            assert_eq!(Ok(7f64), Interpreter::execute("^49 .5".to_string()));
        }

        #[test]
        fn x_power_2_op_int_fract_neg() {
            assert_eq!(Ok(0.2f64), Interpreter::execute("^25 ~.5".to_string()));
        }

        #[test]
        fn x_power_2_op_int_neg() {
            assert_eq!(Ok(0.2f64), Interpreter::execute("^5 ~1".to_string()));
        }

        #[test]
        fn x_power_2_op_fract_fract() {
            assert_eq!(Ok(4.049691346263317f64), Interpreter::execute("^16.4 .5".to_string()));
        }

        #[test]
        fn x_power_3() {
            assert_eq!(Ok(49f64), Interpreter::execute("^(49 .5 2)".to_string()));
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
        fn x_int_int() {
            assert_eq!(Ok(5f64), Interpreter::execute("i5".to_string()));
        }

        #[test]
        fn x_int_fract() {
            assert_eq!(Ok(5f64), Interpreter::execute("i5.7".to_string()));
        }

        #[test]
        fn x_int_neg_int() {
            assert_eq!(Ok(-5f64), Interpreter::execute("i~5".to_string()));
        }

        #[test]
        fn x_int_neg_fract() {
            assert_eq!(Ok(-5f64), Interpreter::execute("i~5.8".to_string()));
        }

        #[test]
        fn x_abs_pos() {
            assert_eq!(Ok(5f64), Interpreter::execute("a5".to_string()));
        }

        #[test]
        fn x_abs_neg() {
            assert_eq!(Ok(5f64), Interpreter::execute("a~5".to_string()));
        }

        #[test]
        fn x_min_two_first() {
            assert_eq!(Ok(128f64), Interpreter::execute("m128 277".to_string()));
        }

        #[test]
        fn x_min_two_second() {
            assert_eq!(Ok(277f64), Interpreter::execute("m328 277".to_string()));
        }

        #[test]
        fn x_min_two_equal() {
            assert_eq!(Ok(128f64), Interpreter::execute("m128 128".to_string()));
        }

        #[test]
        fn x_min_more() {
            assert_eq!(Ok(-31f64), Interpreter::execute("m(128 277 ~31 5)".to_string()));
        }

        #[test]
        fn x_max_two_first() {
            assert_eq!(Ok(128f64), Interpreter::execute("M128 27".to_string()));
        }

        #[test]
        fn x_max_two_second() {
            assert_eq!(Ok(277f64), Interpreter::execute("M28 277".to_string()));
        }

        #[test]
        fn x_max_two_equal() {
            assert_eq!(Ok(128f64), Interpreter::execute("M128 128".to_string()));
        }

        #[test]
        fn x_max_more() {
            assert_eq!(Ok(366f64), Interpreter::execute("M(128 277 ~31 5 366)".to_string()));
        }

        #[test]
        fn x_assign_num_return_value() {
            assert_eq!(Ok(111f64), Interpreter::execute("$4 111".to_string()));
        }

        #[test]
        fn x_assign_num_serial_assignation() {
            assert_eq!(Ok(10f64), Interpreter::execute("$(4 1 2 3 4) F4 7 1 0 +:1 vv0 v1 ".to_string()));
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
        fn x_assignment_maker_inside_override_markers_first() {
            assert_eq!(Ok(35f64), Interpreter::execute("$1 10 $2 20 +(:1 v2 5) v1".to_string()));
        }

        #[test]
        fn x_assignment_maker_before_override_markers_first() {
            assert_eq!(Ok(35f64), Interpreter::execute("$1 10 $2 20 +:(1 v2 5) v1".to_string()));
        }

        #[test]
        fn x_assignment_maker_inside_override_markers_second() {
            assert_eq!(Ok(35f64), Interpreter::execute("$1 10 $2 20 +(v1 :2 5) v2".to_string()));
        }

        #[test]
        fn x_assignment_maker_inside_override_markers_both() {
            assert_eq!(Ok(70f64), Interpreter::execute("$1 10 $2 20 +(:1 :2 5) +v1 v2".to_string()));
        }

        #[test]
        fn x_assignment_maker_before_and_inside_override_markers_both() {
            assert_eq!(Ok(70f64), Interpreter::execute("$1 10 $2 20 +:(1 :2 5) +v1 v2".to_string()));
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
        fn x_equality_many() {
            assert_eq!(Ok(1f64), Interpreter::execute("$1 -15 3 =(12 +7 5 *3 4 /36 3 v1)".to_string()));
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
        fn x_and_0() {
            assert_eq!(Ok(1f64), Interpreter::execute("&".to_string()));
        }

        #[test]
        fn x_and_1_true() {
            assert_eq!(Ok(1f64), Interpreter::execute("& /7 2".to_string()));
        }

        #[test]
        fn x_and_1_false() {
            assert_eq!(Ok(0f64), Interpreter::execute("& -5 5".to_string()));
        }

        #[test]
        fn x_and_2_true() {
            assert_eq!(Ok(1f64), Interpreter::execute("& 2 /7 2".to_string()));
        }

        #[test]
        fn x_and_2_false() {
            assert_eq!(Ok(0f64), Interpreter::execute("& 2 -5 5".to_string()));
        }

        #[test]
        fn x_and_3_true() {
            assert_eq!(Ok(1f64), Interpreter::execute("&(2 /7 2 %14 4)".to_string()));
        }

        #[test]
        fn x_and_3_false() {
            assert_eq!(Ok(0f64), Interpreter::execute("&( 2 -5 5 9)".to_string()));
        }

        #[test]
        fn x_or_0() {
            assert_eq!(Ok(0f64), Interpreter::execute("|".to_string()));
        }

        #[test]
        fn x_or_1_true() {
            assert_eq!(Ok(1f64), Interpreter::execute("| /7 2".to_string()));
        }

        #[test]
        fn x_or_1_false() {
            assert_eq!(Ok(0f64), Interpreter::execute("| -5 5".to_string()));
        }

        #[test]
        fn x_or_2_true() {
            assert_eq!(Ok(1f64), Interpreter::execute("| 0 /7 2".to_string()));
        }

        #[test]
        fn x_or_2_false() {
            assert_eq!(Ok(0f64), Interpreter::execute("| 0 -5 5".to_string()));
        }

        #[test]
        fn x_or_3_true() {
            assert_eq!(Ok(1f64), Interpreter::execute("|(0 +3 ~2  %14 4)".to_string()));
        }

        #[test]
        fn x_or_3_false() {
            assert_eq!(Ok(0f64), Interpreter::execute("|( 0 -5 5 %20 5)".to_string()));
        }

        #[test]
        fn x_xor_0() {
            assert_eq!(Ok(0f64), Interpreter::execute("x".to_string()));
        }

        #[test]
        fn x_xor_1_true() {
            assert_eq!(Ok(1f64), Interpreter::execute("x /7 2".to_string()));
        }

        #[test]
        fn x_xor_1_false() {
            assert_eq!(Ok(0f64), Interpreter::execute("x -5 5".to_string()));
        }

        #[test]
        fn x_xor_2_true() {
            assert_eq!(Ok(1f64), Interpreter::execute("x 0 /7 2".to_string()));
        }

        #[test]
        fn x_xor_2_false_both_true() {
            assert_eq!(Ok(0f64), Interpreter::execute("x 8 5".to_string()));
        }

        #[test]
        fn x_xor_2_false_both_false() {
            assert_eq!(Ok(0f64), Interpreter::execute("x 0 -5 5".to_string()));
        }

        #[test]
        fn x_xor_3_true() {
            assert_eq!(Ok(1f64), Interpreter::execute("x(0 +3 ~3  %14 4)".to_string()));
        }

        #[test]
        fn x_xor_3_false_2_true() {
            assert_eq!(Ok(0f64), Interpreter::execute("x(0 -5 7 %20 7)".to_string()));
        }

        #[test]
        fn x_xor_3_false_3_true() {
            assert_eq!(Ok(0f64), Interpreter::execute("x(1 -5 7 %20 5)".to_string()));
        }

        #[test]
        fn x_xor_3_false_0_true() {
            assert_eq!(Ok(0f64), Interpreter::execute("x(0 -5 5 %20 5)".to_string()));
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

        #[test]
        fn x_for_4_op_asc() {
            assert_eq!(Ok(0f64), Interpreter::execute("F(3 11 2 1)".to_string()));
        }

        #[test]
        fn x_for_5_op_asc() {
            assert_eq!(Ok(10395f64), Interpreter::execute("$0 1 F3 11 2 1 *:0 v1 v0".to_string()));
        }

        #[test]
        fn x_for_5_op_desc() {
            assert_eq!(Ok(10395f64), Interpreter::execute("$0 1 F11 3 ~2 1 *:0 v1 v0".to_string()));
        }

        #[test]
        fn x_for_6_op() {
            assert_eq!(Ok(10395f64), Interpreter::execute("$0 1 F(11 3 ~2 1 *:0 v1 $5 v1) v0".to_string()));
        }

        #[test]
        fn x_for_5_op_exceeds_limit() {
            assert_eq!(Ok(15f64), Interpreter::execute("Z1 2 $0 1 F3 11 2 1 *:0 v1 v0".to_string()));
        }

        #[test]
        fn x_if_0_op() {
            assert_eq!(Ok(0f64), Interpreter::execute("?".to_string()));
        }

        #[test]
        fn x_if_1_op() {
            assert_eq!(Ok(0f64), Interpreter::execute("? 19".to_string()));
        }

        #[test]
        fn x_if_second() {
            assert_eq!(Ok(8f64), Interpreter::execute("?*70 .2 8 2".to_string()));
        }

        #[test]
        fn x_if_third() {
            assert_eq!(Ok(2f64), Interpreter::execute("?-70 70 8 2".to_string()));
        }

        #[test]
        fn x_if_5_op_second() {
            assert_eq!(Ok(4f64), Interpreter::execute("?(19 +3 1 0 7 23)".to_string()));
        }

        #[test]
        fn x_if_5_op_last() {
            assert_eq!(Ok(23f64), Interpreter::execute("$5 0 ?(v5 +3 1 0 7 23)".to_string()));
        }

        #[test]
        fn x_enum_opr_unknown() {
            assert_eq!(Ok(0f64), Interpreter::execute("o1.5 2".to_string()));
        }

        #[test]
        fn x_enum_opr_sign_no_operands() {
            assert_eq!(Ok(0f64), Interpreter::execute("o(0)".to_string()));
        }

        #[test]
        fn x_enum_opr_sign_all_pos() {
            assert_eq!(Ok(1f64), Interpreter::execute("o(0 45 7 99 4022)".to_string()));
        }

        #[test]
        fn x_enum_opr_sign_all_neg() {
            assert_eq!(Ok(-1f64), Interpreter::execute("o(0 ~45 ~7 ~99 ~4022)".to_string()));
        }

        #[test]
        fn x_enum_opr_sign_mixed() {
            assert_eq!(Ok(0f64), Interpreter::execute("o(0 ~45 7 ~99 4022)".to_string()));
        }

        #[test]
        fn x_enum_opr_sign_assign() {
            assert_eq!(Ok(-1f64), Interpreter::execute("$~22 ~4 o0 :~22 v~22".to_string()));
        }

        #[test]
        fn x_enum_opr_override() {
            assert_eq!(Ok(1f64), Interpreter::execute("O(0 55)".to_string()));
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
