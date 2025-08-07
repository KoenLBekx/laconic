#![doc = include_str!("../README.md")]

// The braces after the first comment slashes make Vim fold these comments also.
//{ TODOs
// TODO: resolve TODO's in code.
// TODO: comment all public entities.
// TODO: the characters for the operators should be hard-coded only once: in constants.
//      (done; not done follows below:)
//      These constants can figure in a constant array of tuples
//      holding also other data like number of operands, operator function, e.a.
//      (even data for documentation).
//      The function isKnownOperator should also use this constant array.
// TODO: make private whatever can remain private.
// TODO: include the explanations in analysis/laconic.txt in markdown format as documentation
//      comments.
// TODO: The ScriptError variants that carry an operator mark as char should carry a string
//      so the full enumerated operator name can be passed, e.g.: "o,§dow".
// TODO: check using cargo clippy.
// TODO: use some other numeric type that supports very large numbers with higher precision instead of f64 - see crates.io (dashu?)
//}

//{ uses
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::io::stdout;
use string_io_and_mock::{FileTextHandler, MockTextHandler, TextIOHandler};
//}

// Static lifetime: justified, because the functions referenced
// are compiled into the application and live as long as it runs.
// The first parameter should pass a mutable reference to the character that encoded the expression,
// The second one to an Expression's value property,
// the third one to a Expression's operands property (operands are Expression objects),
// the fourth one to the Shuttle containing other state.
type OperatorFunc = &'static dyn Fn(&mut char, &mut ValueType, &mut [Expression], &mut Shuttle) -> Result<(), ScriptError>;

// --------------------------- Operator constants {
const OPNUM: char = '0';
const OPSTR: char = '§';
const OPNEG: char = '~';
const OPPLS: char = '+';
const OPMNS: char = '-';
const OPMUL: char = '*';
const OPDIV: char = '/';
const OPMOD: char = '%';
const OPEXP: char = '^';
const OPLOG: char = 'l';
const OPINT: char = 'i';
const OPABS: char = 'a';
const OPDEG: char = '°';
const OPSIN: char = 'S';
const OPCOS: char = 'C';
const OPTAN: char = 'T';
const OPAT2: char = 'A';
const OPPII: char = 'p';
const OPEUL: char = 'e';
const OPCON: char = 'c';
const OPCMB: char = ';';
const OPMIN: char = 'm';
const OPMAX: char = 'M';
const OPNRO: char = 'N';
const OPWHI: char = 'W';
const OPFOR: char = 'F';
const OPBRK: char = 'B';
const OPRTN: char = 'R';
const OPEXR: char = 'X';
const OPASG: char = '$';
const OPVAR: char = 'v';
const OPMAS: char = ':';
const OPSTK: char = 'K';
const OPPOP: char = 'k';
const OPEQU: char = '=';
const OPLSS: char = '<';
const OPGRT: char = '>';
const OPNOT: char = '!';
const OPAND: char = '&';
const OPOR_: char = '|';
const OPXOR: char = 'x';
const OPIF_: char = '?';
const OPSET: char = 'Z';
const OPOP1: char = 'o';
const OPOP2: char = 'O';
const OPWRT: char = 'w';
const OPREA: char = 'r';
const OP2NR: char = 'n';
const OPSIG: char = 's';
const OPTYP: char = 't';
const OPBAS: char = 'b';
const OPEMP: char = '€';
const OPEVL: char = 'E';
const OPNLN: char = '¶';
const OPALT: char = ',';
const OPUER: char = 'U';
const OPVAL: char = 'V';
const OPQUO: char = 'q';

// CH_SEPA_NUM can't be used as match pattern.
const CH_NUM_SEPA: char = '_';

const CH_DOT: char = '.';
const CH_SEPA_VERSION: char = '.';
const CH_THOUSANDS: char = ',';
const CH_SEPA_DIGITS: char = ' ';
const CH_OVR_START: char = '(';
const CH_OVR_END: char = ')';
const CH_UNKNOWN_CHAR_CODE: char = '?';
const CH_CLAUSE_START: char = '[';
const CH_CLAUSE_END: char = ']';

const MARK_STRING: char = 's';
const MARK_COMMENT: char = 'c';
const MARK_NUMBER: char = 'n';
// ---------------------------------- }

#[derive(PartialEq)]
enum Atom {
    Operator(char),
    Number(String),
    String(String),
    Comment(String),
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Operator(c) => write!(f, "{c}"),
            Atom::Number(n) => write!(f, "{n} "),
            Atom::String(_) => write!(f, "[s...]"),
            Atom::Comment(_) => write!(f, "[c...]"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum ScriptError {
    ConflictingNumberPartSeparators,
    DigitParsingFailure{position: usize, reason: String},
    DivideByZero(char),
    EmptyOperand(char),
    FileReadFailure{path: String, reason: String},
    FindStringLongerThanSourceString(char),
    InsufficientOperands(char),
    InvalidDatePart(char),
    InvalidDateSequenceNumber(String),
    InvalidNumberBase(f64),
    InvalidOperandMax(char),
    InvalidVersionPart(f64),
    LogarithmOfZeroOrNegativeNumberIsNotSupported,
    NonIntegerPowerOfNegativeNumberIsNotSupported,
    NonNumericOperand(char),
    NonTextOperand(char),
    NumberParsingFailure(String),
    PositionPastLastPossible(usize),
    ReadingUninitializedVariable(String),
    UnclosedBracketsAtEnd,
    UnexpectedClosingBracket{position: usize},
    UnexpectedClosingParenthesis,
    UnexpectedNegativeOperand(char),
    UnknownBracketContentTypeMarker{position: usize, marker: char},
    UnknownConstant(String),
    UnknownNamedOperator(String),
    UnknownOperator{position: usize, operator: char},
    UnknownRoutine(String),
    UserDefinedError(String),
    WriteFailure(String),
    ZeroOrNegativeLogarithmBaseIsNotSupported,
}

#[derive(Clone, Debug, Default, PartialOrd)]
enum ValueType {
    #[default]
    Empty,
    Number(f64),
    Text(String),
    Error(ScriptError),

    /// Only to be used to serve functionality like opr_funcs::min.
    Max,
}

impl ValueType {
    fn get_type_as_num(&self) -> f64 {
        match self {
            ValueType::Empty => 0_f64,
            ValueType::Number(_) => 1_f64,
            ValueType::Text(_) => 2_f64,
            ValueType::Error(_) => 90_f64,
            ValueType::Max => 99_f64,
        }
    }

    pub fn get_num_value(&self, default: f64) -> f64 {
        match self {
            ValueType::Number(num) => *num,
            _ => default,
        }
    }

    pub fn get_string_value(&self, default: String) -> String {
        match self {
            ValueType::Text(txt) => txt.clone(),
            ValueType::Number(num) => format!("{num}"),
            ValueType::Error(script_err) => format!("{script_err:?}"), 
            ValueType::Max => "ValueType::Max".to_string(),
            ValueType::Empty => default,
        }
    }

    /*
    pub fn push_str(&mut self, pushed: &str) {
        let opt_text = match self {
            ValueType::Text(txt) => Some(txt.clone()),
            _ => None,
        };

        if let Some(mut txt) = opt_text {
            txt.push_str(pushed);
            *self = ValueType::Text(txt);
        }
    }
    */
}

impl PartialEq for ValueType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ValueType::Empty, ValueType::Empty) => true,
            (ValueType::Number(s), ValueType::Number(o)) => s == o,
            (ValueType::Text(s), ValueType::Text(o)) => *s == *o,
            (ValueType::Error(s), ValueType::Error(o)) => *s == *o,
            (ValueType::Max, ValueType::Max) => true,
            _ => false,
        }
    }
}

impl Eq for ValueType {}

impl Hash for ValueType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ValueType::Empty => 0u8.hash(state),
            ValueType::Text(s) => s.hash(state),
            ValueType::Number(f) => f.to_be_bytes().hash(state),
            ValueType::Error(_) => self.get_string_value("Error".to_string()).hash(state),
            ValueType::Max => 99u8.hash(state),
        };
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NumberFormat {
    base: f64,
    fractal_digits: f64,
    fractal_separator: char,
    use_thousands_separator: bool,
    thousands_separator: char,
    digit_separator: char,
}

impl NumberFormat {
    const THOUSANDS_GROUP: f64 = 3f64;

    fn new() -> Self {
        NumberFormat {
            base: 10f64,
            fractal_digits: 6f64,
            fractal_separator: CH_DOT,
            use_thousands_separator: false,
            thousands_separator: CH_THOUSANDS,
            digit_separator: CH_SEPA_DIGITS,
        }
    }

    fn format_digit(&self, digit: f64) -> String {
        let val_0 = '0' as u32;
        let diff_a = ('A' as u32) - val_0;

        match self.base {
            2f64..=36f64 => {
                let mut char_val = (digit as u32) + val_0;

                if digit > 9f64 {
                    char_val = char_val + diff_a - 10;
                }

                // No panic!, no expect => if Err, return '?'
                String::from(char::from_u32(char_val).unwrap_or(CH_UNKNOWN_CHAR_CODE))
            },
            37f64.. => {
                format!("{}", digit.trunc())
            },
            _ => "ERR".to_string(),
        }
    }

    pub fn format(&self, numr: f64) -> String {
        match numr {
            n if n.is_nan() => "NaN".to_string(),
            f64::INFINITY => "inf".to_string(),
            f64::NEG_INFINITY => "-inf".to_string(),
            num => {
                let is_positive = num >= 0f64;
                let nr = num.abs();

                let mut int = nr.trunc();

                let round_factor = self.base.powf(self.fractal_digits);
                let mut fract = nr.fract();
                fract = (fract * round_factor).round() / round_factor;

                if fract >= 1f64 {
                    int += 1f64;
                    fract = fract.fract();
                }

                let mut int_text = String::new();
                let mut fract_text = String::new();
                let mut modulus: f64;
                let digit_separator = if self.base > 36f64 { String::from(self.digit_separator) } else { String::new() };
                let mut digit_count: f64;
                let mut mod_digit: String;
                let mut add_thousands_separator: bool;
                let mut thsepa: String;

                digit_count = 0f64;

                while int >= 1f64 {
                    digit_count += 1f64;
                    modulus = int % self.base;
                    mod_digit = self.format_digit(modulus);

                    add_thousands_separator = 
                        self.use_thousands_separator &&
                        (digit_count > 1f64) &&
                        (digit_count % Self::THOUSANDS_GROUP == 1f64) &&
                        (self.base <= 36f64);

                    thsepa = if add_thousands_separator {
                        self.thousands_separator.to_string()
                    } else {
                        String::new()
                    };

                    int_text = format!("{digit_separator}{mod_digit}{thsepa}{int_text}");
                    int -= modulus;
                    int /= self.base;
                }

                if int_text.is_empty() {
                    int_text = "0".to_string();
                } else if self.base > 36f64 {
                    int_text = int_text.trim().to_string();
                }

                digit_count = 0f64;

                while digit_count < self.fractal_digits {
                    digit_count += 1f64;
                    fract *= self.base;

                    add_thousands_separator = 
                        self.use_thousands_separator &&
                        (digit_count > 1f64) &&
                        (digit_count % Self::THOUSANDS_GROUP == 1f64) &&
                        (self.base <= 36f64);

                    thsepa = if add_thousands_separator {
                        self.thousands_separator.to_string()
                    } else {
                        String::new()
                    };

                    fract_text = format!("{}{}{}{}", fract_text, thsepa, self.format_digit(fract.trunc()), digit_separator);
                    fract = fract.fract();
                }

                if !fract_text.is_empty() {
                    fract_text = format!("{}{}", self.fractal_separator, fract_text);
                }

                let sign_text = if is_positive {
                    String::new()
                } else {
                    "-".to_string()
                };

                format!(
                    "{}{}{}",
                    sign_text,
                    int_text,
                    fract_text.trim()
                )
            }
        }
    }

    fn set_base(&mut self, base: f64) {
        if base >= 2f64 {
            self.base = base.trunc();
        } else {
            self.base = 10f64;
        }
    }    

    fn set_fractal_digits(&mut self, fractal_digits: f64) {
        self.fractal_digits = fractal_digits.abs().trunc();
    }

    fn set_fractal_separator(&mut self, separator: String) {
        if !separator.is_empty() {
            let proposed = separator.chars().nth(0).unwrap_or(CH_DOT);

            self.fractal_separator = proposed;
        }
    }

    #[cfg(test)]
    fn set_use_thousands_separator(&mut self, use_it: bool) {
        self.use_thousands_separator = use_it;
    }

    fn set_thousands_separator(&mut self, separator: String) {
        if separator.is_empty() {
            self.use_thousands_separator = false;
        } else {
            let proposed = separator.chars().nth(0).unwrap_or(CH_THOUSANDS);

            self.thousands_separator = proposed;
            self.use_thousands_separator = true;
        }
    }
}

#[derive(Clone)]
struct Expression
{
    operator: OperatorFunc,
    operands: Vec<Expression>,
    value: ValueType,
    opr_mark: char,
    is_last_of_override: bool,
    has_overridden_nr_of_ops: bool,
    alternative_marks_count: u8,
}

impl Expression {
    pub fn new(opr_mark: char, alternative_marks_count: u8) -> Self {
        let opr: OperatorFunc = match (opr_mark, alternative_marks_count) {
            (OPNUM, _) => &opr_funcs::nop,
            (OPSTR, _) => &opr_funcs::string_expr,
            (OPNEG, _) => &opr_funcs::unaryminus,
            (OPPLS, 0) => &opr_funcs::add,
            (OPPLS, 1) => &opr_funcs::add_concat_int,
            (OPMNS, _) => &opr_funcs::minus,
            (OPMUL, _) => &opr_funcs::multiply,
            (OPDIV, 0) => &opr_funcs::divide,
            (OPDIV, 1) => &opr_funcs::int_div,
            (OPMOD, _) => &opr_funcs::modulo,
            (OPEXP, _) => &opr_funcs::power,
            (OPLOG, _) => &opr_funcs::log,
            (OPINT, 0) => &opr_funcs::intgr,
            (OPINT, 1) => &opr_funcs::ceiling,
            (OPABS, _) => &opr_funcs::abs,
            (OPDEG, 0) => &opr_funcs::degrees,
            (OPDEG, 1) => &opr_funcs::radians,
            (OPSIN, 0) => &opr_funcs::sine,
            (OPSIN, 1) => &opr_funcs::asin,
            (OPSIN, 2) => &opr_funcs::sinh,
            (OPSIN, 3) => &opr_funcs::asinh,
            (OPCOS, 0) => &opr_funcs::cosine,
            (OPCOS, 1) => &opr_funcs::acos,
            (OPCOS, 2) => &opr_funcs::cosh,
            (OPCOS, 3) => &opr_funcs::acosh,
            (OPTAN, 0) => &opr_funcs::tangent,
            (OPTAN, 1) => &opr_funcs::atan,
            (OPTAN, 2) => &opr_funcs::tanh,
            (OPTAN, 3) => &opr_funcs::atanh,
            (OPAT2, _) => &opr_funcs::atan2,
            (OPPII, _) => &opr_funcs::pi,
            (OPEUL, _) => &opr_funcs::euler_const,
            (OPCON, _) => &opr_funcs::constants,
            (OPCMB, _) => &opr_funcs::combine,
            (OPMIN, _) => &opr_funcs::min,
            (OPMAX, _) => &opr_funcs::max,
            (OPNRO, _) => &opr_funcs::preceding_nr_operands,
            (OPWHI, _) => &opr_funcs::exec_while,
            (OPFOR, _) => &opr_funcs::exec_for,
            (OPBRK, _) => &opr_funcs::set_break,
            (OPRTN, 0) => &opr_funcs::define_routine_with_new_scope,
            (OPRTN, 1) => &opr_funcs::define_routine_sharing_variables,
            (OPEXR, 0) => &opr_funcs::exec_routine,
            (OPEXR, 1) => &opr_funcs::exec_routine_reverse,
            (OPASG, _) => &opr_funcs::assign_var,
            (OPVAR, 0) => &opr_funcs::get_variable,
            (OPVAR, 1) => &opr_funcs::get_variable_or,
            (OPMAS, 0) => &opr_funcs::assignment_maker,
            (OPMAS, 1) => &opr_funcs::assignment_maker_with_default,
            (OPSTK, 0) => &opr_funcs::push_stack,
            (OPSTK, 1) => &opr_funcs::push_stack_reverse,
            (OPSTK, 2) => &opr_funcs::clear_stack,
            (OPPOP, 0) => &opr_funcs::pop_stack,
            (OPPOP, 1) => &opr_funcs::stack_depth,
            (OPEQU, _) => &opr_funcs::equals,
            (OPLSS, _) => &opr_funcs::less,
            (OPGRT, _) => &opr_funcs::greater,
            (OPNOT, _) => &opr_funcs::not,
            (OPAND, _) => &opr_funcs::and,
            (OPOR_, _) => &opr_funcs::or,
            (OPXOR, _) => &opr_funcs::xor,
            (OPIF_, 0) => &opr_funcs::exec_if,
            (OPIF_, 1) => &opr_funcs::exec_try,
            (OPVAL, _) => &opr_funcs::try_value,
            (OPSET, _) => &opr_funcs::setting,
            (OPOP1, _) => &opr_funcs::enumerated_opr,
            (OPOP2, _) => &opr_funcs::enumerated_opr,
            (OPWRT, 0) => &opr_funcs::write,
            (OPWRT, 1) => &opr_funcs::write_file,
            (OPREA, 0) => &opr_funcs::read,
            (OPREA, 1) => &opr_funcs::read_file,
            (OP2NR, _) => &opr_funcs::to_number,
            (OPSIG, _) => &opr_funcs::sign,
            (OPTYP, _) => &opr_funcs::get_type,
            (OPBAS, 0) => &opr_funcs::input_base,
            (OPBAS, 1) => &opr_funcs::output_base,
            (OPEMP, _) => &opr_funcs::empty,
            (OPEVL, _) => &opr_funcs::eval,
            (OPNLN, _) => &opr_funcs::newline,
            (OPUER, _) => &opr_funcs::user_error,
            (OPQUO, 0) => &opr_funcs::quote,
            (OPQUO, 1) => &opr_funcs::quote_trunc,
            _ => &opr_funcs::nop,
        };

        Expression {
            operator: opr,
            operands: Vec::<Expression>::new(),
            value: ValueType::Empty,
            is_last_of_override: false,
            has_overridden_nr_of_ops: false,
            opr_mark,
            alternative_marks_count,
        }
    }

    pub fn new_number(string_representation: String) -> Self {
        Expression {
            operator: &opr_funcs::nop,
            operands: Vec::<Expression>::new(),
            value: ValueType::Text(string_representation),
            opr_mark: OPNUM,
            is_last_of_override: false,
            has_overridden_nr_of_ops: false,
            alternative_marks_count: 0,
        }
    }

    pub fn new_text(txt: String) -> Self {
        Expression {
            operator: &opr_funcs::string_expr,
            operands: Vec::<Expression>::new(),
            value: ValueType::Text(txt),
            opr_mark: OPSTR,
            is_last_of_override: false,
            has_overridden_nr_of_ops: false,
            alternative_marks_count: 0,
        }
    }

    pub fn push_operand(&mut self, op: Expression) {
        self.operands.push(op);
    }

    pub fn get_value(&self) -> ValueType {
        self.value.clone()
    }

    pub fn get_num_value(&self, default: f64) -> f64 {
        self.value.get_num_value(default)
    }

    pub fn get_string_value(&self, default: String) -> String {
        self.value.get_string_value(default)
    }

    pub fn operate(&mut self, shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        /*
        #[cfg(test)]
        println!("operate {}", self.opr_mark);
        */

        shuttle.assignment_indexes_stack.push(Vec::<ValueType>::new());

        let defer_opd_evaluation = "WF?R".contains(self.opr_mark);

        if !defer_opd_evaluation {
            for op in &mut self.operands {
                op.operate(shuttle)?;
            }
        }

        let operator_result = (self.operator)(&mut self.opr_mark, &mut self.value, &mut self.operands, shuttle);
        let assignment_indexes = shuttle.assignment_indexes_stack.pop().unwrap_or_default();

        match operator_result {
            Ok(_) => (),
            Err(script_error) =>  {
                if shuttle.error_breaks {
                    return Err(script_error);
                } else {
                    self.value = ValueType::Error(script_error);
                }
            },
        }

        /*
        #[cfg(test)]
        println!("operate {}, assignment_indexes: {:?}", self.opr_mark, assignment_indexes);
        */

        for index in assignment_indexes {
            // Assign the resulting value to the variable
            // referenced by the first operand and stored by it in the shuttle.
            shuttle.set_var(index, self.get_value());
        }

        if !"FW".contains(self.opr_mark) {
            shuttle.preceding_nr_operands = self.operands.len() as f64;
        }

        /*
        #[cfg(test)]
        println!("operate {} ==> {:?}", self.opr_mark, self.get_value());
        */

        Ok(())
    }

    pub fn get_representation(&self) -> String {
        let mut exp_rep = match self.opr_mark {
            OPNUM => match self.value {
                ValueType::Empty => "empty_num".to_string(),
                ValueType::Number(num) => num.to_string(),

                // ValueType::Text: for numbers in routine definitions.
                ValueType::Text(ref txt) => txt.clone(),
                _ => "(invalid)".to_string(),
            },
            OPSTR => match self.value {
                ValueType::Empty => "no_value".to_string(),
                ValueType::Text(ref txt) => txt.clone(),
                _ => "(invalid)".to_string(),
            },
            oth => oth.to_string(),
        };

        for _ in 0..self.alternative_marks_count {
            exp_rep.push(OPALT);
        }

        if self.has_overridden_nr_of_ops {
            exp_rep.push(CH_OVR_START);
        }

        if self.is_last_of_override {
            exp_rep.push(CH_OVR_END);

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

        if !("0§".contains(self.opr_mark)) {
            // Vim folding fix braces: {{
            exp_rep.push_str("\n\u{0_2514}\u{0_2500}> ");

            let val_rep = match self.value {
                ValueType::Empty => "no_value".to_string(),
                ValueType::Number(num) => num.to_string(),
                ValueType::Text(ref txt) => txt.clone(),
                _ => "(invalid)".to_string(),
            };

            exp_rep.push_str(val_rep.as_str());
        }

        exp_rep
    }
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let representation = match self.opr_mark  {
            OPNUM => format!("{}", self.get_num_value(0f64)),
            OPSTR => self.get_string_value("(no_value)".to_string()),
            _   => String::new(),
        };

        let out_string = format!("{}{}", self.opr_mark, representation);
        write!(f, "{out_string}")
    }
}

#[derive(Clone)]
struct Routine {
    body: Expression,
    in_new_variables_scope: bool,
}

impl Routine {
    fn get_representation(&self, name: &ValueType) -> String {
        format!(
            "Routine {}, running using {} scope:\n{}",
            name.get_string_value("(No name)".to_string()),
            if self.in_new_variables_scope {"isolated"} else {"shared"},
            self.body.get_representation()
        )
    }
}

enum DateInfoItem {
    Year,
    Month,
    Day,
}

#[derive(Clone)]
struct GregorianDateInfo {
    sequence_nr: f64,
    year: f64,
    month: f64,
    day: f64,
}

impl GregorianDateInfo {
    fn get_info(&self, item: &DateInfoItem) -> f64 {
        match item {
            DateInfoItem::Year => self.year,
            DateInfoItem::Month => self.month,
            DateInfoItem::Day => self.day,
        }
    }
}

// Shuttle objects are used to be passed to every operator function
// to provide state other than the operands.
struct Shuttle {
    nums: Vec<HashMap<ValueType, ValueType>>,
    stack: Vec<ValueType>,
    routines: HashMap<ValueType, Routine>,
    routine_name_stack: Vec<ValueType>,
    assignment_indexes_stack: Vec<Vec<ValueType>>,
    preceding_nr_operands: f64,
    max_iterations: f64,
    orb: f64,
    error_breaks: bool,
    is_quiet: bool,
    golden_ratio: Option<f64>,
    conjug_gold: Option<f64>,
    input_base: f64,
    number_format: NumberFormat,
    writer: Box<dyn output::EchoingWriter>,
    reader: Box<dyn input::StdinOrMock>,
    text_io_handler: Box<dyn TextIOHandler>,
    last_calculated_gregorian_day: Option<GregorianDateInfo>,
    break_target: u32,
    try_outcome_stack: Vec<ValueType>,

    #[cfg(test)]
    golden_ratio_calculations: u8,

    #[cfg(test)]
    date_item_calculations: u8,
}

impl Shuttle {
    fn new(writer: Box<dyn output::EchoingWriter>, reader: Box<dyn input::StdinOrMock>, text_io_handler: Box<dyn TextIOHandler>) -> Self {
        Shuttle {
            nums: vec![HashMap::<ValueType, ValueType>::new()],
            stack: Vec::new(),
            routines: HashMap::new(),
            routine_name_stack: Vec::new(),
            assignment_indexes_stack: Vec::new(),
            preceding_nr_operands: 0f64,
            max_iterations: 10_000f64,
            orb: 0.000_000_01f64,
            error_breaks: true,
            is_quiet: false,
            golden_ratio: None,
            conjug_gold: None,
            input_base: 10f64,
            number_format: NumberFormat::new(),
            writer,
            reader,
            text_io_handler,
            last_calculated_gregorian_day: None,
            break_target: 0_u32,
            try_outcome_stack: Vec::new(),

            #[cfg(test)]
            golden_ratio_calculations: 0u8,

            #[cfg(test)]
            date_item_calculations: 0u8,
        }
    }

    fn get_top_of_vars_stack(&mut self) -> &mut HashMap<ValueType, ValueType> {
        if self.nums.is_empty() {
            self.nums.push(HashMap::<ValueType, ValueType>::new());
        }

        // Not using self.nums.last_mut(), as that would necessitate an .expect() call.
        let last_index = self.nums.len() -1;
        &mut self.nums[last_index]
    }

    fn set_var(&mut self, name: ValueType, value: ValueType) {
        let vars = self.get_top_of_vars_stack();

        if value == ValueType::Empty {
            vars.remove(&name);
        } else {
            vars.insert(name, value);
        }
    }

    fn get_var(&mut self, name: &ValueType) -> ValueType {
        let vars = self.get_top_of_vars_stack();

        match vars.get(name) {
            None => ValueType::Empty,
            Some(v) => v.clone(),
        }
    }

    fn running_routine_name(&self) -> ValueType {
        if self.routine_name_stack.is_empty() {
            return ValueType::Text("main".to_string());
        }

        let last_index = self.routine_name_stack.len() -1;
        self.routine_name_stack[last_index].clone()
    }

    #[cfg(test)]
    pub fn echo_output(&self) -> Option<&[u8]> {
        self.writer.echo_bytes()
    }

    #[cfg(test)]
    fn has_var(&mut self, name: ValueType) -> bool {
        let vars = self.get_top_of_vars_stack();
    
        println!("vars: {:?}", vars);

        vars.contains_key(&name)
    }
}

#[derive(Debug, PartialEq)]
pub enum ExecutionOutcome {
    Empty,
    Number{value: f64, format_info: NumberFormat},
    Text(String),
    Error(String),
}

impl ExecutionOutcome {
    pub fn numeric_value(&self) -> f64 {
        match self {
            ExecutionOutcome::Number{value: n, ..} => *n,
            _ => 0_f64,
        }
    }

    pub fn string_representation(&self) -> String {
        match self {
            ExecutionOutcome::Empty => NO_VALUE.to_string(),
            ExecutionOutcome::Number{value: n, format_info: fi} => (*fi).format(*n),
            ExecutionOutcome::Text(t) => t.clone(),
            ExecutionOutcome::Error(e) => e.clone(),
        }
    }
}

const NO_VALUE: &str = "(no_value)";

pub struct Interpreter {
    shuttle: Shuttle,
}

impl Interpreter {
    pub fn new(writer: Box<dyn output::EchoingWriter>, reader: Box<dyn input::StdinOrMock>, text_io_handler: Box<dyn TextIOHandler>) -> Self {
        let shuttle = Shuttle::new(writer, reader, text_io_handler);

        Interpreter{ shuttle, }
    }

    pub fn new_stdio_filesys() -> Self {
        let writer = Box::new(stdout());
        let reader = Box::new(input::StdinReader::new());
        let text_io_handler = Box::new(FileTextHandler::new());
        Interpreter::new(writer, reader, text_io_handler)
    }

    pub fn execute(&mut self, program: String) -> Result<ExecutionOutcome, ScriptError> {
        self.execute_opts(program, true, false, false)
    }

    pub fn new_and_execute_with_mocked_io(program: String) -> Result<ExecutionOutcome, ScriptError> {
        let writer = Box::new(Vec::<u8>::new());
        let reader = Box::new(input::MockByString::new(Vec::<String>::new()));
        let text_io_handler = Box::new(MockTextHandler::new());
        let mut interpreter = Self::new(writer, reader, text_io_handler);

        interpreter.execute_opts(program, true, false, false)
    }

    pub fn execute_opts(
        &mut self,
        program: String,
        do_execute: bool,
        show_before: bool,
        show_after: bool,
    ) -> Result<ExecutionOutcome, ScriptError> {
        let atoms = Self::split_atoms(&program)?;
        let mut tree: Expression = Self::make_tree(atoms)?;

        if show_before {
            // println!("\nTree before operate() :\n{}", tree.get_representation());
            self.print_state(&tree, true);
        }

        if do_execute {
            tree.operate(&mut self.shuttle)?;
        }

        if show_after {
            // println!("\nTree after operate() :\n{}", tree.get_representation());
            self.print_state(&tree, false);
        }

        Ok(match tree.get_value() {
            ValueType::Empty => ExecutionOutcome::Empty,
            ValueType::Number(ref n) => ExecutionOutcome::Number{value: *n, format_info: self.shuttle.number_format.clone()},
            ValueType::Text(ref s) => ExecutionOutcome::Text(s.clone()),
            ValueType::Error(ref s_err) => ExecutionOutcome::Error(format!("{s_err:?}")),
            ValueType::Max => ExecutionOutcome::Error("Invalid Max operand".to_string()),
        })
    }

    #[cfg(test)]
    pub fn echo_output(&self) -> Option<&[u8]> {
        self.shuttle.echo_output()
    }

    // Numeric overflows will cause number atoms to be
    // f64::INFINITY of f64::NEG_INFINITY.
    fn split_atoms(program: &str) -> Result<Vec<Atom>, ScriptError> {
        let mut result = Vec::<Atom>::new();
        let mut reading_number = false;
        let mut reading_simple_string = false;
        let mut bracket_nesting = 0u8;
        let mut opening_bracket_pos = Vec::<usize>::new();
        let mut current_num = String::new();
        let mut current_string = String::new();
        let mut current_bracket_content = String::new();
        let mut pos = 0usize;

        for c in program.chars() {
            pos += 1;

            if bracket_nesting == 0 {
                if c == CH_NUM_SEPA{
                    // Ignore if not reading a string.
                    if reading_simple_string {
                       current_string.push(c);
                    }
                } else if c.is_whitespace() {
                    if reading_number {
                        result.push(Atom::Number(current_num));
                    } else if reading_simple_string {
                        result.push(Atom::String(current_string.clone()));
                        current_string = String::new();
                    }

                    reading_number = false;
                    reading_simple_string = false;
                    current_num = String::new();
                } else if c == CH_CLAUSE_START {
                    if reading_number {
                        result.push(Atom::Number(current_num));
                        reading_number = false;
                        current_num = String::new();
                    } else if reading_simple_string {
                        result.push(Atom::String(current_string.clone()));
                        reading_simple_string = false;
                        current_string = String::new();
                    }

                    bracket_nesting += 1;
                    opening_bracket_pos.push(pos);
                } else if "()".contains(c) {
                    if reading_number {
                        result.push(Atom::Number(current_num));
                        reading_number = false;
                        current_num = String::new();
                    } else if reading_simple_string {
                        result.push(Atom::String(current_string.clone()));
                        reading_simple_string = false;
                        current_string = String::new();
                    }

                    result.push(Atom::Operator(c));
                } else if reading_simple_string {
                   current_string.push(c);
                } else if c.is_ascii_digit() {
                    reading_number = true;

                    // Simply push the character to the current_num string; parsing happens in
                    // opr_funcs::nop.
                    current_num.push(c);
                } else if c == CH_DOT {
                    reading_number = true;
                    current_num.push(c);
                } else if c == CH_CLAUSE_END {
                    return Err(ScriptError::UnexpectedClosingBracket{position: pos});
                } else if c == OPSTR {
                    if reading_simple_string {
                        current_string.push(c);
                    } else {
                        if reading_number {
                            result.push(Atom::Number(current_num));
                            reading_number = false;
                            current_num = String::new();
                        }

                        reading_simple_string = true;
                    }
                } else {
                    if reading_number {
                        result.push(Atom::Number(current_num));
                        reading_number = false;
                        current_num = String::new();
                    }

                    if Self::is_known_operator(c) {
                        result.push(Atom::Operator(c));
                    } else {
                        return Err(ScriptError::UnknownOperator{position: pos, operator: c});
                    }
                }
            } else {
                match c {
                    CH_CLAUSE_START => {
                        bracket_nesting += 1;
                        current_bracket_content.push(c);
                        opening_bracket_pos.push(pos);
                    },
                    CH_CLAUSE_END => {
                        bracket_nesting -= 1;

                        if bracket_nesting == 0 {
                            // An empty bracket content is ignored.
                            if !current_bracket_content.is_empty() {
                                match current_bracket_content.chars().next().unwrap() {
                                    first if "scn".contains(first) => {
                                        let rest = match current_bracket_content.get(1..) {
                                            None => String::new(),
                                            Some(r) => r.to_string(),
                                        };

                                        match first {
                                            MARK_STRING => result.push(Atom::String(rest)),
                                            MARK_COMMENT => result.push(Atom::Comment(rest)),
                                            MARK_NUMBER => result.push(Atom::Number(rest)),
                                            _ => (),
                                        }
                                    },
                                    other => {
                                        let marker_pos = match opening_bracket_pos.pop() {
                                            None => 0,
                                            Some(ps) => ps + 1,
                                        };

                                        return Err(ScriptError::UnknownBracketContentTypeMarker{position: marker_pos, marker: other});
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
            return Err(ScriptError::UnclosedBracketsAtEnd);
        }

        if reading_number {
            result.push(Atom::Number(current_num));
        } else if reading_simple_string {
            result.push(Atom::String(current_string));
        }

        Ok(result)
    }

    fn make_tree(atoms: Vec<Atom>) -> Result<Expression, ScriptError> {
        let mut exp_stack = Vec::<Expression>::new();
        let mut override_start_found = false;
        let mut needed_ops: usize;
        let mut alternative_marks_count = 0u8;

        for exp in atoms.iter().rev() {

            /*
            #[cfg(test)]
            println!("exp_stack: {:?}", exp_stack);
            */

            match exp {
                Atom::Number(string_rep) => {
                    let new_exp = Expression::new_number(string_rep.to_string());
                    exp_stack.push(new_exp);
                },
                Atom::String(s) => {
                    let new_exp = Expression::new_text(s.to_string());
                    exp_stack.push(new_exp);
                },
                Atom::Operator(c) => {
                    needed_ops = match *c {
                        OPNEG | OPINT | OPABS | OPVAR | OPMAS | OPNOT |
                        OPWRT | OPDEG | OPSIN | OPCOS | OPTAN | OPCON |
                        OPSIG | OPTYP | OPBAS | OPSTK | OPEXR | OP2NR |
                        OPBRK | OPEVL | OPUER | OPQUO
                            => 1,
                        OPPLS | OPMNS | OPMUL | OPDIV | OPEXP | OPLOG |
                        OPMOD | OPAND | OPOR_ | OPXOR | OPASG | OPWHI |
                        OPCMB | OPMIN | OPMAX | OPEQU | OPLSS | OPGRT |
                        OPSET | OPOP1 | OPRTN | OPAT2
                            => 2,
                        OPIF_ | OPOP2
                            => 3,
                        OPFOR
                            => 5,
                        _
                            => 0,
                    };

                    match *c {
                        CH_OVR_START => override_start_found = true,
                        OPALT =>  {
                            alternative_marks_count = alternative_marks_count.saturating_add(1);
                        },
                        op_for_stack => {
                            if [OPOP1, OPOP2].contains(&op_for_stack) {
                                needed_ops += (2 * alternative_marks_count) as usize;
                            } else if
                                (
                                    (op_for_stack == OPIF_) ||
                                    (op_for_stack == OPWRT) ||
                                    (op_for_stack == OPVAR) ||
                                    (op_for_stack == OPMAS)
                                ) &&
                                (alternative_marks_count > 0)
                            {
                                needed_ops = 2;
                            } else if (op_for_stack == OPREA) && (alternative_marks_count > 0) {
                                needed_ops = 1;
                            } else if (op_for_stack == OPSTK) && (alternative_marks_count == 2) {
                                needed_ops = 0;
                            }

                            let mut new_exp =
                                Expression::new(op_for_stack, alternative_marks_count);

                            alternative_marks_count = 0;

                            // The number of arguments for the : and , operators can't be overridden.
                            // The override_start marker will be applied to the previous operator
                            // instead.
                            if override_start_found && (!":,".contains(op_for_stack)) {
                                new_exp.has_overridden_nr_of_ops = true;
                                override_start_found = false;

                                while let Some(e) = exp_stack.pop() {
                                    if e.opr_mark == CH_OVR_END {
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
                                        if e.opr_mark == CH_OVR_END {
                                            return Err(ScriptError::UnexpectedClosingParenthesis);
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

        // If the stack has any expression having opr_mark CH_OVR_END, return an error.
        for exp in exp_stack.iter() {
            if exp.opr_mark == CH_OVR_END {
                return Err(ScriptError::UnexpectedClosingParenthesis);
            }
        }

        // If the stack has more than one expression, put whatever remains on the stack as operands in a ; - expression.
        Ok(match exp_stack.len() {
            0 => Expression::new_number("0".to_string()),
            1 => exp_stack.pop().unwrap_or(Expression::new(OPEMP, 0)),
            _ => {
                let mut result = Expression::new(OPCMB, 0);

                for exp in exp_stack.into_iter().rev() {
                    result.push_operand(exp);
                }

                result
            },
        })
    }

    fn is_known_operator(op: char) -> bool {
        "~+-*/^lia%°SCTApec$v:Kk§,?WF;mMNn()=<>!&|xZoOwrstbRX€BE¶UVq".contains(op)
    }

    fn print_state(&self, tree: &Expression, before: bool) {
        for (name, routine) in self.shuttle.routines.iter() {
            println!("{}", routine.get_representation(name));
        }

        println!(
            "\nTree {} operate() :\n{}",
            if before {"before"} else {"after"},
            tree.get_representation()
        );
    }

    pub fn is_quiet(&self) -> bool {
        self.shuttle.is_quiet
    }

    pub fn suppress_exit_on_error(&mut self, do_suppress: bool) {
        self.shuttle.error_breaks = !do_suppress;
    }
}

fn are_near(num1: f64, num2: f64, precision: f64) -> bool {
    (num1 - num2).abs() <= precision
}

#[cfg(test)]
fn are_very_near(num1: f64, num2: f64) -> bool {
    are_near(num1, num2, 0.00000001f64)
}

pub mod input {
    use std::io::stdin;

    pub trait StdinOrMock {
        fn read_line(&mut self) -> Option<String>;
    }

    pub struct StdinReader {
    }

    impl StdinReader {
        pub fn new() -> Self {
            StdinReader{}
        }
    }

    impl Default for StdinReader {
        fn default() -> Self {
            Self::new()
        }
    }

    impl StdinOrMock for StdinReader {
        fn read_line(&mut self) -> Option<String> {
            let mut buffer = String::new();

            match stdin().read_line(&mut buffer) {
                Ok(_) => Some(buffer),
                Err(_) => None,
            }
        }
    }

    pub struct MockByString {
        fake_input: Vec<String>,
    }

    impl MockByString {
        pub fn new(fake_input: Vec<String>) -> Self {
            MockByString {
                fake_input,
            }
        }
    }

    impl StdinOrMock for MockByString {
        fn read_line(&mut self) -> Option<String> {
            self.fake_input.pop()
        }
    }
}

pub mod output {
    pub trait OutputEchoer {
        fn echo_bytes(&self) -> Option<&[u8]> {
            None
        }
    }

    impl OutputEchoer for Vec<u8> {
        fn echo_bytes(&self) -> Option<&[u8]> {
            Some(self.as_slice())
        }
    }

    impl OutputEchoer for std::io::Stdout {}
    impl OutputEchoer for std::io::Sink {}

    pub trait EchoingWriter: std::io::Write + OutputEchoer {}

    impl EchoingWriter for Vec<u8> {}
    impl EchoingWriter for std::io::Stdout {}
    impl EchoingWriter for std::io::Sink {}
}

pub(crate) mod opr_funcs {
    use crate::{CH_DOT, CH_SEPA_DIGITS, CH_SEPA_VERSION, OPCMB, OPEMP, OPMNS, OPNEG};
    use std::collections::HashMap;
    use std::ffi::OsStr;
    use super::{DateInfoItem, Expression, GregorianDateInfo, Interpreter, NO_VALUE, Routine, ScriptError, Shuttle, ValueType, are_near};

    const DAYS_IN_YEAR :u32 = 365;

    const WEEKDAYS: [&str; 7] = [
        "SAT",
        "SUN",
        "MON",
        "TUE",
        "WED",
        "THU",
        "FRI",
    ];

    fn check_nr_operands(opr_mark: &char, operands: &[Expression], nr_required: usize) -> Result<(), ScriptError> {
        if operands.len() < nr_required {
            Err(ScriptError::InsufficientOperands(*opr_mark))
        } else {
            Ok(())
        }
    }

    fn single_number_operation<F: FnMut(f64) -> f64>(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], mut operation: F) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        *result_value = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(num) => ValueType::Number(operation(num)),
            ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        Ok(())
    }

    fn has_any_string_operands(operands: &[Expression]) -> bool {
        let mut opd_count = 0usize;

        if operands.is_empty() {
            return false;
        }

        loop{
            if let ValueType::Text(_) = operands[opd_count].get_value() {
                return true;
            }

            opd_count += 1;

            if opd_count == operands.len() {
                break;
            }
        }

        false
    }

    fn parse_number(string_rep: &str, input_base: f64, ignore_non_numeric_chars: bool) -> Result<f64, String> {
        let mut string_rep_ext = string_rep.trim().to_string();
        string_rep_ext.push(CH_SEPA_DIGITS);

        let mut num = 0f64;
        let mut sign_factor = 1f64;
        let mut periods_found = 0u8;
        let mut first_frac = 0usize;
        let mut digit_value =  0u32;
        let mut has_pending_digit = false;
        let mut frac_pos = 0i32;

        let mut digits = Vec::<u32>::new();
        let using_multichar_digits = input_base > 36f64;
        let mut char_val: u32;
        let diff_0 = '0' as u32;
        let diff_a = ('A' as u32) - 10u32;

        let last_char_pos = string_rep_ext.len() - 1;

        // Compose an array of u32 digit values.
        for (char_count, c) in string_rep_ext.to_uppercase().chars().enumerate() {
            match c {
                OPMNS | OPNEG => {
                    if char_count == 0 {
                        sign_factor = -1f64;
                    } else {
                        return Err("Non-leading sign indicator".to_string());
                    }
                }
                CH_DOT => {
                    if using_multichar_digits && has_pending_digit {
                        digits.push(digit_value);
                        digit_value = 0u32;
                        has_pending_digit = false;
                    }

                    periods_found += 1;

                    if periods_found == 1 {
                        first_frac = digits.len();
                    } else if !ignore_non_numeric_chars {
                        return Err("Excess periods found in number input".to_string());
                    }
                },
                ch @ '0'..='9' | ch @ 'A'..='Z' => {
                    char_val = ch as u32;

                    if using_multichar_digits {
                        has_pending_digit = true;
                        digit_value = (digit_value * 10) + char_val - diff_0;
                    } else {
                        digits.push(char_val - if ch <= '9' {
                            diff_0
                        } else {
                            diff_a
                        });
                    }
                },
                // Ignore spaces if single-character digits are used (base <= 36).
                CH_SEPA_DIGITS => {
                    if using_multichar_digits && has_pending_digit {
                        digits.push(digit_value);
                        digit_value = 0u32;
                        has_pending_digit = false;
                    } else if (!ignore_non_numeric_chars) && (char_count < last_char_pos) {
                        return Err("Illegal spaces in number input".to_string());
                    }
                },
                // Ignore underscores.
                '_' => (),
                _ =>  {
                    if !ignore_non_numeric_chars {
                        return Err("Illegal characters in number input".to_string());
                    }
                },
            }
        }

        if periods_found == 0 {
            first_frac = digits.len();
        }

        for (dcount, d) in digits.into_iter().enumerate() {
            digit_value = if (d as f64) >= input_base {
                if ignore_non_numeric_chars {
                    (input_base as u32) - 1u32
                } else {
                    return Err("Digit value too high for base of input number".to_string());
                }
            } else {
                d
            };

            if dcount < first_frac {
                num = (num * input_base) + (digit_value as f64);
            } else {
                frac_pos += 1;
                num += (digit_value as f64) / input_base.powi(frac_pos);
            }
        }

        Ok(num * sign_factor)
    }

    pub fn nop(_opr_mark: &mut char, result_value: &mut ValueType, _operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        // If still needed, parse the string representation to a number.
        if let ValueType::Text(string_rep) = result_value {
            match parse_number(string_rep, shuttle.input_base, true) {
                Ok(num) => *result_value = ValueType::Number(num),
                Err(msg) => return Err(ScriptError::NumberParsingFailure(msg)),
            }
        }

        Ok(())
    }

    pub fn empty(_opr_mark: &mut char, result_value: &mut ValueType, _operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Empty;

        Ok(())
    }

    pub fn string_expr(_opr_mark: &mut char, _result_value: &mut ValueType, _operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        // No need to do anything.
        // Meant for Expressions that contain a fixed string value from creation.

        Ok(())
    }
    
    fn concat_expressions(opr_mark: &mut char, operands: &mut [Expression], shuttle: &mut Shuttle, do_truncate: bool) -> Result<String, ScriptError> {
        let mut string_outcome = "".to_string();

        for op in operands {
            string_outcome.push_str(
                (match op.get_value() {
                    ValueType::Empty => "".to_string(),   
                    ValueType::Number(num) if do_truncate => format!("{}", num.trunc() as i64),
                    ValueType::Number(num) => shuttle.number_format.format(num),
                    ValueType::Text(txt) => txt,
                    ValueType::Error(s_err) => format!("{s_err:?}"),
                    ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
                }).as_str()
            );
        }

        Ok(string_outcome)
    }

    fn add_base(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle, do_truncate: bool) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        if has_any_string_operands(operands) {
            *result_value = ValueType::Text(
                concat_expressions(opr_mark, operands, shuttle, do_truncate)?
            );
        } else {
            let mut num_outcome = 0f64;

            for op in operands {
                match op.get_value() {
                    ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                    ValueType::Number(num) => num_outcome += num,
                    ValueType::Error(s_err) => return Err(s_err),
                    ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
                    _ => (),
                }
            }

            *result_value = ValueType::Number(num_outcome);
        }

        Ok(())
    }

    pub fn add(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        add_base(opr_mark, result_value, operands, shuttle, false)
    }

    pub fn add_concat_int(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        add_base(opr_mark, result_value, operands, shuttle, true)
    }
    
    pub fn multiply(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;
        let mut outcome = 1f64;

        for op in operands {
            match op.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => outcome *= num,
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        *result_value = ValueType::Number(outcome);

        Ok(())
    }
    
    pub fn power(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;
        let mut outcome = 1_f64;

        for (count, op) in operands.iter().enumerate() {
            match op.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => match count {
                    0 => outcome = num,
                    _ => {
                        if (outcome < 0_f64) && (num.fract() != 0_f64) {
                            return Err(ScriptError::NonIntegerPowerOfNegativeNumberIsNotSupported);
                        }
                        outcome = outcome.powf(num);
                    },
                },
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        *result_value = ValueType::Number(outcome);

        Ok(())
    }

    pub fn minus(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;
        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match op.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => match count {
                    0 => outcome = num,
                    _ => outcome -= num,
                },
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        *result_value = ValueType::Number(outcome);

        Ok(())
    }
    
    pub fn divide(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;
        let mut outcome = 1f64;

        for (count, op) in operands.iter().enumerate() {
            match op.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => match count {
                    0 => outcome = num,
                    _ =>  {
                        if num == 0f64 {
                            return Err(ScriptError::DivideByZero(*opr_mark));
                        }

                        outcome /= num;
                    },
                },
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        *result_value = ValueType::Number(outcome);

        Ok(())
    }
    
    pub fn int_div(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut dividend = 1f64;
        let mut outcome = 0f64;
        let mut divisor: f64;

        for (count, op) in operands.iter().enumerate() {
            match op.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => match count {
                    0 => dividend = num,
                    1 => {
                        divisor = num;

                        if divisor == 0f64 {
                            return Err(ScriptError::DivideByZero(*opr_mark));
                        }

                        let result_sign = dividend.signum() * divisor.signum();
                        dividend = dividend.abs();
                        divisor = divisor.abs();
                        outcome = dividend.div_euclid(divisor) * result_sign;
                        shuttle.stack.push(ValueType::Number(dividend.rem_euclid(divisor) * result_sign));
                    },
                    _ => (),
                },
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        *result_value = ValueType::Number(outcome);

        Ok(())
    }
    
    pub fn modulo(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;
        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match op.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => match count {
                    0 => outcome = num,
                    _ => {
                        if num == 0_f64 {
                            return Err(ScriptError::DivideByZero(*opr_mark));
                        }

                        outcome %= num;
                    },
                },
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        *result_value = ValueType::Number(outcome);

        Ok(())
    }

    pub fn combine(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.last() {
            Some(e) => e.get_value(),
            None => return Err(ScriptError::InsufficientOperands(*opr_mark)),
        };

        Ok(())
    }
    
    pub fn min(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut outcome = ValueType::Max;
        let mut current: ValueType;

        for op in &*operands {
            current = op.get_value();

            if outcome > current {
                outcome = current;
            }
        }

        if outcome == ValueType::Max {
            outcome = ValueType::Empty;
        }

        *result_value = outcome;

        Ok(())
    }
    
    pub fn max(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut outcome = ValueType::Empty;
        let mut current: ValueType;

        for op in &*operands {
            current = op.get_value();

            if outcome < current {
                outcome = current;
            }
        }

        *result_value = outcome;

        Ok(())
    }

    pub fn unaryminus(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| -n)
    }

    pub fn intgr(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.trunc())
    }

    pub fn ceiling(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(
            opr_mark,
            result_value,
            operands,
            |n|  {
                if n.is_sign_positive() {
                    n.ceil()
                } else {
                    n.floor()
                }
            })
    }

    pub fn abs(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.abs())
    }

    pub fn degrees(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.to_degrees())
    }

    pub fn radians(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.to_radians())
    }

    pub fn sine(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.sin())
    }

    pub fn asin(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.asin())
    }

    pub fn sinh(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.sinh())
    }

    pub fn asinh(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.asinh())
    }

    pub fn cosine(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.cos())
    }

    pub fn acos(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.acos())
    }

    pub fn cosh(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.cosh())
    }

    pub fn acosh(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.acosh())
    }

    pub fn tangent(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.tan())
    }

    pub fn atan(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.atan())
    }

    pub fn tanh(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.tanh())
    }

    pub fn atanh(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.atanh())
    }

    pub fn atan2(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut num_y = 1_f64;
        let mut num_x = 1_f64;

        for (count, opd) in operands[0..=1].iter().enumerate() {
            match opd.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => match count {
                    0 => num_y = num,
                    1 => num_x = num,
                    _ => (),
                },
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            };
        }

        *result_value = ValueType::Number(num_y.atan2(num_x));

        Ok(())
    }

    pub fn log(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut base = 10_f64;
        let mut product = 1_f64;

        for (count, op) in operands.iter().enumerate() {
            match op.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => match count {
                    0 => base = num,
                    1 => product = num,
                    _ => (),
                },
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        if base <= 0_f64 {
            return Err(ScriptError::ZeroOrNegativeLogarithmBaseIsNotSupported);
        }

        if product <= 0_f64 {
            return Err(ScriptError::LogarithmOfZeroOrNegativeNumberIsNotSupported);
        }

        *result_value = ValueType::Number(product.log(base));

        Ok(())
    }

    pub fn pi(_opr_mark: &mut char, result_value: &mut ValueType, _operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Number(std::f64::consts::PI);

        Ok(())
    }

    pub fn euler_const(_opr_mark: &mut char, result_value: &mut ValueType, _operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Number(std::f64::consts::E);

        Ok(())
    }

    pub fn newline(_opr_mark: &mut char, result_value: &mut ValueType, _operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Text("\n".to_string());

        Ok(())
    }

    fn get_golden_ratio_from_shuttle(shuttle: &mut Shuttle) -> f64 {
        match shuttle.golden_ratio {
            Some(gr) => gr,
            None => {
                let golden_ratio = (1f64 + 5f64.sqrt()) / 2f64;
                shuttle.golden_ratio = Some(golden_ratio);

                #[cfg(test)]
                {
                    shuttle.golden_ratio_calculations += 1u8;
                }

                golden_ratio
            },
        }
    }

    fn get_conjugate_golden_ratio_from_shuttle(shuttle: &mut Shuttle) -> f64 {
        match shuttle.conjug_gold {
            Some(cjgr) => cjgr,
            None => {
                let conjug = (1f64 - 5f64.sqrt()) / 2f64;
                shuttle.conjug_gold = Some(conjug);

                conjug
            },
        }
    }

    pub fn constants(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            return Err(ScriptError::InsufficientOperands(*opr_mark));
        }

        let const_index = operands[0].get_value();

        *result_value = match const_index {
            ValueType::Text(name) if name == "gold" =>
                ValueType::Number(get_golden_ratio_from_shuttle(shuttle)) ,
            ValueType::Text(name) if name == "cogold" =>
                ValueType::Number(get_conjugate_golden_ratio_from_shuttle(shuttle)),
            ValueType::Text(name) if name == "n" => ValueType::Text("\n".to_string()),
            ValueType::Text(name) if name == "empty" => ValueType::Empty,
            ValueType::Text(name) if name == "rtn" => shuttle.running_routine_name(),
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            _ => return Err(ScriptError::UnknownConstant(const_index.get_string_value("???".to_string()))),
        };

        Ok(())
    }

    pub fn assign_var(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut name_is_string = false;
        let mut name: ValueType;
        let mut name_base = "".to_string();
        let is_series = operands.len() > 2;

        let mut index = 0f64;

        match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(num) =>  {
                index = num;
            },
            ValueType::Text(txt) => {
                name_base = txt;
                name_is_string = true;
            }
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        }

        let mut counter = 0f64;
        let mut reg_val = ValueType::Empty;

        for opd in &operands[1..operands.len()] {
            reg_val = opd.get_value();

            if name_is_string {
                if is_series {
                    name = ValueType::Text(format!("{name_base}{counter}"));
                } else {
                    name = ValueType::Text(name_base.clone());
                }
            } else {
                name = ValueType::Number(index + counter);
            }

            shuttle.set_var(name, reg_val.clone());
            counter += 1f64;
        }

        *result_value = reg_val;

        Ok(())
    }

    pub fn get_variable(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        let index = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(num) => ValueType::Number(num),
            ValueType::Text(txt) => ValueType::Text(txt),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        *result_value = shuttle.get_var(&index);

        Ok(())
    }

    pub fn get_variable_or(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let index = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(num) => ValueType::Number(num),
            ValueType::Text(txt) => ValueType::Text(txt),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        *result_value = match shuttle.get_var(&index){
            ValueType::Empty => {
                let new_val = operands[1].get_value();
                shuttle.set_var(index, new_val.clone());

                new_val
            },
            non_empty => non_empty,
        };

        Ok(())
    }

    pub fn assignment_maker_base(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle, assign_default_if_empty: bool) -> Result<(), ScriptError> {

        let required_nr_ops = if assign_default_if_empty{
            2
        } else {
            1
        };

        check_nr_operands(opr_mark, operands, required_nr_ops)?;

        let index = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(num) => ValueType::Number(num),
            ValueType::Text(txt) => ValueType::Text(txt),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        let stack_len = shuttle.assignment_indexes_stack.len();

        // Add the variable's index to the list of variables to be assigned to
        // for the operator that's the parent of the : operator, if any.
        if stack_len >= 2 {
            shuttle
                .assignment_indexes_stack
                [stack_len - 2]
                .push(index.clone());
        }

        *result_value = match shuttle.get_var(&index) {
            ValueType::Empty if assign_default_if_empty => {
                let new_val = operands[1].get_value();
                shuttle.set_var(index, new_val.clone());

                new_val
            },
            other => other,
        };

        Ok(())
    }

    pub fn assignment_maker(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        assignment_maker_base(opr_mark, result_value, operands, shuttle, false)
    }

    pub fn assignment_maker_with_default(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        assignment_maker_base(opr_mark, result_value, operands, shuttle, true)
    }

    pub fn push_stack(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        let mut outcome = ValueType::Empty;

        for op in operands.iter() {
            outcome = (*op).get_value();
            shuttle.stack.push(outcome.clone());
        }

        *result_value = outcome;

        Ok(())
    }

    pub fn push_stack_reverse(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        let mut outcome = ValueType::Empty;

        for op in operands.iter().rev() {
            outcome = (*op).get_value();
            shuttle.stack.push(outcome.clone());
        }

        *result_value = outcome;

        Ok(())
    }

    pub fn pop_stack(_opr_mark: &mut char, result_value: &mut ValueType, _operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match shuttle.stack.pop() {
            None => ValueType::Empty,
            Some(vt) => vt,
        };

        Ok(())
    }

    pub fn stack_depth(_opr_mark: &mut char, result_value: &mut ValueType, _operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Number(shuttle.stack.len() as f64);

        Ok(())
    }

    pub fn clear_stack(_opr_mark: &mut char, result_value: &mut ValueType, _operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Number(shuttle.stack.len() as f64);
        shuttle.stack.clear();

        Ok(())
    }
    
    pub fn equals(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut are_equal = true;
        let mut is_numeric = true;
        let mut first = ValueType::Empty;
        let mut first_type = 0f64;
        let mut op_value: ValueType;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => {
                    first = op.get_value();
                    first_type = first.get_type_as_num();
                    is_numeric = first_type == 1f64;
                },
                _ => {
                    op_value = op.get_value();

                    if first_type != op_value.get_type_as_num() {
                        are_equal = false;
                    } else if is_numeric {
                        are_equal = are_equal && are_near(first.get_num_value(0f64), op.get_num_value(0f64), shuttle.orb);
                    } else {
                        are_equal = are_equal && (first == op_value)
                    }
                },
            }
        }

        *result_value = ValueType::Number(if are_equal {
            1f64
        } else {
            0f64
        });

        Ok(())
    }
    
    pub fn less(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut outcome = true;
        let mut previous = ValueType::Empty;
        let mut current: ValueType;
        let ops = operands.iter().enumerate();

        for (count, op) in ops {
            match count {
                0 => previous = op.get_value(),
                _ => {
                    current = op.get_value();
                    outcome = outcome && (previous < current);
                    previous = current;
                },
            }

            if !outcome {
                break;
            }
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });

        Ok(())
    }
    
    pub fn greater(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut outcome = true;
        let mut previous = ValueType::Max;
        let mut current: ValueType;
        let ops = operands.iter().enumerate();

        for (count, op) in ops {
            match count {
                0 => previous = op.get_value(),
                _ => {
                    current = op.get_value();
                    outcome = outcome && (previous > current);
                    previous = current;
                },
            }

            if !outcome {
                break;
            }
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });

        Ok(())
    }

    pub fn not(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        let mut outcome = true;

        for op in operands {
            match op.get_value() {
                ValueType::Empty => (), // Empty is considered a falsy value.
                ValueType::Text(ref s) => outcome &= s.is_empty(),
                ValueType::Number(n) => outcome &= are_near(0f64, n, shuttle.orb),
                ValueType::Error(_) => (), // an Error is considered a falsy value.
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });

        Ok(())
    }

    pub fn and(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut outcome = true;

        for op in operands {
            match op.get_value() {
                ValueType::Empty => outcome = false,
                ValueType::Text(ref s) => outcome &= !s.is_empty(),
                ValueType::Number(n) => outcome &= !are_near(0f64, n, shuttle.orb),
                ValueType::Error(_) => outcome = false,
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });

        Ok(())
    }

    pub fn or(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut outcome = false;

        for op in operands {
            match op.get_value() {
                ValueType::Empty => (),
                ValueType::Text(ref s) => outcome |= !s.is_empty(),
                ValueType::Number(n) => outcome |= !are_near(0f64, n, shuttle.orb),
                ValueType::Error(_) => (),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });

        Ok(())
    }

    pub fn xor(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut trues = 0usize;

        for op in operands {
            match op.get_value() {
                ValueType::Empty => (),
                ValueType::Text(ref s) =>  {
                    if !s.is_empty() {
                        trues += 1;
                    }
                },
                ValueType::Number(n) => {
                    if !are_near(0f64, n, shuttle.orb) {
                      trues += 1;
                    }
                },
                ValueType::Error(_) => (),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        *result_value = ValueType::Number(if trues == 1 {
            1f64
        } else {
            0f64
        });

        Ok(())
    }
    
    pub fn setting(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut setting_name = ValueType::Empty;
        let mut setting_value = ValueType::Empty;

        for (count, op) in operands.iter().enumerate() {
            match op.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(_) | ValueType::Text(_) => match count {
                    0 => setting_name = op.get_value(),
                    1 => setting_value = op.get_value(),
                    _ => (),
                },
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        match setting_name {
            ValueType::Text(name) if name ==  "prec" => shuttle.orb = setting_value.get_num_value(0f64),
            ValueType::Text(name) if name == "loops" => shuttle.max_iterations = setting_value.get_num_value(0f64),
            ValueType::Text(name) if name == "ign" => shuttle.error_breaks = setting_value.get_num_value(0f64) == 0_f64,
            ValueType::Text(name) if name == "quiet" => shuttle.is_quiet = setting_value.get_num_value(0f64) != 0_f64,
            _ => (),
        }

        *result_value = setting_value;

        Ok(())
    }

    pub fn enumerated_opr(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        let opr_func = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Text(name) if name == "r" => round,
            ValueType::Text(name) if name == "fib" => fibonacci,
            ValueType::Text(name) if name == "uni" => get_unicode_chars,
            ValueType::Text(name) if name == "ucv" => get_unicode_value,
            ValueType::Text(name) if name == "len" => get_length,
            ValueType::Text(name) if name == "find" => find_in_string,
            ValueType::Text(name) if name == "repl" => replace_in_string,
            ValueType::Text(name) if name == "split" => split,
            ValueType::Text(name) if name == "sub" => substring,
            ValueType::Text(name) if name == "lower" => lower,
            ValueType::Text(name) if name == "upper" => upper,
            ValueType::Text(name) if name == "proper" => proper,
            ValueType::Text(name) if name == "fmt" => set_number_format,
            ValueType::Text(name) if name == "leap" => opr_leap,
            ValueType::Text(name) if name == "dow" => dow,
            ValueType::Text(name) if name == "greg" => gregorian_seq,
            ValueType::Text(name) if name == "gregy" => gregorian_year,
            ValueType::Text(name) if name == "gregm" => gregorian_month,
            ValueType::Text(name) if name == "gregd" => gregorian_day,
            ValueType::Text(name) if name == "gregt" => gregorian_text,
            ValueType::Text(name) if name == "gregn" => gregorian_days_in_month,
            ValueType::Text(name) if name == "version" => get_version,
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            unknown =>  {
                return Err(ScriptError::UnknownNamedOperator(
                    unknown.get_string_value("???".to_string())));
            },
        };

        (opr_func)(opr_mark, result_value, &mut operands[1..], shuttle)?;

        Ok(())
    }

    pub fn round(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(opr_mark, result_value, operands, |n| n.round())
    }

    pub fn fibonacci(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(
            opr_mark,
            result_value,
            operands,
            |n| {
                let sig = n.signum();
                let index = n.abs().trunc() as i32;
                let golden_ratio = get_golden_ratio_from_shuttle(shuttle);
                let conjug = get_conjugate_golden_ratio_from_shuttle(shuttle);

                let mut fib = ((golden_ratio.powi(index) - conjug.powi(index)) / 5_f64.sqrt()).round();

                if (sig < 0_f64) && (n < -1_f64) && (n % 2_f64 == 0_f64) {
                   fib = -fib; 
                }

                fib
            })
    }

    pub fn get_version(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        const VERSION: &str = env!("CARGO_PKG_VERSION");

        *result_value = if operands.is_empty() {
            ValueType::Text(VERSION.to_string())
        } else {
            let version_nums: Vec<&str> = VERSION.split(CH_SEPA_VERSION).collect();

            #[allow(clippy::get_first)]
            match operands[0].get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => match num {
                    0_f64 => ValueType::Text(VERSION.to_string()),
                    1_f64 => ValueType::Text(version_nums.get(0).unwrap_or(&"0").to_string()),
                    2_f64 => ValueType::Text(version_nums.get(1).unwrap_or(&"0").to_string()),
                    3_f64 => ValueType::Text(version_nums.get(2).unwrap_or(&"0").to_string()),
                    _ => return Err(ScriptError::InvalidVersionPart(num)),
                },
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        };

        Ok(())
    }

    pub fn set_number_format(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        let prev_format = shuttle.number_format.clone();

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => match op.get_value() {
                    ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                    ValueType::Number(num) => shuttle.number_format.set_fractal_digits(num),
                    ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                    ValueType::Error(s_err) => return Err(s_err),
                    ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
                },
                1 => match op.get_value() {
                    ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                    ValueType::Number(_) => return Err(ScriptError::NonTextOperand(*opr_mark)),
                    ValueType::Text(txt) => shuttle.number_format.set_fractal_separator(txt),
                    ValueType::Error(s_err) => return Err(s_err),
                    ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
                },
                2 => match op.get_value() {
                    ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                    ValueType::Number(_) => return Err(ScriptError::NonTextOperand(*opr_mark)),
                    ValueType::Text(txt) => shuttle.number_format.set_thousands_separator(txt),
                    ValueType::Error(s_err) => return Err(s_err),
                    ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
                },
                _ => (),
            }
        }

        let has_conflict = if shuttle.number_format.use_thousands_separator {
            (shuttle.number_format.fractal_separator == shuttle.number_format.thousands_separator)
            || (shuttle.number_format.fractal_separator == shuttle.number_format.digit_separator)
            || (shuttle.number_format.thousands_separator == shuttle.number_format.digit_separator)
        } else  {
            shuttle.number_format.fractal_separator == shuttle.number_format.digit_separator
        };

        if has_conflict {
            shuttle.number_format = prev_format;
            return Err(ScriptError::ConflictingNumberPartSeparators);
        }

        *result_value = ValueType::Empty;

        Ok(())
    }

    pub fn sign(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        let mut all_positive = true;
        let mut all_negative = true;

        for opd in &*operands {
            match opd.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) =>  {
                    all_positive = all_positive && (num > 0f64);
                    all_negative = all_negative && (num < 0f64);
                },
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        *result_value = ValueType::Number(
            if all_positive {
                1f64
            } else if all_negative {
                -1f64
            } else {
                0f64
            }
        );

        Ok(())
    }

    pub fn quote_base(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle, do_truncate: bool) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        *result_value = ValueType::Text(
            concat_expressions(opr_mark, operands, shuttle, do_truncate)?
        );

        Ok(())
    }

    pub fn quote(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        quote_base(opr_mark, result_value, operands, shuttle, false)
    }

    pub fn quote_trunc(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        quote_base(opr_mark, result_value, operands, shuttle, true)
    }

    pub fn get_unicode_chars(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        let mut result_string = String::new();

        for opd in &*operands {
            match opd.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => match char::from_u32(num as u32) {
                    // num is invalid unicode point
                    None => result_string.push_str(format!("¿{}?", num.trunc()).as_str()),

                    Some(ch) => result_string.push(ch),
                },
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        *result_value = ValueType::Text(result_string);

        Ok(())
    }

    pub fn get_length(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        let mut tot_len = 0usize;

        for opd in operands {
            tot_len +=
            match opd.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Text(txt) => txt,
                ValueType::Number(num) => shuttle.number_format.format(num),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }.chars().collect::<Vec<char>>().len();
        }

        *result_value = ValueType::Number(tot_len as f64);

        Ok(())
    }

    pub(crate) fn is_leap_year(year: f64) -> bool {
        let int_year = year as u32;

        if int_year % 4 != 0 {
            false
        } else if int_year % 100 != 0 {
            true
        } else {
            int_year % 400 == 0
        }
    }

    pub fn opr_leap(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        single_number_operation(
            opr_mark,
            result_value,
            operands,
            |n| 
                if is_leap_year(n) {
                   1_f64
                } else {
                    0_f64
                }
            )
    }

    pub(crate) fn days_in_month(month: u32, is_leap_year: bool) -> u32 {
        match month {
            1 => 31,
            2 if is_leap_year => 29,
            2 => 28,
            3 => 31,
            4 => 30,
            5 => 31,
            6 => 30,
            7 => 31,
            8 => 31,
            9 => 30,
            10 => 31,
            11 => 30,
            12 => 31,
            _ => 0,
        }
    }

    pub fn gregorian_days_in_month(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(*opr_mark));
        }

        let year = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(num) => num,
            ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        let month = match operands[1].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(num) => num,
            ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        if (year < 0_f64) || !(1_f64..=12_f64).contains(&month) {
            return Err(ScriptError::InvalidDatePart(*opr_mark));
        }

        *result_value = ValueType::Number(days_in_month(month as u32, is_leap_year(year)) as f64);

        Ok(())
    }

    fn day_of_week(year: u32, month: u32, day: u32) -> u32 {
        let century = year / 100;
        let year_in_century = year % 100;
        let century_offset = (7 - ((century % 4) * 2)) % 7;
        let year_offset = century_offset + year_in_century + (year_in_century / 4);
        let mut month_offset = 0;

        for m in 1..month {
            month_offset += days_in_month(m, false);
        }

        if is_leap_year(year as f64) && (month <= 2) {
            month_offset = 7 + month_offset - 1;
        }

        (year_offset + month_offset + day) % 7
    }

    pub fn dow(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        let mut year = 0_u32;
        let mut month = 0_u32;
        let mut day = 0_u32;

        match operands.len() {
            0 => return Err(ScriptError::InsufficientOperands(*opr_mark)),
            1 => {
                let mut dummy = ValueType::Empty;

                match gregorian_date_item(opr_mark, &mut dummy, operands, shuttle, DateInfoItem::Year) {
                    Ok(date_item) => {
                        year = date_item.get_info(&DateInfoItem::Year) as u32;
                        month = date_item.get_info(&DateInfoItem::Month) as u32;
                        day = date_item.get_info(&DateInfoItem::Day) as u32;
                    },
                    Err(se) => return Err(se),
                }
            },
            2 => return Err(ScriptError::InsufficientOperands(*opr_mark)),
            _ =>  {
                for (count, opd) in operands.iter().enumerate() {
                    match opd.get_value() {
                        ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                        ValueType::Number(n) => if n >= 0_f64 {
                            match count {
                                0 => year = n as u32,
                                1 => month = n as u32,
                                2 => day = n as u32,
                                _ => (),
                            }
                        } else {
                            return Err(ScriptError::UnexpectedNegativeOperand(*opr_mark));
                        },
                        ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                        ValueType::Error(s_err) => return Err(s_err),
                        ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
                    }
                }

                if (month) == 0 || (month > 12) {
                    return Err(ScriptError::InvalidDatePart(*opr_mark));
                }

                if (day == 0) || (day > 31) {
                    return Err(ScriptError::InvalidDatePart(*opr_mark));
                }
            },
        }

        /*
        #[cfg(test)]
        println!("YMD: {}-{}-{}", year, month, day);
        */

        let day_of_week = day_of_week(year, month, day);
        *result_value = ValueType::Number(day_of_week as f64);

        Ok(())
    }

    pub fn gregorian_seq(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 3)?;
        
        let mut year_float = 0_f64;
        let mut year = 0_u32;
        let mut month = 0_u32;
        let mut day = 0_u32;

        for (count, opd) in operands.iter().enumerate() {
            match opd.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(n) => if n >= 0_f64 {
                    match count {
                        0 => {
                            year_float = n;
                            year = n as u32;
                        },
                        1 => month = n as u32,
                        2 => day = n as u32,
                        _ => (),
                    }
                } else {
                    return Err(ScriptError::UnexpectedNegativeOperand(*opr_mark));
                },
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        if (month) == 0 || (month > 12) {
            return Err(ScriptError::InvalidDatePart(*opr_mark));
        }

        if (day == 0) || (day > 31) {
            return Err(ScriptError::InvalidDatePart(*opr_mark));
        }

        // One leap year
        let days_in_four_years = (DAYS_IN_YEAR * 4) + 1;

        // The first year isn't a leap year.
        let days_in_century = (days_in_four_years * 25) - 1;

        // The first year is a leap year again.
        let days_in_four_centuries = (days_in_century * 4) + 1;

        let mut result = if year > 0 {
            366
        } else {
            0
        };

        let mut years_counted = 0;
        let mut year_tested: u32;

        let mut year_increment = 400;

        loop {
            year_tested = years_counted + year_increment;

            if year_tested < year {
                result += days_in_four_centuries;
                years_counted = year_tested;
            } else {
                break;
            }

            /*
            #[cfg(test)]
            println!("years_counted: {}", years_counted);
            */
        }

        year_increment = 100;

        loop {
            year_tested = years_counted + year_increment;

            if year_tested < year {
                result += days_in_century;
                years_counted = year_tested;
            } else {
                break;
            }

            /*
            #[cfg(test)]
            println!("years_counted: {}", years_counted);
            */
        }

        year_increment = 4;

        loop {
            year_tested = years_counted + year_increment;

            if year_tested < year {
                result += days_in_four_years;
                years_counted = year_tested;
            } else {
                break;
            }

            /*
            #[cfg(test)]
            println!("years_counted: {}", years_counted);
            */
        }

        year_increment = 1;

        loop {
            year_tested = years_counted + year_increment;

            if year_tested < year {
                result += DAYS_IN_YEAR;
                years_counted = year_tested;
            } else {
                break;
            }

            /*
            #[cfg(test)]
            println!("years_counted: {}", years_counted);
            */
        }

        let is_leap_year = is_leap_year(year_float);
        let mut months_counted = 0;

        loop {
            months_counted += 1;

            if months_counted < month {
                result += days_in_month(months_counted, is_leap_year);
            } else {
                break;
            }
        }

        result += day;

        *result_value = ValueType::Number(result as f64);

        Ok(())
    }

    pub(crate) fn greg_sequence_to_date(seq_nr: f64) -> Result<GregorianDateInfo, String> {
        if seq_nr < 1_f64 {
            return Err("greg_sequence_to_date: sequence number should be > 0.".to_string());
        }

        // One leap year
        // ... but not always !
        let days_in_four_years_no_leap = DAYS_IN_YEAR * 4;
        let days_in_four_years = days_in_four_years_no_leap + 1;

        // The first year isn't a leap year.
        let days_in_century = (days_in_four_years * 25) - 1;

        // The first year is a leap year again.
        let days_in_four_centuries = (days_in_century * 4) + 1;

        let days_in_leap_century = days_in_century + 1;
        let days_in_leap_year = DAYS_IN_YEAR + 1;

        /*
        #[cfg(test)]
        {
            println!("days in four years no leap: {}", days_in_four_years_no_leap);
            println!("days in four years: {}", days_in_four_years);
            println!("days in century: {}", days_in_century);
            println!("days in leap century: {}", days_in_leap_century);
            println!("days in four centuries: {}", days_in_four_centuries);
        }
        */

        let mut seq = seq_nr as u32;
        let mut year = 0;
        let mut month: u32;
        let day: u32;

        /*
        #[cfg(test)]
        println!("Starting from seq {}", seq);
        */

        let mut count = seq / days_in_four_centuries;
        year += 400 * count;
        seq %= days_in_four_centuries;

        /*
        #[cfg(test)]
        println!("- days in 4 centuries:    seq={}, year={}", seq, year);
        */

        if is_leap_year(year as f64) && (seq >= days_in_leap_century) {
            seq -= days_in_leap_century;
            year += 100;
        }

        /*
        #[cfg(test)]
        println!("- 1st (leap)century:      seq={}, year={}", seq, year);
        */

        count = seq / days_in_century;
        year += 100 * count;
        seq %= days_in_century;

        /*
        #[cfg(test)]
        println!("- count centuries:        seq={}, year={}, count={}", seq, year, count);
        */

        if (seq >= days_in_four_years_no_leap) && !is_leap_year(year as f64) {
            year += 4;
            seq -= days_in_four_years_no_leap;
        }

        /*
        #[cfg(test)]
        println!("- 1st no-leap 4 years:    seq={}, year={}", seq, year);
        */

        count = seq / days_in_four_years;
        year += 4 * count;
        seq %= days_in_four_years;

        /*
        #[cfg(test)]
        println!("- count * 4 years:        seq={}, year={}, count={}", seq, year, count);
        */

        if is_leap_year(year as f64) && (seq >= days_in_leap_year) {
            seq -= days_in_leap_year;
            year += 1;
        }

        /*
        #[cfg(test)]
        println!("- 1st (leap)year:         seq={}, year={}", seq, year);
        */

        count = seq / DAYS_IN_YEAR;
        year += count;
        seq %= DAYS_IN_YEAR;

        /*
        #[cfg(test)]
        println!("- count years:            seq={}, year={}, count={}", seq, year, count);
        */

        if seq == 0 {
            year -= 1;
            month = 12;
            day = 31;
        } else {
            let is_leap = is_leap_year(year as f64);
            month = 1;
            let mut month_days: u32;

            loop {
                month_days = days_in_month(month, is_leap);

                if seq > month_days {
                    month += 1;

                    if month == 13 {
                        return Err("greg_sequence_to_date: month can never be 13.".to_string());
                    }

                    seq -= month_days;
                } else {
                    break;
                }
            }

            day = seq;
        }

        Ok(GregorianDateInfo {
            sequence_nr: seq_nr,
            year: year as f64,
            month: month as f64,
            day: day as f64,
        })
    }

    pub fn gregorian_date_item(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle, item: DateInfoItem) -> Result<GregorianDateInfo, ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        let seq_nr = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(num) => num,
            ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        let date_info_opt = match shuttle.last_calculated_gregorian_day {
            Some(ref date_info) =>
                if date_info.sequence_nr == seq_nr {
                    Some(date_info.clone())
                } else {
                    None
                },
            None => None,
        };

        #[cfg(test)]
        if shuttle.date_item_calculations == 255 {
            shuttle.date_item_calculations = 0;
        }

        let date_info = match date_info_opt {
            Some(di) => di,
            None => {

                #[cfg(test)]
                {
                    shuttle.date_item_calculations += 1;
                }

                match greg_sequence_to_date(seq_nr) {
                    Ok(date_info) => {
                        shuttle.last_calculated_gregorian_day = Some(date_info.clone());
                        date_info
                    },
                    Err(msg) => return Err(ScriptError::InvalidDateSequenceNumber(msg)),
                }
            }
        };

        *result_value = ValueType::Number(date_info.get_info(&item));

        Ok(date_info)
    }

    pub fn gregorian_year(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        match gregorian_date_item(opr_mark, result_value, operands, shuttle, DateInfoItem::Year) {
            Ok(_) => Ok(()),
            Err(se) => Err(se),
        }
    }

    pub fn gregorian_month(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        match gregorian_date_item(opr_mark, result_value, operands, shuttle, DateInfoItem::Month) {
            Ok(_) => Ok(()),
            Err(se) => Err(se),
        }
    }

    pub fn gregorian_day(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        match gregorian_date_item(opr_mark, result_value, operands, shuttle, DateInfoItem::Day) {
            Ok(_) => Ok(()),
            Err(se) => Err(se),
        }
    }

    pub fn gregorian_text(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        let sepa = if operands.len() > 1 {
            match operands[1].get_value() {
                ValueType::Empty => String::new(),
                ValueType::Number(_) => return Err(ScriptError::NonTextOperand(*opr_mark)),
                ValueType::Text(t) => t,
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        } else {
            String::new()
        };

        let mut dummy = ValueType::Empty;

        match gregorian_date_item(opr_mark, &mut dummy, operands, shuttle, DateInfoItem::Year) {
            Ok(date_info) => {
                let year = date_info.get_info(&DateInfoItem::Year);
                let month = date_info.get_info(&DateInfoItem::Month);
                let day = date_info.get_info(&DateInfoItem::Day);
                let weekday = day_of_week(year as u32, month as u32, day as u32) as usize;

                *result_value = ValueType::Text(format!(
                    "{:04}{}{:02}{}{:02}{}{}",
                    year,
                    sepa.clone(),
                    month,
                    sepa,
                    day,
                    sepa,
                    WEEKDAYS[weekday]
                ));

                Ok(())
            },
            Err(se) => Err(se),
        }
    }

    pub fn get_unicode_value(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        let characters: Vec::<char> = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Text(txt) => txt.chars().collect(),
            ValueType::Number(num) => shuttle.number_format.format(num).chars().collect(),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        let pos = if operands.len() < 2 {
            0_usize
        } else {
            match operands[1].get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => num as usize,
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        };

        if pos >= characters.len() {
            return Err(ScriptError::PositionPastLastPossible(pos));
        }

        *result_value = ValueType::Number(characters[pos] as u32 as f64);

        Ok(())
    }

    fn find_in_string_base(characters: &[char], find_characters: &[char], start_pos: usize) -> Option<usize> {
        let source_len = characters.len();
        let find_len = find_characters.len();

        if (find_len == 0_usize) || (source_len == 0_usize) {
            return None;
        }

        if find_len > source_len {
            return None;
        }

        let last_start_pos = source_len - find_len;

        if start_pos > last_start_pos {
            return None;
        }

        for compare_start_pos in start_pos..=last_start_pos {
            let mut matches = true;

            for (compare_find_pos, comp_char) in characters.iter().skip(compare_start_pos).take(find_len).enumerate() {
                matches = *comp_char == find_characters[compare_find_pos];

                if !matches {
                    break;
                }
            }

            if matches {
                return Some(compare_start_pos);
            }
        }

        None
    }

    pub fn find_in_string(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let characters: Vec::<char> = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Text(txt) => txt.chars().collect(),
            ValueType::Number(num) => shuttle.number_format.format(num).chars().collect(),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        let source_len = characters.len();

        let find_characters: Vec<char> = match operands[1].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Text(txt) => txt.chars().collect(),
            ValueType::Number(num) => shuttle.number_format.format(num).chars().collect(),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        let find_len = find_characters.len();

        if find_len > source_len {
            return Err(ScriptError::FindStringLongerThanSourceString(*opr_mark));
        }

        let start_pos = if operands.len() >= 3 {
            match operands[2].get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => num as usize,
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        } else {
            0_usize
        };

        let last_start_pos = source_len - find_len;

        if start_pos > last_start_pos {
            return Err(ScriptError::PositionPastLastPossible(start_pos));
        }

        if (find_len == 0_usize) || (source_len == 0_usize) {
            *result_value = ValueType::Empty;

            return Ok(());
        }

        *result_value = match find_in_string_base(&characters, &find_characters, start_pos) {
            Some(pos) => ValueType::Number(pos as f64),
            None => ValueType::Empty,
        };

        Ok(())
    }

    pub fn replace_in_string(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 3)?;

        let use_condition = operands.len() > 3;

        if use_condition && (operands.len() < 6) {
            check_nr_operands(opr_mark, operands, 6)?;
        }

        let characters: Vec<char> = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Text(txt) => txt.chars().collect(),
            ValueType::Number(num) => shuttle.number_format.format(num).chars().collect(),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        let source_len = characters.len();

        let find_characters: Vec<char> = match operands[1].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Text(txt) => txt.chars().collect(),
            ValueType::Number(num) => shuttle.number_format.format(num).chars().collect(),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        let find_len = find_characters.len();

        if find_len > source_len {
            return Err(ScriptError::FindStringLongerThanSourceString(*opr_mark));
        }

        let repl_characters: Vec<char> = match operands[2].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Text(txt) => txt.chars().collect(),
            ValueType::Number(num) => shuttle.number_format.format(num).chars().collect(),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        let repl_len = repl_characters.len();

        let pos_var = if use_condition {
            match operands[3].get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Text(content) => ValueType::Text(content),
                ValueType::Number(content) => ValueType::Number(content),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        } else {
            ValueType::Empty
        };

        let seq_var = if use_condition {
            match operands[4].get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Text(content) => ValueType::Text(content),
                ValueType::Number(content) => ValueType::Number(content),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        } else {
            ValueType::Empty
        };

        // Returns empty value.
        let empty_routine = Routine{
            body: Expression::new(OPEMP, 0),
            in_new_variables_scope: true,
        };

        let mut found_routine = if use_condition {
            let name = match operands[5].get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Text(content) => ValueType::Text(content),
                ValueType::Number(content) => ValueType::Number(content),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            };

            if let Some(rout) = shuttle.routines.get(&name) {
                rout.clone()
            } else {
                empty_routine.clone()
            }
        } else {
            empty_routine.clone()
        };

        let mut result_characters = characters.clone();
        let mut start_pos = 0_usize;
        let mut orig_pos = 0_usize;
        let mut count = 0f64;

        while let Some(found_pos) = find_in_string_base(&result_characters, &find_characters, start_pos) {
            let end_pos = found_pos + find_len;
            orig_pos += found_pos - start_pos;

            let do_it = if use_condition {
                shuttle.set_var(pos_var.clone(), ValueType::Number(orig_pos as f64));
                shuttle.set_var(seq_var.clone(), ValueType::Number(count));
                found_routine.body.operate(shuttle)?;

                found_routine.body.get_num_value(0_f64)
            } else {
                1_f64
            };

            if do_it != 0_f64 {
                result_characters.splice(found_pos..end_pos, repl_characters.clone());
                start_pos += repl_len;
            } else {
                start_pos += find_len;
            }

            orig_pos += find_len;
            count += 1_f64;
        }

        *result_value = ValueType::Text(result_characters.iter().collect());

        Ok(())
    }

    pub fn split(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 3)?;

        let mut source = "".to_string();
        let mut separator = "".to_string();
        let mut name_prefix = "".to_string();

        for (count, opd) in operands.iter().enumerate() {
            match opd.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Text(txt) => {
                    match count {
                        0 => source = txt,
                        1 => separator = txt,
                        2 => name_prefix = txt,
                        _ => (),
                    }
                },
                ValueType::Number(_) => return Err(ScriptError::NonTextOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        let is_separator_empty = separator.is_empty();

        let mut count = 0;

        for fragment in source.split(separator.as_str()) {
            if !((fragment.is_empty()) && is_separator_empty) {
                shuttle.set_var(
                    ValueType::Text(format!("{name_prefix}{count}")),
                    ValueType::Text(fragment.to_string())
                );

                count += 1;
            }
        }

        *result_value = ValueType::Number(count as f64);

        Ok(())
    }

    pub fn substring(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let characters: Vec::<char> = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Text(txt) => txt.chars().collect(),
            ValueType::Number(num) => shuttle.number_format.format(num).chars().collect(),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        let start_pos = match operands[1].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(num) => {
                if num < 0_f64 {
                    return Err(ScriptError::UnexpectedNegativeOperand(*opr_mark));
                } else {
                    num as usize
                }
            }, 
            ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        if start_pos > characters.len() {
            return Err(ScriptError::PositionPastLastPossible(start_pos));
        }

        let sub_len = if operands.len() >= 3 {
            match operands[2].get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => {
                    if num < 0_f64 {
                        return Err(ScriptError::UnexpectedNegativeOperand(*opr_mark));
                    } else {
                        num as usize
                    }
                }, 
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        } else {
            characters.len() - start_pos
        };

        let end_pos_excl = start_pos + sub_len;

        if end_pos_excl > characters.len() {
            return Err(ScriptError::PositionPastLastPossible(end_pos_excl));
        }

        *result_value = ValueType::Text(characters[start_pos..end_pos_excl].iter().collect());

        Ok(())
    }

    pub fn lower(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        *result_value = ValueType::Text(match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Text(txt) => txt,
            ValueType::Number(num) => shuttle.number_format.format(num),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        }.to_lowercase());

        Ok(())
    }

    pub fn upper(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        *result_value = ValueType::Text(match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Text(txt) => txt,
            ValueType::Number(num) => shuttle.number_format.format(num),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        }.to_uppercase());

        Ok(())
    }

    pub fn proper(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        let mut result = String::new();
        let mut is_new_word = true;

        for c in match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Text(txt) => txt,
            ValueType::Number(num) => shuttle.number_format.format(num),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        }.chars(){
            let new_chars: Box<dyn Iterator<Item = char>> =  
                if is_new_word {
                    Box::new(c.to_uppercase())
                } else {
                    Box::new(c.to_lowercase())
                };

            for newc in new_chars {
                result.push(newc);
            }

            is_new_word =  c.is_whitespace();
        }

        *result_value = ValueType::Text(result);

        Ok(())
    }

    pub fn get_type(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;
        *result_value = ValueType::Number(operands[0].get_value().get_type_as_num());

        Ok(())
    }

    pub fn input_base(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        *result_value = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(num) => {
                let num_val = num.trunc();

                if (num_val > 1f64) && (num_val < f64::INFINITY) {
                    shuttle.input_base = num_val;
                } else {
                    return Err(ScriptError::InvalidNumberBase(num_val));
                }

                ValueType::Number(shuttle.input_base)
            },
            ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        Ok(())
    }

    pub fn output_base(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        *result_value = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(num) => {
                let num_val = num.trunc();

                if (num_val > 1f64) && (num_val < f64::INFINITY) {
                    shuttle.number_format.set_base(num_val);
                } else {
                    return Err(ScriptError::InvalidNumberBase(num_val));
                }

                ValueType::Number(shuttle.input_base)
            },
            ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        Ok(())
    }
    
    pub fn write(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;
        let mut tot_written = 0_usize;

        for op in operands {
            match shuttle.writer.write(
                (match op.get_value() {
                    ValueType::Empty => NO_VALUE.to_string(),   
                    ValueType::Number(num) => shuttle.number_format.format(num),
                    ValueType::Text(txt) => txt,
                    ValueType::Error(s_err) => format!("{s_err:?}"),
                    ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
                }).as_bytes()
            ) {
                Ok(nr_bytes) => tot_written += nr_bytes,
                Err(io_err) => return Err(ScriptError::WriteFailure(io_err.to_string())),
            }
        }

        let _ = shuttle.writer.flush();
        
        *result_value = ValueType::Number(tot_written as f64);

        Ok(())
    }

    pub fn eval(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        /*
        #[cfg(test)]
        println!("fn eval: opr_mark={}, operands[0]={:?} ", *opr_mark, operands[0]);
        */

        // Only the first operand is used; subsequent ones are completely ignored.
        match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(num) => *result_value = ValueType::Number(num),
            ValueType::Text(program) => {
                match Interpreter::split_atoms(&program) {
                    Ok(atoms) =>  {
                        match Interpreter::make_tree(atoms) {
                            Ok(mut tree) => {
                                tree.operate(shuttle)?;
                                *result_value = tree.get_value();
                            },
                            Err(s_err) => if shuttle.error_breaks {
                                return Err(s_err);
                            } else {
                                *result_value = ValueType::Error(s_err);
                            },
                        }
                    },
                    Err(s_err) => if shuttle.error_breaks {
                        return Err(s_err);
                    } else {
                        *result_value = ValueType::Error(s_err);
                    },
                }
            },
            ValueType::Error(s_err) => *result_value = ValueType::Error(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        }
        
        Ok(())
    }

    pub fn read(_opr_mark: &mut char, result_value: &mut ValueType, _operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        let input_string = match shuttle.reader.read_line() {
            Some(s) => s.replace("\n", "").replace("\r", ""),
            None => String::new(),
        };
        
        *result_value = match parse_number(&input_string, shuttle.input_base, false) {
            Ok(num) => ValueType::Number(num),
            Err(_) => ValueType::Text(input_string),
        };

        Ok(())
    }

    pub fn read_file(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Text(path) => {
                let os_path = OsStr::new(path.as_str());

                match shuttle.text_io_handler.read_text(os_path) {
                    Ok(content) => *result_value = ValueType::Text(content),
                    Err(msg) => return Err(ScriptError::FileReadFailure{ path, reason: msg.to_string()}),
                }
            },
            ValueType::Number(_) => return Err(ScriptError::NonTextOperand(*opr_mark)),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        }

        Ok(())
    }

    pub fn write_file(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let content = match operands[1].get_value() {
            ValueType::Empty => NO_VALUE.to_string(),
            ValueType::Number(num) => shuttle.number_format.format(num),
            ValueType::Text(txt) => txt,
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        let len = content.len();

        match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(_) => return Err(ScriptError::NonTextOperand(*opr_mark)),
            ValueType::Text(path) => {
                let os_path = OsStr::new(path.as_str());

                match shuttle.text_io_handler.write_text(os_path, content) {
                    Ok(_) => *result_value = ValueType::Number(len as f64),
                    Err(msg) => return Err(ScriptError::FileReadFailure{path, reason: msg.to_string()}),
                }
            },
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        }

        Ok(())
    }

    pub fn to_number(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        // Only read operands[0]; ignore further operands.
        *result_value = match operands[0].get_value() {
            ValueType::Empty => ValueType::Number(0_f64),
            ValueType::Number(n) => ValueType::Number(n),
            ValueType::Text(t) => {
                match parse_number(&t, shuttle.input_base, false) {
                    Ok(n) => ValueType::Number(n),
                    Err(err) => return Err(ScriptError::NumberParsingFailure(err.clone())),
                }
            },
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        Ok(())
    }

    pub fn preceding_nr_operands(_opr_mark: &mut char, result_value: &mut ValueType, _operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Number(shuttle.preceding_nr_operands);

        Ok(())
    }

    pub fn exec_while(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut outcome = 0f64;
        let mut op_count: usize;
        let op_len = operands.len();
        let mut iter_count = 0f64;
        let mut op: &mut Expression;

        'outer: loop {

            if (shuttle.max_iterations > 0f64) && (iter_count >= shuttle.max_iterations) {
                break;
            }

            op_count = 0;

            while op_count < op_len {
                op = &mut operands[op_count];

                match op_count {
                    0 =>  {
                        op.operate(shuttle)?;

                        if are_near(0f64, op.get_num_value(0f64), shuttle.orb) {
                                    break 'outer;
                        }
                    },
                    _ => {
                        iter_count += 1f64;
                        op.operate(shuttle)?;
                        outcome = op.get_num_value(0f64);

                        if shuttle.break_target > 0_u32 {
                            break 'outer;
                        }
                    },
                }

                op_count += 1;
            }
        }

        if shuttle.break_target > 0_u32 {
            shuttle.break_target -= 1_u32;
        }

        shuttle.preceding_nr_operands = iter_count;

        *result_value = ValueType::Number(outcome);

        Ok(())
    }
    
    pub fn exec_for(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 5)?;

        let mut outcome = 0f64;
        let mut counter_val = 0f64;
        let mut end_val = 0f64;
        let mut increment = 1f64;
        let mut counter_var = ValueType::Number(0f64);
        let mut iter_count = 0f64;

        if operands.len() < 5 {
            *result_value = ValueType::Number(outcome);

            return Err(ScriptError::InsufficientOperands(*opr_mark));
        }

        for (op_count, op) in operands[0..=3].iter_mut().enumerate() {
            if op_count <= 3 {
                op.operate(shuttle)?;
            }

            match op.get_value() {
                ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                ValueType::Number(num) => match op_count {
                    0 => counter_val = num,
                    1 => end_val = num,
                    2 => increment = num,
                    3 => counter_var = ValueType::Number(num),
                    _ => (),
                },
                ValueType::Text(txt) if op_count == 3 => counter_var = ValueType::Text(txt),
                ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
                ValueType::Error(s_err) => return Err(s_err),
                ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
            }
        }

        let ascending = counter_val <= end_val;
        let op_len = operands.len();
        let mut op_count: usize;
        let mut op: &mut Expression;

        'outer: loop {
            if (shuttle.max_iterations > 0f64) && (iter_count >= shuttle.max_iterations) {
                break;
            }

            if  (ascending && (counter_val > end_val)) ||
                ((!ascending) && (counter_val < end_val)) {
                    break;
            }

            shuttle.set_var(counter_var.clone(), ValueType::Number(counter_val));
            op_count = 4;

            iter_count += 1f64;

            while op_count < op_len {
                op = &mut operands[op_count];
                op.operate(shuttle)?;

                if op_count + 1 == op_len {
                    outcome = op.get_num_value(0f64);
                }

                if shuttle.break_target > 0_u32 {
                    break 'outer;
                }

                op_count += 1;
            }

            if ascending {
                counter_val += increment;
            } else {
                counter_val -= increment;
            }
        }

        if shuttle.break_target > 0_u32 {
            shuttle.break_target -= 1_u32;
        }

        shuttle.preceding_nr_operands = iter_count;

        *result_value = ValueType::Number(outcome);

        Ok(())
    }

    pub fn set_break(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        let target = match operands[0].get_value() {
            ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
            ValueType::Number(num) => num ,
            ValueType::Text(_) => return Err(ScriptError::NonNumericOperand(*opr_mark)),
            ValueType::Error(s_err) => return Err(s_err),
            ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
        };

        if target < 0_f64 {
            return Err(ScriptError::UnexpectedNegativeOperand(*opr_mark));
        }

        shuttle.break_target = target as u32;

        *result_value = ValueType::Number(target);

        Ok(())
    }

    pub fn exec_if(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 3)?;

        let mut outcome = ValueType::Empty;
        let mut use_second = true;

        for op_tuple in operands.iter_mut().enumerate() {
            match op_tuple {
                (0, op) => {
                    op.operate(shuttle)?;
                    use_second = !are_near(0f64, op.get_num_value(0f64), shuttle.orb);
                },
                (1, op) if use_second => {
                    op.operate(shuttle)?;
                    outcome = op.get_value();
                },
                (1, _) => (),
                (_, op) if !use_second => {
                    op.operate(shuttle)?;
                    outcome = op.get_value();
                }
                _ => (),
            }
        }

        *result_value = outcome;

        Ok(())
    }

    pub fn exec_try(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let error_breaks_status_found = shuttle.error_breaks;
        shuttle.error_breaks = false;

        let had_success = match operands[0].operate(shuttle) {
            Ok(_) =>  {
                shuttle.try_outcome_stack.push(operands[0].get_value());

                match operands[0].get_value() {
                    ValueType::Empty => true,
                    ValueType::Number(_) => true,
                    ValueType::Text(_) => true,
                    ValueType::Error(_) => false,
                    ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
                }
            },
            Err(s_err) => {
                shuttle.try_outcome_stack.push(ValueType::Error(s_err));

                false
            },
        };

        shuttle.error_breaks = error_breaks_status_found;

        let op_to_execute = if had_success {
            if operands.len() >= 3 {
                2
            } else {
                0
            }
        } else {
            1
        };

        let op_result = if op_to_execute > 0 {
            operands[op_to_execute].operate(shuttle)
        } else {
            Ok(())
        };

        if !shuttle.try_outcome_stack.is_empty() {
            shuttle.try_outcome_stack.pop();
        }

        op_result?;

        *result_value = operands[op_to_execute].get_value();

        Ok(())
    }

    pub fn try_value(_opr_mark: &mut char, result_value: &mut ValueType, _operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        let stack_len = shuttle.try_outcome_stack.len();

        *result_value = if stack_len == 0 {
            ValueType::Empty
        } else {
            // Not using shuttle.try_outcome_stack.last, as that would necessitate an .expect() call.
            shuttle.try_outcome_stack[stack_len - 1].clone()
        };

        Ok(())
    }
    
    pub fn define_routine_sharing_variables(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        define_routine(opr_mark, result_value, operands, shuttle, false)?;

        Ok(())
    }
    
    pub fn define_routine_with_new_scope(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        define_routine(opr_mark, result_value, operands, shuttle, true)?;

        Ok(())
    }

    fn define_routine(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle, in_new_variables_scope: bool) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 2)?;

        let mut name = ValueType::Empty;
        let mut expressions = Vec::<Expression>::new();

        for op_tuple in operands.iter_mut().enumerate() {
            match op_tuple {
                (0, op) => {
                    op.operate(shuttle)?;
                    name = match op.get_value() {
                        ValueType::Empty => return Err(ScriptError::EmptyOperand(*opr_mark)),
                        ValueType::Number(num) => ValueType::Number(num),
                        ValueType::Text(txt) => ValueType::Text(txt),
                        ValueType::Error(s_err) => return Err(s_err),
                        ValueType::Max => return Err(ScriptError::InvalidOperandMax(*opr_mark)),
                    };
                },
                (_, op) => {
                    expressions.push(op.clone());
                },
            }
        }

        let body = match expressions.len() {
            0 => Expression::new(OPEMP, 0), // Should never occur after test check_nr_operands above
            1 => expressions.pop().unwrap_or(Expression::new(OPEMP, 0)),
            _ => {
                let mut combinator = Expression::new(OPCMB, 0);

                for expr in expressions.into_iter() {
                    combinator.operands.push(expr);
                }

                combinator
            },
        };

        shuttle.routines.insert(name.clone(), Routine{body, in_new_variables_scope,});
        *result_value = name;

        Ok(())
    }

    pub fn exec_routine_base(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle, reverse: bool) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        *result_value = ValueType::Empty;

        /*
        // Returns empty value.
        let mut found_routine = Routine{
            body: Expression::new(OPEMP, 0),
            in_new_variables_scope: true,
        };
        */
        
        let mut found_routine: Routine;

        let name = operands[0].get_value();

        if let Some(expr) = shuttle.routines.get(&name) {
            found_routine = expr.clone();
        } else {
            return Err(ScriptError::UnknownRoutine(
                name.get_string_value("???".to_string())));
        }

        if reverse {
            for op in operands[1..].iter().rev() {
                shuttle.stack.push(op.get_value());
            }
        } else {
            for op in operands[1..].iter() {
                shuttle.stack.push(op.get_value());
            }
        }

        if found_routine.in_new_variables_scope {
            shuttle.nums.push(HashMap::new());
        }

        shuttle.routine_name_stack.push(name);

        let call_result =  found_routine.body.operate(shuttle);

        if found_routine.in_new_variables_scope {
            shuttle.nums.pop().unwrap_or_default();
        }

        shuttle.routine_name_stack.pop().unwrap_or_default();

        match call_result {
            Ok(_) => (),
            Err(s_err) => return Err(s_err),
        }

        *result_value = found_routine.body.get_value();

        Ok(())
    }

    pub fn exec_routine(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        exec_routine_base(opr_mark, result_value, operands, shuttle, false)
    }

    pub fn exec_routine_reverse(opr_mark: &mut char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        exec_routine_base(opr_mark, result_value, operands, shuttle, true)
    }

    pub fn user_error(opr_mark: &mut char, _result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        check_nr_operands(opr_mark, operands, 1)?;

        match operands[0].get_value() {
            ValueType::Empty => Err(ScriptError::UserDefinedError("(no reason)".to_string())),
            ValueType::Number(num) => Err(ScriptError::UserDefinedError(shuttle.number_format.format(num))),
            ValueType::Text(msg) => Err(ScriptError::UserDefinedError(msg.clone())),
            ValueType::Error(s_err) => Err(s_err),
            ValueType::Max => Err(ScriptError::InvalidOperandMax(*opr_mark)),
        }
    }
}

#[cfg(test)]
mod tests {
    mod number_format {
        use crate::NumberFormat;

        #[test]
        fn nf_base10_zero() {
            let nf = NumberFormat::new();
            assert_eq!("0.000000".to_string(), nf.format(0f64));
        }

        #[test]
        fn nf_base10_minus_zero() {
            let nf = NumberFormat::new();
            assert_eq!("0.000000".to_string(), nf.format(-0f64));
        }

        #[test]
        fn nf_base10_pos_int() {
            let nf = NumberFormat::new();
            assert_eq!("529.000000".to_string(), nf.format(529f64));
        }

        #[test]
        fn nf_base10_pos_fract_only() {
            let nf = NumberFormat::new();
            assert_eq!("0.529000".to_string(), nf.format(0.529f64));
        }

        #[test]
        fn nf_base10_pos_mixed() {
            let nf = NumberFormat::new();
            assert_eq!("387.529000".to_string(), nf.format(387.529f64));
        }

        #[test]
        fn nf_base10_neg_mixed() {
            let nf = NumberFormat::new();
            assert_eq!("-387.529000".to_string(), nf.format(-387.529f64));
        }

        #[test]
        fn nf_base10_pos_mixed_required_digits() {
            let mut nf = NumberFormat::new();
            nf.set_fractal_digits(3f64);
            assert_eq!("387.529".to_string(), nf.format(387.529f64));
        }

        #[test]
        fn nf_base10_more_digits() {
            let mut nf = NumberFormat::new();
            nf.set_fractal_digits(3f64);
            assert_eq!("387.523".to_string(), nf.format(387.5227f64));
        }

        #[test]
        fn nf_base10_pos_int_less_required_digits() {
            let mut nf = NumberFormat::new();
            nf.set_fractal_digits(3f64);
            assert_eq!("4387.000".to_string(), nf.format(4387f64));
        }

        #[test]
        fn nf_base2_pos_mixed() {
            let mut nf = NumberFormat::new();
            nf.set_base(2f64);
            nf.set_fractal_digits(6f64);
            assert_eq!("1011.011000".to_string(), nf.format(11.375f64));
        }

        #[test]
        fn nf_base5_neg_mixed() {
            let mut nf = NumberFormat::new();
            nf.set_base(5f64);
            nf.set_fractal_digits(6f64);
            assert_eq!("-102.100000".to_string(), nf.format(-27.2f64));
        }

        #[test]
        fn nf_base16_pos_int() {
            let mut nf = NumberFormat::new();
            nf.set_base(16f64);
            nf.set_fractal_digits(0f64);
            assert_eq!("E20D".to_string(), nf.format(57869f64));
        }

        #[test]
        fn nf_base16_neg_mixed() {
            let mut nf = NumberFormat::new();
            nf.set_base(16f64);
            nf.set_fractal_digits(6f64);
            assert_eq!("-12C.C00000".to_string(), nf.format(-300.75f64));
        }

        #[test]
        fn nf_base36_mixed() {
            let mut nf = NumberFormat::new();
            nf.set_base(36f64);
            nf.set_fractal_digits(6f64);
            assert_eq!("BK.100000".to_string(), nf.format(416.027777778f64));
        }

        #[test]
        fn nf_base100_mixed() {
            let mut nf = NumberFormat::new();
            nf.set_base(100f64);
            nf.set_fractal_digits(6f64);
            assert_eq!("7 25 8.7 8 0 0 0 0".to_string(), nf.format(72508.0708f64));
        }

        #[test]
        fn nf_base60_neg_mixed() {
            let mut nf = NumberFormat::new();
            nf.set_base(60f64);
            nf.set_fractal_digits(6f64);
            assert_eq!("-1 59.20 0 0 0 0 0".to_string(), nf.format(-119.33333333334));
        }

        #[test]
        fn nf_base1() {
            let mut nf = NumberFormat::new();
            nf.set_base(1f64);
            nf.set_fractal_digits(0f64);
            assert_eq!("42".to_string(), nf.format(42f64));
        }

        #[test]
        fn nf_base_fract() {
            let mut nf = NumberFormat::new();
            nf.set_base(3.5f64);
            nf.set_fractal_digits(0f64);
            assert_eq!("101".to_string(), nf.format(10f64));
        }

        #[test]
        fn nf_base_neg() {
            let mut nf = NumberFormat::new();
            nf.set_base(-3f64);
            nf.set_fractal_digits(0f64);
            assert_eq!("10".to_string(), nf.format(10f64));
        }

        #[test]
        fn nf_base10_thousands_separator() {
            let mut nf = NumberFormat::new();
            nf.set_fractal_digits(6f64);
            nf.set_use_thousands_separator(true);
            assert_eq!("43,500.000,000".to_string(), nf.format(43500f64));
        }

        #[test]
        fn nf_base10_neg_thousands_separator() {
            let mut nf = NumberFormat::new();
            nf.set_fractal_digits(0f64);
            nf.set_use_thousands_separator(true);
            assert_eq!("-100,774".to_string(), nf.format(-100774f64));
        }

        #[test]
        fn nf_base100_never_thousands_separator() {
            let mut nf = NumberFormat::new();
            nf.set_base(100f64);
            nf.set_fractal_digits(6f64);
            nf.set_use_thousands_separator(true);
            assert_eq!("4 35 0.0 0 0 0 0 0".to_string(), nf.format(43500f64));
        }
    }

    mod split {
        use crate::*;

        #[test]
        fn split_positive_integer() {
            let result = Interpreter::split_atoms("138");
            assert_eq!(Ok(vec![Atom::Number("138".to_string())]), result);
        }

        #[test]
        fn split_positive_integer_contains_underscore() {
            let result = Interpreter::split_atoms("1_38");
            assert_eq!(Ok(vec![Atom::Number("138".to_string())]), result);
        }

        #[test]
        fn split_positive_integer_starts_with_underscore() {
            let result = Interpreter::split_atoms("_138");
            assert_eq!(Ok(vec![Atom::Number("138".to_string())]), result);
        }

        #[test]
        fn split_positive_integer_ends_with_underscore() {
            let result = Interpreter::split_atoms("138_");
            assert_eq!(Ok(vec![Atom::Number("138".to_string())]), result);
        }

        #[test]
        fn split_positive_fractal_greater_than_1() {
            let result = Interpreter::split_atoms("22.6");
            assert_eq!(Ok(vec![Atom::Number("22.6".to_string())]), result);
        }

        #[test]
        fn split_positive_fractal_smaller_than_1_starting_with_dot() {
            let result = Interpreter::split_atoms(".922");
            assert_eq!(Ok(vec![Atom::Number(".922".to_string())]), result);
        }

        #[test]
        fn split_positive_fractal_smaller_than_1_starting_with_0() {
            let result = Interpreter::split_atoms("0.922");
            assert_eq!(Ok(vec![Atom::Number("0.922".to_string())]), result);
        }

        #[test]
        fn split_positive_fractal_smaller_than_1_starting_with_00() {
            let result = Interpreter::split_atoms("00.922");
            assert_eq!(Ok(vec![Atom::Number("00.922".to_string())]), result);
        }

        #[test]
        fn split_positive_fractal_greater_than_1_contains_underscores() {
            let result = Interpreter::split_atoms("_22_._6_");
            assert_eq!(Ok(vec![Atom::Number("22.6".to_string())]), result);
        }

        #[test]
        fn split_positive_integer_ends_with_dot() {
            let result = Interpreter::split_atoms("22.");
            assert_eq!(Ok(vec![Atom::Number("22.".to_string())]), result);
        }

        #[test]
        fn split_positive_integer_dot() {
            let result = Interpreter::split_atoms(".");
            assert_eq!(Ok(vec![Atom::Number(".".to_string())]), result);
        }

        #[test]
        fn split_two_numbers() {
            let result = Interpreter::split_atoms("741 _.60");
            assert_eq!(Ok(vec![Atom::Number("741".to_string()), Atom::Number(".60".to_string())]), result);
        }

        #[test]
        fn split_known_operator() {
            let result = Interpreter::split_atoms("*");
            assert_eq!(Ok(vec![Atom::Operator('*')]), result);
        }

        #[test]
        fn split_unknown_operator() {
            let result = Interpreter::split_atoms("'");
            assert_eq!(Err(ScriptError::UnknownOperator{position: 1, operator: '\''}), result);
        }

        #[test]
        fn split_mixed() {
            let result = Interpreter::split_atoms(";.11:+_5.2§Simple§?[cAnd now the second operand]9119 ~ 45 + 3 2[sAaand another string][nFF88]");

            assert_eq!(Ok(vec![
                Atom::Operator(';'),
                Atom::Number(".11".to_string()),
                Atom::Operator(':'),
                Atom::Operator('+'),
                Atom::Number("5.2".to_string()),
                Atom::String("Simple§?".to_string()),
                Atom::Comment("And now the second operand".to_string()),
                Atom::Number("9119".to_string()),
                Atom::Operator('~'),
                Atom::Number("45".to_string()),
                Atom::Operator('+'),
                Atom::Number("3".to_string()),
                Atom::Number("2".to_string()),
                Atom::String("Aaand another string".to_string()),
                Atom::Number("FF88".to_string()),
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
            assert_eq!(Err(ScriptError::UnknownBracketContentTypeMarker{position: 2, marker: 'x'}), result);
        }

        #[test]
        fn split_unexpected_closing_bracket() {
            let result = Interpreter::split_atoms("[cLaconic]$2 174]v2");
            assert_eq!(Err(ScriptError::UnexpectedClosingBracket{position: 17}), result);
        }

        #[test]
        fn split_unclosed_bracket() {
            let result = Interpreter::split_atoms("[cLaconic] [s $2 174 v2");
            assert_eq!(Err(ScriptError::UnclosedBracketsAtEnd), result);
        }

        #[test]
        fn split_unclosed_bracket_nested() {
            let result = Interpreter::split_atoms("[c [cLaconic] $2 174 v2");
            assert_eq!(Err(ScriptError::UnclosedBracketsAtEnd), result);
        }

        #[test]
        fn split_simple_string() {
            let result = Interpreter::split_atoms("§Chomsky!");
            assert_eq!(Ok(vec![Atom::String("Chomsky!".to_string())]), result);
        }

        #[test]
        fn split_simple_string_ends_with_whitespace() {
            let result = Interpreter::split_atoms("§Chomsky! 45");
            assert_eq!(Ok(vec![Atom::String("Chomsky!".to_string()), Atom::Number("45".to_string())]), result);
        }

        #[test]
        fn split_simple_string_ends_with_opening_bracket() {
            let result = Interpreter::split_atoms("§Arundhati[cTest]");
            assert_eq!(Ok(vec![Atom::String("Arundhati".to_string()), Atom::Comment("Test".to_string())]), result);
        }

        #[test]
        fn split_simple_string_ends_with_opening_parenthesis() {
            let result = Interpreter::split_atoms("§Arundhati(30)");
            assert_eq!(Ok(vec![Atom::String("Arundhati".to_string()), Atom::Operator('('), Atom::Number("30".to_string()), Atom::Operator(')')]), result);
        }

        #[test]
        fn split_simple_string_ends_with_closing_parenthesis() {
            let result = Interpreter::split_atoms("(§Arundhati)");
            assert_eq!(Ok(vec![Atom::Operator('('), Atom::String("Arundhati".to_string()), Atom::Operator(')')]), result);
        }

        #[test]
        fn split_simple_string_get_var() {
            let result = Interpreter::split_atoms("v§reg1");
            assert_eq!(Ok(vec![Atom::Operator('v'), Atom::String("reg1".to_string())]), result);
        }
    }

    mod value_type {
        use std::hash::{DefaultHasher, Hash, Hasher};
        use crate::ValueType;

        #[test]
        fn vt_hash_same_value() {
            let mut hasher1 = DefaultHasher::new();
            let mut hasher2 = DefaultHasher::new();

            let vt1 = ValueType::Text("reg2".to_string());
            let vt2 = ValueType::Text("reg2".to_string());

            vt1.hash(&mut hasher1);
            let h1 = hasher1.finish();

            vt2.hash(&mut hasher2);
            let h2 = hasher2.finish();

            // println!("Hashes : {:?} and {:?}", h1, h2);

            assert_eq!(h1, h2)
        }

        #[test]
        fn vt_hash_diff_value() {
            let mut hasher1 = DefaultHasher::new();
            let mut hasher2 = DefaultHasher::new();

            let vt1 = ValueType::Text("reg1".to_string());
            let vt2 = ValueType::Text("reg2".to_string());

            vt1.hash(&mut hasher1);
            let h1 = hasher1.finish();

            vt2.hash(&mut hasher2);
            let h2 = hasher2.finish();

            // println!("Hashes : {:?} and {:?}", h1, h2);

            assert_ne!(h1, h2)
        }
    }

    mod expr {
        use crate::*;
        use crate::input::MockByString;
        use string_io_and_mock::MockTextHandler;

        #[test]
        fn expr_new_number_get_value() {
            let mut exp = Expression::new_number("47.11".to_string());
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut shuttle = Shuttle::new(writer, reader, text_io_handler);
            let _ = exp.operate(&mut shuttle);
            assert_eq!(47.11f64, exp.get_num_value(0f64));

            // Check if we can get the value a second time.
            assert_eq!(47.11f64, exp.get_num_value(0f64));
        }

        #[test]
        fn expr_add_numbers() {
            let mut exp = Expression::new('+', 0);
            exp.push_operand(Expression::new_number("4000.3".to_string()));
            exp.push_operand(Expression::new_number("500.1".to_string()));

            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut shuttle = Shuttle::new(writer, reader, text_io_handler);
            let _ = exp.operate(&mut shuttle);

            // Fails due to precision error: right value is 4500.400000000001.
            // assert_eq!(4500.4f64, exp.get_num_value(0f64));

            assert!(are_very_near(4500.4f64, exp.get_num_value(0f64)));
        }

        #[test]
        fn expr_add_expressions() {
            let mut exp = Expression::new('+', 0);

            let mut op1 = Expression::new('+', 0);
            op1.push_operand(Expression::new_number("43".to_string()));
            op1.push_operand(Expression::new_number("500".to_string()));

            let mut op2 = Expression::new('+', 0);
            op2.push_operand(Expression::new_number("9.03".to_string()));
            op2.push_operand(Expression::new_number("22".to_string()));

            exp.push_operand(op1);
            exp.push_operand(op2);

            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut shuttle = Shuttle::new(writer, reader, text_io_handler);
            let _ = exp.operate(&mut shuttle);

            assert_eq!(574.03f64, exp.get_num_value(0f64));
        }

        #[test]
        fn expr_assign_num_reg_dot3() {
            let mut exp = Expression::new('$', 0);
            exp.push_operand(Expression::new_number("4000.3".to_string()));
            exp.push_operand(Expression::new_number("500.1".to_string()));

            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut shuttle = Shuttle::new(writer, reader, text_io_handler);
            let _ = exp.operate(&mut shuttle);

            match shuttle.get_var(&ValueType::Number(4000.3f64)) {
                ValueType::Number(num) => assert_eq!(500.1f64, num),
                _ => panic!("The value found in shuttle.nums should be ValueType::Number."),
            }
        }

        #[test]
        fn expr_set_orb() {
            let mut exp = Expression::new('Z', 0);
            exp.push_operand(Expression::new_text("prec".to_string()));
            exp.push_operand(Expression::new_number("0.001".to_string()));

            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut shuttle = Shuttle::new(writer, reader, text_io_handler);

            let _ = exp.operate(&mut shuttle);

            assert_eq!(0.001f64, shuttle.orb);
        }

        #[test]
        fn expr_set_max_iterations() {
            let mut exp = Expression::new('Z', 0);
            exp.push_operand(Expression::new_text("loops".to_string()));
            exp.push_operand(Expression::new_number("500".to_string()));

            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut shuttle = Shuttle::new(writer, reader, text_io_handler);

            let _ = exp.operate(&mut shuttle);

            assert_eq!(500f64, shuttle.max_iterations);
        }
    }

    mod ops {
        use crate::{Expression, Shuttle, ValueType};
        use crate::opr_funcs::*;
        use crate::input::MockByString;
        use string_io_and_mock::MockTextHandler;

        #[test]
        fn nop_doesnt_change_value() {
            let mut the_value = ValueType::Number(500f64);
            let mut ops = Vec::<Expression>::new();

            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut shuttle = Shuttle::new(writer, reader, text_io_handler);

            let _ = nop(&mut '0', &mut the_value, &mut ops, &mut shuttle);

            assert!(ops.is_empty());

            match the_value {
                ValueType::Number(n) => assert_eq!(500f64, n),
                _ => panic!("A ValueType::Number was expected, other variant was found."),
            }
        }

        #[test]
        fn op_const_golden_ratio() {
            let mut the_value = ValueType::Empty;
            let mut ops = vec![Expression::new_text("gold".to_string())];

            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut shuttle = Shuttle::new(writer, reader, text_io_handler);

            // Have the golden ratio retrieved twice from the shuttle.
            let _ = constants(&mut 'c', &mut the_value, &mut ops, &mut shuttle);
            let _ = constants(&mut 'c', &mut the_value, &mut ops, &mut shuttle);

            // Verify that the calculation only happened once.
            assert_eq!(1u8, shuttle.golden_ratio_calculations);
        }
    }

    mod exec {
        use crate::{ExecutionOutcome, Interpreter, NO_VALUE, ScriptError, ValueType, are_very_near};
        use crate::input::{MockByString, StdinReader};
        use crate::opr_funcs::{greg_sequence_to_date, is_leap_year};
        use std::ffi::OsStr;
        use string_io_and_mock::{MockTextHandler, TextIOHandler};

        #[test]
        fn x_nop_hex_case_insensitive() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("b16 =[n1A][n1a]".to_string()).unwrap().numeric_value())
        }

        #[test]
        fn x_remaining_stack_items_while_making_tree() {
            assert_eq!(16f64, Interpreter::new_and_execute_with_mocked_io("+ 44 1 * 8 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_nested_ops() {
            assert_eq!(97f64, Interpreter::new_and_execute_with_mocked_io("+4+90 3".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_add_3_op() {
            assert_eq!(229f64, Interpreter::new_and_execute_with_mocked_io("+(3 111 115)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_add_2_op() {
            assert_eq!(226f64, Interpreter::new_and_execute_with_mocked_io("+111 115".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_add_2_op_one_is_error() {
            assert_eq!("DivideByZero('/')", Interpreter::new_and_execute_with_mocked_io("Z§ign 1 +111 /1 0".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_add_variant_2_numbers() {
            assert_eq!(226f64, Interpreter::new_and_execute_with_mocked_io("+,111 115".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_add_1_op() {
            assert_eq!(Err(ScriptError::InsufficientOperands('+')), Interpreter::new_and_execute_with_mocked_io("+111".to_string()));
        }

        #[test]
        fn x_add_0_op() {
            assert_eq!(Err(ScriptError::InsufficientOperands('+')), Interpreter::new_and_execute_with_mocked_io("+".to_string()));
        }

        #[test]
        fn x_add_empty() {
            assert_eq!(Err(ScriptError::EmptyOperand('+')), Interpreter::new_and_execute_with_mocked_io("+€4".to_string()));
        }

        #[test]
        fn x_add_strings() {
            assert_eq!("23.750000 km.".to_string(), Interpreter::new_and_execute_with_mocked_io("+23.75 [s km.]".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_add_strings_truncating_numbers() {
            assert_eq!("23 km.".to_string(), Interpreter::new_and_execute_with_mocked_io("+,23.75 [s km.]".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_add_strings_3() {
            assert_eq!("Alexandra David-Neel".to_string(), Interpreter::new_and_execute_with_mocked_io("$10 [sAlexandra] $11 [sDavid-Neel] +(v10 [s ] v11)".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_add_strings_one_is_error() {
            assert_eq!("Result:DivideByZero('/')".to_string(), Interpreter::new_and_execute_with_mocked_io("Z§ign 1 + §Result: /38 0".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_minus_3_op() {
            assert_eq!(-9f64, Interpreter::new_and_execute_with_mocked_io("-(111 115 +3 2)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_minus_2_op() {
            assert_eq!(-4f64, Interpreter::new_and_execute_with_mocked_io("-111 115".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_minus_1_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('-')),
                Interpreter::new_and_execute_with_mocked_io("-111".to_string()));
        }

        #[test]
        fn x_minus_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('-')),
                Interpreter::new_and_execute_with_mocked_io("-".to_string()));
        }

        #[test]
        fn x_minus_string_no_stop() {
            assert_eq!(38_f64, Interpreter::new_and_execute_with_mocked_io("Z§ign 1 -4 /5 0 38".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_minus_string_stop_on_error() {
            assert_eq!(
                Err(ScriptError::DivideByZero('/')),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 0 -9 /5 0 44".to_string()));
        }

        #[test]
        fn x_minus_empty() {
            assert_eq!(Err(ScriptError::EmptyOperand('-')), Interpreter::new_and_execute_with_mocked_io("-€ 38".to_string()));
        }

        #[test]
        fn x_combine_3_op() {
            assert_eq!(54f64, Interpreter::new_and_execute_with_mocked_io(";(1 +7 3 54)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_combine_3_op_bis() {
            assert_eq!(10f64, Interpreter::new_and_execute_with_mocked_io(";(1 54 +7 3)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_combine_2_op() {
            assert_eq!(54f64, Interpreter::new_and_execute_with_mocked_io("; 1 54".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_combine_1_op() {
            assert_eq!(54f64, Interpreter::new_and_execute_with_mocked_io(";54".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_combine_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands(';')),
                Interpreter::new_and_execute_with_mocked_io(";".to_string()));
        }

        #[test]
        fn x_override_to_1() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('+')),
                Interpreter::new_and_execute_with_mocked_io("+- 50 +(2) 3".to_string()));
        }
        
        #[test]
        fn x_override_with_operator_operands() {
            assert_eq!(13f64, Interpreter::new_and_execute_with_mocked_io("$0 10?(=%8 6 2 +:0 3 +:0 4 57)v0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_nested_override() {
            assert_eq!(57f64, Interpreter::new_and_execute_with_mocked_io("+(- 50 +(2 1 1) 3 7 1)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_nested_override2() {
            assert_eq!(32f64, Interpreter::new_and_execute_with_mocked_io("*(2 +(3 2 3) 2)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_missing_end_of_override() {
            assert_eq!(41f64, Interpreter::new_and_execute_with_mocked_io("+ 50 -(2 3 7 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_missing_start_of_override() {
            assert_eq!(
                Err(ScriptError::UnexpectedClosingParenthesis),
                Interpreter::new_and_execute_with_mocked_io("+-50 2) 3".to_string()));
        }

        #[test]
        fn x_missing_start_of_override_after_expected_ops() {
            assert_eq!(
                Err(ScriptError::UnexpectedClosingParenthesis),
                Interpreter::new_and_execute_with_mocked_io("+-50 2 3)".to_string()));
        }

        #[test]
        fn x_multiply_3_op() {
            assert_eq!(210f64, Interpreter::new_and_execute_with_mocked_io("*(3 14 5)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_multiply_2_op() {
            assert_eq!(11100f64, Interpreter::new_and_execute_with_mocked_io("*111 100".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_multiply_1_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('*')),
                Interpreter::new_and_execute_with_mocked_io("*111".to_string()));
        }

        #[test]
        fn x_multiply_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('*')),
                Interpreter::new_and_execute_with_mocked_io("+ 10 *".to_string()));
        }

        #[test]
        fn x_multiply_no_stop_on_err() {
            assert_eq!("DivideByZero('/')", Interpreter::new_and_execute_with_mocked_io("Z§ign 1 *111 /8 0".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_multiply_stop_on_err() {
            assert_eq!(
                Err(ScriptError::DivideByZero('/')),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 0 *111 /8 0".to_string()));
        }

        #[test]
        fn x_multiply_empty() {
            assert_eq!(Err(ScriptError::EmptyOperand('*')), Interpreter::new_and_execute_with_mocked_io("* € 100".to_string()));
        }

        #[test]
        fn x_divide_3_op() {
            assert_eq!(0.5f64, Interpreter::new_and_execute_with_mocked_io("/(70 20 7)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_divide_2_op() {
            assert_eq!(3.5f64, Interpreter::new_and_execute_with_mocked_io("/70 20".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_divide_1_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('/')),
                Interpreter::new_and_execute_with_mocked_io("/70".to_string()));
        }

        #[test]
        fn x_divide_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('/')),
                Interpreter::new_and_execute_with_mocked_io("+5 /".to_string()));
        }

        #[test]
        fn x_divide_by_zero() {
            assert_eq!(
                Err(ScriptError::DivideByZero('/')),
                Interpreter::new_and_execute_with_mocked_io("/8 0".to_string()));
        }

        #[test]
        fn x_modulo_3_op_integer() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("%(70 20 3)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_modulo_2_op_integer() {
            assert_eq!(10f64, Interpreter::new_and_execute_with_mocked_io("%70 20".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_modulo_2_op_fractal() {
            assert!(are_very_near(0.7f64, Interpreter::new_and_execute_with_mocked_io("%70 3.3".to_string()).unwrap().numeric_value()));
        }

        #[test]
        fn x_modulo_1_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('%')),
                Interpreter::new_and_execute_with_mocked_io("%70".to_string()));
        }

        #[test]
        fn x_modulo_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('%')),
                Interpreter::new_and_execute_with_mocked_io("%".to_string()));
        }

        #[test]
        fn x_modulo_by_zero() {
            assert_eq!(
                Err(ScriptError::DivideByZero('%')),
                Interpreter::new_and_execute_with_mocked_io("%8 0".to_string()));
        }

        #[test]
        fn x_intdiv_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('/')),
                Interpreter::new_and_execute_with_mocked_io("/,".to_string()));
        }

        #[test]
        fn x_intdiv_1_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('/')),
                Interpreter::new_and_execute_with_mocked_io("/,20".to_string()));
        }

        #[test]
        fn x_intdiv_2_op_pos() {
            assert_eq!(3f64, Interpreter::new_and_execute_with_mocked_io("/,70 20".to_string()).unwrap().numeric_value());
            assert_eq!(10f64, Interpreter::new_and_execute_with_mocked_io("/,70 20 k".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_intdiv_2_op_neg() {
            assert_eq!(3f64, Interpreter::new_and_execute_with_mocked_io("/,~70 ~20".to_string()).unwrap().numeric_value());
            assert_eq!(10f64, Interpreter::new_and_execute_with_mocked_io("/,~70 ~20 k".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_intdiv_2_op_pos_neg() {
            assert_eq!(-3f64, Interpreter::new_and_execute_with_mocked_io("/,70 ~20".to_string()).unwrap().numeric_value());
            assert_eq!(-10f64, Interpreter::new_and_execute_with_mocked_io("/,70 ~20 k".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_intdiv_2_op_neg_pos() {
            assert_eq!(-3f64, Interpreter::new_and_execute_with_mocked_io("/,~70 20".to_string()).unwrap().numeric_value());
            assert_eq!(-10f64, Interpreter::new_and_execute_with_mocked_io("/,~70 20 k".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_intdiv_2_op_int_frac() {
            assert_eq!(3f64, Interpreter::new_and_execute_with_mocked_io("/,70 20.5".to_string()).unwrap().numeric_value());
            assert_eq!(8.5f64, Interpreter::new_and_execute_with_mocked_io("/,70 20.5 k".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_intdiv_2_op_frac_frac() {
            assert_eq!(2f64, Interpreter::new_and_execute_with_mocked_io("/,3.7 1.4".to_string()).unwrap().numeric_value());
            assert!(are_very_near(0.9f64, Interpreter::new_and_execute_with_mocked_io("/,3.7 1.4 k".to_string()).unwrap().numeric_value()));
        }

        #[test]
        fn x_intdiv_3_op() {
            assert_eq!(3f64, Interpreter::new_and_execute_with_mocked_io("/,(70 20 8)".to_string()).unwrap().numeric_value());
            assert_eq!(10f64, Interpreter::new_and_execute_with_mocked_io("/,(70 20 8) k".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_intdiv_by_zero() {
            assert_eq!(
                Err(ScriptError::DivideByZero('/')),
                Interpreter::new_and_execute_with_mocked_io("/,8 0".to_string()));
        }

        #[test]
        fn x_power_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('^')),
                Interpreter::new_and_execute_with_mocked_io("^".to_string()));
        }

        #[test]
        fn x_power_1_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('^')),
                Interpreter::new_and_execute_with_mocked_io("^49".to_string()));
        }

        #[test]
        fn x_power_2_op_int_int() {
            assert_eq!(36f64, Interpreter::new_and_execute_with_mocked_io("^6 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_power_2_op_int_fract() {
            assert_eq!(7f64, Interpreter::new_and_execute_with_mocked_io("^49 .5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_power_2_op_int_fract_neg() {
            assert_eq!(0.2f64, Interpreter::new_and_execute_with_mocked_io("^25 ~.5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_power_2_op_int_neg() {
            assert_eq!(0.2f64, Interpreter::new_and_execute_with_mocked_io("^5 ~1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_power_2_op_fract_fract() {
            assert_eq!(4.049691346263317f64, Interpreter::new_and_execute_with_mocked_io("^16.4 .5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_power_3() {
            assert_eq!(49f64, Interpreter::new_and_execute_with_mocked_io("^(49 .5 2)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_power_integer_of_negative_base() {
            assert_eq!(9_f64, Interpreter::new_and_execute_with_mocked_io("^~3 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_power_fractal_of_negative_base() {
            assert_eq!(
                Err(ScriptError::NonIntegerPowerOfNegativeNumberIsNotSupported),
                Interpreter::new_and_execute_with_mocked_io("^~3 /1 3".to_string()));
        }

        #[test]
        fn x_power_error_no_stop() {
            assert_eq!(
                "Outcome: DivideByZero('/')".to_string(),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 1 +[sOutcome: ] ^/4 0 2".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_power_stop_on_err() {
            assert_eq!(
                Err(ScriptError::DivideByZero('/')),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 0 ^/8 0 2".to_string()));
        }

        #[test]
        fn x_unaryminus_3_op() {
            assert_eq!(-5.02f64, Interpreter::new_and_execute_with_mocked_io("~(5.02 8 3)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_unaryminus_1_op() {
            assert_eq!(-5.02f64, Interpreter::new_and_execute_with_mocked_io("~5.02".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_unaryminus_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('~')),
                Interpreter::new_and_execute_with_mocked_io("+ 2 ~".to_string()));
        }

        #[test]
        fn x_int_int() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("i5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_int_fract() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("i5.7".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_int_neg_int() {
            assert_eq!(-5f64, Interpreter::new_and_execute_with_mocked_io("i~5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_int_neg_fract() {
            assert_eq!(-5f64, Interpreter::new_and_execute_with_mocked_io("i~5.8".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_int_alt_int() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("i,5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_int_alt_fract() {
            assert_eq!(6f64, Interpreter::new_and_execute_with_mocked_io("i,5.7".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_int_alt_neg_int() {
            assert_eq!(-5f64, Interpreter::new_and_execute_with_mocked_io("i,~5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_int_alt_neg_fract() {
            assert_eq!(-6f64, Interpreter::new_and_execute_with_mocked_io("i,~5.3".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_int_alt_alt_fract() {
            assert_eq!(6f64, Interpreter::new_and_execute_with_mocked_io("i,5.7".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_int_alt_main() {
            assert_eq!(11f64, Interpreter::new_and_execute_with_mocked_io("+i,5.7 i5.7".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_abs_pos() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("a5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_abs_neg() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("a~5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_min_two_first() {
            assert_eq!(128f64, Interpreter::new_and_execute_with_mocked_io("m128 277".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_min_two_second() {
            assert_eq!(277f64, Interpreter::new_and_execute_with_mocked_io("m328 277".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_min_two_equal() {
            assert_eq!(128f64, Interpreter::new_and_execute_with_mocked_io("m128 128".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_min_more() {
            assert_eq!(-31f64, Interpreter::new_and_execute_with_mocked_io("m(128 277 ~31 5)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_min_empty() {
            assert_eq!(
                NO_VALUE.to_string(),
                Interpreter::new_and_execute_with_mocked_io("m(128 € [s Huh?] 277 ~31 5)".to_string())
                    .unwrap().string_representation());
        }

        #[test]
        fn x_min_num_and_string() {
            assert_eq!(-31f64, Interpreter::new_and_execute_with_mocked_io("m§Voilà ~31".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_min_string_and_error() {
            assert_eq!("Voilà".to_string(), Interpreter::new_and_execute_with_mocked_io("Z§ign 1 m§Voilà /1 0".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_min_strings() {
            assert_eq!("Voilà".to_string(), Interpreter::new_and_execute_with_mocked_io("m§Voilà §voilà".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_max_two_first() {
            assert_eq!(128f64, Interpreter::new_and_execute_with_mocked_io("M128 27".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_max_two_second() {
            assert_eq!(277f64, Interpreter::new_and_execute_with_mocked_io("M28 277".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_max_two_equal() {
            assert_eq!(128f64, Interpreter::new_and_execute_with_mocked_io("M128 128".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_max_more() {
            assert_eq!(366f64, Interpreter::new_and_execute_with_mocked_io("M(128 277 ~31 5 366)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_max_empty() {
            assert_eq!(
                " Huh?".to_string(),
                Interpreter::new_and_execute_with_mocked_io("M(128 € [s Huh?] 277 ~31 5)".to_string())
                    .unwrap().string_representation());
        }

        #[test]
        fn x_max_error_stop_on_error() {
            assert_eq!(
                Err(ScriptError::UserDefinedError("myError".to_string())),
                Interpreter::new_and_execute_with_mocked_io("M(128 U§myError [s Huh?] 277 ~31 5)".to_string())
            );
        }

        #[test]
        fn x_max_error_no_stop() {
            assert_eq!(
                r#"UserDefinedError("myError")"#.to_string(),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 1 M(128 U§myError [s Huh?] 277 ~31 5)".to_string())
                    .unwrap().string_representation());
        }

        #[test]
        fn x_max_num_and_string() {
            assert_eq!("Voilà".to_string(), Interpreter::new_and_execute_with_mocked_io("M§Voilà ~31".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_max_strings() {
            assert_eq!("voilà".to_string(), Interpreter::new_and_execute_with_mocked_io("M§Voilà §voilà".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_assign_num_return_value() {
            assert_eq!(111f64, Interpreter::new_and_execute_with_mocked_io("$4 111".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assign_string() {
            assert_eq!("汉字".to_string(), Interpreter::new_and_execute_with_mocked_io("$4 [s汉字] v4".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_assign_num_serial_assignation() {
            assert_eq!(10f64, Interpreter::new_and_execute_with_mocked_io("$(4 1 2 3 4) $1 0 F4 7 1 0 +:1 vv0 v1 ".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assign_num_assignation_string_name() {
            assert_eq!(10f64, Interpreter::new_and_execute_with_mocked_io("$§reg 10 v§reg".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assign_num_serial_assignation_string_name() {
            assert_eq!(10f64, Interpreter::new_and_execute_with_mocked_io("$(§reg 1 2 3 4) +(v§reg0 v§reg1 v§reg2 v§reg3)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assign_to_neg_register() {
            assert_eq!(7f64, Interpreter::new_and_execute_with_mocked_io("$~10 7 v~10".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assign_empty_removes_var() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);

            interpreter.execute_opts("$38 5".to_string(), true, false, false).unwrap();
            assert_eq!(true, interpreter.shuttle.has_var(ValueType::Number(38f64)));
            assert_eq!(5f64, interpreter.execute_opts("v38".to_string(), true, false, false).unwrap().numeric_value());

            interpreter.execute_opts("$38 €".to_string(), true, false, false).unwrap();
            assert_eq!(false, interpreter.shuttle.has_var(ValueType::Number(38f64)));
        }

        #[test]
        fn x_get_num_reg() {
            assert_eq!(90f64, Interpreter::new_and_execute_with_mocked_io("$/21 2 90 $4 45 v/21 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_get_num_reg_uninit() {
            assert_eq!(
                0_f64,
                Interpreter::new_and_execute_with_mocked_io("tv200".to_string()).unwrap().numeric_value()
            );
        }

        #[test]
        fn x_var_or_had_value() {
            assert_eq!(180f64, Interpreter::new_and_execute_with_mocked_io("$22 90 $§copy v,22 130 +v§copy v22".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_var_or_uninit() {
            assert_eq!(2f64, Interpreter::new_and_execute_with_mocked_io("$§copy v,22 1 +v§copy v22".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_var_or_had_empty() {
            assert_eq!(130f64, Interpreter::new_and_execute_with_mocked_io("$22 € v,22 130".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_add_assign() {
            assert_eq!(90f64, Interpreter::new_and_execute_with_mocked_io("$18 88 +:-21 3 2 v18".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_inside_override_markers_first() {
            assert_eq!(35f64, Interpreter::new_and_execute_with_mocked_io("$1 10 $2 20 +(:1 v2 5) v1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_before_override_markers_first() {
            assert_eq!(35f64, Interpreter::new_and_execute_with_mocked_io("$1 10 $2 20 +:(1 v2 5) v1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_inside_override_markers_second() {
            assert_eq!(35f64, Interpreter::new_and_execute_with_mocked_io("$1 10 $2 20 +(v1 :2 5) v2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_inside_override_markers_both() {
            assert_eq!(70f64, Interpreter::new_and_execute_with_mocked_io("$1 10 $2 20 +(:1 :2 5) +v1 v2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_before_and_inside_override_markers_both() {
            assert_eq!(70f64, Interpreter::new_and_execute_with_mocked_io("$1 10 $2 20 +:(1 :2 5) +v1 v2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_with_default_non_empty_found() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("$1 4 +:,1 2 1 v1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_with_default_empty_found() {
            assert_eq!(3f64, Interpreter::new_and_execute_with_mocked_io("+:,1 2 1 v1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_with_default_empty_found_op_before_parenth() {
            assert_eq!(3f64, Interpreter::new_and_execute_with_mocked_io("+:,(1 2 1) v1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_with_default_empty_found_op_before_parenth_alt_after() {
            assert_eq!(3f64, Interpreter::new_and_execute_with_mocked_io("+:(,1 2 1) v1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_with_default_empty_found_op_after_parenth() {
            assert_eq!(3f64, Interpreter::new_and_execute_with_mocked_io("+(:,1 2 1) v1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_several() {
            assert_eq!(16f64, Interpreter::new_and_execute_with_mocked_io("++:,1 2 :,5 6 v5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_mixed() {
            assert_eq!(16f64, Interpreter::new_and_execute_with_mocked_io("$5 6 ++:,1 2 :5 v5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_mixed_reversed() {
            assert_eq!(16f64, Interpreter::new_and_execute_with_mocked_io("$5 6 ++:5 :,1 2 v5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_assignment_maker_mixed_unassigned() {
            assert_eq!(
                Err(ScriptError::EmptyOperand('+')),
                Interpreter::new_and_execute_with_mocked_io("++(:,1 2 :5 6) v5".to_string())
            );
        }

        #[test]
        fn x_unaryminus_assign() {
            assert_eq!(-88f64, Interpreter::new_and_execute_with_mocked_io("$18 88 ~:18 v18".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_2_exact() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("=21.3 21.3".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_2_in_orb() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .1 =21.3 21.35".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_2_outside_orb() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .01 =21.3 21.35".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_many() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("$1 -15 3 =(12 +7 5 *3 4 /36 3 v1)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_many_false() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("$1 -15 3 =(12 +7 5 *3 4 111 /36 3 v1)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_different_type() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("=21.3 §xxx".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_strings_true() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("=[sxxx] §xxx".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_strings_false() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("=[sxxx !!] §xxx".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_strings_upper_and_lower_case() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("=§xxx §XXX".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_empty() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("$§void € =€ v§void".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_number_and_empty() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("=708 €".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_error_and_same_error_no_stop() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§ign 1 = /4 0 /4 0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_error_and_same_error_stop_on_error() {
            assert_eq!(
                Err(ScriptError::UserDefinedError(r#"aaa"#.to_string())),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 0 = U§aaa U§aaa".to_string()));
        }

        #[test]
        fn x_equality_error_and_other_error() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("Z§ign 1 = U§aaa U§bbb".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_equality_error_and_other_type() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("Z§ign 1 = U§aaa 25".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_less_true() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("< 5.000001 5.000002".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_less_false() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("< 5.000002 5.000002".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_less_3_true() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("<(5.000001 5.000002 6)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_less_3_false() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("<(5.000001 5.000002 3)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_less_mixed() {
            // v111 as uninitialized variable is VaueType::Empty.
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("<(€ ~2036 2 6 §Fruehling §Zambetas)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greater_true() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("> 5.000002 5.000001".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greater_false() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("> 5.000001 5.000002".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greater_3_false() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io(">(5.000003 5.000002 6)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greater_3_true() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io(">(5.000002 5.000001 3)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greater_mixed() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io(">(§Zambetas §Fruehling 1_000_000.2 2024 ~85 €)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_not_1() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("!1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_not_0() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("!0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_not_other() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("!~145".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_not_3_true() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("!(0 0 0)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_not_3_false() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("!(0 0 2)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_not_3_false_mixed() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("!(0 [sWow!] 0)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_not_empty() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("!€".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_not_empty_simple_string() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("!§".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_not_empty_string() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("![s]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_not_string() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("!§Voilà".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_not_not_empty() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("!!€".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_not_error_no_stop() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§ign 1 !U§aaa".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_not_error_stop_on_error() {
            assert_eq!(
                Err(ScriptError::NonNumericOperand('*')),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 0 * 2 §one_and_twenty".to_string()));
        }

        #[test]
        fn x_and_0_operands() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('&')),
                Interpreter::new_and_execute_with_mocked_io("&".to_string()));
        }

        #[test]
        fn x_and_1_true() {
            assert_eq!(Err(ScriptError::InsufficientOperands('&')), Interpreter::new_and_execute_with_mocked_io("& /7 2".to_string()));
        }

        #[test]
        fn x_and_1_false() {
            assert_eq!(Err(ScriptError::InsufficientOperands('&')), Interpreter::new_and_execute_with_mocked_io("& -5 5".to_string()));
        }

        #[test]
        fn x_and_2_true() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("& 2 /7 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_and_2_false() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("& 2 -5 5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_and_3_true() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("&(2 /7 2 %14 4)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_and_3_false() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("&( 2 -5 5 9)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_and_empty() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("& 9 €".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_and_empty_string() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("& 9 [s]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_and_string() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("& 9 §Nostromo".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_and_error_no_stop() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("Z§ign 1 & §Nostromo U§failed".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_and_error_stop_on_error() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('=')),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 0&1=".to_string()));
        }

        #[test]
        fn x_or_0() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('|')),
                Interpreter::new_and_execute_with_mocked_io("|".to_string()));
        }

        #[test]
        fn x_or_1_true() {
            assert_eq!(Err(ScriptError::InsufficientOperands('|')), Interpreter::new_and_execute_with_mocked_io("| /7 2".to_string()));
        }

        #[test]
        fn x_or_1_false() {
            assert_eq!(Err(ScriptError::InsufficientOperands('|')), Interpreter::new_and_execute_with_mocked_io("| -5 5".to_string()));
        }

        #[test]
        fn x_or_2_true() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("| 0 /7 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_or_2_false() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("| 0 -5 5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_or_3_true() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("|(0 +3 ~2  %14 4)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_or_3_false() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("|( 0 -5 5 %20 5)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_or_empty() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("| 9 €".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_or_empty_string() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("| 9 [s]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_or_string() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("| 9 §Nostromo".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_or_error_no_stop() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("Z§ign 1 | U§failed 0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_or_error_stop_on_error() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('=')),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 0|0=".to_string()));
        }

        #[test]
        fn x_xor_0() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('x')),
                Interpreter::new_and_execute_with_mocked_io("x".to_string()));
        }

        #[test]
        fn x_xor_1_true() {
            assert_eq!(Err(ScriptError::InsufficientOperands('x')), Interpreter::new_and_execute_with_mocked_io("x /7 2".to_string()));
        }

        #[test]
        fn x_xor_1_false() {
            assert_eq!(Err(ScriptError::InsufficientOperands('x')), Interpreter::new_and_execute_with_mocked_io("x -5 5".to_string()));
        }

        #[test]
        fn x_xor_2_true() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("x 0 /7 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_xor_2_false_both_true() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("x 8 5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_xor_2_false_both_false() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("x 0 -5 5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_xor_3_true() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("x(0 +3 ~3  %14 4)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_xor_3_false_2_true() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("x(0 -5 7 %20 7)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_xor_3_false_3_true() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("x(1 -5 7 %20 5)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_xor_3_false_0_true() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("x(0 -5 5 %20 5)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_xor_empty() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("x 9 v§uninit".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_xor_empty_string() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("x 9 [s]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_xor_string() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("x 9 §Nostromo".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_xor_error_no_stop() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§ign 1 x U§failed 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_xor_error_stop_on_error() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('=')),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 0x1=".to_string()));
        }

        #[test]
        fn x_while_simple() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("$1 4 W>v1 2 $1 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_while_executed() {
            assert_eq!(10f64, Interpreter::new_and_execute_with_mocked_io("$0 1 $1 0 W!>v0 4 ;+:1 v0 +:0 1 v1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_while_executed_op_nr_overridden() {
            assert_eq!(10f64, Interpreter::new_and_execute_with_mocked_io("$0 1 $1 0 W(!>v0 4 +:1 v0 +:0 1) v1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_while_never_executed() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("$0 1 $1 0 W!>v0 ~1 ;+:1 v0 +:0 1 v1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_while_max_iterations() {
            assert_eq!(2f64, Interpreter::new_and_execute_with_mocked_io("$§sum 1 $§countDown 10 Z§loops 2 W>v§countDown 0 ;+:§sum v§countDown -:§countDown 1 N".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_while_break() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("$§last 10 $§count 1 W(!>v§count v§last ?=v§count 5 B1 0 +:§count 1) v§count".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_while_break_2() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("$§last 10 $§count 1 W(1 W(!>v§count v§last ?=v§count 5 B2 0 +:§count 1) +:§count 10) v§count".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_while_break_0() {
            assert_eq!(4f64, Interpreter::new_and_execute_with_mocked_io("$§last 3 $§count 1 W(!>v§count v§last B0 +:§count 1) v§count".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_for_4_op_asc() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('F')),
                Interpreter::new_and_execute_with_mocked_io("F(3 11 2 1)".to_string()));
        }

        #[test]
        fn x_for_5_op_asc() {
            assert_eq!(10395f64, Interpreter::new_and_execute_with_mocked_io("$0 1 F3 11 2 1 *:0 v1 v0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_for_5_op_desc() {
            assert_eq!(10395f64, Interpreter::new_and_execute_with_mocked_io("$0 1 F11 3 2 1 *:0 v1 v0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_for_6_op() {
            assert_eq!(10395f64, Interpreter::new_and_execute_with_mocked_io("$0 1 F(11 3 2 1 *:0 v1 $5 v1) v0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_for_5_op_exceeds_limit() {
            assert_eq!(15f64, Interpreter::new_and_execute_with_mocked_io("Z§loops 2 $0 1 F3 11 2 1 *:0 v1 v0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_for_descending() {
            assert_eq!(24f64, Interpreter::new_and_execute_with_mocked_io("$0 1 F4 1 1 1 *:0v1 v0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_for_break() {
            assert_eq!(5_f64, Interpreter::new_and_execute_with_mocked_io("F1 10 1 1 ?=v1 5 B1 0 v1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_for_break_0() {
            assert_eq!(4_f64, Interpreter::new_and_execute_with_mocked_io("F1 4 1 1 ?=v1 2 B0 0 v1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_for_break_2() {
            assert_eq!(2_f64, Interpreter::new_and_execute_with_mocked_io("$0 0 F1 3 1 2 F1 10 1 1 ?=v1 3 B2 +:0 1 v0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_if_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('?')),
                Interpreter::new_and_execute_with_mocked_io("?".to_string()));
        }

        #[test]
        fn x_if_1_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('?')),
                Interpreter::new_and_execute_with_mocked_io("? 19".to_string()));
        }

        #[test]
        fn x_if_second() {
            assert_eq!(8f64, Interpreter::new_and_execute_with_mocked_io("?*70 .2 8 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_if_third() {
            assert_eq!(2f64, Interpreter::new_and_execute_with_mocked_io("?-70 70 8 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_if_5_op_second() {
            assert_eq!(4f64, Interpreter::new_and_execute_with_mocked_io("?(19 +3 1 0 7 23)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_if_5_op_last() {
            assert_eq!(23f64, Interpreter::new_and_execute_with_mocked_io("$5 0 ?(v5 +3 1 0 7 23)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_if_strings() {
            assert_eq!(ExecutionOutcome::Text("Smaller".to_string()), Interpreter::new_and_execute_with_mocked_io("?<4 5 [sSmaller] [sGreater]".to_string()).unwrap());
        }

        #[test]
        fn x_if_mixed() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("?5 5 [sNot five]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_enum_opr_unknown() {
            assert_eq!(
                Err(ScriptError::UnknownNamedOperator("1.5".to_string())),
                Interpreter::new_and_execute_with_mocked_io("o1.5 2".to_string()));
        }

        #[test]
        fn x_sign_no_operands() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('s')),
                Interpreter::new_and_execute_with_mocked_io("s".to_string()));
        }

        #[test]
        fn x_sign_all_pos() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("s(45 7 99 4022)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_sign_all_neg() {
            assert_eq!(-1f64, Interpreter::new_and_execute_with_mocked_io("s(~45 ~7 ~99 ~4022)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_sign_mixed() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("s(~45 7 ~99 4022)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_sign_assign() {
            assert_eq!(-1f64, Interpreter::new_and_execute_with_mocked_io("$~22 ~4 s:~22 v~22".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_enum_opr_override() {
            assert_eq!("fg".to_string(), Interpreter::new_and_execute_with_mocked_io("O(§uni 102 103)".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_enum_opr_commas() {
            assert_eq!("fgh".to_string(), Interpreter::new_and_execute_with_mocked_io("o,§uni 102 103 104".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_enum_opr_unicode_chars() {
            assert_eq!("\n".to_string(), Interpreter::new_and_execute_with_mocked_io("O§uni 10".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_enum_opr_unicode_chars_more() {
            assert_eq!("Союз".to_string(), Interpreter::new_and_execute_with_mocked_io("o(§uni 1057 1086 1102 1079)".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_enum_opr_unicode_chars_more_bis() {
            assert_eq!("Союз".to_string(), Interpreter::new_and_execute_with_mocked_io("o§uni(1057 1086 1102 1079)".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_enum_opr_version_no_second_operand() {
            const VERSION: &'static str = env!("CARGO_PKG_VERSION");
            assert_eq!(VERSION.to_string(), Interpreter::new_and_execute_with_mocked_io("o§version".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_enum_opr_version_full() {
            const VERSION: &'static str = env!("CARGO_PKG_VERSION");
            assert_eq!(VERSION.to_string(), Interpreter::new_and_execute_with_mocked_io("o§version 0".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_enum_opr_version_invalid_operand() {
            assert_eq!(
                Err(ScriptError::InvalidVersionPart(8_f64)),
                Interpreter::new_and_execute_with_mocked_io("o§version 8".to_string()));
        }

        #[test]
        fn x_enum_opr_version_major() {
            const VERSION: &'static str = env!("CARGO_PKG_VERSION");
            let version_nums: Vec<&str> = VERSION.split('.').collect();
            let requested = version_nums.get(0).unwrap_or(&"_").to_string();
            assert_eq!(requested, Interpreter::new_and_execute_with_mocked_io("o§version 1".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_enum_opr_version_minor() {
            const VERSION: &'static str = env!("CARGO_PKG_VERSION");
            let version_nums: Vec<&str> = VERSION.split('.').collect();
            let requested = version_nums.get(1).unwrap_or(&"_").to_string();
            assert_eq!(requested, Interpreter::new_and_execute_with_mocked_io("o§version 2".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_enum_opr_version_revision() {
            const VERSION: &'static str = env!("CARGO_PKG_VERSION");
            let version_nums: Vec<&str> = VERSION.split('.').collect();
            let requested = version_nums.get(2).unwrap_or(&"_").to_string();
            assert_eq!(requested, Interpreter::new_and_execute_with_mocked_io("o§version 3".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_interpreter_execute() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            interpreter.execute("w[sHello!]".to_string()).unwrap();

            assert_eq!(6usize, interpreter.echo_output().unwrap().len());
        }

        #[test]
        fn x_write() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            interpreter.execute_opts("w[sHello!]".to_string(), true, false, false).unwrap();

            assert_eq!(b'H', interpreter.echo_output().unwrap()[0]);
        }

        #[test]
        fn x_write_more() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            interpreter.execute_opts("w([sA] [sB] [sC])".to_string(), true, false, false).unwrap();

            assert_eq!(3, interpreter.echo_output().unwrap().len());
        }

        #[test]
        fn x_write_number() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            interpreter.execute_opts("w+100 300".to_string(), true, false, false).unwrap();

            // 400.000000 = 10 chars.
            assert_eq!(10, interpreter.echo_output().unwrap().len());

            assert_eq!(
                vec![b'4', b'0', b'0', b'.', b'0', b'0', b'0', b'0', b'0', b'0'],
                *interpreter.echo_output().unwrap());
        }

        #[test]
        fn x_write_number_formatted() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            interpreter.execute_opts("b,16 o(§fmt 2 §. §,) w31.5".to_string(), true, false, false).unwrap();

            // 1F.80\n
            // TO BE SOLVED assert_eq!(6, writer.len());
            // TO BE SOLVED assert_eq!(vec![b'1', b'F', b'.', b'8', b'0', b'\n'], *writer);
        }

        #[test]
        fn x_write_nothing() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('w')),
                Interpreter::new_and_execute_with_mocked_io("w".to_string()));

        }

        #[test]
        fn x_write_to_stdout() {
            // To be tested using the -- --nocapture argument.
            let writer = Box::new(std::io::stdout());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            interpreter.execute_opts("w[s\n========== Hello from x_write_to_stdout() ! ==========\n]".to_string(), true, false, false).unwrap();
        }

        #[test]
        fn x_write_to_sink() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            interpreter.execute_opts("w[sGarbage]".to_string(), true, false, false).unwrap();
        }

        #[test]
        fn x_read_number_base10() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["16.2".to_string()]));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            assert_eq!(17f64, interpreter.execute_opts("w[sEnter a number:]+r0.8".to_string(), true, false, false).unwrap().numeric_value());
        }

        #[test]
        fn x_read_neg_number_base10_minus() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["-16.2".to_string()]));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            assert_eq!(-14.2f64, interpreter.execute_opts("w[sEnter a number:]+r2".to_string(), true, false, false).unwrap().numeric_value());
        }

        #[test]
        fn x_read_neg_number_base10_tilde() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["~16.2".to_string()]));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            assert_eq!(-14.2f64, interpreter.execute_opts("w[sEnter a number:]+r2".to_string(), true, false, false).unwrap().numeric_value());
        }

        #[test]
        fn x_read_neg_number_base10_non_leading() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["16-".to_string()]));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            let outcome = interpreter.execute_opts("w[sEnter a number:]+r 2".to_string(), true, false, false).unwrap().string_representation();

            println!("Outcome: {:?}", outcome);

            assert_eq!(2f64, interpreter.execute_opts("w[sEnter a number:]+r2".to_string(), true, false, false).unwrap().numeric_value());
        }

        #[test]
        fn x_read_number_base16() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["1F.3".to_string()]));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            assert_eq!(32f64, interpreter.execute_opts("b16 w[sEnter a number:]+r[n0.D]".to_string(), true, false, false).unwrap().numeric_value());
        }

        #[test]
        fn x_read_string() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["1a1".to_string()]));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            assert_eq!("1a1200.000000".to_string(), interpreter.execute_opts("w[sEnter something:]+r200".to_string(), true, false, false).unwrap().string_representation());
        }

        #[test]
        fn x_read_number_and_string() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["200".to_string(), "1a1".to_string()]));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            assert_eq!("1a1200.000000".to_string(), interpreter.execute_opts("+rr".to_string(), true, false, false).unwrap().string_representation());
        }

        #[test]
        #[ignore]
        fn x_read_from_stdin() {
            // To be tested using the -- --ignored --nocapture arguments.
            let writer = Box::new(std::io::stdout());
            let reader = Box::new(StdinReader::new());
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            interpreter.execute_opts("Z§ign 1 w[sEnter a number: ] $0r w(+[sYou entered: ]v0) w(+¶[sDouble is: ] *v0 2 ¶)".to_string(), true, false, false).unwrap();
        }

        #[test]
        fn x_radians() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .000005 = °,180 p".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_alternative_inside_override() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .000005 = °(,180) p".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_alternative_before_override() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .000005 = °,(180) p".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_sin() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .00001 = S°,45 0.7071".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_arcsin() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .5= °S,0.7071 45".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_sinh() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .00001 = S,,°,20 .356198".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_asinh() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .00001 = S,,, .356198 °,20".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_cos() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .00001 = C°,45 0.7071".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_arccos() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .5 = °C,0.7071 45".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_cosh() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .00001 = C,,°,30 1.140237".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_acosh() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .00001 = C,,,1.140237 °,30".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_tan() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .00001 = T°,45 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_arctan() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .00001 = °T,1 45".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_tanh() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .00001 = T,,°,160 .992521".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_atanh() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .00001 = T,,, .992521 °,160".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_degrees() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .000005 = 180 °p".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_pi() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .000005 =p 3.14159".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_euler_const() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .000005 =e 2.71828".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_log_no_2_args() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('l')),
                Interpreter::new_and_execute_with_mocked_io("l10".to_string()));
        }

        #[test]
        fn x_log_more_args() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .000001 =2 l(10 100 3046)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_log10() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .000001 =3 l10 1000".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_log_ln() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .000001 =1 le e".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_log_negative_base() {
            assert_eq!(
                Err(ScriptError::ZeroOrNegativeLogarithmBaseIsNotSupported),
                Interpreter::new_and_execute_with_mocked_io("l ~10 ~1000".to_string()));
        }

        #[test]
        fn x_log_of_negative_number() {
            assert_eq!(
                Err(ScriptError::LogarithmOfZeroOrNegativeNumberIsNotSupported),
                Interpreter::new_and_execute_with_mocked_io("l 10 ~1000".to_string()));
        }

        #[test]
        fn x_consts_unknown() {
            assert_eq!(
                Err(ScriptError::UnknownConstant("-2".to_string())),
                Interpreter::new_and_execute_with_mocked_io("c~2".to_string()));
        }

        #[test]
        fn x_consts_golden_ratio() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .000001   $0 10   $1 *v0c§gold   =/+v0v1 v1 /v1 v0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_consts_conjugate_golden_ratio() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .000001 = c§cogold /-1 ^5 .5 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_consts_empty() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("tc§empty".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_get_type_empty() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("t€".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_get_type_number() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("t %38 5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_get_type_string() {
            assert_eq!(2f64, Interpreter::new_and_execute_with_mocked_io("t +[sTotal: ]38".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_get_type_error() {
            assert_eq!(90f64, Interpreter::new_and_execute_with_mocked_io("Z§ign 1 t*(7)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_var_having_string_name() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("$0 11 $[sMy Number] 3 +v[sMy Number] 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_var_having_simple_string_name() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("$0 11 $§myNum 3 +v§myNum 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_var_having_string_name_make_assign() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("$§myNum 3 +:§myNum 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_base2() {
            assert_eq!(14.5f64, Interpreter::new_and_execute_with_mocked_io("b2 1110.1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_base16() {
            assert_eq!(14.5f64, Interpreter::new_and_execute_with_mocked_io("b16 [ne.8]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_base10_digit_too_great() {
            assert_eq!(99f64, Interpreter::new_and_execute_with_mocked_io("b10 [n9F]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_base36() {
            assert_eq!(71.5f64, Interpreter::new_and_execute_with_mocked_io("b36 [n1Z.I]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_base38() {
            assert_eq!(77.5f64, Interpreter::new_and_execute_with_mocked_io("b38 [n 2 1.19]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_base38_starts_with_period() {
            assert_eq!(0.5f64, Interpreter::new_and_execute_with_mocked_io("b38 [n .19]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_base37_ends_with_period() {
            assert_eq!(114f64, Interpreter::new_and_execute_with_mocked_io("b37 [n 3 3.]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_base40_second_period_ignored() {
            assert_eq!(0.5125f64, Interpreter::new_and_execute_with_mocked_io("b40 [n .20.20]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_base5_space_separated() {
            assert_eq!(11.2f64, Interpreter::new_and_execute_with_mocked_io("b5 [n 2 1 . 1]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_base40_starts_and_ends_with_period() {
            assert_eq!(0.5f64, Interpreter::new_and_execute_with_mocked_io("b40 [n .20.]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_base100_spaces_around_period() {
            assert_eq!(243.15f64, Interpreter::new_and_execute_with_mocked_io("b100 [n 2 43 . 15]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_base60_double_spaces() {
            assert_eq!(121.2f64, Interpreter::new_and_execute_with_mocked_io("b60 [n 2  1  .  12]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_base_negative() {
            assert_eq!(
                Err(ScriptError::InvalidNumberBase(-2_f64)),
                Interpreter::new_and_execute_with_mocked_io("b~2 1110.1".to_string()));
        }

        #[test]
        fn x_obase_negative() {
            assert_eq!(
                Err(ScriptError::InvalidNumberBase(-2_f64)),
                Interpreter::new_and_execute_with_mocked_io("b,~2 1110.1".to_string()));
        }

        #[test]
        fn x_obase2() {
            assert_eq!("1000110.000000".to_string(), Interpreter::new_and_execute_with_mocked_io("b,2 70".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_split_underscore_in_simple_string() {
            assert_eq!("_".to_string(), Interpreter::new_and_execute_with_mocked_io("§_ ".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_set_fmt() {
            assert_eq!("4=500_555=6".to_string(), Interpreter::new_and_execute_with_mocked_io("o(§fmt 4 §_ §=) 4500.55559".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_set_fmt_round() {
            assert_eq!("4=500_23".to_string(), Interpreter::new_and_execute_with_mocked_io("o(§fmt 2 §_ §=) 4500.229".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_set_fmt_round_to_int() {
            assert_eq!("2".to_string(), Interpreter::new_and_execute_with_mocked_io("o(§fmt 0 §. §,) 1.9".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_set_fmt_round_to_neg_int() {
            assert_eq!("-2".to_string(), Interpreter::new_and_execute_with_mocked_io("o(§fmt 0 §. §,) ~1.9".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_add_fmt() {
            assert_eq!("Total: 4=500_555=6".to_string(), Interpreter::new_and_execute_with_mocked_io("o(§fmt 4 §_ §=) +[sTotal: ] 4500.55559".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_prec_nr_operands() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io(";+(7 3 9 -74 3 1) N".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_prec_nr_operands_none() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("N".to_string()).unwrap().numeric_value()); } #[test] fn x_prec_nr_operands_avg() {
            assert_eq!(18.2f64, Interpreter::new_and_execute_with_mocked_io("/+(7 3 9 -74 3 1) N".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_prec_nr_operands_while() {
            assert_eq!(6f64, Interpreter::new_and_execute_with_mocked_io("$0 6;W>v0 0-:0 1N".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_prec_nr_operands_for() {
            assert_eq!(3f64, Interpreter::new_and_execute_with_mocked_io("F5 1 2 0 0N".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_stack_init_get_depth() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("k,".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_stack_init_pop() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("tk".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_stack_pop_more_than_pushed() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("K(45 71) kk tk".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_stack_push_one_get_depth() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("K5k,".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_stack_push_one_pop() {
            assert_eq!(2f64, Interpreter::new_and_execute_with_mocked_io("K%27 5 k".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_stack_push_more_get_depth() {
            assert_eq!(4f64, Interpreter::new_and_execute_with_mocked_io("K(5 ~45 /41 2 §Aha!) k,".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_stack_push_more_pop() {
            assert_eq!("Aha!".to_string(), Interpreter::new_and_execute_with_mocked_io("K(5 ~45 /41 2 §Aha! 99) kk".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_stack_pop_reverse_order() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("K3 K9 >kk".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_stack_push_reverse_order() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("K,(9 7 5 3) >(kkkk)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_stack_clear() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("K(7 5 9 6 2 1) K,, k,".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_empty() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("t€".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_routine_undeclared() {
            assert_eq!(
                Err(ScriptError::UnknownRoutine("unknown".to_string())),
                Interpreter::new_and_execute_with_mocked_io("tX§unknown".to_string()));
        }

        #[test]
        fn x_routine_one_operand() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('R')),
                Interpreter::new_and_execute_with_mocked_io("R(§oneOp) X§oneOp".to_string()));
        }

        #[test]
        fn x_routine_two_operands() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("R§tau *2p Z§prec .01 =6.28 X§tau".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_routine_more_operands_call_twice() {
            assert_eq!(50f64, Interpreter::new_and_execute_with_mocked_io("R(§sumStack $100 0 Wk,+:100k v100) +X(§sumStack 20 30 ~10 2)X(§sumStack 9 3 ~4)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_routine_redefine() {
            assert_eq!(9f64, Interpreter::new_and_execute_with_mocked_io("R2 ^2 .5 R2 9 X2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_routine_declaration_yields_name() {
            assert_eq!("tau".to_string(), Interpreter::new_and_execute_with_mocked_io("R§tau *p2".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_routine_sharing_variables() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("R,(§empty_vars $§end k $§start k Fv§start v§end 1 §count $v§count €) $(11 5 5 5 5 5) K(11 15) X§empty_vars tv15".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_routine_new_scope() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("R(§empty_vars $§end k $§start k Fv§start v§end 1 §count $v§count €) $(11 5 5 5 5 5) K(11 15) X§empty_vars v15".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_routine_get_name() {
            assert_eq!("tau".to_string(), Interpreter::new_and_execute_with_mocked_io("R,§tau ; $100 c§rtn *p2 X§tau v100".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_routine_get_name_in_main_after_routine_execution() {
            assert_eq!("main".to_string(), Interpreter::new_and_execute_with_mocked_io("R§tau *p2 X§tau c§rtn".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_routine_get_name_in_main() {
            assert_eq!("main".to_string(), Interpreter::new_and_execute_with_mocked_io("c§rtn".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_routine_exec_lifo_args() {
            assert_eq!(-5f64, Interpreter::new_and_execute_with_mocked_io("R§minus -kk X(§minus 8 3)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_routine_exec_inverse_args() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("R§minus -kk X,(§minus 8 3)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_interpreter_keeps_variables() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["-16.2".to_string()]));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            assert_eq!(20_f64, interpreter.execute_opts("$§theNum 20 v§theNum".to_string(), true, false, false).unwrap().numeric_value());
            assert_eq!(25_f64, interpreter.execute_opts("+v§theNum 5".to_string(), true, false, false).unwrap().numeric_value());
        }

        #[test]
        fn x_interpreter_keeps_stack() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["-16.2".to_string()]));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            assert_eq!(251_f64, interpreter.execute_opts("K 251".to_string(), true, false, false).unwrap().numeric_value());
            assert_eq!(251_f64, interpreter.execute_opts("k".to_string(), true, false, false).unwrap().numeric_value());
        }

        #[test]
        fn x_interpreter_keeps_routines() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["-16.2".to_string()]));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            assert_eq!("dup".to_string(), interpreter.execute_opts("R§dup *k2".to_string(), true, false, false).unwrap().string_representation());
            assert_eq!(2000_f64, interpreter.execute_opts("X(§dup 1000)".to_string(), true, false, false).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_num() {
            assert_eq!(8_f64, Interpreter::new_and_execute_with_mocked_io("n8".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_string_valid() {
            assert_eq!(29_f64, Interpreter::new_and_execute_with_mocked_io("n§29".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_string_valid_base16() {
            assert_eq!(26_f64, Interpreter::new_and_execute_with_mocked_io("b16 n§1A".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_string_valid_base50() {
            assert_eq!(5050_f64, Interpreter::new_and_execute_with_mocked_io("b50 n[s2 1 0]".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_string_invalid_base10_spaces() {
            assert_eq!(
                Err(ScriptError::NumberParsingFailure("Digit value too high for base of input number".to_string())),
                Interpreter::new_and_execute_with_mocked_io("b50 n[s1 72]".to_string()));
        }

        #[test]
        fn x_to_num_string_valid_minus() {
            assert_eq!(-29_f64, Interpreter::new_and_execute_with_mocked_io("n§-29".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_string_valid_tilde() {
            assert_eq!(-29_f64, Interpreter::new_and_execute_with_mocked_io("n§~29".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_string_valid_underscore() {
            assert_eq!(29_f64, Interpreter::new_and_execute_with_mocked_io("n§2_9".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_string_valid_underscore_only() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("n§_".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_string_valid_one_dot_middle() {
            assert_eq!(1_f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .01 =n§4.125 4.125".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_string_valid_one_dot_starting() {
            assert_eq!(1_f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .01 =n§.125 .125".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_string_valid_one_dot_ending() {
            assert_eq!(27_f64, Interpreter::new_and_execute_with_mocked_io("n§27.".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_string_valid_one_dot_only() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("n§.".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_string_two_dots() {
            assert_eq!(523.7478_f64, Interpreter::new_and_execute_with_mocked_io("n523.74.78".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_empty() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("n€".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_to_num_string_invalid() {
            assert_eq!(
                Err(ScriptError::NumberParsingFailure("Illegal spaces in number input".to_string())),
                Interpreter::new_and_execute_with_mocked_io("n[stwenty two]".to_string()));
        }

        #[test]
        fn x_to_num_string_invalid_non_leading_minus() {
            assert_eq!(
                Err(ScriptError::NumberParsingFailure("Non-leading sign indicator".to_string())),
                Interpreter::new_and_execute_with_mocked_io("n§25-8".to_string()));
        }

        #[test]
        fn x_to_num_string_invalid_non_leading_tilde() {
            assert_eq!(
                Err(ScriptError::NumberParsingFailure("Non-leading sign indicator".to_string())),
                Interpreter::new_and_execute_with_mocked_io("n§25~8".to_string()));
        }

        #[test]
        fn x_len_no_args() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('o')),
                Interpreter::new_and_execute_with_mocked_io("o§len".to_string()));
        }

        #[test]
        fn x_len_one_string() {
            assert_eq!(8_f64, Interpreter::new_and_execute_with_mocked_io("o§len §TokiPona".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_len_more_strings() {
            assert_eq!(15_f64, Interpreter::new_and_execute_with_mocked_io("O§len §Antwerp §Brussels".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_len_num_formatted() {
            assert_eq!(6_f64, Interpreter::new_and_execute_with_mocked_io("o(§fmt 3 §. §_) o§len 15.8".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_len_string_empty() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("o§len §".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_len_strings_empty() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("o(§len [s] [s] §)".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_len_string_non_ascii() {
            assert_eq!(5_f64, Interpreter::new_and_execute_with_mocked_io("o§len §Αθηνά".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_sub() {
            assert_eq!("wa".to_string(), Interpreter::new_and_execute_with_mocked_io("o,§sub §Mirwart 3 2".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_sub_non_ascii() {
            assert_eq!("κός".to_string(), Interpreter::new_and_execute_with_mocked_io("o,§sub §Γραμματικός 8 3".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_sub_number() {
            // assert_eq!("123,45600".to_string(), Interpreter::new_and_execute_with_mocked_io("o,§fmt 5 §. §! o,§fmt 5 §, § 123.456".to_string()).unwrap().string_representation());
            assert_eq!(",4560".to_string(), Interpreter::new_and_execute_with_mocked_io("o,§fmt 5 §. §! o,§fmt 5 §, § o,§sub 123.456 3 5".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_sub_no_operands() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('o')),
                Interpreter::new_and_execute_with_mocked_io("o(§sub)".to_string()));
        }

        #[test]
        fn x_sub_one_operand() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('o')),
                Interpreter::new_and_execute_with_mocked_io("o§sub §Zupla".to_string()));
        }

        #[test]
        fn x_sub_start_pos_is_string() {
            assert_eq!(
                Err(ScriptError::NonNumericOperand('O')),
                Interpreter::new_and_execute_with_mocked_io("O§sub §Zupla §two".to_string()));
        }

        #[test]
        fn x_sub_negative_start_pos() {
            assert_eq!(
                Err(ScriptError::UnexpectedNegativeOperand('O')),
                Interpreter::new_and_execute_with_mocked_io("O§sub §Zupla ~3".to_string()));
        }

        #[test]
        fn x_sub_start_pos_is_source_length_requested_length_not_given() {
            assert_eq!(
                "".to_string(),
                Interpreter::new_and_execute_with_mocked_io("O§sub §Zupla 5".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_sub_start_pos_is_source_length_requested_length_zero() {
            assert_eq!(
                "".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o,§sub §Zupla 5 0".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_sub_start_pos_is_source_length_but_more_chars_requested() {
            assert_eq!(
                Err(ScriptError::PositionPastLastPossible(6)),
                Interpreter::new_and_execute_with_mocked_io("o,§sub §Zupla 5 1".to_string()));
        }

        #[test]
        fn x_sub_length_is_string() {
            assert_eq!(
                Err(ScriptError::NonNumericOperand('o')),
                Interpreter::new_and_execute_with_mocked_io("o,§sub §Zupla 1 §end".to_string()));
        }

        #[test]
        fn x_sub_no_length_given() {
            assert_eq!(
                "pla".to_string(),
                Interpreter::new_and_execute_with_mocked_io("O§sub §Zupla 2".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_sub_length_is_too_great() {
            assert_eq!(
                Err(ScriptError::PositionPastLastPossible(6)),
                Interpreter::new_and_execute_with_mocked_io("o,§sub §Zupla 1 5".to_string()));
        }

        #[test]
        fn x_ucv_ascii() {
            assert_eq!(99_f64, Interpreter::new_and_execute_with_mocked_io("O§ucv §pacifism 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_ucv_no_position_given() {
            assert_eq!(112_f64, Interpreter::new_and_execute_with_mocked_io("o§ucv §pacifism".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_ucv_hanzi() {
            assert_eq!(32147_f64, Interpreter::new_and_execute_with_mocked_io("O§ucv [s易經] 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_ucv_emoticon() {
            assert_eq!(128525_f64, Interpreter::new_and_execute_with_mocked_io("O§ucv [s😀😍] 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_ucv_no_string_given() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('o')),
                Interpreter::new_and_execute_with_mocked_io("o(§ucv)".to_string()));
        }

        #[test]
        fn x_ucv_first_operand_is_number() {
            assert_eq!(53_f64, Interpreter::new_and_execute_with_mocked_io("O§ucv 352 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_ucv_second_operand_is_string() {
            assert_eq!(
                Err(ScriptError::NonNumericOperand('O')),
                Interpreter::new_and_execute_with_mocked_io("O§ucv §Oostende §three".to_string()));
        }

        #[test]
        fn x_ucv_index_beyond_string_length() {
            assert_eq!(
                Err(ScriptError::PositionPastLastPossible(20)),
                Interpreter::new_and_execute_with_mocked_io("O§ucv §Oostende 20".to_string()));
        }

        #[test]
        fn x_find_match_in_middle() {
            assert_eq!(2_f64, Interpreter::new_and_execute_with_mocked_io("o,§find §aaabcde §ab 0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_find_match_at_start() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("o,§find §aaabaaacde §aaa 0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_find_match_at_end() {
            assert_eq!(4_f64, Interpreter::new_and_execute_with_mocked_io("o,§find §acabcde §cde 0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_find_no_start_pos_given() {
            assert_eq!(2_f64, Interpreter::new_and_execute_with_mocked_io("o,§find §aaabcde §ab".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_find_not_found() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("to,§find §aaabcde §xyz 0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_find_source_string_length_zero_second_has_chars() {
            assert_eq!(
                Err(ScriptError::FindStringLongerThanSourceString('o')),
                Interpreter::new_and_execute_with_mocked_io("o,§find § §www 0".to_string()));
        }

        #[test]
        fn x_find_source_string_and_find_string_length_zero() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("o,§find § § 0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_find_start_pos_beyond_last_occurrence() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("to,§find §aaabcde §ab 3".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_find_start_pos_beyond_first_occurrence() {
            assert_eq!(5_f64, Interpreter::new_and_execute_with_mocked_io("o,§find §aaabcabde §ab 3".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_find_no_operands() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('o')),
                Interpreter::new_and_execute_with_mocked_io("o(§find)".to_string()));
        }

        #[test]
        fn x_find_no_second_operand() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('o')),
                Interpreter::new_and_execute_with_mocked_io("o§find §Yerseke".to_string()));
        }

        #[test]
        fn x_find_empty_first_operand() {
            assert_eq!(
                Err(ScriptError::EmptyOperand('o')),
                Interpreter::new_and_execute_with_mocked_io("$0 € o,§find v0 §where 0".to_string()));
        }

        #[test]
        fn x_find_empty_find_string() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("to,§find §aaabcabde § 3".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_find_numeric_first_operand() {
            assert_eq!(1_f64, Interpreter::new_and_execute_with_mocked_io("o,§find 1334 §33 0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_find_start_pos_is_string() {
            assert_eq!(
                Err(ScriptError::NonNumericOperand('o')),
                Interpreter::new_and_execute_with_mocked_io("o,§find §Metapontium §pont §zero".to_string()));
        }

        #[test]
        fn x_find_start_pos_beyond_last_possible() {
            assert_eq!(
                Err(ScriptError::PositionPastLastPossible(9)),
                Interpreter::new_and_execute_with_mocked_io("o,§find §Metapontium §ium 9".to_string()));
        }

        #[test]
        fn x_scripted_replace_in_middle_and_at_end() {
           assert_eq!(
               "fan can".to_string(),
               Interpreter::new_and_execute_with_mocked_io(
                   "$§src [sfat cat] $§f §t $§r §n W(;($(§pos o(§find v§src v§f)) v§pos) $§src +(o,§sub v§src 0 v§pos v§r O§sub v§src +v§pos o§len v§f)) v§src".to_string()
               ).unwrap().string_representation()
           );
        }

        #[test]
        fn x_lower_no_operand() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('o')),
                Interpreter::new_and_execute_with_mocked_io("o(§lower)".to_string())
            );
        }

        #[test]
        fn x_lower_from_upper() {
            assert_eq!(
                "aix-la-chapelle".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o§lower §AIX-LA-CHAPELLE".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_lower_from_mixed() {
            assert_eq!(
                "aix-la-chapelle".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o§lower §Aix-La-Chapelle".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_lower_from_lower() {
            assert_eq!(
                "aix-la-chapelle".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o§lower §aix-la-chapelle".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_lower_from_upper_greek() {
            assert_eq!(
                "τούζλα".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o§lower §ΤΟΎΖΛΑ".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_lower_from_hanzi() {
            assert_eq!(
                "易經".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o§lower [s易經]".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_upper_no_operand() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('o')),
                Interpreter::new_and_execute_with_mocked_io("o(§upper)".to_string())
            );
        }

        #[test]
        fn x_upper_from_upper() {
            assert_eq!(
                "AIX-LA-CHAPELLE".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o§upper §AIX-LA-CHAPELLE".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_upper_from_mixed() {
            assert_eq!(
                "AIX-LA-CHAPELLE".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o§upper §Aix-La-Chapelle".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_upper_from_lower() {
            assert_eq!(
                "AIX-LA-CHAPELLE".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o§upper §aix-la-chapelle".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_upper_from_lower_greek() {
            assert_eq!(
                "ΤΟΎΖΛΑ".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o§upper §τούζλα".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_upper_from_hanzi() {
            assert_eq!(
                "易經".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o§upper [s易經]".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_proper_from_lower() {
            assert_eq!(
                "How Now Brown Cow".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o§proper [show now brown cow]".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_proper_from_upper() {
            assert_eq!(
                "How Now Brown Cow".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o§proper [sHOW NOW BROWN COW]".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_proper_sz() {
            assert_eq!(
                "SS".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o§proper §ß".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_proper_number() {
            assert_eq!(
                "4_520".to_string(),
                Interpreter::new_and_execute_with_mocked_io("o,§fmt 0 §. §_ o§proper 4520".to_string()).unwrap().string_representation()
            );
        }

        #[test]
        fn x_proper_empty() {
            assert_eq!(
                Err(ScriptError::EmptyOperand('o')),
                Interpreter::new_and_execute_with_mocked_io("o§proper c§empty".to_string())
            );
        }

        #[test]
        fn is_leap_year_1900() {
            assert!(!is_leap_year(1900_f64));
        }

        #[test]
        fn is_leap_year_2000() {
            assert!(is_leap_year(2000_f64));
        }

        #[test]
        fn is_leap_year_2011() {
            assert!(!is_leap_year(2011_f64));
        }

        #[test]
        fn is_leap_year_2012() {
            assert!(is_leap_year(2012_f64));
        }

        #[test]
        fn x_leap_false() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("o§leap 1998".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_leap_true() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("o§leap 1996".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_dow() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("o,§dow 2025 5 31".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_dow_20cent() {
            assert_eq!(3f64, Interpreter::new_and_execute_with_mocked_io("o,§dow 1903 12 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_dow_2000_jan() {
            assert_eq!(0f64, Interpreter::new_and_execute_with_mocked_io("o,§dow 2000 1 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_dow_2000_feb() {
            assert_eq!(3f64, Interpreter::new_and_execute_with_mocked_io("o,§dow 2000 2 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_dow_1900_jan() {
            assert_eq!(2f64, Interpreter::new_and_execute_with_mocked_io("o,§dow 1900 1 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_dow_1900_dec() {
            assert_eq!(1f64, Interpreter::new_and_execute_with_mocked_io("o,§dow 1900 12 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_dow_2024_dec() {
            assert_eq!(3f64, Interpreter::new_and_execute_with_mocked_io("o,§dow 2024 12 31".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_dow_2024_feb() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("o,§dow 2024 2 29".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_dow_seq_nr() {
            assert_eq!(5f64, Interpreter::new_and_execute_with_mocked_io("o,§dow 720593".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greg_first() {
            assert_eq!(1_f64, Interpreter::new_and_execute_with_mocked_io("o,§greg 0 1 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greg_first_month_of_march() {
            assert_eq!(70_f64, Interpreter::new_and_execute_with_mocked_io("o,§greg 0 3 10".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greg_last_of_first_year() {
            assert_eq!(366_f64, Interpreter::new_and_execute_with_mocked_io("o,§greg 0 12 31".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greg_one_year_plus() {
            assert_eq!(731_f64, Interpreter::new_and_execute_with_mocked_io("o,§greg 1 12 31".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greg_last_of_first_century() {
            let expected = ((365 * 100) + 25) as f64;
            assert_eq!(expected, Interpreter::new_and_execute_with_mocked_io("o,§greg 99 12 31".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greg_last_of_second_century() {
            let expected = ((365 * 200) + (24 * 2) + 1) as f64;
            assert_eq!(expected, Interpreter::new_and_execute_with_mocked_io("o,§greg 199 12 31".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greg_last_of_fourth_century() {
            let expected = ((365 * 400) + (24 * 4) + 1) as f64;
            assert_eq!(expected, Interpreter::new_and_execute_with_mocked_io("o,§greg 399 12 31".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greg_last_of_fifth_century() {
            let expected = ((365 * 500) + (24 * 5) + 2) as f64;
            assert_eq!(expected, Interpreter::new_and_execute_with_mocked_io("o,§greg 499 12 31".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greg_in_2000() {
            let expected = ((365 * 2000) + (24 * 20) + 5 + 61) as f64;
            assert_eq!(expected, Interpreter::new_and_execute_with_mocked_io("o,§greg 2000 3 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn greg_seq_0() {
            let date_info = greg_sequence_to_date(0_f64);
            assert!(date_info.is_err());
        }

        #[test]
        fn greg_seq_1() {
            let date_info = greg_sequence_to_date(1_f64).unwrap();
            assert_eq!(0_f64, date_info.year);
            assert_eq!(1_f64, date_info.month);
            assert_eq!(1_f64, date_info.day);
        }

        #[test]
        fn greg_seq_0_2_29() {
            let date_info = greg_sequence_to_date(60_f64).unwrap();
            assert_eq!(0_f64, date_info.year);
            assert_eq!(2_f64, date_info.month);
            assert_eq!(29_f64, date_info.day);
        }

        #[test]
        fn greg_seq_0_12_31() {
            let date_info = greg_sequence_to_date(366_f64).unwrap();
            assert_eq!(0_f64, date_info.year);
            assert_eq!(12_f64, date_info.month);
            assert_eq!(31_f64, date_info.day);
        }

        #[test]
        fn greg_seq_1_1_1() {
            let date_info = greg_sequence_to_date(367_f64).unwrap();
            assert_eq!(1_f64, date_info.year);
            assert_eq!(1_f64, date_info.month);
            assert_eq!(1_f64, date_info.day);
        }

        #[test]
        fn greg_seq_1_12_31() {
            let date_info = greg_sequence_to_date(731_f64).unwrap();
            assert_eq!(1_f64, date_info.year);
            assert_eq!(12_f64, date_info.month);
            assert_eq!(31_f64, date_info.day);
        }

        #[test]
        fn greg_seq_2_1_1() {
            let date_info = greg_sequence_to_date(732_f64).unwrap();
            assert_eq!(2_f64, date_info.year);
            assert_eq!(1_f64, date_info.month);
            assert_eq!(1_f64, date_info.day);
        }

        #[test]
        fn greg_seq_4_2_29() {
            let date_info = greg_sequence_to_date(1521_f64).unwrap();
            assert_eq!(4_f64, date_info.year);
            assert_eq!(2_f64, date_info.month);
            assert_eq!(29_f64, date_info.day);
        }

        #[test]
        fn greg_seq_6_3_1() {
            let date_info = greg_sequence_to_date(2252_f64).unwrap();
            assert_eq!(6_f64, date_info.year);
            assert_eq!(3_f64, date_info.month);
            assert_eq!(1_f64, date_info.day);
        }

        #[test]
        fn greg_seq_99_12_31() {
            let date_info = greg_sequence_to_date(36525_f64).unwrap();
            assert_eq!(99_f64, date_info.year);
            assert_eq!(12_f64, date_info.month);
            assert_eq!(31_f64, date_info.day);
        }

        #[test]
        fn greg_seq_100_1_1() {
            let date_info = greg_sequence_to_date(36526_f64).unwrap();
            assert_eq!(100_f64, date_info.year);
            assert_eq!(1_f64, date_info.month);
            assert_eq!(1_f64, date_info.day);
        }

        #[test]
        fn greg_seq_100_3_1() {
            let date_info = greg_sequence_to_date(36585_f64).unwrap();
            assert_eq!(100_f64, date_info.year);
            assert_eq!(3_f64, date_info.month);
            assert_eq!(1_f64, date_info.day);
        }

        #[test]
        fn greg_seq_399_12_31() {
            let date_info = greg_sequence_to_date(146097_f64).unwrap();
            assert_eq!(399_f64, date_info.year);
            assert_eq!(12_f64, date_info.month);
            assert_eq!(31_f64, date_info.day);
        }

        #[test]
        fn greg_seq_400_1_1() {
            let date_info = greg_sequence_to_date(146098_f64).unwrap();
            assert_eq!(400_f64, date_info.year);
            assert_eq!(1_f64, date_info.month);
            assert_eq!(1_f64, date_info.day);
        }

        #[test]
        fn greg_seq_400_3_1() {
            let date_info = greg_sequence_to_date(146158_f64).unwrap();
            assert_eq!(400_f64, date_info.year);
            assert_eq!(3_f64, date_info.month);
            assert_eq!(1_f64, date_info.day);
        }

        #[test]
        fn greg_seq_600_3_1() {
            let date_info = greg_sequence_to_date(219206_f64).unwrap();
            assert_eq!(600_f64, date_info.year);
            assert_eq!(3_f64, date_info.month);
            assert_eq!(1_f64, date_info.day);
        }

        #[test]
        fn greg_seq_2024_12_31() {
            let date_info = greg_sequence_to_date(739617_f64).unwrap();
            assert_eq!(2024_f64, date_info.year);
            assert_eq!(12_f64, date_info.month);
            assert_eq!(31_f64, date_info.day);
        }

        #[test]
        fn greg_seq_2025_9_10() {
            let date_info = greg_sequence_to_date(739870_f64).unwrap();
            assert_eq!(2025_f64, date_info.year);
            assert_eq!(9_f64, date_info.month);
            assert_eq!(10_f64, date_info.day);
        }

        #[test]
        fn greg_seq_101_1_1() {
            let date_info = greg_sequence_to_date(36891_f64).unwrap();
            assert_eq!(101_f64, date_info.year);
            assert_eq!(1_f64, date_info.month);
            assert_eq!(1_f64, date_info.day);
        }

        #[test]
        fn greg_seq_1972_11_30() {
            let date_info = greg_sequence_to_date(720593_f64).unwrap();
            assert_eq!(1972_f64, date_info.year);
            assert_eq!(11_f64, date_info.month);
            assert_eq!(30_f64, date_info.day);
        }

        #[test]
        fn x_gregy() {
            assert_eq!(2025_f64, Interpreter::new_and_execute_with_mocked_io("o§gregy 739870".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_gregm() {
            assert_eq!(9_f64, Interpreter::new_and_execute_with_mocked_io("o§gregm 739870".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_gregd() {
            assert_eq!(10_f64, Interpreter::new_and_execute_with_mocked_io("o§gregd 739870".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_greg_ymd() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            interpreter.execute_opts("o§gregy 730545".to_string(), true, false, false).unwrap();
            interpreter.execute_opts("o§gregm 730545".to_string(), true, false, false).unwrap();
            interpreter.execute_opts("o§gregd 730545".to_string(), true, false, false).unwrap();

            assert_eq!(1, interpreter.shuttle.date_item_calculations);
        }

        #[test]
        fn x_gregt_no_separator_argument() {
            assert_eq!("20250401TUE".to_string(), Interpreter::new_and_execute_with_mocked_io("o§gregt 739708".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_gregt_separator_argument_length_zero() {
            assert_eq!("20250401TUE".to_string(), Interpreter::new_and_execute_with_mocked_io("O§gregt 739708 §".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_gregt_separator_dash() {
            assert_eq!("2025-04-01-TUE".to_string(), Interpreter::new_and_execute_with_mocked_io("O§gregt 739708 §-".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_gregt_separator_blank() {
            assert_eq!("2025 04 01 TUE".to_string(), Interpreter::new_and_execute_with_mocked_io("O§gregt 739708 [s ]".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_gregt_year_two_digits() {
            assert_eq!("0055-04-01-THU".to_string(), Interpreter::new_and_execute_with_mocked_io("O§gregt 20180 §-".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_gregt_year_five_digits() {
            assert_eq!("10001-04-01-SUN".to_string(), Interpreter::new_and_execute_with_mocked_io("O§gregt 3652882 §-".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_gregt_erroneous_arg_0() {
            assert_eq!(
                Err(ScriptError::NonNumericOperand('o')),
                Interpreter::new_and_execute_with_mocked_io("o§gregt §zupla".to_string())
            );
        }

        #[test]
        fn x_gregt_erroneous_separator_arg() {
            assert_eq!(
                Err(ScriptError::NonTextOperand('O')),
                Interpreter::new_and_execute_with_mocked_io("O§gregt 739708 0".to_string())
            );
        }

        #[test]
        fn x_eval_simple() {
            assert_eq!(140_f64, Interpreter::new_and_execute_with_mocked_io("$0 40 $§prog [s+:0 100] Ev§prog".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_eval_number() {
            assert_eq!(250_f64, Interpreter::new_and_execute_with_mocked_io("$§prog [s250] Ev§prog".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_eval_empty() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("$§prog [sc§empty] tEv§prog".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_eval_routines_incl_names() {
            assert_eq!(5_f64, Interpreter::new_and_execute_with_mocked_io("$§prog [sR§incr +k1 R§half /k2] Ev§prog K9 KX§incr X§half".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_eval_routine_code_only() {
            assert_eq!(6_f64, Interpreter::new_and_execute_with_mocked_io("R§incr E[s+k1] R§half E[s/k2] K9 KX§incr KX§half X§incr".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_eval_text_repeated_same_string() {
            assert_eq!("Mary Renault".to_string(), Interpreter::new_and_execute_with_mocked_io("$0 §initial F1 3 1 1 $0E[s[sMary Renault]] v0".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_eval_text_repeated_with_different_strings() {
            assert_eq!("bbb".to_string(), Interpreter::new_and_execute_with_mocked_io("F(1 2 1 1 $0 ?=v1 1 §§aaa §§bbb $2 Ev0) v2".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_eval_numeric_repeated() {
            assert_eq!(6_f64, Interpreter::new_and_execute_with_mocked_io("$0 0 F1 3 1 1 $0E*v1 2 v0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_eval_no_args() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('E')),
                Interpreter::new_and_execute_with_mocked_io("E".to_string())
            );
        }

        #[test]
        fn x_read_file_no_args() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('r')),
                Interpreter::new_and_execute_with_mocked_io("r,".to_string())
            );
        }

        #[test]
        fn x_read_file_not_found() {
            assert_eq!(
                Err(ScriptError::FileReadFailure { path: "huh.what".to_string(), reason: "entity not found".to_string() }),
                Interpreter::new_and_execute_with_mocked_io("r, §huh.what".to_string())
            );
        }

        #[test]
        fn x_read_file() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));

            let content = "Gagaku".to_string();
            let file_name = OsStr::new("unitTests/test.input");
            let mut text_io_handler = Box::new(MockTextHandler::new());
            text_io_handler.write_text(&file_name, content.clone()).unwrap();

            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);

            assert_eq!(
                content,
                interpreter.execute_opts("r,§unitTests/test.input".to_string(), true, false, false)
                    .unwrap().string_representation());
        }

        #[test]
        fn x_write_file_no_args() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('w')),
                Interpreter::new_and_execute_with_mocked_io("w,".to_string())
            );
        }

        #[test]
        fn x_write_file_new() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));

            let new_content = "new content".to_string();
            let file_name = OsStr::new("unitTests/test.output");
            let text_io_handler = Box::new(MockTextHandler::new());

            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);

            assert_eq!(
                11_f64,
                interpreter.execute_opts("w,§unitTests/test.output [snew content]".to_string(), true, false, false)
                    .unwrap().numeric_value());

            assert_eq!(new_content, interpreter.shuttle.text_io_handler.read_text(&file_name).unwrap())
        }

        #[test]
        fn x_write_file_existing() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));

            let old_content = "previous content".to_string();
            let new_content = "new content".to_string();
            let file_name = OsStr::new("unitTests/test.output");
            let mut text_io_handler = Box::new(MockTextHandler::new());
            text_io_handler.write_text(&file_name, old_content.clone()).unwrap();

            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);

            assert_eq!(
                11_f64,
                interpreter.execute_opts("w,§unitTests/test.output [snew content]".to_string(), true, false, false)
                    .unwrap().numeric_value());

            assert_eq!(new_content, interpreter.shuttle.text_io_handler.read_text(&file_name).unwrap())
        }

        #[test]
        fn x_newline_operator() {
            assert_eq!("\n".to_string(), Interpreter::new_and_execute_with_mocked_io("¶".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_newline_constant() {
            assert_eq!("\n".to_string(), Interpreter::new_and_execute_with_mocked_io("c§n".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_fmt_switch_chars() {
            assert_eq!("4,25".to_string(), Interpreter::new_and_execute_with_mocked_io("o,§fmt 2 §, §. +§ 4.25".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_fmt_no_1000_separator() {
            assert_eq!("4,25".to_string(), Interpreter::new_and_execute_with_mocked_io("o,§fmt 2 §, § +§ 4.25".to_string()).unwrap().string_representation());
        }
    
        #[test]
        fn x_fmt_invalid() {
            assert_eq!(
                Err(ScriptError::ConflictingNumberPartSeparators),
                Interpreter::new_and_execute_with_mocked_io("o,§fmt 3 §, §,".to_string())
            );
        }

        #[test]
        fn x_gregn_30() {
            assert_eq!(30_f64, Interpreter::new_and_execute_with_mocked_io("O§gregn 2000 4".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_gregn_31() {
            assert_eq!(31_f64, Interpreter::new_and_execute_with_mocked_io("O§gregn 2000 12".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_gregn_month_1() {
            assert_eq!(31_f64, Interpreter::new_and_execute_with_mocked_io("O§gregn 2000 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_gregn_year_0() {
            assert_eq!(29_f64, Interpreter::new_and_execute_with_mocked_io("O§gregn 0 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_gregn_feb_no_leap() {
            assert_eq!(28_f64, Interpreter::new_and_execute_with_mocked_io("O§gregn 1900 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_gregn_feb_leap() {
            assert_eq!(29_f64, Interpreter::new_and_execute_with_mocked_io("O§gregn 1996 2".to_string()).unwrap().numeric_value());
        }
    
        #[test]
        fn x_gregn_0_operands() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('o')),
                Interpreter::new_and_execute_with_mocked_io("o(§fmt)".to_string())
            );
        }
    
        #[test]
        fn x_gregn_1_operand() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('o')),
                Interpreter::new_and_execute_with_mocked_io("o§gregn 2000".to_string())
            );
        }
    
        #[test]
        fn x_gregn_string_year() {
            assert_eq!(
                Err(ScriptError::NonNumericOperand('O')),
                Interpreter::new_and_execute_with_mocked_io("O§gregn §2000 5".to_string())
            );
        }
    
        #[test]
        fn x_gregn_string_month() {
            assert_eq!(
                Err(ScriptError::NonNumericOperand('O')),
                Interpreter::new_and_execute_with_mocked_io("O§gregn 2000 §5".to_string())
            );
        }
    
        #[test]
        fn x_gregn_year_negative() {
            assert_eq!(
                Err(ScriptError::InvalidDatePart('O')),
                Interpreter::new_and_execute_with_mocked_io("O§gregn ~2 5".to_string())
            );
        }
    
        #[test]
        fn x_gregn_month_0() {
            assert_eq!(
                Err(ScriptError::InvalidDatePart('O')),
                Interpreter::new_and_execute_with_mocked_io("O§gregn 1980 0".to_string())
            );
        }
    
        #[test]
        fn x_gregn_month_13() {
            assert_eq!(
                Err(ScriptError::InvalidDatePart('O')),
                Interpreter::new_and_execute_with_mocked_io("O§gregn 1980 13".to_string())
            );
        }

        #[test]
        fn x_split_single_character_sepa() {
            assert_eq!(
                "E".to_string(),
                Interpreter::new_and_execute_with_mocked_io(
                    "$§pre §fragment o,§split [sA;B;C;D;E] §; v§pre v+,v§pre 4"
                        .to_string()).unwrap().string_representation());

            assert_eq!(
                1_f64,
                Interpreter::new_and_execute_with_mocked_io("
                        $(0 §A §B §C §D §E)
                        $§pre §fragment
                        o,§split [sA;B;C;D;E] §; v§pre
                        F
                            0 4 1 20
                            ?
                                =
                                    vv20
                                    v+,v§pre v20
                                $21 1
                                ;
                                    $21 0
                                    B
                        v21
                    ".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_split_multi_character_sepa() {
            assert_eq!(
                "E".to_string(),
                Interpreter::new_and_execute_with_mocked_io(
                    "$§pre §fragment o,§split [sA--B--C--D--E] §-- v§pre v+,v§pre 4"
                        .to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_split_sepa_at_start() {
            assert_eq!(
                "".to_string(),
                Interpreter::new_and_execute_with_mocked_io(
                    "$§pre §fragment o,§split [s--A--B--C--D--E] §-- v§pre v+,v§pre 0"
                        .to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_split_sepa_at_end() {
            assert_eq!(
                "".to_string(),
                Interpreter::new_and_execute_with_mocked_io(
                    "$§pre §fragment o,§split [sA--B--C--D--E--] §-- v§pre v+,v§pre 5"
                        .to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_split_sepa_not_in_source() {
            assert_eq!(
                "A--B--C--D--E".to_string(),
                Interpreter::new_and_execute_with_mocked_io(
                    "$§pre §fragment o,§split [sA--B--C--D--E] §| v§pre v+,v§pre 0"
                        .to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_split_empty_fragment() {
            assert_eq!(
                "".to_string(),
                Interpreter::new_and_execute_with_mocked_io(
                    "$§pre §fragment o,§split [sA:B::C:D:E] §: v§pre v+,v§pre 2"
                        .to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_split_empty_separator() {
            assert_eq!(
                1_f64,
                Interpreter::new_and_execute_with_mocked_io("
                        $(0 §A §B §C §D §E)
                        $§pre §fragment
                        $
                            §count
                            o,§split [sABCDE] § v§pre
                        F
                            0 -v§count 1 1 20
                            ?
                                =
                                    vv20
                                    v+,v§pre v20
                                $21 1
                                ;
                                    $21 0
                                    B1
                        =(
                            1
                            v21
                            =0 tv+,v§pre 6
                            =5 v§count
                        )
                    ".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_split_empty_source() {
            assert_eq!(
                "".to_string(),
                Interpreter::new_and_execute_with_mocked_io(
                    "$§pre §fragment o,§split § §: v§pre v+,v§pre 0"
                        .to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_split_empty_source_and_separator() {
            assert_eq!(
                0_f64,
                Interpreter::new_and_execute_with_mocked_io(
                    "$§pre §fragment o,§split § § v§pre tv+,v§pre 0"
                        .to_string()).unwrap().numeric_value());

            assert_eq!(
                0_f64,
                Interpreter::new_and_execute_with_mocked_io(
                    "$§pre §fragment $§count o,§split § § v§pre v§count"
                        .to_string()).unwrap().numeric_value());
        }
    
        #[test]
        fn x_split_numeric_separator() {
            assert_eq!(
                Err(ScriptError::NonTextOperand('o')),
                Interpreter::new_and_execute_with_mocked_io("o,§split §A1B1C1D1E 1 §frag".to_string())
            );
        }

        #[test]
        fn x_atan2() {
            assert_eq!(1_f64, Interpreter::new_and_execute_with_mocked_io("Z§prec .000001 = 1.165904 A7 3".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_round_int() {
            assert_eq!(8_f64, Interpreter::new_and_execute_with_mocked_io("o§r 8".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_round_pos_frac_lower() {
            assert_eq!(8_f64, Interpreter::new_and_execute_with_mocked_io("o§r 8.3".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_round_pos_frac_higher() {
            assert_eq!(8_f64, Interpreter::new_and_execute_with_mocked_io("o§r 7.6".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_round_pos_frac_halfway() {
            assert_eq!(8_f64, Interpreter::new_and_execute_with_mocked_io("o§r 7.5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_round_neg_frac_lower() {
            assert_eq!(-8_f64, Interpreter::new_and_execute_with_mocked_io("o§r ~8.3".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_round_neg_frac_higher() {
            assert_eq!(-8_f64, Interpreter::new_and_execute_with_mocked_io("o§r ~7.9".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_round_neg_frac_halfway() {
            assert_eq!(-8_f64, Interpreter::new_and_execute_with_mocked_io("o§r ~7.5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_repl_no_condition() {
            assert_eq!("filadelfia".to_string(), Interpreter::new_and_execute_with_mocked_io("o,§repl §philadelphia §ph §f".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_repl_condition_on_position_first() {
            assert_eq!("filadelphia".to_string(), Interpreter::new_and_execute_with_mocked_io("O,,§repl §philadelphia §ph §f §pos §seq R,§cond =v§pos 0".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_repl_condition_on_position_second() {
            assert_eq!("philadelfia".to_string(), Interpreter::new_and_execute_with_mocked_io("O,,§repl §philadelphia §ph §f §pos §seq R,§cond =v§pos 8".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_repl_condition_on_sequence_first() {
            assert_eq!("filadelphia".to_string(), Interpreter::new_and_execute_with_mocked_io("O,,§repl §philadelphia §ph §f §pos §seq R,§cond =v§seq 0".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_repl_condition_on_sequence_second() {
            assert_eq!("philadelfia".to_string(), Interpreter::new_and_execute_with_mocked_io("O,,§repl §philadelphia §ph §f §pos §seq R,§cond =v§seq 1".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_repl_condition_always_false() {
            assert_eq!("philadelphia".to_string(), Interpreter::new_and_execute_with_mocked_io("O,,§repl §philadelphia §ph §f §pos §seq R,§cond 0".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_repl_condition_pos_too_far() {
            assert_eq!("philadelphia".to_string(), Interpreter::new_and_execute_with_mocked_io("O,,§repl §philadelphia §ph §f §pos §seq R,§cond >v§pos 20".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_repl_condition_routine_not_found() {
            assert_eq!("philadelphia".to_string(), Interpreter::new_and_execute_with_mocked_io("O,,§repl §philadelphia §ph §f §pos §seq §inexistent".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_repl_condition_true_substring_not_found() {
            assert_eq!("philadelphia".to_string(), Interpreter::new_and_execute_with_mocked_io("O,,§repl §philadelphia §ch §f §pos §seq R,§cond 1".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_repl_condition_on_position_both_shorter_replacement() {
            assert_eq!("filadelfia".to_string(), Interpreter::new_and_execute_with_mocked_io("O,,§repl §philadelphia §ph §f §pos §seq R,§cond |=v§pos 8 =v§pos 0".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_repl_condition_on_position_both_longer_replacement() {
            assert_eq!("fffiladelfffia".to_string(), Interpreter::new_and_execute_with_mocked_io("O,,§repl §philadelphia §ph §fff §pos §seq R,§cond |=v§pos 8 =v§pos 0".to_string()).unwrap().string_representation());
        }
    
        #[test]
        fn x_repl_3_operands() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('O')),
                Interpreter::new_and_execute_with_mocked_io("O§repl §Amsterdam §Ams".to_string())
            );
        }
    
        #[test]
        fn x_repl_6_operands() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('o')),
                Interpreter::new_and_execute_with_mocked_io("o,,§repl §Amsterdam §Ams §Rot §pos §seq".to_string())
            );
        }

        #[test]
        fn x_repl_condition_numeric_variables() {
            assert_eq!("Amsterdam_Rotterdam".to_string(), Interpreter::new_and_execute_with_mocked_io("O,,§repl §Amsterdam_Amsterdam §Ams §Rot 0 1 R,2 =v1 1".to_string()).unwrap().string_representation());
        }
    
        #[test]
        fn x_fibonacci_missing_index() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('o')),
                Interpreter::new_and_execute_with_mocked_io("o(§fib)".to_string())
            );
        }

        #[test]
        fn x_fibonacci_0() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("o§fib 0".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_fibonacci_greater_than_0() {
            assert_eq!(144_f64, Interpreter::new_and_execute_with_mocked_io("o§fib 12".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_fibonacci_1() {
            assert_eq!(1_f64, Interpreter::new_and_execute_with_mocked_io("o§fib 1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_fibonacci_2() {
            assert_eq!(1_f64, Interpreter::new_and_execute_with_mocked_io("o§fib 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_fibonacci_3() {
            assert_eq!(2_f64, Interpreter::new_and_execute_with_mocked_io("o§fib 3".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_fibonacci_fractal_index() {
            assert_eq!(832_040_f64, Interpreter::new_and_execute_with_mocked_io("o§fib 30.99".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_fibonacci_negative_index_1() {
            assert_eq!(1_f64, Interpreter::new_and_execute_with_mocked_io("o§fib ~1".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_fibonacci_negative_index_2() {
            assert_eq!(-1_f64, Interpreter::new_and_execute_with_mocked_io("o§fib ~2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_fibonacci_negative_index_5() {
            assert_eq!(5_f64, Interpreter::new_and_execute_with_mocked_io("o§fib ~5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_set_ign_1() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            interpreter.execute_opts("Z§ign 1".to_string(), true, false, false).unwrap();

            assert!(!interpreter.shuttle.error_breaks);
        }

        #[test]
        fn x_set_ign_0() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            interpreter.execute_opts("Z§ign 0".to_string(), true, false, false).unwrap();

            assert!(interpreter.shuttle.error_breaks);
        }

        #[test]
        fn x_set_quiet_0() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            interpreter.execute_opts("Z§quiet 0".to_string(), true, false, false).unwrap();

            assert!(!interpreter.is_quiet());
        }

        #[test]
        fn x_set_quiet_1() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);

            assert!(!interpreter.is_quiet());

            interpreter.execute_opts("Z§quiet 1".to_string(), true, false, false).unwrap();

            assert!(interpreter.is_quiet());
        }

        #[test]
        fn x_suppress_errors() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            interpreter.suppress_exit_on_error(true);

            assert_eq!(
                "end".to_string(),
                interpreter.execute_opts("+(4) §end".to_string(), true, false, false).unwrap().string_representation()
            );
        }

        #[test]
        fn x_combine_stop_on_error() {
            assert_eq!(
                Err(ScriptError::DivideByZero('/')),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 0 ;/4 0 +8 2".to_string()));
        }

        #[test]
        fn x_combine_no_stop_on_error() {
            assert_eq!(10_f64, Interpreter::new_and_execute_with_mocked_io("Z§ign 1 ;/4 0 +8 2".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_eval_no_stop_on_error() {
            assert_eq!(10_f64, Interpreter::new_and_execute_with_mocked_io("Z§ign 1 ;E[s[?___]] +5 5".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_eval_stop_on_error() {
            assert_eq!(
                Err(ScriptError::UnknownBracketContentTypeMarker { position: 2, marker: '?' }),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 0 ;E[s[?___]] +5 5".to_string())
            );
        }

        #[test]
        fn x_user_error_no_stop() {
            assert_eq!(r#"UserDefinedError("I failed")"#.to_string(), Interpreter::new_and_execute_with_mocked_io("Z§ign 1 +8 U[sI failed]".to_string()).unwrap().string_representation());
        }

        #[test]
        fn x_user_error_stop() {
            assert_eq!(
                Err(ScriptError::UserDefinedError("I failed".to_string())),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 0 +8 U[sI failed]".to_string())
            );
        }

        #[test]
        fn x_value_outside_try() {
            assert_eq!(0_f64, Interpreter::new_and_execute_with_mocked_io("tV".to_string()).unwrap().numeric_value());
        }

        #[test]
        fn x_try_2_ops_no_error() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            let outcome = interpreter.execute_opts("?,/7 2 13".to_string(), true, false, false).unwrap().numeric_value();

            assert_eq!(3.5_f64, outcome);
            assert!(interpreter.shuttle.try_outcome_stack.is_empty());
            assert!(interpreter.shuttle.error_breaks);
        }

        #[test]
        fn x_try_3_ops_no_error() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            let outcome = interpreter.execute_opts("?,(/7 2 13 V)".to_string(), true, false, false).unwrap().numeric_value();

            assert_eq!(3.5_f64, outcome);
            assert!(interpreter.shuttle.try_outcome_stack.is_empty());
            assert!(interpreter.shuttle.error_breaks);
        }

        #[test]
        fn x_try_3_ops_no_error_write_file() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            let outcome = interpreter.execute_opts("?,(w, §test.txt §Asprovalta V V)".to_string(), true, false, false).unwrap().numeric_value();

            assert_eq!(10_f64, outcome);
            assert!(interpreter.shuttle.try_outcome_stack.is_empty());
            assert!(interpreter.shuttle.error_breaks);
        }

        #[test]
        fn x_try_2_ops_error() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            let outcome = interpreter.execute_opts("?,/7 0 13".to_string(), true, false, false).unwrap().numeric_value();

            assert_eq!(13_f64, outcome);
            assert!(interpreter.shuttle.try_outcome_stack.is_empty());
            assert!(interpreter.shuttle.error_breaks);
        }

        #[test]
        fn x_try_3_ops_error() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            let outcome = interpreter.execute_opts("?,(/7 0 0 13)".to_string(), true, false, false).unwrap().numeric_value();

            assert_eq!(0_f64, outcome);
            assert!(interpreter.shuttle.try_outcome_stack.is_empty());
            assert!(interpreter.shuttle.error_breaks);
        }

        #[test]
        fn x_try_nested_value_of_nested() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            let outcome = interpreter.execute_opts("?,/7 0 ?,(/7 1 99 V)".to_string(), true, false, false).unwrap().numeric_value();

            assert_eq!(7_f64, outcome);
            assert!(interpreter.shuttle.try_outcome_stack.is_empty());
            assert!(interpreter.shuttle.error_breaks);
        }

        #[test]
        fn x_try_nested_value_of_upper() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            let outcome = interpreter.execute_opts("?,(/7 1 99 ;?,(2 44 V) V)".to_string(), true, false, false).unwrap().numeric_value();

            assert_eq!(7_f64, outcome);
            assert!(interpreter.shuttle.try_outcome_stack.is_empty());
            assert!(interpreter.shuttle.error_breaks);
        }

        #[test]
        fn x_try_nested_error_text() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            let outcome = interpreter.execute_opts("?,U§failed V".to_string(), true, false, false).unwrap().string_representation();

            assert_eq!(r#"UserDefinedError("failed")"#.to_string(), outcome);
            assert!(interpreter.shuttle.try_outcome_stack.is_empty());
            assert!(interpreter.shuttle.error_breaks);
        }

        #[test]
        fn x_try_nested_checking_error_text() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let text_io_handler = Box::new(MockTextHandler::new());
            let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
            let outcome = interpreter.execute_opts("?,/22 0 ?=1 tO§find +§ V §DivideByZero §div qV".to_string(), true, false, false).unwrap().string_representation();

            assert_eq!("div".to_string(), outcome);
            assert!(interpreter.shuttle.try_outcome_stack.is_empty());
            assert!(interpreter.shuttle.error_breaks);
        }
    
        #[test]
        fn x_quote() {
            assert_eq!(
                r#"-3,50helloUserDefinedError("xxx")"#.to_string(),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 1 o,§fmt 2 §, § q(~3.5 € §hello U§xxx)".to_string()).unwrap().string_representation());
        }
    
        #[test]
        fn x_quote_truncate() {
            assert_eq!(
                r#"-3helloUserDefinedError("xxx")"#.to_string(),
                Interpreter::new_and_execute_with_mocked_io("Z§ign 1 o,§fmt 2 §, § q,(~3.5 € §hello U§xxx)".to_string()).unwrap().string_representation());
        }
    }

    /*
    mod any_typeid_assumptions {
        use std::any::{Any, TypeId};
        use std::io::stdout;

        #[test]
        fn test_if_vec_u8() {
            let vec_u8_typeid = TypeId::of::<Vec<u8>>();
            let byte_vec = Vec::<u8>::new();
            let box_byte_vec = Box::new(Vec::<u8>::new());
            let dyn_box_byte_vec: Box<dyn std::io::Write> = Box::new(Vec::<u8>::new());
            let f64_vec = Vec::<f64>::new();
            let s_out = stdout();

            assert_eq!(vec_u8_typeid, byte_vec.type_id());
            assert_eq!(vec_u8_typeid, (*box_byte_vec).type_id());
            assert_ne!(vec_u8_typeid, (&*dyn_box_byte_vec).type_id());
            assert_ne!(vec_u8_typeid, f64_vec.type_id());
            assert_ne!(vec_u8_typeid, s_out.type_id());

            // Is the box reuseable ?
            assert_eq!(vec_u8_typeid, (*box_byte_vec).type_id());
        }
    }
    */

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
