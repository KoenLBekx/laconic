//{ TODO: resolve TODO's in code.
// TODO: take ValueType::Text and ValueType::Empty into account for all operators.
// TODO: implement all intended operators.
// TODO: implement different variable arrays - see the 'u' operator.
//      Default array: 0.
//      This means that the key for HashMap Shuttle.nums has to be a (ValueType, ValueType) tuple.
// TODO: have all operators have an acceptable behavior with less or more operands than standard.
//      (Special attention for ':' !)
// TODO: make private whatever can remain private.
// TODO: remove our outcomment code reported as never used by the compiler.
// TODO: include the explanations in analysis/laconic.txt in markdown format as documentation
//      comments.
// TODO: use giantity ?
//}

use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

// Static lifetime: justified, because the functions referenced
// are compiled into the application and live as long as it runs.
// The first parameter should refer to an Expression's value property,
// the second one to a Expression's operands property (operands are Expression objects),
// the third one to the Shuttle containing other state.
type OperatorFunc = &'static dyn Fn(char, &mut ValueType, &mut [Expression], &mut Shuttle) -> Result<(), ScriptError>;

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
            Atom::Operator(c) => write!(f, "{}", c),
            Atom::Number(ref n) => write!(f, "{} ", n),
            Atom::String(_) => write!(f, "[s...]"),
            Atom::Comment(_) => write!(f, "[c...]"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ScriptError {
    DigitParsingFailure{position: usize, reason: String},
    UnexpectedClosingParenthesis,
    UnexpectedClosingBracket{position: usize},
    UnknownOperator{position: usize, operator: char},
    UnclosedBracketsAtEnd,
    UnknownBracketContentTypeMarker{position: usize, marker: char},
    InsufficientOperands(char),
    UnknownConstant(String),
    UnknownNamedOperator(String),
    UnknownRoutine(String),
    DivideByZero(char),
}

#[derive(Clone, Debug, PartialOrd)]
enum ValueType {
    Empty,
    Number(f64),
    Text(String),

    /// Only to be used to serve functionality like opr_funcs::min.
    Max,
}

impl ValueType {
    fn get_type_as_num(&self) -> f64 {
        match self {
            ValueType::Empty => 0f64,
            ValueType::Number(_) => 1f64,
            ValueType::Text(_) => 2f64,
            ValueType::Max => 99f64,
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
            ValueType::Text(ref txt) => txt.clone(),
            ValueType::Number(ref num) => format!("{}", num),
            ValueType::Max => "ValueType::Max".to_string(),
            _ => default,
        }
    }
}

impl PartialEq for ValueType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ValueType::Empty, ValueType::Empty) => true,
            (ValueType::Number(s), ValueType::Number(o)) => s == o,
            (ValueType::Text(ref s), ValueType::Text(ref o)) => *s == *o,
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
            ValueType::Max => 99u8.hash(state),
        };
    }
}

struct NumberFormat {
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
            fractal_separator: '.',
            use_thousands_separator: false,
            thousands_separator: ',',
            digit_separator: ' ',
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

                String::from(
                    char::from_u32(char_val)
                    .expect("Function format_digit should always be able to convert a number to a character."))
            },
            37f64.. => {
                format!("{}", digit.trunc())
            },
            _ => "ERR".to_string(),
        }
    }

    fn format(&self, numr: f64) -> String {

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

                    int_text = format!("{}{}{}{}", digit_separator, mod_digit, thsepa, int_text);
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
            let proposed = separator.chars().nth(0)
                .expect("A string of length > 0 should have a first character.");

            if proposed != self.thousands_separator {
                self.fractal_separator = proposed;
            }
        }
    }

    fn set_use_thousands_separator(&mut self, use_it: bool) {
        self.use_thousands_separator = use_it;
    }

    fn set_thousands_separator(&mut self, separator: String) {
        if separator.is_empty() {
            self.use_thousands_separator = false;
        } else {
            let proposed = separator.chars().nth(0)
                .expect("A string of length > 0 should have a first character.");

            if proposed != self.fractal_separator {
                self.thousands_separator = proposed;
                self.use_thousands_separator = true;
            }
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
            'l' => &opr_funcs::log,
            'i' if alternative_marks_count == 0 => &opr_funcs::intgr,
            'i' => &opr_funcs::ceiling,
            'a' => &opr_funcs::abs,
            '°' if alternative_marks_count == 0 => &opr_funcs::degrees,
            '°' => &opr_funcs::radians,
            'S' if alternative_marks_count == 0 => &opr_funcs::sine,
            'S' => &opr_funcs::asin,
            'C' if alternative_marks_count == 0 => &opr_funcs::cosine,
            'C' => &opr_funcs::acos,
            'T' if alternative_marks_count == 0 => &opr_funcs::tangent,
            'T' => &opr_funcs::atan,
            'p' => &opr_funcs::pi,
            'e' => &opr_funcs::euler_const,
            'c' => &opr_funcs::constants,
            ';' => &opr_funcs::combine,
            'm' => &opr_funcs::min,
            'M' => &opr_funcs::max,
            'N' => &opr_funcs::preceding_nr_operands,
            'W' => &opr_funcs::exec_while,
            'F' => &opr_funcs::exec_for,
            'R' if alternative_marks_count == 0 => &opr_funcs::define_routine_with_new_scope,
            'R' => &opr_funcs::define_routine_sharing_variables,
            'X' => &opr_funcs::exec_routine,
            '$' => &opr_funcs::assign_number_register,
            'v' => &opr_funcs::get_number_register,
            ':' => &opr_funcs::assignment_maker,
            'K' => &opr_funcs::push_stack,
            'k' if alternative_marks_count == 0 => &opr_funcs::pop_stack,
            'k' => &opr_funcs::stack_depth,
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
            'w' => &opr_funcs::write,
            'r' => &opr_funcs::read,
            'n' => &opr_funcs::to_number,
            's' => &opr_funcs::sign,
            't' => &opr_funcs::get_type,
            'b' if alternative_marks_count == 0 => &opr_funcs::input_base,
            'b' => &opr_funcs::output_base,
            'Ø' => &opr_funcs::empty,
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
            opr_mark: '0',
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
            opr_mark: '"',
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
        /*
        match self.value {
            ValueType::Number(num) => num,
            _ => default,
        }
        */

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

        (self.operator)(self.opr_mark, &mut self.value, &mut self.operands, shuttle)?;

        let assignment_indexes = shuttle.assignment_indexes_stack.pop()
            .expect("fn operate should always find elements in shuttle.assignment_indexes_stack after execution of the operator.");

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
            '0' => match self.value {
                ValueType::Empty => "empty_num".to_string(),
                ValueType::Number(num) => num.to_string(),

                // ValueType::Text: for numbers in routine definitions.
                ValueType::Text(ref txt) => txt.clone(),

                _ => panic!("An expression with opr_mark '0' should have a ValueType::Number, ValueType::Text or ValueType::Empty"),
            },
            '"' => match self.value {
                ValueType::Empty => "no_value".to_string(),
                ValueType::Text(ref txt) => txt.clone(),
                _ => panic!("An expression with opr_mark '\"' should either have a ValueType::Text or ValueType::Empty"),
            },
            oth => oth.to_string(),
        };

        for _ in 0..self.alternative_marks_count {
            exp_rep.push('`');
        }

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
                ValueType::Max => panic!("An Expression having opr_mark 0 or \" should not have a value of ValueType::Max."),
            };

            exp_rep.push_str(val_rep.as_str());
        }

        exp_rep
    }

    pub fn is_empty(&self) -> bool {
        match self.value {
            ValueType::Empty => true,
            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        match self.value {
            ValueType::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_text(&self) -> bool {
        match self.value {
            ValueType::Text(_) => true,
            _ => false,
        }
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

#[derive(Clone)]
struct Routine {
    body: Expression,
    in_new_variables_scope: bool,
}

// Shuttle objects are used to be passed to every operator function
// to provide state other than the operands.
struct Shuttle {
    nums: Vec<HashMap<ValueType, ValueType>>,
    stack: Vec<ValueType>,
    routines: HashMap<ValueType, Routine>,
    assignment_indexes_stack: Vec<Vec<ValueType>>,
    preceding_nr_operands: f64,
    max_iterations: f64,
    orb: f64,
    golden_ratio: Option<f64>,
    input_base: f64,
    number_format: NumberFormat,
    writer: Box<dyn output::EchoingWriter>,
    reader: Box<dyn input::StdinOrMock>,

    #[cfg(test)]
    golden_ratio_calculations: u8,
}

impl Shuttle {
    fn new(writer: Box<dyn output::EchoingWriter>, reader: Box<dyn input::StdinOrMock>) -> Self {
        Shuttle {
            nums: vec![HashMap::<ValueType, ValueType>::new()],
            stack: Vec::new(),
            routines: HashMap::new(),
            assignment_indexes_stack: Vec::new(),
            preceding_nr_operands: 0f64,
            max_iterations: 10_000f64,
            orb: 0.000_000_01f64,
            golden_ratio: None,
            input_base: 10f64,
            number_format: NumberFormat::new(),
            writer,
            reader,
            #[cfg(test)]
            golden_ratio_calculations: 0u8,
        }
    }

    fn get_top_of_vars_stack(&mut self) -> &mut HashMap<ValueType, ValueType> {
        if self.nums.is_empty() {
            self.nums.push(HashMap::<ValueType, ValueType>::new());
        }

        self.nums.last_mut().expect("The Shuttle.nums stack vector should always have at least one HashMap element.")
    }

    fn set_var(&mut self, name: ValueType, value: ValueType) {
        let vars = self.get_top_of_vars_stack();
        vars.insert(name, value);
    }

    fn get_var(&mut self, name: &ValueType) -> ValueType {
        let vars = self.get_top_of_vars_stack();

        match vars.get(name) {
            None => ValueType::Empty,
            Some(v) => v.clone(),
        }
    }

    pub fn echo_output(&self) -> Option<&[u8]> {
        self.writer.echo_bytes()
    }
}

#[derive(Debug, PartialEq)]
pub struct ExecutionOutcome {
    pub numeric_value: f64,
    pub string_representation: String,
}

impl ExecutionOutcome {
    pub fn new(numeric_value: f64, string_representation: String) -> Self {
        Self {
            numeric_value,
            string_representation,
        }
    }
}

const NO_VALUE: &str = "(no_value)";

pub struct Interpreter {
    shuttle: Shuttle,
}

impl Interpreter {
    pub fn new(writer: Box<dyn output::EchoingWriter>, reader: Box<dyn input::StdinOrMock>) -> Self {
        let shuttle = Shuttle::new(writer, reader);

        Interpreter{ shuttle, }
    }

    pub fn execute_with_mocked_io(program: String) -> Result<ExecutionOutcome, ScriptError> {
        let writer = Box::new(Vec::<u8>::new());
        let reader = Box::new(input::MockByString::new(Vec::<String>::new()));
        let mut interpreter = Self::new(writer, reader);

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
            println!("\nTree before operate() :\n{}", tree.get_representation());
        }

        if do_execute {
            tree.operate(&mut self.shuttle)?;
        }

        if show_after {
            println!("\nTree after operate() :\n{}", tree.get_representation());
        }

        Ok(match tree.get_value() {
            ValueType::Number(ref n) => ExecutionOutcome::new(*n, self.shuttle.number_format.format(*n)),
            ValueType::Text(ref s) => ExecutionOutcome::new(0f64, s.clone()),
            ValueType::Empty => ExecutionOutcome::new(0f64, NO_VALUE.to_string()),
            ValueType::Max => panic!("An Expression should not have a value of ValueType::Max."),
        })
    }

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
                if c == '_'{
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
                } else if c == '[' {
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
                } else if c == '.' {
                    reading_number = true;
                    current_num.push(c);
                } else if c == ']' {
                    return Err(ScriptError::UnexpectedClosingBracket{position: pos});
                } else if c == '#' {
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
                                    first if "scn".contains(first) => {
                                        let rest = match current_bracket_content.get(1..) {
                                            None => String::new(),
                                            Some(r) => r.to_string(),
                                        };

                                        match first {
                                            's' => result.push(Atom::String(rest)),
                                            'c' => result.push(Atom::Comment(rest)),
                                            'n' => result.push(Atom::Number(rest)),
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
                        chr if "~iav:!w°SCTcstbKXn"   .contains(chr) => 1,
                        chr if "+-*/^l%&|x$W;mM=<>ZoR".contains(chr) => 2,
                        chr if "?O"                   .contains(chr) => 3,
                        chr if "F"                    .contains(chr) => 5,
                        _                                            => 0,
                    };

                    match *c {
                        '(' => override_start_found = true,
                        '`' =>  {
                            if alternative_marks_count < u8::MAX {
                                alternative_marks_count += 1;
                            }
                        }
                        // ')' => override_end_found = true,
                        op_for_stack => {
                            if "oO".contains(op_for_stack) {
                                needed_ops += (2 * alternative_marks_count) as usize;
                            }

                            let mut new_exp =
                                Expression::new(op_for_stack, alternative_marks_count);

                            alternative_marks_count = 0;

                            // The number of arguments for the : and ` operators can't be overridden.
                            // The override_start marker will be applied to the previous operator
                            // instead.
                            if override_start_found && (!":`".contains(op_for_stack)) {
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

        // If the stack has any expression having opr_mark ')', return an error.
        for exp in exp_stack.iter() {
            if exp.opr_mark == ')' {
                return Err(ScriptError::UnexpectedClosingParenthesis);
            }
        }

        // If the stack has more than one expression, put whatever remains on the stack as operands in a ; - expression.
        Ok(match exp_stack.len() {
            0 => Expression::new_number("0".to_string()),
            1 => exp_stack.pop().expect("Is should be possible to pop the single Expression from the expression stack."),
            _ => {
                let mut result = Expression::new(';', 0);

                for exp in exp_stack.into_iter().rev() {
                    result.push_operand(exp);
                }

                result
            },
        })
    }

    fn is_known_operator(op: char) -> bool {
        "~+-*/^lia%°SCTpec$v:Kk§`?WF;mMNn()=<>!&|xZoOwrstbRXØ".contains(op)
    }
}

/*
impl Default for Interpreter {
    fn default() -> Self {
        let mut writer = Box::new(Vec::<u8>::new());
        let mut reader = Box::new(input::MockByString::new(Vec::<String>::new()));
        Self::new(writer, reader)
    }
}
*/

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
    use std::collections::HashMap;
    use super::{Expression, NO_VALUE, Routine, ScriptError, Shuttle, ValueType, are_near};

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

    fn parse_number(string_rep: &String, input_base: f64, ignore_non_numeric_chars: bool) -> Result<f64, String> {
        let mut string_rep_ext = string_rep.clone().trim().to_string();
        string_rep_ext.push(' ');

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
                '-' | '~' => {
                    if char_count == 0 {
                        sign_factor = -1f64;
                    } else {
                        return Err("Non-leading sign indicator".to_string());
                    }
                }
                '.' => {
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
                        digits.push(char_val - if ch <= '9' { diff_0 } else { diff_a });
                    }
                },
                // Ignore spaces if single-character digits are used (base <= 36).
                ' ' => {
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

        // TODO : treat a second period as the start of the repeated numbers sequence.
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

    pub fn nop(_opr_mark: char, result_value: &mut ValueType, _operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        // If still needed, parse the string representation to a number.
        // TODO : for now, invalid strings are parsed to NaN; later on, a
        // ScriptError::InvalidNumber is needed.

        match result_value {
            ValueType::Text(string_rep) => {

                *result_value = ValueType::Number(parse_number(&string_rep, shuttle.input_base, true).
                    expect("Function parse_number should never fail when called from function nop."));
            },
            _ => (),
        }

        Ok(())
    }

    pub fn empty(_opr_mark: char, result_value: &mut ValueType, _operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Empty;

        Ok(())
    }

    pub fn string_expr(_opr_mark: char, _result_value: &mut ValueType, _operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        // Don't do anything.
        // Meant for Expressions that contain a fixed string value from creation.

        Ok(())
    }
    
    pub fn add(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        if has_any_string_operands(operands) {
            let mut string_outcome = String::new();

            // &* : fresh borrow avoids move of the operands.
            for op in &*operands {
                string_outcome.push_str(
                    (match op.get_value() {
                        ValueType::Empty => NO_VALUE.to_string(),   
                        ValueType::Number(num) => shuttle.number_format.format(num),
                        ValueType::Text(txt) => txt,
                        ValueType::Max => panic!("An Expression should not have a value of ValueType::Max."),
                    }).as_str()
                );
            }

            *result_value = ValueType::Text(string_outcome);

        } else {
            let mut num_outcome = 0f64;

            // &* : fresh borrow avoids move of the operands.
            for op in &*operands {
                num_outcome += op.get_num_value(0f64);
            }

            *result_value = ValueType::Number(num_outcome);
        }

        Ok(())
    }
    
    pub fn multiply(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_num_value(0f64),
                _ => outcome *= op.get_num_value(1f64),
            }
        }

        *result_value = ValueType::Number(outcome);

        Ok(())
    }
    
    pub fn power(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_num_value(0f64),
                _ => outcome = outcome.powf(op.get_num_value(1f64)),
            }
        }

        *result_value = ValueType::Number(outcome);

        Ok(())
    }
    
    pub fn minus(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut outcome = 0f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_num_value(0f64),
                _ => outcome -= op.get_num_value(0f64),
            }
        }

        *result_value = ValueType::Number(outcome);

        Ok(())
    }
    
    pub fn divide(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut outcome = 0f64;
        let mut num_val: f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_num_value(0f64),
                _ => {
                    num_val = op.get_num_value(1f64);

                    if num_val == 0f64 {
                        return Err(ScriptError::DivideByZero(opr_mark));
                    }

                    outcome /= op.get_num_value(1f64);
                },
            }
        }

        *result_value = ValueType::Number(outcome);

        Ok(())
    }
    
    pub fn modulo(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut outcome = 0f64;
        let mut num_val: f64;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => outcome += op.get_num_value(0f64),
                _ => {
                    num_val = op.get_num_value(1f64);

                    if num_val == 0f64 {
                        return Err(ScriptError::DivideByZero(opr_mark));
                    }

                    outcome %= op.get_num_value(1f64);
                },
            }
        }

        *result_value = ValueType::Number(outcome);

        Ok(())
    }

    pub fn combine(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.last() {
            Some(e) => e.get_value(),
            None => return Err(ScriptError::InsufficientOperands(opr_mark)),
        };

        Ok(())
    }
    
    pub fn min(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

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
    
    pub fn max(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

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

    pub fn unaryminus(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.first() {
            None => return Err(ScriptError::InsufficientOperands(opr_mark)),
            Some(e) => ValueType::Number(- e.get_num_value(0f64)),
        };

        Ok(())
    }

    pub fn intgr(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Number(
            match operands.first() {
                None => return Err(ScriptError::InsufficientOperands(opr_mark)),
                Some(e) => {
                    e.get_num_value(0f64).trunc()
                },
            }
        );

        Ok(())
    }

    pub fn ceiling(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Number(
            match operands.first() {
                None => return Err(ScriptError::InsufficientOperands(opr_mark)),
                Some(e) => {
                    let num = e.get_num_value(0f64);

                    if num.is_sign_positive() {
                        num.ceil()
                    } else {
                        num.floor()
                    }
                },
            }
        );

        Ok(())
    }

    pub fn abs(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.first() {
            None => return Err(ScriptError::InsufficientOperands(opr_mark)),
            Some(e) => ValueType::Number(e.get_num_value(0f64).abs()),
        };

        Ok(())
    }

    pub fn degrees(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.first() {
            None => return Err(ScriptError::InsufficientOperands(opr_mark)),
            Some(e) => {
                ValueType::Number(e.get_num_value(0f64).to_degrees())
            },
        };

        Ok(())
    }

    pub fn radians(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.first() {
            None => return Err(ScriptError::InsufficientOperands(opr_mark)),
            Some(e) => {
                ValueType::Number(e.get_num_value(0f64).to_radians())
            },
        };

        Ok(())
    }

    pub fn sine(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.first() {
            None => return Err(ScriptError::InsufficientOperands(opr_mark)),
            Some(e) => {
                ValueType::Number(e.get_num_value(0f64).sin())
            },
        };

        Ok(())
    }

    pub fn asin(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.first() {
            None => return Err(ScriptError::InsufficientOperands(opr_mark)),
            Some(e) => {
                ValueType::Number(e.get_num_value(0f64).asin())
            },
        };

        Ok(())
    }

    pub fn cosine(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.first() {
            None => return Err(ScriptError::InsufficientOperands(opr_mark)),
            Some(e) => {
                ValueType::Number(e.get_num_value(0f64).cos())
            },
        };

        Ok(())
    }

    pub fn acos(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.first() {
            None => return Err(ScriptError::InsufficientOperands(opr_mark)),
            Some(e) => {
                ValueType::Number(e.get_num_value(0f64).acos())
            },
        };

        Ok(())
    }

    pub fn tangent(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.first() {
            None => return Err(ScriptError::InsufficientOperands(opr_mark)),
            Some(e) => {
                ValueType::Number(e.get_num_value(0f64).tan())
            },
        };

        Ok(())
    }

    pub fn atan(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.first() {
            None => return Err(ScriptError::InsufficientOperands(opr_mark)),
            Some(e) => {
                ValueType::Number(e.get_num_value(0f64).atan())
            },
        };

        Ok(())
    }

    pub fn log(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            *result_value = ValueType::Number(0f64);

            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let base = operands[0].get_num_value(0f64);
        let product = operands[1].get_num_value(0f64);
        *result_value = ValueType::Number(product.log(base));

        Ok(())
    }

    pub fn pi(_opr_mark: char, result_value: &mut ValueType, _operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Number(std::f64::consts::PI);

        Ok(())
    }

    pub fn euler_const(_opr_mark: char, result_value: &mut ValueType, _operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Number(std::f64::consts::E);

        Ok(())
    }

    pub fn constants(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let const_index = operands[0].get_value();

        *result_value = match const_index {
            ValueType::Text(name) if name == "gold".to_string() =>  {
                match shuttle.golden_ratio {
                    Some(gr) => ValueType::Number(gr),
                    None => {
                        let golden_ratio = (1f64 + 5f64.sqrt()) / 2f64;
                        shuttle.golden_ratio = Some(golden_ratio);

                        #[cfg(test)]
                        {
                            shuttle.golden_ratio_calculations += 1u8;
                        }

                        ValueType::Number(golden_ratio)
                    },
                }
            },
            _ => return Err(ScriptError::UnknownConstant(const_index.get_string_value("???".to_string()))),
        };

        Ok(())
    }

    pub fn assign_number_register(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let name_is_string = operands[0].get_value().get_type_as_num() == 2f64;
        let mut name = operands[0].get_value();

        /*
        #[cfg(test)]
        println!("fn assign...: name_is_string={}", name_is_string);
        */

        let mut name_base = String::new();
        let mut index = 0f64;

        if name_is_string {
            name_base = name.get_string_value("void".to_string());
        } else {
            index = name.get_num_value(0f64);
        }

        /*
        #[cfg(test)]
        println!("fn assign...: name_base={}", name_base.clone());
        */

        let mut counter = 0f64;
        let mut reg_val = ValueType::Number(0f64);

        for opd in &operands[1..operands.len()] {
            /*
            #[cfg(test)]
            println!("fn assign...: name={:?}", name.clone());
            */

            reg_val = opd.get_value();
            shuttle.set_var(name, reg_val.clone());

            counter += 1f64;

            if name_is_string {
                name = ValueType::Text(format!("{}{}", name_base, counter));
            } else {
                name = ValueType::Number(index + counter);
            }
        }

        *result_value = reg_val;

        Ok(())
    }

    pub fn get_number_register(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 1 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let index = operands[0].get_value();

        /*
        #[cfg(test)]
        println!("fn get_number_register: index={:?}", index.clone());
        */

        *result_value = shuttle.get_var(&index);

        Ok(())
    }

    pub fn assignment_maker(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let index = operands[0].get_value();
        let stack_len = shuttle.assignment_indexes_stack.len();

        // Add the variable's index to the list of variables to be assigned to
        // for the operator that's the parent of the : operator, if any.
        if stack_len >= 2 {
            shuttle
                .assignment_indexes_stack
                [stack_len - 2]
                .push(index.clone());
        }

        *result_value = shuttle.get_var(&index);

        Ok(())
    }

    pub fn push_stack(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut outcome = ValueType::Empty;

        for op in operands.iter() {
            outcome = (*op).get_value();
            shuttle.stack.push(outcome.clone());
        }

        *result_value = outcome;

        Ok(())
    }

    pub fn pop_stack(_opr_mark: char, result_value: &mut ValueType, _operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match shuttle.stack.pop() {
            None => ValueType::Empty,
            Some(vt) => vt,
        };

        Ok(())
    }

    pub fn stack_depth(_opr_mark: char, result_value: &mut ValueType, _operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Number(shuttle.stack.len() as f64);

        Ok(())
    }
    
    pub fn equals(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

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
    
    pub fn less(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut outcome = true;
        let mut first = ValueType::Empty;
        let mut current: ValueType;
        let mut ops = operands.iter().enumerate();

        loop {
            match ops.next(){
                Some((count, op)) => {
                    match count {
                        0 => first = op.get_value(),
                        _ => {
                            current = op.get_value();
                            outcome = outcome && (first < current);
                            first = current;
                        },
                    }

                    if !outcome {
                        break;
                    }
                },
                None => break,
            }
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });

        Ok(())
    }
    
    pub fn greater(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut outcome = true;
        let mut first = ValueType::Empty;
        let mut current: ValueType;
        let mut ops = operands.iter().enumerate();

        loop {
            match ops.next(){
                Some((count, op)) => {
                    match count {
                        0 => first = op.get_value(),
                        _ => {
                            current = op.get_value();
                            outcome = outcome && (first > current);
                            first = current;
                        },
                    }

                    if !outcome {
                        break;
                    }
                },
                None => break,
            }
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });

        Ok(())
    }

    pub fn not(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut outcome = true;

        for op in operands {
            match op.get_value() {
                ValueType::Empty => (),
                ValueType::Text(ref s) => outcome &= s.len() == 0,
                ValueType::Number(n) => outcome &= are_near(0f64, n, shuttle.orb),
                ValueType::Max => panic!("An Expression should not have a value of ValueType::Max."),
            }
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });

        Ok(())
    }

    pub fn and(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut outcome = true;

        for op in operands {
            match op.get_value() {
                ValueType::Empty => outcome = false,
                ValueType::Text(ref s) => outcome &= s.len() > 0,
                ValueType::Number(n) => outcome &= !are_near(0f64, n, shuttle.orb),
                ValueType::Max => panic!("An Expression should not have a value of ValueType::Max."),
            }
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });

        Ok(())
    }

    pub fn or(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut outcome = false;

        for op in operands {
            match op.get_value() {
                ValueType::Empty => (),
                ValueType::Text(ref s) => outcome |= s.len() > 0,
                ValueType::Number(n) => outcome |= !are_near(0f64, n, shuttle.orb),
                ValueType::Max => panic!("An Expression should not have a value of ValueType::Max."),
            }
        }

        *result_value = ValueType::Number(if outcome {
            1f64
        } else {
            0f64
        });

        Ok(())
    }

    pub fn xor(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut trues = 0usize;

        for op in operands {
            match op.get_value() {
                ValueType::Empty => (),
                ValueType::Text(ref s) =>  {
                    if s.len() > 0 {
                        trues += 1;
                    }
                },
                ValueType::Number(n) => {
                    if !are_near(0f64, n, shuttle.orb) {
                      trues += 1;
                    }
                },
                ValueType::Max => panic!("An Expression should not have a value of ValueType::Max."),
            }
        }

        *result_value = ValueType::Number(if trues == 1 {
            1f64
        } else {
            0f64
        });

        Ok(())
    }
    
    pub fn setting(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut setting_name = ValueType::Empty;
        let mut setting_value = ValueType::Empty;

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => setting_name = op.get_value(),
                1 => setting_value = op.get_value(),
                _ => (),
            }
        }

        match setting_name {
            ValueType::Text(name) if name ==  "prec".to_string() => shuttle.orb = setting_value.get_num_value(0f64),
            ValueType::Text(name) if name == "loops".to_string() => shuttle.max_iterations = setting_value.get_num_value(0f64),
            _ => (),
        }

        *result_value = setting_value;

        Ok(())
    }

    pub fn enumerated_opr(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        let default_outcome = ValueType::Number(0f64);

        if operands.is_empty() {
            *result_value = default_outcome;

            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let opr_func = match operands[0].get_value() {
            ValueType::Text(name) if name == "uni".to_string() => get_unicode_chars,
            ValueType::Text(name) if name == "len".to_string() => get_length,
            ValueType::Text(name) if name == "fmt".to_string() => set_number_format,
            ValueType::Text(name) if name == "version".to_string() => get_version,
            unknown =>  {
                *result_value = default_outcome;

                return Err(ScriptError::UnknownNamedOperator(
                    unknown.get_string_value("???".to_string())));
            },
        };

        (opr_func)(opr_mark, result_value, &mut operands[1..], shuttle)?;

        Ok(())
    }

    pub fn get_version(_opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        const VERSION: &'static str = env!("CARGO_PKG_VERSION");
        let version_nums: Vec<&str> = VERSION.split('.').collect();

        *result_value = if operands.len() == 0 {
            ValueType::Text(VERSION.to_string())
        } else {
            match operands[0].get_num_value(0_f64) {
                0_f64 => ValueType::Text(VERSION.to_string()),
                1_f64 => ValueType::Text(version_nums.get(0).unwrap_or(&"0").to_string()),
                2_f64 => ValueType::Text(version_nums.get(1).unwrap_or(&"0").to_string()),
                3_f64 => ValueType::Text(version_nums.get(2).unwrap_or(&"0").to_string()),
                _ => ValueType::Text(VERSION.to_string()),
            }
        };

        Ok(())
    }

    pub fn set_number_format(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        for (count, op) in operands.iter().enumerate() {
            match count {
                0 => shuttle.number_format.set_fractal_digits(op.get_num_value(0f64)),
                1 => shuttle.number_format.set_fractal_separator(op.get_string_value(".".to_string())),
                2 => shuttle.number_format.set_thousands_separator(op.get_string_value(",".to_string())),
                _ => (),
            }
        }

        *result_value = ValueType::Empty;

        Ok(())
    }

    pub fn sign(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut all_positive = true;
        let mut all_negative = true;

        let mut opd_val: f64;

        for opd in &*operands {
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

        Ok(())
    }

    pub fn get_unicode_chars(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut result_string = String::new();
        let mut opd_val: f64;

        for opd in &*operands {
            opd_val = opd.get_num_value(0f64);

            match char::from_u32(opd_val as u32) {
                // opd_val is invalid unicode point
                None => result_string.push_str(format!("¿{}?", opd_val).as_str()),

                Some(ch) => result_string.push(ch),
            }
        }

        *result_value = ValueType::Text(result_string);

        Ok(())
    }

    pub fn get_length(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut tot_len = 0usize;

        for opd in operands {
            tot_len +=
            match opd.get_value() {
                ValueType::Text(txt) => txt.len(),
                ValueType::Number(num) => shuttle.number_format.format(num).len(),
                _ => 0,
            }
        }

        *result_value = ValueType::Number(tot_len as f64);

        Ok(())
    }

    pub fn get_type(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], _shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        *result_value = ValueType::Number(operands[0].get_value().get_type_as_num());

        Ok(())
    }

    pub fn input_base(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.first() {
            None => return Err(ScriptError::InsufficientOperands(opr_mark)),
            Some(e) => {
                let num_val = e.get_num_value(0f64).trunc();

                if (num_val > 1f64) && (num_val < f64::INFINITY) {
                    shuttle.input_base = num_val;
                }

                ValueType::Number(shuttle.input_base)
            },
        };

        Ok(())
    }

    pub fn output_base(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = match operands.first() {
            None => return Err(ScriptError::InsufficientOperands(opr_mark)),
            Some(e) => {
                let num_val = e.get_num_value(0f64).trunc();

                if (num_val > 1f64) && (num_val < f64::INFINITY) {
                    shuttle.number_format.set_base(num_val);
                }

                ValueType::Number(shuttle.number_format.base)
            },
        };

        Ok(())
    }
    
    pub fn write(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.is_empty() {
            /*
            let _ = shuttle.writer.write(b"(empty output)\n");
            let _ = shuttle.writer.flush();
            
            *result_value = ValueType::Text(String::new());
            */

            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        for op in operands {
            let _ = shuttle.writer.write(
                (match op.get_value() {
                    ValueType::Empty => NO_VALUE.to_string(),   
                    ValueType::Number(num) => shuttle.number_format.format(num),
                    ValueType::Text(txt) => txt,
                    ValueType::Max => panic!("An Expression should not have a value of ValueType::Max."),
                }).as_bytes()
            );

            let _ = shuttle.writer.write(b"\n");
        }

        let _ = shuttle.writer.flush();
        
        // TODO: provide sensible value in result_value.
        *result_value = ValueType::Text(String::new());

        Ok(())
    }

    pub fn read(_opr_mark: char, result_value: &mut ValueType, _operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        let input_string = match shuttle.reader.read_line() {
            Some(s) => s.replace("\n", "").replace("\r", ""),
            None => String::new(),
        };
        
        // TODO: Err-values should result in a ScriptError Result of fn read.
        *result_value = match parse_number(&input_string, shuttle.input_base, false) {
            Ok(num) => ValueType::Number(num),
            Err(_) => ValueType::Text(input_string),
        };

        Ok(())
    }

    pub fn to_number(_opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = if operands.len() == 0 {
            ValueType::Number(f64::NAN)
        } else {
            // Only read operands[0]; ignore further operands.
            match operands[0].get_value() {
                ValueType::Number(n) => ValueType::Number(n),
                ValueType::Text(t) => {
                    match parse_number(&t, shuttle.input_base, false) {
                        Ok(n) => ValueType::Number(n),
                        Err(_) => ValueType::Number(f64::NAN),
                    }
                },
                _ => ValueType::Number(f64::NAN),
            }
        };

        Ok(())
    }

    pub fn preceding_nr_operands(_opr_mark: char, result_value: &mut ValueType, _operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Number(shuttle.preceding_nr_operands);

        Ok(())
    }

    pub fn exec_while(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

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
                    },
                }

                op_count += 1;
            }
        }

        shuttle.preceding_nr_operands = iter_count;

        *result_value = ValueType::Number(outcome);

        Ok(())
    }
    
    pub fn exec_for(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 5 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut outcome = 0f64;
        let mut counter_val = 0f64;
        let mut end_val = 0f64;
        let mut increment = 1f64;
        let mut counter_var = ValueType::Number(0f64);
        let mut iter_count = 0f64;

        if operands.len() < 5 {
            *result_value = ValueType::Number(outcome);

            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        for (op_count, op) in operands.iter_mut().enumerate() {
            if op_count <= 3 {
                op.operate(shuttle)?;
            }

            match op_count {
                0 => counter_val = op.get_num_value(0f64),
                1 => end_val = op.get_num_value(0f64),
                2 => increment = op.get_num_value(1f64),
                3 => counter_var = op.get_value(),
                _ => (),
            }
        }

        let ascending = counter_val <= end_val;
        let op_len = operands.len();
        let mut op_count: usize;
        let mut op: &mut Expression;

        loop {
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

                op_count += 1;
            }

            if ascending {
                counter_val += increment;
            } else {
                counter_val -= increment;
            }
        }

        shuttle.preceding_nr_operands = iter_count;

        *result_value = ValueType::Number(outcome);

        Ok(())
    }

    pub fn exec_if(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        if operands.len() < 3 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

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
    
    pub fn define_routine_sharing_variables(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        define_routine(opr_mark, result_value, operands, shuttle, false)?;

        Ok(())
    }
    
    pub fn define_routine_with_new_scope(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        define_routine(opr_mark, result_value, operands, shuttle, true)?;

        Ok(())
    }

    fn define_routine(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle, in_new_variables_scope: bool) -> Result<(), ScriptError> {
        if operands.len() < 2 {
            *result_value = ValueType::Empty;
            
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        let mut name = ValueType::Empty;
        let mut expressions = Vec::<Expression>::new();

        for op_tuple in operands.iter_mut().enumerate() {
            match op_tuple {
                (0, op) => {
                    op.operate(shuttle)?;
                    name = op.get_value();
                },
                (_, op) => {
                    expressions.push(op.clone());
                },
            }
        }

        let body = if expressions.len() > 1 {
            let mut combinator = Expression::new(';', 0);

            for expr in expressions.into_iter() {
                combinator.operands.push(expr);
            }

            combinator
        } else {
            expressions.pop().expect("A one-element vector should support a pop() call.")
        };

        shuttle.routines.insert(name.clone(), Routine{body, in_new_variables_scope,});
        *result_value = name;

        Ok(())
    }

    pub fn exec_routine(opr_mark: char, result_value: &mut ValueType, operands: &mut [Expression], shuttle: &mut Shuttle) -> Result<(), ScriptError> {
        *result_value = ValueType::Empty;

        if operands.len() < 1 {
            return Err(ScriptError::InsufficientOperands(opr_mark));
        }

        // Returns empty value.
        let mut found_routine = Routine{
            body: Expression::new('Ø', 0),
            in_new_variables_scope: true,
        };

        for op_tuple in operands.iter_mut().enumerate() {
            match op_tuple {
                (0, op) => {
                    let name = op.get_value();

                    if let Some(ref expr) = shuttle.routines.get(&name) {
                        found_routine = (*expr).clone();
                    } else {
                        return Err(ScriptError::UnknownRoutine(
                            name.get_string_value("???".to_string())));
                    }
                },
                (_, op) => {
                    shuttle.stack.push(op.get_value());
                },
            }
        }

        if found_routine.in_new_variables_scope {
            shuttle.nums.push(HashMap::new());
        }

        found_routine.body.operate(shuttle)?;

        if found_routine.in_new_variables_scope {
            shuttle.nums.pop().expect("Popping a pushed Hashmap from Shuttle.nums should always succeed.");
        }

        *result_value = found_routine.body.get_value();

        Ok(())
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
            let result = Interpreter::split_atoms(";.11:+_5.2#Simple#?[cAnd now the second operand]9119 ~ 45 + 3 2[sAaand another string][nFF88]");

            assert_eq!(Ok(vec![
                Atom::Operator(';'),
                Atom::Number(".11".to_string()),
                Atom::Operator(':'),
                Atom::Operator('+'),
                Atom::Number("5.2".to_string()),
                Atom::String("Simple#?".to_string()),
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
            let result = Interpreter::split_atoms("#Chomsky!");
            assert_eq!(Ok(vec![Atom::String("Chomsky!".to_string())]), result);
        }

        #[test]
        fn split_simple_string_ends_with_whitespace() {
            let result = Interpreter::split_atoms("#Chomsky! 45");
            assert_eq!(Ok(vec![Atom::String("Chomsky!".to_string()), Atom::Number("45".to_string())]), result);
        }

        #[test]
        fn split_simple_string_ends_with_opening_bracket() {
            let result = Interpreter::split_atoms("#Arundhati[cTest]");
            assert_eq!(Ok(vec![Atom::String("Arundhati".to_string()), Atom::Comment("Test".to_string())]), result);
        }

        #[test]
        fn split_simple_string_ends_with_opening_parenthesis() {
            let result = Interpreter::split_atoms("#Arundhati(30)");
            assert_eq!(Ok(vec![Atom::String("Arundhati".to_string()), Atom::Operator('('), Atom::Number("30".to_string()), Atom::Operator(')')]), result);
        }

        #[test]
        fn split_simple_string_ends_with_closing_parenthesis() {
            let result = Interpreter::split_atoms("(#Arundhati)");
            assert_eq!(Ok(vec![Atom::Operator('('), Atom::String("Arundhati".to_string()), Atom::Operator(')')]), result);
        }

        #[test]
        fn split_simple_string_get_var() {
            let result = Interpreter::split_atoms("v#reg1");
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

        #[test]
        fn expr_new_number_get_value() {
            let mut exp = Expression::new_number("47.11".to_string());
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let mut shuttle = Shuttle::new(writer, reader);
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
            let mut shuttle = Shuttle::new(writer, reader);
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
            let mut shuttle = Shuttle::new(writer, reader);
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
            let mut shuttle = Shuttle::new(writer, reader);
            let _ = exp.operate(&mut shuttle);

            match shuttle.get_var(&ValueType::Number(4000.3f64)) {
                ValueType::Empty => panic!("The numerical register assigned to has not been found back."),
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
            let mut shuttle = Shuttle::new(writer, reader);

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
            let mut shuttle = Shuttle::new(writer, reader);

            let _ = exp.operate(&mut shuttle);

            assert_eq!(500f64, shuttle.max_iterations);
        }
    }

    mod ops {
        use crate::{Expression, Shuttle, ValueType};
        use crate::opr_funcs::*;
        use crate::input::MockByString;

        #[test]
        fn nop_doesnt_change_value() {
            let mut the_value = ValueType::Number(500f64);
            let mut ops = Vec::<Expression>::new();

            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let mut shuttle = Shuttle::new(writer, reader);

            let _ = nop('0', &mut the_value, &mut ops, &mut shuttle);

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
            let mut shuttle = Shuttle::new(writer, reader);

            // Have the golden ratio retrieved twice from the shuttle.
            let _ = constants('c', &mut the_value, &mut ops, &mut shuttle);
            let _ = constants('c', &mut the_value, &mut ops, &mut shuttle);

            // Verify that the calculation only happened once.
            assert_eq!(1u8, shuttle.golden_ratio_calculations);
        }
    }

    mod exec {
        use crate::{ExecutionOutcome, Interpreter, NO_VALUE, are_very_near, ScriptError};
        use crate::input::{MockByString, StdinReader};

        #[test]
        fn x_nop_hex_case_insensitive() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("b16 =[n1A][n1a]".to_string()).unwrap().numeric_value)
        }

        #[test]
        fn x_remaining_stack_items_while_making_tree() {
            assert_eq!(16f64, Interpreter::execute_with_mocked_io("+ 44 1 * 8 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_nested_ops() {
            assert_eq!(97f64, Interpreter::execute_with_mocked_io("+4+90 3".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_add_3_op() {
            assert_eq!(229f64, Interpreter::execute_with_mocked_io("+(3 111 115)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_add_2_op() {
            assert_eq!(226f64, Interpreter::execute_with_mocked_io("+111 115".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_add_1_op() {
            assert_eq!(Err(ScriptError::InsufficientOperands('+')), Interpreter::execute_with_mocked_io("+111".to_string()));
        }

        #[test]
        fn x_add_0_op() {
            assert_eq!(Err(ScriptError::InsufficientOperands('+')), Interpreter::execute_with_mocked_io("+".to_string()));
        }

        #[test]
        fn x_add_strings() {
            assert_eq!("23.750000 km.".to_string(), Interpreter::execute_with_mocked_io("+23.75 [s km.]".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_add_strings_3() {
            assert_eq!("Alexandra David-Neel".to_string(), Interpreter::execute_with_mocked_io("$10 [sAlexandra] $11 [sDavid-Neel] +(v10 [s ] v11)".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_minus_3_op() {
            assert_eq!(-9f64, Interpreter::execute_with_mocked_io("-(111 115 +3 2)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_minus_2_op() {
            assert_eq!(-4f64, Interpreter::execute_with_mocked_io("-111 115".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_minus_1_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('-')),
                Interpreter::execute_with_mocked_io("-111".to_string()));
        }

        #[test]
        fn x_minus_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('-')),
                Interpreter::execute_with_mocked_io("-".to_string()));
        }

        #[test]
        fn x_combine_3_op() {
            assert_eq!(54f64, Interpreter::execute_with_mocked_io(";(1 +7 3 54)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_combine_3_op_bis() {
            assert_eq!(10f64, Interpreter::execute_with_mocked_io(";(1 54 +7 3)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_combine_2_op() {
            assert_eq!(54f64, Interpreter::execute_with_mocked_io("; 1 54".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_combine_1_op() {
            assert_eq!(54f64, Interpreter::execute_with_mocked_io(";54".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_combine_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands(';')),
                Interpreter::execute_with_mocked_io(";".to_string()));
        }

        #[test]
        fn x_override_to_1() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('+')),
                Interpreter::execute_with_mocked_io("+- 50 +(2) 3".to_string()));
        }
        
        #[test]
        fn x_override_with_operator_operands() {
            assert_eq!(13f64, Interpreter::execute_with_mocked_io("$0 10?(=%8 6 2 +:0 3 +:0 4 57)v0".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_nested_override() {
            assert_eq!(57f64, Interpreter::execute_with_mocked_io("+(- 50 +(2 1 1) 3 7 1)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_nested_override2() {
            assert_eq!(32f64, Interpreter::execute_with_mocked_io("*(2 +(3 2 3) 2)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_missing_end_of_override() {
            assert_eq!(41f64, Interpreter::execute_with_mocked_io("+ 50 -(2 3 7 1".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_missing_start_of_override() {
            assert_eq!(
                Err(ScriptError::UnexpectedClosingParenthesis),
                Interpreter::execute_with_mocked_io("+-50 2) 3".to_string()));
        }

        #[test]
        fn x_missing_start_of_override_after_expected_ops() {
            assert_eq!(
                Err(ScriptError::UnexpectedClosingParenthesis),
                Interpreter::execute_with_mocked_io("+-50 2 3)".to_string()));
        }

        #[test]
        fn x_multiply_3_op() {
            assert_eq!(210f64, Interpreter::execute_with_mocked_io("*(3 14 5)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_multiply_2_op() {
            assert_eq!(11100f64, Interpreter::execute_with_mocked_io("*111 100".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_multiply_1_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('*')),
                Interpreter::execute_with_mocked_io("*111".to_string()));
        }

        #[test]
        fn x_multiply_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('*')),
                Interpreter::execute_with_mocked_io("+ 10 *".to_string()));
        }

        #[test]
        fn x_divide_3_op() {
            assert_eq!(0.5f64, Interpreter::execute_with_mocked_io("/(70 20 7)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_divide_2_op() {
            assert_eq!(3.5f64, Interpreter::execute_with_mocked_io("/70 20".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_divide_1_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('/')),
                Interpreter::execute_with_mocked_io("/70".to_string()));
        }

        #[test]
        fn x_divide_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('/')),
                Interpreter::execute_with_mocked_io("+5 /".to_string()));
        }

        #[test]
        fn x_divide_by_zero() {
            assert_eq!(
                Err(ScriptError::DivideByZero('/')),
                Interpreter::execute_with_mocked_io("/8 0".to_string()));
        }

        #[test]
        fn x_modulo_3_op_integer() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("%(70 20 3)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_modulo_2_op_integer() {
            assert_eq!(10f64, Interpreter::execute_with_mocked_io("%70 20".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_modulo_2_op_fractal() {
            assert!(are_very_near(0.7f64, Interpreter::execute_with_mocked_io("%70 3.3".to_string()).unwrap().numeric_value));
        }

        #[test]
        fn x_modulo_1_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('%')),
                Interpreter::execute_with_mocked_io("%70".to_string()));
        }

        #[test]
        fn x_modulo_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('%')),
                Interpreter::execute_with_mocked_io("%".to_string()));
        }

        #[test]
        fn x_power_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('^')),
                Interpreter::execute_with_mocked_io("^".to_string()));
        }

        #[test]
        fn x_power_1_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('^')),
                Interpreter::execute_with_mocked_io("^49".to_string()));
        }

        #[test]
        fn x_power_2_op_int_int() {
            assert_eq!(36f64, Interpreter::execute_with_mocked_io("^6 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_power_2_op_int_fract() {
            assert_eq!(7f64, Interpreter::execute_with_mocked_io("^49 .5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_power_2_op_int_fract_neg() {
            assert_eq!(0.2f64, Interpreter::execute_with_mocked_io("^25 ~.5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_power_2_op_int_neg() {
            assert_eq!(0.2f64, Interpreter::execute_with_mocked_io("^5 ~1".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_power_2_op_fract_fract() {
            assert_eq!(4.049691346263317f64, Interpreter::execute_with_mocked_io("^16.4 .5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_power_3() {
            assert_eq!(49f64, Interpreter::execute_with_mocked_io("^(49 .5 2)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_unaryminus_3_op() {
            assert_eq!(-5.02f64, Interpreter::execute_with_mocked_io("~(5.02 8 3)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_unaryminus_1_op() {
            assert_eq!(-5.02f64, Interpreter::execute_with_mocked_io("~5.02".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_unaryminus_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('~')),
                Interpreter::execute_with_mocked_io("+ 2 ~".to_string()));
        }

        #[test]
        fn x_int_int() {
            assert_eq!(5f64, Interpreter::execute_with_mocked_io("i5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_int_fract() {
            assert_eq!(5f64, Interpreter::execute_with_mocked_io("i5.7".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_int_neg_int() {
            assert_eq!(-5f64, Interpreter::execute_with_mocked_io("i~5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_int_neg_fract() {
            assert_eq!(-5f64, Interpreter::execute_with_mocked_io("i~5.8".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_int_alt_int() {
            assert_eq!(5f64, Interpreter::execute_with_mocked_io("i`5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_int_alt_fract() {
            assert_eq!(6f64, Interpreter::execute_with_mocked_io("i`5.7".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_int_alt_neg_int() {
            assert_eq!(-5f64, Interpreter::execute_with_mocked_io("i`~5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_int_alt_neg_fract() {
            assert_eq!(-6f64, Interpreter::execute_with_mocked_io("i`~5.3".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_int_alt_alt_fract() {
            assert_eq!(6f64, Interpreter::execute_with_mocked_io("i``5.7".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_int_alt_main() {
            assert_eq!(11f64, Interpreter::execute_with_mocked_io("+i`5.7 i5.7".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_abs_pos() {
            assert_eq!(5f64, Interpreter::execute_with_mocked_io("a5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_abs_neg() {
            assert_eq!(5f64, Interpreter::execute_with_mocked_io("a~5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_min_two_first() {
            assert_eq!(128f64, Interpreter::execute_with_mocked_io("m128 277".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_min_two_second() {
            assert_eq!(277f64, Interpreter::execute_with_mocked_io("m328 277".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_min_two_equal() {
            assert_eq!(128f64, Interpreter::execute_with_mocked_io("m128 128".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_min_more() {
            assert_eq!(-31f64, Interpreter::execute_with_mocked_io("m(128 277 ~31 5)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_min_empty() {
            assert_eq!(
                NO_VALUE.to_string(),
                Interpreter::execute_with_mocked_io("m(128 v#notUsedYet [s Huh?] 277 ~31 5)".to_string())
                    .unwrap().string_representation);
        }

        #[test]
        fn x_min_num_and_string() {
            assert_eq!(-31f64, Interpreter::execute_with_mocked_io("m#Voilà ~31".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_min_strings() {
            assert_eq!("Voilà".to_string(), Interpreter::execute_with_mocked_io("m#Voilà #voilà".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_max_two_first() {
            assert_eq!(128f64, Interpreter::execute_with_mocked_io("M128 27".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_max_two_second() {
            assert_eq!(277f64, Interpreter::execute_with_mocked_io("M28 277".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_max_two_equal() {
            assert_eq!(128f64, Interpreter::execute_with_mocked_io("M128 128".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_max_more() {
            assert_eq!(366f64, Interpreter::execute_with_mocked_io("M(128 277 ~31 5 366)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_max_empty() {
            assert_eq!(
                " Huh?".to_string(),
                Interpreter::execute_with_mocked_io("M(128 v#notUsedYet [s Huh?] 277 ~31 5)".to_string())
                    .unwrap().string_representation);
        }

        #[test]
        fn x_max_num_and_string() {
            assert_eq!("Voilà".to_string(), Interpreter::execute_with_mocked_io("M#Voilà ~31".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_max_strings() {
            assert_eq!("voilà".to_string(), Interpreter::execute_with_mocked_io("M#Voilà #voilà".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_assign_num_return_value() {
            assert_eq!(111f64, Interpreter::execute_with_mocked_io("$4 111".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_assign_string() {
            assert_eq!("汉字".to_string(), Interpreter::execute_with_mocked_io("$4 [s汉字] v4".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_assign_num_serial_assignation() {
            assert_eq!(10f64, Interpreter::execute_with_mocked_io("$(4 1 2 3 4) F4 7 1 0 +:1 vv0 v1 ".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_assign_num_serial_assignation_string_name() {
            assert_eq!(7f64, Interpreter::execute_with_mocked_io("$(#reg 1 2 3 4) +v#reg2 v#reg3".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_assign_to_neg_register() {
            assert_eq!(7f64, Interpreter::execute_with_mocked_io("$~10 7 v~10".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_get_num_reg() {
            assert_eq!(90f64, Interpreter::execute_with_mocked_io("$/21 2 90 $4 45 v/21 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_get_num_reg_uninit() {
            assert_eq!(NO_VALUE.to_string(), Interpreter::execute_with_mocked_io("v200".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_add_assign() {
            assert_eq!(90f64, Interpreter::execute_with_mocked_io("$18 88 +:-21 3 2 v18".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_assignment_maker_inside_override_markers_first() {
            assert_eq!(35f64, Interpreter::execute_with_mocked_io("$1 10 $2 20 +(:1 v2 5) v1".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_assignment_maker_before_override_markers_first() {
            assert_eq!(35f64, Interpreter::execute_with_mocked_io("$1 10 $2 20 +:(1 v2 5) v1".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_assignment_maker_inside_override_markers_second() {
            assert_eq!(35f64, Interpreter::execute_with_mocked_io("$1 10 $2 20 +(v1 :2 5) v2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_assignment_maker_inside_override_markers_both() {
            assert_eq!(70f64, Interpreter::execute_with_mocked_io("$1 10 $2 20 +(:1 :2 5) +v1 v2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_assignment_maker_before_and_inside_override_markers_both() {
            assert_eq!(70f64, Interpreter::execute_with_mocked_io("$1 10 $2 20 +:(1 :2 5) +v1 v2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_unaryminus_assign() {
            assert_eq!(-88f64, Interpreter::execute_with_mocked_io("$18 88 ~:18 v18".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_equality_2_exact() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("=21.3 21.3".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_equality_2_in_orb() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .1 =21.3 21.35".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_equality_2_outside_orb() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("Z#prec .01 =21.3 21.35".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_equality_many() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("$1 -15 3 =(12 +7 5 *3 4 /36 3 v1)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_equality_many_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("$1 -15 3 =(12 +7 5 *3 4 111 /36 3 v1)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_equality_different_type() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("=21.3 #xxx".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_equality_strings_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("=[sxxx] #xxx".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_equality_strings_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("=[sxxx !!] #xxx".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_equality_strings_upper_and_lower_case() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("=#xxx #XXX".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_equality_empty() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("=v44 v44".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_equality_number_and_empty() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("=708 v44".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_less_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("< 5.000001 5.000002".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_less_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("< 5.000002 5.000002".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_less_3_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("<(5.000001 5.000002 6)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_less_3_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("<(5.000001 5.000002 3)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_less_mixed() {
            // v111 as uninitialized variable is VaueType::Empty.
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("<(v111 ~2036 2 6 #Fruehling #Zambetas)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_greater_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("> 5.000002 5.000001".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_greater_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("> 5.000001 5.000002".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_greater_3_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io(">(5.000003 5.000002 6)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_greater_3_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io(">(5.000002 5.000001 3)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_greater_mixed() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io(">(#Zambetas #Fruehling 1_000_000.2 2024 ~85 v111)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_not_1() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("!1".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_not_0() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("!0".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_not_other() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("!~145".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_not_3_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("!(0 0 0)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_not_3_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("!(0 0 2)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_not_3_false_mixed() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("!(0 [sWow!] 0)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_not_empty() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("!v#uninit".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_not_empty_simple_string() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("!#".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_not_empty_string() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("![s]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_not_string() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("!#Voilà".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_not_not_empty() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("!!v#uninit".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_and_0_operands() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('&')),
                Interpreter::execute_with_mocked_io("&".to_string()));
        }

        #[test]
        fn x_and_1_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("& /7 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_and_1_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("& -5 5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_and_2_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("& 2 /7 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_and_2_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("& 2 -5 5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_and_3_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("&(2 /7 2 %14 4)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_and_3_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("&( 2 -5 5 9)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_and_empty() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("& 9 v#uninit".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_and_empty_string() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("& 9 [s]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_and_string() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("& 9 #Nostromo".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_or_0() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('|')),
                Interpreter::execute_with_mocked_io("|".to_string()));
        }

        #[test]
        fn x_or_1_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("| /7 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_or_1_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("| -5 5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_or_2_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("| 0 /7 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_or_2_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("| 0 -5 5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_or_3_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("|(0 +3 ~2  %14 4)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_or_3_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("|( 0 -5 5 %20 5)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_or_empty() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("| 9 v#uninit".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_or_empty_string() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("| 9 [s]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_or_string() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("| 9 #Nostromo".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_xor_0() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('x')),
                Interpreter::execute_with_mocked_io("x".to_string()));
        }

        #[test]
        fn x_xor_1_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("x /7 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_xor_1_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("x -5 5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_xor_2_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("x 0 /7 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_xor_2_false_both_true() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("x 8 5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_xor_2_false_both_false() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("x 0 -5 5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_xor_3_true() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("x(0 +3 ~3  %14 4)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_xor_3_false_2_true() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("x(0 -5 7 %20 7)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_xor_3_false_3_true() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("x(1 -5 7 %20 5)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_xor_3_false_0_true() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("x(0 -5 5 %20 5)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_xor_empty() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("x 9 v#uninit".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_xor_empty_string() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("x 9 [s]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_xor_string() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("x 9 #Nostromo".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_while_simple() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("$1 4 W>v1 2 $1 1".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_while_executed() {
            assert_eq!(10f64, Interpreter::execute_with_mocked_io("$0 1 $1 0 W!>v0 4 ;+:1 v0 +:0 1 v1".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_while_executed_op_nr_overridden() {
            assert_eq!(10f64, Interpreter::execute_with_mocked_io("$0 1 $1 0 W(!>v0 4 +:1 v0 +:0 1) v1".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_while_never_executed() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("$0 1 $1 0 W!>v0 ~1 ;+:1 v0 +:0 1 v1".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_while_max_iterations() {
            assert_eq!(2f64, Interpreter::execute_with_mocked_io("$#sum 1 $#countDown 10 Z#loops 2 W>v#countDown 0 ;+:#sum v#countDown -:#countDown 1 N".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_for_4_op_asc() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('F')),
                Interpreter::execute_with_mocked_io("F(3 11 2 1)".to_string()));
        }

        #[test]
        fn x_for_5_op_asc() {
            assert_eq!(10395f64, Interpreter::execute_with_mocked_io("$0 1 F3 11 2 1 *:0 v1 v0".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_for_5_op_desc() {
            assert_eq!(10395f64, Interpreter::execute_with_mocked_io("$0 1 F11 3 2 1 *:0 v1 v0".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_for_6_op() {
            assert_eq!(10395f64, Interpreter::execute_with_mocked_io("$0 1 F(11 3 2 1 *:0 v1 $5 v1) v0".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_for_5_op_exceeds_limit() {
            assert_eq!(15f64, Interpreter::execute_with_mocked_io("Z#loops 2 $0 1 F3 11 2 1 *:0 v1 v0".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_for_descending() {
            assert_eq!(24f64, Interpreter::execute_with_mocked_io("$0 1 F4 1 1 1 *:0v1 v0".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_if_0_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('?')),
                Interpreter::execute_with_mocked_io("?".to_string()));
        }

        #[test]
        fn x_if_1_op() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('?')),
                Interpreter::execute_with_mocked_io("? 19".to_string()));
        }

        #[test]
        fn x_if_second() {
            assert_eq!(8f64, Interpreter::execute_with_mocked_io("?*70 .2 8 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_if_third() {
            assert_eq!(2f64, Interpreter::execute_with_mocked_io("?-70 70 8 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_if_5_op_second() {
            assert_eq!(4f64, Interpreter::execute_with_mocked_io("?(19 +3 1 0 7 23)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_if_5_op_last() {
            assert_eq!(23f64, Interpreter::execute_with_mocked_io("$5 0 ?(v5 +3 1 0 7 23)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_if_strings() {
            assert_eq!(ExecutionOutcome::new(0f64, "Smaller".to_string()), Interpreter::execute_with_mocked_io("?<4 5 [sSmaller] [sGreater]".to_string()).unwrap());
        }

        #[test]
        fn x_if_mixed() {
            assert_eq!(ExecutionOutcome::new(5f64, "5.000000".to_string()), Interpreter::execute_with_mocked_io("?5 5 [sNot five]".to_string()).unwrap());
        }

        #[test]
        fn x_enum_opr_unknown() {
            assert_eq!(
                Err(ScriptError::UnknownNamedOperator("1.5".to_string())),
                Interpreter::execute_with_mocked_io("o1.5 2".to_string()));
        }

        #[test]
        fn x_sign_no_operands() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('s')),
                Interpreter::execute_with_mocked_io("s".to_string()));
        }

        #[test]
        fn x_sign_all_pos() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("s(45 7 99 4022)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_sign_all_neg() {
            assert_eq!(-1f64, Interpreter::execute_with_mocked_io("s(~45 ~7 ~99 ~4022)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_sign_mixed() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("s(~45 7 ~99 4022)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_sign_assign() {
            assert_eq!(-1f64, Interpreter::execute_with_mocked_io("$~22 ~4 s:~22 v~22".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_enum_opr_override() {
            assert_eq!("fg".to_string(), Interpreter::execute_with_mocked_io("O(#uni 102 103)".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_enum_opr_backticks() {
            assert_eq!("fgh".to_string(), Interpreter::execute_with_mocked_io("o`#uni 102 103 104".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_enum_opr_unicode_chars() {
            assert_eq!("\n".to_string(), Interpreter::execute_with_mocked_io("O#uni 10".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_enum_opr_unicode_chars_more() {
            assert_eq!("Союз".to_string(), Interpreter::execute_with_mocked_io("o(#uni 1057 1086 1102 1079)".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_enum_opr_unicode_chars_more_bis() {
            assert_eq!("Союз".to_string(), Interpreter::execute_with_mocked_io("o#uni(1057 1086 1102 1079)".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_enum_opr_version_no_second_operand() {
            const VERSION: &'static str = env!("CARGO_PKG_VERSION");
            assert_eq!(VERSION.to_string(), Interpreter::execute_with_mocked_io("o#version".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_enum_opr_version_full() {
            const VERSION: &'static str = env!("CARGO_PKG_VERSION");
            assert_eq!(VERSION.to_string(), Interpreter::execute_with_mocked_io("o#version 0".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_enum_opr_version_invalid_operand() {
            const VERSION: &'static str = env!("CARGO_PKG_VERSION");
            assert_eq!(VERSION.to_string(), Interpreter::execute_with_mocked_io("o#version 8".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_enum_opr_version_major() {
            const VERSION: &'static str = env!("CARGO_PKG_VERSION");
            let version_nums: Vec<&str> = VERSION.split('.').collect();
            let requested = version_nums.get(0).unwrap_or(&"_").to_string();
            assert_eq!(requested, Interpreter::execute_with_mocked_io("o#version 1".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_enum_opr_version_minor() {
            const VERSION: &'static str = env!("CARGO_PKG_VERSION");
            let version_nums: Vec<&str> = VERSION.split('.').collect();
            let requested = version_nums.get(1).unwrap_or(&"_").to_string();
            assert_eq!(requested, Interpreter::execute_with_mocked_io("o#version 2".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_enum_opr_version_revision() {
            const VERSION: &'static str = env!("CARGO_PKG_VERSION");
            let version_nums: Vec<&str> = VERSION.split('.').collect();
            let requested = version_nums.get(2).unwrap_or(&"_").to_string();
            assert_eq!(requested, Interpreter::execute_with_mocked_io("o#version 3".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_write() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let mut interpreter = Interpreter::new(writer, reader);
            interpreter.execute_opts("w[sHello!]".to_string(), true, false, false).unwrap();

            assert_eq!(b'H', interpreter.echo_output().unwrap()[0]);
        }

        #[test]
        fn x_write_more() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let mut interpreter = Interpreter::new(writer, reader);
            interpreter.execute_opts("w([sA] [sB] [sC])".to_string(), true, false, false).unwrap();

            assert_eq!(6, interpreter.echo_output().unwrap().len());
        }

        #[test]
        fn x_write_number() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let mut interpreter = Interpreter::new(writer, reader);
            interpreter.execute_opts("w+100 300".to_string(), true, false, false).unwrap();

            // 400.000000\n = 11 chars.
            assert_eq!(11, interpreter.echo_output().unwrap().len());

            assert_eq!(
                vec![b'4', b'0', b'0', b'.', b'0', b'0', b'0', b'0', b'0', b'0', b'\n'],
                *interpreter.echo_output().unwrap());
        }

        #[test]
        fn x_write_number_formatted() {
            let writer = Box::new(Vec::<u8>::new());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let mut interpreter = Interpreter::new(writer, reader);
            interpreter.execute_opts("b`16 o(#fmt 2 #. #,) w31.5".to_string(), true, false, false).unwrap();

            // 1F.80\n
            // TO BE SOLVED assert_eq!(6, writer.len());
            // TO BE SOLVED assert_eq!(vec![b'1', b'F', b'.', b'8', b'0', b'\n'], *writer);
        }

        #[test]
        fn x_write_nothing() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('w')),
                Interpreter::execute_with_mocked_io("w".to_string()));

        }

        #[test]
        fn x_write_to_stdout() {
            // To be tested using the -- --nocapture argument.
            let writer = Box::new(std::io::stdout());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let mut interpreter = Interpreter::new(writer, reader);
            interpreter.execute_opts("w[s\n========== Hello from x_write_to_stdout() ! ==========\n]".to_string(), true, false, false).unwrap();
        }

        #[test]
        fn x_write_to_sink() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(Vec::<String>::new()));
            let mut interpreter = Interpreter::new(writer, reader);
            interpreter.execute_opts("w[sGarbage]".to_string(), true, false, false).unwrap();
        }

        #[test]
        fn x_read_number_base10() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["16.2".to_string()]));
            let mut interpreter = Interpreter::new(writer, reader);
            assert_eq!(17f64, interpreter.execute_opts("w[sEnter a number:]+r0.8".to_string(), true, false, false).unwrap().numeric_value);
        }

        #[test]
        fn x_read_neg_number_base10_minus() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["-16.2".to_string()]));
            let mut interpreter = Interpreter::new(writer, reader);
            assert_eq!(-14.2f64, interpreter.execute_opts("w[sEnter a number:]+r2".to_string(), true, false, false).unwrap().numeric_value);
        }

        #[test]
        fn x_read_neg_number_base10_tilde() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["~16.2".to_string()]));
            let mut interpreter = Interpreter::new(writer, reader);
            assert_eq!(-14.2f64, interpreter.execute_opts("w[sEnter a number:]+r2".to_string(), true, false, false).unwrap().numeric_value);
        }

        #[test]
        fn x_read_neg_number_base10_non_leading() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["16-".to_string()]));
            let mut interpreter = Interpreter::new(writer, reader);
            let outcome = interpreter.execute_opts("w[sEnter a number:]+r 2".to_string(), true, false, false).unwrap().string_representation;

            println!("Outcome: {:?}", outcome);

            assert_eq!(2f64, interpreter.execute_opts("w[sEnter a number:]+r2".to_string(), true, false, false).unwrap().numeric_value);
        }

        #[test]
        fn x_read_number_base16() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["1F.3".to_string()]));
            let mut interpreter = Interpreter::new(writer, reader);
            assert_eq!(32f64, interpreter.execute_opts("b16 w[sEnter a number:]+r[n0.D]".to_string(), true, false, false).unwrap().numeric_value);
        }

        #[test]
        fn x_read_string() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["1a1".to_string()]));
            let mut interpreter = Interpreter::new(writer, reader);
            assert_eq!("1a1200.000000".to_string(), interpreter.execute_opts("w[sEnter something:]+r200".to_string(), true, false, false).unwrap().string_representation);
        }

        #[test]
        fn x_read_number_and_string() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["200".to_string(), "1a1".to_string()]));
            let mut interpreter = Interpreter::new(writer, reader);
            assert_eq!("1a1200.000000".to_string(), interpreter.execute_opts("+rr".to_string(), true, false, false).unwrap().string_representation);
        }

        #[test]
        #[ignore]
        fn x_read_from_stdin() {
            // To be tested using the -- --ignored --nocapture arguments.
            let writer = Box::new(std::io::stdout());
            let reader = Box::new(StdinReader::new());
            let mut interpreter = Interpreter::new(writer, reader);
            interpreter.execute_opts("w[sEnter a number:] $0r w+[sYou entered: ]v0 w+[sDouble is: ]*v0 2".to_string(), true, false, false).unwrap();
        }

        #[test]
        fn x_radians() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .000005 = °`180 p".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_alternative_inside_override() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .000005 = °(`180) p".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_alternative_before_override() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .000005 = °`(180) p".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_sin() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .00001 = S°`45 0.7071".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_arcsin() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .5= °S`0.7071 45".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_cos() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .00001 = C°`45 0.7071".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_arccos() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .5 = °C`0.7071 45".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_tan() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .00001 = T°`45 1".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_arctan() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .00001 = °T`1 45".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_degrees() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .000005 = 180 °p".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_pi() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .000005 =p 3.14159".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_euler_const() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .000005 =e 2.71828".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_log_no_2_args() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('l')),
                Interpreter::execute_with_mocked_io("l10".to_string()));
        }

        #[test]
        fn x_log_more_args() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .000001 =2 l(10 100 3046)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_log10() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .000001 =3 l10 1000".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_log_ln() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .000001 =1 le e".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_consts_unknown() {
            assert_eq!(
                Err(ScriptError::UnknownConstant("-2".to_string())),
                Interpreter::execute_with_mocked_io("c~2".to_string()));
        }

        #[test]
        fn x_consts_golden_ratio() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("Z#prec .000001   $0 10   $1 *v0c#gold   =/+v0v1 v1 /v1 v0".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_get_type_empty() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("t v9494".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_get_type_number() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("t %38 5".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_get_type_string() {
            assert_eq!(2f64, Interpreter::execute_with_mocked_io("t +[sTotal: ]38".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_var_having_string_name() {
            assert_eq!(5f64, Interpreter::execute_with_mocked_io("$0 11 $[sMy Number] 3 +v[sMy Number] 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_var_having_simple_string_name() {
            assert_eq!(5f64, Interpreter::execute_with_mocked_io("$0 11 $#myNum 3 +v#myNum 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_var_having_string_name_make_assign() {
            assert_eq!(5f64, Interpreter::execute_with_mocked_io("$#myNum 3 +:#myNum 2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_base2() {
            assert_eq!(14.5f64, Interpreter::execute_with_mocked_io("b2 1110.1".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_base16() {
            assert_eq!(14.5f64, Interpreter::execute_with_mocked_io("b16 [ne.8]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_base10_digit_too_great() {
            assert_eq!(99f64, Interpreter::execute_with_mocked_io("b10 [n9F]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_base36() {
            assert_eq!(71.5f64, Interpreter::execute_with_mocked_io("b36 [n1Z.I]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_base38() {
            assert_eq!(77.5f64, Interpreter::execute_with_mocked_io("b38 [n 2 1.19]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_base38_starts_with_period() {
            assert_eq!(0.5f64, Interpreter::execute_with_mocked_io("b38 [n .19]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_base37_ends_with_period() {
            assert_eq!(114f64, Interpreter::execute_with_mocked_io("b37 [n 3 3.]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_base40_second_period_ignored() {
            assert_eq!(0.5125f64, Interpreter::execute_with_mocked_io("b40 [n .20.20]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_base5_space_separated() {
            assert_eq!(11.2f64, Interpreter::execute_with_mocked_io("b5 [n 2 1 . 1]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_base40_starts_and_ends_with_period() {
            assert_eq!(0.5f64, Interpreter::execute_with_mocked_io("b40 [n .20.]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_base100_spaces_around_period() {
            assert_eq!(243.15f64, Interpreter::execute_with_mocked_io("b100 [n 2 43 . 15]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_base60_double_spaces() {
            assert_eq!(121.2f64, Interpreter::execute_with_mocked_io("b60 [n 2  1  .  12]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_obase2() {
            assert_eq!("1000110.000000".to_string(), Interpreter::execute_with_mocked_io("b`2 70".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_split_underscore_in_simple_string() {
            assert_eq!("_".to_string(), Interpreter::execute_with_mocked_io("#_ ".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_set_fmt() {
            assert_eq!("4=500_555=6".to_string(), Interpreter::execute_with_mocked_io("o(#fmt 4 #_ #=) 4500.55559".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_set_fmt_round() {
            assert_eq!("4=500_23".to_string(), Interpreter::execute_with_mocked_io("o(#fmt 2 #_ #=) 4500.229".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_set_fmt_round_to_int() {
            assert_eq!("2".to_string(), Interpreter::execute_with_mocked_io("o(#fmt 0 #. #,) 1.9".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_set_fmt_round_to_neg_int() {
            assert_eq!("-2".to_string(), Interpreter::execute_with_mocked_io("o(#fmt 0 #. #,) ~1.9".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_add_fmt() {
            assert_eq!("Total: 4=500_555=6".to_string(), Interpreter::execute_with_mocked_io("o(#fmt 4 #_ #=) +[sTotal: ] 4500.55559".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_prec_nr_operands() {
            assert_eq!(5f64, Interpreter::execute_with_mocked_io(";+(7 3 9 -74 3 1) N".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_prec_nr_operands_none() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("N".to_string()).unwrap().numeric_value); } #[test] fn x_prec_nr_operands_avg() {
            assert_eq!(18.2f64, Interpreter::execute_with_mocked_io("/+(7 3 9 -74 3 1) N".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_prec_nr_operands_while() {
            assert_eq!(6f64, Interpreter::execute_with_mocked_io("$0 6;W>v0 0-:0 1N".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_prec_nr_operands_for() {
            assert_eq!(3f64, Interpreter::execute_with_mocked_io("F5 1 2 0 0N".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_stack_init_get_depth() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("k`".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_stack_init_pop() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("tk".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_stack_pop_more_than_pushed() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("K(45 71) kk tk".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_stack_push_one_get_depth() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("K5k`".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_stack_push_one_pop() {
            assert_eq!(2f64, Interpreter::execute_with_mocked_io("K%27 5 k".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_stack_push_more_get_depth() {
            assert_eq!(4f64, Interpreter::execute_with_mocked_io("K(5 ~45 /41 2 #Aha!) k`".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_stack_push_more_pop() {
            assert_eq!("Aha!".to_string(), Interpreter::execute_with_mocked_io("K(5 ~45 /41 2 #Aha! 99) kk".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_stack_pop_reverse_order() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("K3 K9 >kk".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_empty() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("tØ".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_routine_undeclared() {
            assert_eq!(
                Err(ScriptError::UnknownRoutine("unknown".to_string())),
                Interpreter::execute_with_mocked_io("tX#unknown".to_string()));
        }

        #[test]
        fn x_routine_one_operand() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('R')),
                Interpreter::execute_with_mocked_io("R(#oneOp) X#oneOp".to_string()));
        }

        #[test]
        fn x_routine_two_operands() {
            assert_eq!(1f64, Interpreter::execute_with_mocked_io("R#tau *2p Z#prec .01 =6.28 X#tau".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_routine_more_operands_call_twice() {
            assert_eq!(50f64, Interpreter::execute_with_mocked_io("R(#sumStack $100 0 Wk`+:100k v100) +X(#sumStack 20 30 ~10 2)X(#sumStack 9 3 ~4)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_routine_redefine() {
            assert_eq!(9f64, Interpreter::execute_with_mocked_io("R2 ^2 .5 R2 9 X2".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_routine_declaration_yields_name() {
            assert_eq!("tau".to_string(), Interpreter::execute_with_mocked_io("R#tau *p2".to_string()).unwrap().string_representation);
        }

        #[test]
        fn x_routine_sharing_variables() {
            assert_eq!(0f64, Interpreter::execute_with_mocked_io("R`(#empty_vars $#end k $#start k Fv#start v#end 1 #count $v#count Ø) $(11 5 5 5 5 5) K(11 15) X#empty_vars tv15".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_routine_new_scope() {
            assert_eq!(5f64, Interpreter::execute_with_mocked_io("R(#empty_vars $#end k $#start k Fv#start v#end 1 #count $v#count Ø) $(11 5 5 5 5 5) K(11 15) X#empty_vars v15".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_interpreter_keeps_variables() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["-16.2".to_string()]));
            let mut interpreter = Interpreter::new(writer, reader);
            assert_eq!(20_f64, interpreter.execute_opts("$#theNum 20 v#theNum".to_string(), true, false, false).unwrap().numeric_value);
            assert_eq!(25_f64, interpreter.execute_opts("+v#theNum 5".to_string(), true, false, false).unwrap().numeric_value);
        }

        #[test]
        fn x_interpreter_keeps_stack() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["-16.2".to_string()]));
            let mut interpreter = Interpreter::new(writer, reader);
            assert_eq!(251_f64, interpreter.execute_opts("K 251".to_string(), true, false, false).unwrap().numeric_value);
            assert_eq!(251_f64, interpreter.execute_opts("k".to_string(), true, false, false).unwrap().numeric_value);
        }

        #[test]
        fn x_interpreter_keeps_routines() {
            let writer = Box::new(std::io::sink());
            let reader = Box::new(MockByString::new(vec!["-16.2".to_string()]));
            let mut interpreter = Interpreter::new(writer, reader);
            assert_eq!("dup".to_string(), interpreter.execute_opts("R#dup *k2".to_string(), true, false, false).unwrap().string_representation);
            assert_eq!(2000_f64, interpreter.execute_opts("X(#dup 1000)".to_string(), true, false, false).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_num() {
            assert_eq!(8_f64, Interpreter::execute_with_mocked_io("n8".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_string_valid() {
            assert_eq!(29_f64, Interpreter::execute_with_mocked_io("n#29".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_string_valid_base16() {
            assert_eq!(26_f64, Interpreter::execute_with_mocked_io("b16 n#1A".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_string_valid_base50() {
            assert_eq!(5050_f64, Interpreter::execute_with_mocked_io("b50 n[s2 1 0]".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_string_valid_base10_spaces() {
            assert!(Interpreter::execute_with_mocked_io("b50 n[s1 72]".to_string()).unwrap().numeric_value.is_nan());
        }

        #[test]
        fn x_to_num_string_valid_minus() {
            assert_eq!(-29_f64, Interpreter::execute_with_mocked_io("n#-29".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_string_valid_tilde() {
            assert_eq!(-29_f64, Interpreter::execute_with_mocked_io("n#~29".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_string_valid_underscore() {
            assert_eq!(29_f64, Interpreter::execute_with_mocked_io("n#2_9".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_string_valid_underscore_only() {
            assert_eq!(0_f64, Interpreter::execute_with_mocked_io("n#_".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_string_valid_one_dot_middle() {
            assert_eq!(1_f64, Interpreter::execute_with_mocked_io("Z#prec .01 =n#4.125 4.125".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_string_valid_one_dot_starting() {
            assert_eq!(1_f64, Interpreter::execute_with_mocked_io("Z#prec .01 =n#.125 .125".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_string_valid_one_dot_ending() {
            assert_eq!(27_f64, Interpreter::execute_with_mocked_io("n#27.".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_string_valid_one_dot_only() {
            assert_eq!(0_f64, Interpreter::execute_with_mocked_io("n#.".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_string_two_dots() {
            assert_eq!(523.7478_f64, Interpreter::execute_with_mocked_io("n523.74.78".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_to_num_empty() {
            assert!(Interpreter::execute_with_mocked_io("nØ".to_string()).unwrap().numeric_value.is_nan());
        }

        #[test]
        fn x_to_num_string_invalid() {
            assert!(Interpreter::execute_with_mocked_io("n[stwenty two]".to_string()).unwrap().numeric_value.is_nan());
        }

        #[test]
        fn x_to_num_string_invalid_non_leading_minus() {
            assert!(Interpreter::execute_with_mocked_io("n#25-8".to_string()).unwrap().numeric_value.is_nan());
        }

        #[test]
        fn x_to_num_string_invalid_non_leading_tilde() {
            assert!(Interpreter::execute_with_mocked_io("n#25~8".to_string()).unwrap().numeric_value.is_nan());
        }

        #[test]
        fn x_to_num_string_num_nan() {
            assert!(Interpreter::execute_with_mocked_io("nnØ".to_string()).unwrap().numeric_value.is_nan());
        }

        #[test]
        fn x_adding_nan() {
            assert!(Interpreter::execute_with_mocked_io("+20 nØ".to_string()).unwrap().numeric_value.is_nan());
        }

        #[test]
        fn x_len_no_args() {
            assert_eq!(
                Err(ScriptError::InsufficientOperands('o')),
                Interpreter::execute_with_mocked_io("o#len".to_string()));
        }

        #[test]
        fn x_len_one_string() {
            assert_eq!(8_f64, Interpreter::execute_with_mocked_io("o#len #TokiPona".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_len_more_strings() {
            assert_eq!(15_f64, Interpreter::execute_with_mocked_io("O#len #Antwerp #Brussels".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_len_num_formatted() {
            assert_eq!(6_f64, Interpreter::execute_with_mocked_io("o(#fmt 3 #. #_) o#len 15.8".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_len_string_empty() {
            assert_eq!(0_f64, Interpreter::execute_with_mocked_io("o#len #".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_len_strings_empty() {
            assert_eq!(0_f64, Interpreter::execute_with_mocked_io("o(#len [s] [s] #)".to_string()).unwrap().numeric_value);
        }

        #[test]
        fn x_len_string_non_ascii() {
            assert_eq!(10_f64, Interpreter::execute_with_mocked_io("o#len #Αθηνά".to_string()).unwrap().numeric_value);
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
