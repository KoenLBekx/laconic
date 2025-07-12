use std::env::args;
use std::fs::read_to_string;
use laconic::Interpreter;

fn main() {
    let mut script = String::new();
    let mut show_before = false;
    let mut show_after = false;
    let mut do_execute = true;
    let mut expect_file_name = false;

    for arg_tuple in args().enumerate() {
        match arg_tuple {
            (0, _) => (),
            (_, arg) =>  {
                if !arg.starts_with('-') {
                    if expect_file_name {
                        expect_file_name = false;

                        match read_to_string(&arg) {
                            Ok(content) => script.push_str(&content),
                            Err(msg) => panic!("Error while reading {}: {}", &arg, msg),
                        }
                    } else {
                        script.push_str(&arg);
                    }
                } else {
                    if arg.contains('a') {
                        show_after = true;
                    }

                    if arg.contains('b') {
                        show_before = true;
                    }

                    if arg.contains('n') {
                        do_execute = false;
                    }

                    if arg.contains('i') && !expect_file_name {
                        expect_file_name = true;
                    }
                }
            },
        }
    }

    if !script.is_empty() {
        /*
        let writer = Box::new(stdout());
        let reader = Box::new(laconic::input::StdinReader::new());
        let text_io_handler = Box::new(FileTextHandler::new());
        let mut interpreter = Interpreter::new(writer, reader, text_io_handler);
        */

        let mut interpreter = Interpreter::new_stdio_filesys();

        match interpreter.execute_opts(
            script,
            do_execute,
            show_before,
            show_after
        ) {
            Ok(outcome) => println!("{}", outcome.string_representation()),
            Err(err) => println!("{err:?}"),
        }
    } else {
        show_syntax();
    }
}

fn show_syntax() {
    println!("Usage: laconic <options> 'script'");
    println!();
    println!("Options :");
    println!("    -n    Don't execute the script");
    println!("    -b    Show the script as a tree of operators before execution.");
    println!("    -a    Show the script as a tree of operators after  execution.");
    println!("    -i    Include the contents of a file into the script.");
    println!();
    println!("Options can be combined:");
    println!("    laconic -bn '*440 ^2 /1 12");
    println!("    won't calculate, but will show the operators tree of a calculation");
    println!("    of the first semitone higher than the diapason.");
    println!();
    println!("E.g. : laconic ' $0 1 $1 0 W!>v0 4 ;+:1 v0 +:0 1 v1'");
    println!("       will yield 10 (summation of 4)");
    println!("In a shell that supports multi-line commands,");
    println!("the script can be split over multiple lines, e.g.:");
    println!();
    println!("laconic");
    println!("' $0 1'");
    println!("' $1 0'");
    println!("' W'");
    println!("'   !>v0 4'");
    println!("'   ;'");
    println!("'     +:1 v0'");
    println!("'     +:0 1'");
    println!("' v1'");
    println!();
    println!("Statements in a script file can be included in the script");
    println!("using the -i parameter:");
    println!();
    println!("Statements in the command line before the -i parameter ");
    println!("will be included before the script file's statements;");
    println!("Statements in the command line after the -i parameter ");
    println!("will be included after the script file's statements.");
    println!();
    println!("E.g. :");
    println!("      given script file clearStack.lac having the statements");
    println!();
    println!("          Wk,k");
    println!();
    println!("      the below command");
    println!();
    println!("          $ laconic 'o§fmt 0 K(10 20 30 40) wk,' -i clearStack.lac 'wk,'");
    println!();
    println!("      will output:");
    println!();
    println!("          4");
    println!("          0");
    println!();
    println!("It's even possible to include multiple script files this way: e.g.:");
    println!();
    println!("          $ laconic -i script1.lac -i script2.lac '[c Statements ...]' -i script3.lac '[c Other statements ...]'");
    println!();
}
