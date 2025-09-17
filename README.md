# **Laconic: the language**

> *I got carried away, because I had fun.*

> *I needed an embeddable concise expression interpreter, but Laconic nearly became a programming language:*

> *besides numeric, boolean, string and date operators, it provides variables, tests, loops, routines, standard input & output, file I/O and error handling.*

> *- Koen Bekx*

## Introduction

Laconic is a Polish notation expression interpreter :<br/>
> `*+4 2 3`<br/>
> evaluates to 18.

The Laconic crate provides both
- an executable that can be called from the command line:

 > `...$ laconic '*+4 2 3'`

 > Execute the `laconic` executable without any parameters to get help.

- a library exposing `struct Interpreter`, which can be used by other applications:
```
    use laconic::Interpreter;
    let mut interpreter = Interpreter::new_stdio_filesys();

    let exe_result = interpreter.execute("*+4 2 3".to_string());

    assert!(exe_result.is_ok());
    assert_eq!(18_f64, exe_result.unwrap().numeric_value());
```
The many operators understood by the Laconic interpreter, together with the way it expects numbers and strings to be represented, constitute the *Laconic language* - see the crate's documentation.

>Note that a `struct Interpreter` instance can interprete or execute several scripts or expressions
>after one another, and preserves Laconic variables and routines that were assigned or declared
>in previous executions.

>This would allow some Laconic snippets in a specific file type to assign values to variables
>which can be used by subsequent snippets.

>Practical example:

>> a PostScript image file that would contain Laconic expression snippets in double quotes.

>> An application could preprocess this file by replacing the snippets with the actual numerical value
>> returned by their interpretation by the same Laconic `Interpreter` instance and, next,
>> removing the single-snippet lines that aren't valid PostScript commands.
