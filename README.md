<!--style type="text/css">
    :not(pre) > code {
        border: .5px solid #A0A0A0;
    }
</style-->
<style>td:first-child{font-size: 1.5rem;}</style>
# **Laconic: the language**

> *I got carried away, because I had fun.*

> *I needed a concise expression interpreter, but Laconic nearly became a programming language:*

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

- a library exposing `struct Interpreter`, which can be used by other applications;
```
    use laconic::Interpreter;
    let mut interpreter = Interpreter::new_stdio_filesys();

    let exe_result = interpreter.execute("*+4 2 3".to_string());

    assert!(exe_result.is_ok());
    assert_eq!(18_f64, exe_result.unwrap().numeric_value());
```
The many operators understood by the Laconic interpreter, together with the way it expects numbers and strings to be represented, constitute the *Laconic language* - see below.

Note that a `struct Interpreter` instance can interprete or execute several scripts or expressions
after one another, and preserves Laconic variables and routines that were assigned or declared
in previous executions.

This would allow some Laconic snippets in a specific file type to assign values to variables
which can be used by subsequent snippets.

Practical example:

> a PostScript image file that would contain Laconic expression snippets in double quotes.

> An application could preprocess this file by replacing the snippets with the actual numerical value
> returned by their interpretation by the same Laconic `Interpreter` instance and, next,
> removing the single-snippet lines that aren't valid PostScript commands.

## General syntax of the Laconic language

As said, Laconic interpretes "Polish" expressions: operators precede their operands.

As every operator has a default number of operands, no parentheses are needed if one doesn't
want to exceed this default number.

So, as both the multiplication and addition operators have two operands by default, `*+4 2 3` first executes `+4 2` and next `*6 3`<br/>
(So `+4 2` really is the `*` operator's first operand.)

*There is a way to have operators process more operands by using parentheses
grouping the operands: `+(7 8 9)` evaluates to 24.<br/>
Many operators, however, simply ignore excess operands: `~(4 25)` evaluates to -4.*

## Number precision

Laconic handles all numbers as `f64`: 64-bit float values. Mind that this might entail precision limitations. 

These precision limitations can be mitigated by setting an *orb* or precision allowance of comparison operators using a `Z#prec` expression in the script - see the operator's description below.

## Basic elements of the Laconic language

Laconic expressions or scripts, which are technically the same, consist of basic elements that can either be
- whitespace
- operators
- numbers
- simple strings
- string bracket contents
- number bracket contents
- comment bracket contents

All of these are case-sensitive.

If there are more elements than expected by the preceding operators,
the preceding results are discarded and the value of the remaining expression is returned.

However, this allows for a long sequence of expressions to precede the final one, the value of which will be returned or printed. This way, Laconic is a script interpreter instead of a single expression interpreter.

E.g.:<br/>
> `*+4 2 3 25`<br/>
> will return 25, just like<br/>
> `*+4 2 3 + 19 6`

***Note:** the characters backspace, double and single quote are nowhere used in Laconic, as they interfere with command line syntax.*

## Whitespace

The characters \[blank\], '\t', '\n',  and '\r' are considered whitespace.

They can be used
- to separate number operands if necessary,
- to terminate a simple string (see below)
- or to enhance readability.

## Operators and operations: general

All operators consist of a single character that precedes its operands.

***Note:** there are also "named operations" - see the `O` and `o` operators.*

See below for a detailed explanation of each operator.

As all operators consist of only one character, there's nearly never a need to separate an operator from preceding or following elements using a whitespace,
but it's permitted for readability.

> *The only instance where an operator needs to be separated by whitespace from a previous element, is when that previous element is a simple string - without preceding whitespace, the operator would be interpreted as a continuation of that simple string.*

Every operator has a default number of operands it expects. E.g., the `+` operator expects two operands :

> `+ 1 2` yields 3.

However, many operators can operate on more operands, too, but in order to deviate from the default number, parentheses have to be used:

> `+(1 2 3)` yields 6, just like<br/>
> `++1 2 3`

Many operators have a different behavior when followed by one or more *variant operator*s : `,`. E.g.:<br/>
> `°1` yields one radian as degrees (ca. 57.295779).<br/>
> `°,1` yields one degree as radians (ca. 0.017452).

Note: whenever used below, the term "operation" designates an operator with all its operands, that may be operators with operands too. In fact, an operation has a tree structure that can be visualized by passing a -b and/or a -a parameter to the laconic executable. This can be a help for debugging an expression or script. For example, the command

> `... $ laconic '$0 200 ?,(+v0 7 ;w++[sProblem: ] V ¶ 0 ;w+[sAddition succeeded] ¶ V)' -a`

> will display the below tree:

<pre>
    Tree after operate() :
    ;
    │	$
    │	│	0
    │	│	200
    │	└─> 200
    │	?,(
    │	│	+
    │	│	│	v
    │	│	│	│	0
    │	│	│	└─> 200
    │	│	│	7
    │	│	└─> 207
    │	│	;
    │	│	│	w
    │	│	│	│	+
    │	│	│	│	│	+
    │	│	│	│	│	│	Problem:
    │	│	│	│	│	│	V
    │	│	│	│	│	│	└─> no_value
    │	│	│	│	│	└─> no_value
    │	│	│	│	│	¶
    │	│	│	│	│	└─> no_value
    │	│	│	│	└─> no_value
    │	│	│	└─> no_value
    │	│	│	0
    │	│	└─> no_value
    │	│	;)
    │	│	│	w
    │	│	│	│	+
    │	│	│	│	│	Addition succeeded
    │	│	│	│	│	¶
    │	│	│	│	│	└─>
    │	│	│	│	│
    │	│	│	│	└─> Addition succeeded
    │	│	│	│
    │	│	│	└─> 19
    │	│	│	V
    │	│	│	└─> 207
    │	│	└─> 207
    │	└─> 207
    └─> 207
</pre>

## Named operations

The number of viable characters to represent operators is limited as, for many people, non-Latin characters are difficult to enter using a keyboard. 

So when Laconic wants to express every operator using only one character, it's a bit short of characters.

That's why there are also operators that are designated by an operation name; these are called "named operations".

The o and O operators take that name as their first operand in order to perform the operation of a named operation.

The difference between both operators is that<br/>
- the lower case `o` operator takes 2 operands:<br/>
> - the operation name<br/>
> - and 1 operation operand;<br/>
- the upper case `O` operator takes 3 operands:<br/>
> - the operation name<br/>
> - and 2 operation operands.

Furthermore, adding the variant operator (`,`) to both the O and o operands increases their expected number of operands by (2 * number_of_variants), so

- the `o,` operator takes 4 operands;<br/>
- the `O,` operator takes 5 operands;<br/>
- the `o,,` operator takes 6 operands;<br/>
- the `O,,` operator takes 7 operands;<br/>
- etc.

Both operators, however, can have their number of operands overridden by parentheses,
in which case they can be used interchangeably and following variant operators don't affect the number of expected operands (they can still affect the behaviour, though).

The operation name, which is the first operand of the `o` and `O` operators, can either be a number or a string.
For readability, however, strings are chosen for the implemented operators.

As stated, this string is the first operand and can be written in any valid way a first-operand-string can follow an operator:<br/>
`o #name`<br/>
`o#name`<br/>
`o [sname]`<br/>
or even on a new line:<br/>
<pre>
o
  #name
</pre>

See the *Named Operations List* section below for the implemented named operations.

## Value types

Every element, except whitespace and comments, has a value. Likewise, every operator returns a value - it's the operator's value. The value of elements or operators can be operands of other operators.

There are 4 types of values, each having a numeric id:
- 0: empty
- 1: number
- 2: string
- 90: error

The **empty value** is returned by unassigned variables, the `€` operator and some other operators when nothing useful can be returned. Many operators return an error value when processing operands having an empty value: e.g.: `+20 €`

**Number values** are returned by number elements and many numeric operators.

**String values** are returned by string elements and many string-related operators.

**Error values** are returned by the `U` (user error) operator and many other operators when they find incompatible operands or other problems.

The type of any element or any operator's return value can be tested using the `t` operator, e.g.:

> `t€` yields 0: empty<br/>
> `t/9 3` yields 1: number<br/>
> `t[sI am a string]` yields 2: string<br/>
> `Z#ign 1 t/33 0` yields 90: error (because of division by 0; without the `Z#ign 1` operation execution would have stopped immediately after the division-by-zero and the `t` operation would never have been executed completely.)

## Error handling

A script's author or Laconic executable user can choose how Laconic handles errors: either by halting or ignoring.

> **Halting**: whenever an operator returns an error value, script execution is stopped immediately and an error value is returned. The string representation of this error value can be displayed;

> **Ignoring**: Laconic treats the error outcome as just another operand value and continues execution, having operators using the error value as operand return the same error value. However, this allows the very script to handle the error.

Halting is the default way of error handling.

In scripts:

> The error handling mode can be set (and changed again) in scripts using the `Z#ign` or `?,` operators:

>> `Z#ign 1` has Laconic ignore any errors until a `Z#ign 0` operation is met:

>>> `ta€` *type of absolute value of empty* should return value 90, because *absolute value of empty* returns an error. However, the `t` operator is never executed fully, as the `a` absolute value operator halts the script execution on finding an empty operand.

>>> `Z#ign 1 ta€` does yield 90: after `Z#ign 1` has Laconic ignore errors, the `a€` operation returns an error and the `t` type operator can evaluate it to find it has type 90: error.

>> The `?,` operator takes two operands and tries and evaluates the first one. If this first operand has a non-error value, `?,` returns that one. Otherwise, the value of the second operand is returned.

>>> `?,a€ #Oops!` yields the string "Oops!", as the `?,` operator tries to evaluate its first operand (`a€`: *absolute value of empty*), gets an error value, and therefore returns the value of its second operand: the string element `#Oops!`.

>> By the way, if the `?,` operator has a third operand, this operand's value is returned on successful execution of the first one:

>>> `?,(a72 #Oops! #Ok)` yields the string "Ok".

Using the `laconic` executable, one can add the `-I` parameter to a command to have the Laconic interpreter ignore errors instead of halting immediately. E.g.:

> `...$ laconic -I '+[sOutcome: ] /15 0'`<br/>
> will output<br/>
> `Outcome: DivideByZero('/')`<br/>
> which means that the `+` (concatenation) operator still could operate after the zero-division error occurred.

## Numbers: base 10

Base 10 numbers consist of characters 0 up to 9, and may contain exactly one period as fractal separator.

(Unary minus, ~, is an operator.)

If the number is between 1 and -1 exclusive, the period may be the first character of the number.

For readability, underscores _ may be inserted anywhere in a number - in fact, prior to the interpretion of an expression, all underscores are discarded from it:

> `.000_001 = 0.000001`<br/>
> `1_000_000 = 1000000`

A number is allowed to end with a period :

> `40. = 40`

And a lonely period is interpreted as zero :

> `. = 0`

Surplus periods in a number will be ignored:

> `1.0.0.2`

is interpreted as

> `1.002`

## Numbers: other bases

The default number base is 10.

However, using the `b` and `b,` operators, one can switch to other bases for input and output, respectively.

Digits for bases greater than 10 consist of 0 to 9 and the appropriate number of upper case letters needed:

- base  2 :  0-1
- base  3 :  0-2
- base  8 :  0-7
- base 10 :  0-9
- base 11 :  0-9 and A
- base 12 :  0-9 and A-B
- base 16 :  0-9 and A-F
- base 17 :  0-9 and A-G
- base 36 :  0-9 and A-Z
- base 37 :  0 - 9 and 10 - 36
- base 200 : 0 - 9 and 10 - 199

Numbers in bases higher than 10 having digits above 9 should be enclosed in number bracket contents : `[n...]`.

e.g.:<br/>
> `b16 [n1A]` yields 26.<br/>
> `b16 1A` will consider the A character to be an operator, and might result in an error.<br/>
> `b16 11` yields 17.

If a number expressed in a base higher than 36 has digits greater than 35,
it has to be expressed as number bracket contents: `[n...]` clauses as series of base10 numbers separated by spaces:

> `b50 [n1 20]` yields 70(base10)

This reduces calculating hours, minutes and seconds to calculating seconds in base 60:

> `b,60 b60 /[n5 20 8] 4` yields [1 20 2(base60)] or 4802(base10):<br/>
> a fourth of 5 hours, 20 minutes and 8 seconds is 4802 seconds or 1 hour, 20 minutes and 2 seconds.

Combinations of `b,` and `b` commands allow conversion of numbers between bases.
In order to avoid mistakes in the specification of the output base,
it's best to put the `b,` (output base) operator before the `b` (input base) operator.

It's even possible to process numbers of different bases together:

> `*17 ;b16 21` yields 561(base10).

If a number has a digit which is too high for its base, that digit is reduced to the highest one:

e.g.:<br/>
> `b 2 104`<br/>
> yields the same as<br/>
> `b 2 101`<br/>
> to wit 5(base10).

## Strings

Strings are either<br/>
- enclosed between `[s` and `]` : string bracket contents
- simple strings: enclosed between any of<br/>
> `#` and whitespace<br/>
> `#` and `[`<br/>
> `#` and `(`<br/>
> `#` and `)`

***Note** that the `#` character doesn't terminate a simple string, so it can be part of one.*

A blank can be expressed by<br/>
`[s ]`

An empty string can be expressed by
- `[s]`
- `#` on itself: followed by whitespace, `[`, `(`, `)` or at the end of a script.

Eg.:<br/>

> `[sKunji Namparshespa]` yields the string "Kunji Namparshespa".<br/>
> `#Petrov` yields the string "Petrov".<br/>
> `#易經` yields the string "易經", just like `b16 o(#uni 6613 [n7D93])`.

If any of their operands is a string, some numerical operators will perform string-related operations :

`+` and `+,` Concatenation, converting any numeric operand to a string:
> `+, [sTotal: ] 353`<br/>
> yields<br/>
> "Total: 353"

Other numerical operators having string operands, or other combinations of number and string operands,
might throw errors. Please refer to the individual operators' descriptions below.

String brackets support nesting, so<br/>
> `+ #!!! [s [s...]]`<br/>
> yields<br/>
> "!!! [s...]"

A newline can be expressed by one of both newline constant operators:
- `¶`
- `c#n`

E.g.:<br/>
> `Z#quiet 1 $#sum 500 w+,(#Total: c#n v#sum c#n)`<br/>
> will write "Total:" and "500" on two consecutive lines.

## Date and time.

### Date

Laconic offers several date-related named operations, the names of which begin with `greg`, as Laconic only handles dates in the Gregorian Calendar that started at Friday, October 15, 1582.

The `o,#greg year month day` named operation calculates a sequence number for any given date in the Gregorian era:<br/>
> `o,#greg 1582 10 15` yields 1<br/>
> `o,#greg 1582 10 18` yields 4<br/>
> `o,#greg 1970 1 1` yields 141418

The `o,#dow year month day` named operation yields a number from 0 to 6 for Saturday to Friday:<br/>
> `o,#dow 1582 10 15` yields 6: Friday<br/>
> `o,#dow 1582 10 18` yields 2: Monday

*(This numbering is consistent with Greek tradition: Monday is δευτέρα, the "second", Tuesday is τρίτη, the "third", etc.)*

The `o#dow sequenceNumber` named operation also yields a weekday number from 0 to 6:<br/>
> `o#dow 1` yields 6: Friday<br/>
> `o#dow 4` yields 2: Monday

For other date-related operations, see the `o#greg...` operations in the *Named operations list* section.

### Time

Laconic handles time as a number of seconds since the start of the Unix Epoch at January 1, 1970, 00h00m00s UTC.

Laconic doesn't know about time zones: all hour, minute or second values derived from this number of seconds express Universal Time.

The `c#utc` operation returns the number of seconds elapsed since the start of the Unix Epoch, as reported by your system.

Several other operations convert that number of seconds to a date, or hour, minute and seconds-in-minute values - see the `o#time...` operations in the *Named operations list* section.

## Comments

Comments are enclosed between `[c` and `]`.

They are completely ignored by Laconic: removed from expressions or scripts before their very evaluation.

Eg.:<br/>
> `$20 100[c Let's assign 100 to variable 20.]v20[c This entire expression should yield 100.]`

Just like string brackets, comment brackets support nesting, so one can have a string bracket in a comment:<br/>
> `[c Just some more comment content: [sMystring]]`

## Variables

Laconic's variables live in a `HashMap` dictionary that maps variable identifiers to Laconic values.

Variable identifiers can be strings as well as numbers.

A variable can be assigned using the `$` operator:

> `$#tau *2p` assigns the value of twice Pi to variable having identifier "tau".

> `$0 *2p` assigns the value of twice Pi to variable having identifier 0 - a number.

Identifier 0 is not the same as identifier "0":<br/>
> `$#0 *2p` assigns the value of twice Pi to variable having identifier "0" - a string.

One can even have spaces in variable identifiers:

> `$[sMax value] 200` assigns 200 to variable having identifier "Max value".

The value of a variable can be read by the `v` operator:

<pre>
$
    #diapason
    440
$
    #halftone
    ^2 /1 12
*
    v#diapason
    ^
        v#halftone
        2
</pre>
> yields the frequency of tone B<sub>4</sub> according to ISO 16.

One can supply a default value when reading a variable, in case it's uninitialized or holds the empty value: the `v,` operator takes two operands:
- a variable identifier
- a default value

When the variable referred to by the first operand is empty or uninitialized, the value of the second operand is assigned to it and returned.

> E.g.:
>> `[cVariable 5 is uninitialized] v,5 1` yields 1<br/>
>> `[cVariable 5 is uninitialized] v,5 1 v5` yields 1<br/>
>> `$5 240 v,5 1` yields 240

Variables can also hold pointers to other variables:

> `$#pointer 5 vv#pointer` returns the value of variable having identifier 5.

Variable identifiers can also be calculated:

> `$#month 1 $+,#daysInMonth v#month 31` assigns the value 31 to variable having identifier "daysInMonth1".

> Expressions like these, together with variables holding a pointer like v#month, allow for array-like constructs.

Assigning the empty value (`€` or an empty operation outcome) to a variable, removes it from the variables collection.

## Reading & assigning a variable

There is a shorthand way of making an operator assign its return value to a variable that's one of its operands: replace the `v` operator with the `:` read-and-assign operator:

Say you have a counter variable "counter" that needs to be increased every time something specific occurs.

Your script initializes that counter variable using a<br/>
`$#counter 0`<br/>
operation. Next, your script executes a loop. In every iteration of that loop, a condition is tested and, if true, your counter needs to be increased by 1.

Your script could perform that increase using the below operation:

<pre>
$
    #counter
    +
        v #counter
        1
</pre>

This would work all right, but there's a shorter way:

<pre>
+
    : #counter
    1
</pre>

The `:` read-and-assign operator has one operand and does two things:
- it reads the value of the variable designed by its operand;
- it tells the parent operator to assign its outcome to that operand.

So, if variable "counter" holds value `4`, operation

> `+ :#counter 1`

where `+` is the parent operator of `:`, would process as follows:
- `:#counter` returns `4`
- `+ 4 1` returns `5`
- this value `5` is assigned to variable "counter"
- and the entire operation returns `5` again, which may or may not be used by encapsulating parent operators.

Note that the two below operations are identical, as the `+` operator is commutative:
> `+ :#counter 1`<br/>
> `+ 1 :#counter`

It's even possible to assign a calculation outcome to more than one variable at once:

<pre>
    $0 21
    $1 5
    * :0 :1
</pre>
This script would result in both variables 0 and 1 having the value 105.

There is also a variant `:,` operator, that takes two arguments
- the identifier of the variable to be read and assigned;
- a default value if this variable is empty. E.g.:

<pre>
  $
    #count
    €
  +
    :,
      #count
      0
    1
  v#count
</pre>
> yields 1 - if the non-variant version of the `:` operator would have been used, the `+` operator would have raised an error about an empty operand.

One might wonder if, using this `:` operator, the `$` (assignment) operator becomes redundant.

Functionally, one could indeed replace all<br/>
`$ #myVar #value`<br/>
expressions with<br/>
`; :#myVar #value`<br/>
as the `;` (combination) operator would read, but not use, the value of variable "myVar", and then assign the value of its second operand to it due to the `:` operator used to read variable "myVar".

However, technically, the `$` (assignment) operator entails less processing, as evidenced by the expression trees:

<pre>
$ laconic -aq '$ #myVar 8'

Tree after operate() :
$
│	myVar
│	8
└─> 8
</pre>

Use of the `;:` way involves two instead of one operators, so more processing:

<pre>
$ laconic -aq '; :#myVar 8'

Tree after operate() :
;
│	:
│	│	myVar
│	└─> (no_value)
│	8
└─> 8
</pre>

## Serial assignment of variables

It is possible to assign a series of values to a series of variables using one `$` operation:

> `$(100 30 20 10)` assigns<br/>
> 30 to variable 100<br/>
> 20 to variable 101<br/>
> 10 to variable 102.

When the given variable identifier is a string, the assigned variables will have as identifier the original string + a counter starting from 0:

> `$(rate 30 20 10)` assigns<br/>
> 30 to variable "rate0"<br/>
> 20 to variable "rate1"<br/>
> 10 to variable "rate2".

## Stack

Laconic's execution environment provides one Last-In-First-Out stack, the main use of which is to pass values to routines that don't share the calling operator's memory.

One can push values onto the stack using the `K` operator:

> `K 40` pushes the value 40 onto the stack.

One can pop and read items from the stack using the `k` operator:

> `K40 k` yields 40.

For easier passing arguments to routines, one can push operands in reverse order on the stack using the `K,` operand:

> `K,(9 7 5 3) >(kkkk)` yields 1 (the larger numbers were on top of the stack and popped first)

The `k,` operator returns the height of the stack: its number of values:

> `K(#A #B 25) k,` yields 3.

For clearing the stack, one can use the `K,,` operator, which would do the same as:

> `Wk,k` : While (`W`) the stack has items (`k,`), pop the topmost item (`k`).

## Routines

In order to reuse Laconic script code, it is possible to write routines that can be called repeatedly by other code.

Laconic offers two kinds of routines:
- routines that share - i.e., can read and write - the variables of the calling operator's environment;
- routines that have an isolated set of variables and can't access the ones of the calling environment.

These two kinds of routines are declared using two different operator variants:
- `R` declares routines that will run in an isolated environment;
- `R,` declares routines that will share the caller's variables.

> *Note: routines will always share the stack and routines known to their caller, and the caller will always have access to stack items pushed by a called routine or other routines declared by a called routine. In other words, the stack and collection of routines are global.*

The `R` and `R,` operators expect two operands:
- the routine's name or identifier, which can be any value type (empty, number or string);
- the operation to be performed.

If more than two arguments are given (using parentheses), all operands after the routine name will be executed when the routine is run.

> *Note: another way to have more operators executed is by combining them by the appropriate number of preceding `;` operators - see this operator's description.*

E.g., the below script declares a routine that calculates an average of the stack items, ignoring non-numeric values:

<pre>
R(
	#average
	$#count k,
	$#total 0
	W
		k,
		;
			$#next k
			?
				=1 tv#next
				+:#total v#next 
				-:#count 1
	?
		=0 v#count
		0
		/v#total v#count
)
</pre>

A routine can be called using the `X` (eXecute) operator. This operator expects only one operand: the routine's name.

Arguments can be passed to the routine by using the `K` operator to push them on the stack. E.g., given the above "average" routine being declared,

> `K(1 2 3 2) X#average` yields 2.

Alternatively, if more operands are given to the `X` operator, these are pushed on the stack for the routine to pop them (or not) as arguments. So the "average" routine can also be called as follows:

> `X(#average 1 2 3 2)`

> ***Note: as the stack is a last-in-first-out stack, stack items will be read by a routine in the reverse order they were pushed. One can avoid this using the `K,` or `X,` operators instead.***

As a routine's name is just another expression value, it's possible to use variables and stack items as pointers to routines.

A routine's code has access to its own name or identifier using the `c#rtn` operation. When this operation is used outside of any routine, it returns "main".

Routines can be stored in script files, which are UTF-8 text files containing Laconic code. These routintes can be imported to a main script in two ways:
- either by preceding calling code in the script by an Evaluate-after-reading operation: `Er,#myRoutines.lac X[sRoutine from file]`
- or else by including the script file by preceding the main code by an `-i` parameter when running the command-line Laconic interpreter: `...$ laconic -i myRoutines.lac 'X[sRoutine from file]'`

## Unit tests on routines

Routines are mainly written to reuse code. So they tend to contain important code you want to rely on.

A popular way to ensure you really can rely on your code is submitting it to *unit tests*: little test programs that use a routine and compare its outcome to an expected result.

These unit tests should test both normal, common cases as well as boundary cases (lowest and highest input values, erroneous input, negative input where only positive numbers are expected, etc.)

It is entirely possible to use Laconic scripts to write unit tests on routines in Laconic scripts:

In scripts/unitTestUtils.lac, you'll find two routines that will help you in adding unit tests to your "production" routines:
- `assert_eq`
- `executeUnitTests`

An example of their usage can be found in `scripts/dowName.lac`, which contains
- routine `dowName`, which converts a weekday number to the English name of the day of the week;
- several individual unit test routines;
- routine `dowNameUnitTests`, which executes all the unit tests and reports on their outcome.

In order to use the `dowName` routine, include it using the -i parameter:

`... $ laconic -i /home/user/path_to_laconic_scripts/dowName.lac 'X(#downName 5)'`

In order to execute the unit tests on routine `dowName`, be sure to `cd` to the directory where `unitTestUtils.lac` resides, and execute:

<pre>
... $ cd path_where_unitTestUtils.lac_is_stored
... $ laconic -i /home/user/path_to_other_laconic_scripts/dowName.lac 'X#downNameUnitTests'
</pre>

## Arithmetic operators

|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|
|:-:|:-:|:-:|:-:|:-:|
|~|unary<br/>minus|1|ignored|number|
|+|addition<br/>if no string<br/>operands|2|added|number|
|+|concatenation<br/>if at least 1<br/>string<br/>operand|2|added|string;<br/>numbers are<br/>formatted<br/>as per<br/>o,#fmt command|
|+,|concatenation<br/>if at least 1<br/>string<br/>operand|2|added|string;<br/>numbers are<br/>truncated<br/>to integers|
|-|subtraction|2|subtracted|number|
|*|multiplication|2|multiplied|number|
|/|division|2|new<br/>division|number|
|/,|integer<br/>division;<br>remainder<br/>is pushed<br/>to stack|2|ignored|number:<br>quotient|
|%|modulo|2|modulo<br/>from<br/>modulo...|number|
|^|exponen-<br/>tiation|2|new<br/>exponen-<br/>tiation|number|
|i|integer|1|ignored|number<br/>truncated<br/>towards<br/>zero|
|i,|ceiling|1|ignored|number<br/>filled<br/>towards<br/>nearest<br/>integer<br/>away from<br/>zero|
|@|rounding|1|ignored|number<br/>truncated<br/>or filled<br/>towards<br/>nearest<br/>integer|
|a|abs|1|ignored|number<br/>absolute<br/>value|
|l|logarithm<br/>from 2nd<br/>operand<br/>in base<br/>1st operand|2|ignored|number|
|s|sign|1|tested also|1 if all<br/>operands are<br/>positive,<br/>-1 if all are<br/>negative,<br/>0 if mixed.|
|b|input<br/>number<br/>base|1|ignored|the new<br/>base|
|b,|output<br/>number<br/>base|1|ignored|the new<br/>base|
|n|parse<br/>number<br/>from<br/>string|1|ignored|number|


`~` Unary minus
> required operands: 1<br/>
> excess operands: ignored<br/>
> returns: the negative value of its first operand

> e.g:<br/>
>> `+1~4` yields -3<br/>
>> `-1~4` yields 5

`+` Addition or concatenation
> required operands: 2<br/>
> excess operands: added also<br/>
> returns: if all operands are numeric, the sum of these operands.<br/>
>> If at least one operand is a string, the concatenation of all operands, converted to strings.<br/>
>> Numbers will be formatted according to the formatting settings requested using the o,#fmt operator or default settings.

> e.g:<br/>
>> `+5 6` yields 11<br/>
>> `$0 50 +([sPrice: ] v0 [s EUR])` yields "Price: 50.000000 EUR"

`+,` Addition or concatenation with different formatting
> required operands: 2<br/>
> excess operands: added also<br/>
> returns: if all operands are numeric, the sum of these operands (same as `+`).<br/>
>> If at least one operand is a string, the concatenation of all operands, converted to strings.<br/>
>> Numbers will be truncated towards zero.

> e.g:<br/>
>> `+,5 6` yields 11<br/>
>> `$0 50 +,([sPrice: ] v0 [s EUR])` yields "Price: 50 EUR"

`-` Subtraction
> required operands: 2<br/>
> excess operands: subtracted also<br/>
> returns: the difference of the first operand and the sum of the subsequent ones.

> e.g:<br/>
>> `-80 20` yields 60<br/>
>> `-(80 20 10)` yields 20

`*` Multiplication
> required operands: 2<br/>
> excess operands: multiplied also<br/>
> returns: the product of all its operands

> e.g:<br/>
>> `*1.1 5` yields 5.5<br/>
>> `*(1.1 5 2)` yields 11

`/` Division
> required operands: 2<br/>
> excess operands: further division<br/>
> returns: the quotient of the first operand divided by the product of the subsequent ones

> e.g:<br/>
>> `/100 4` yields 25<br/>
>> `/(100 4 5)` yields 5

`/,` Integer division and remainder
> required operands: 2<br/>
> excess operands: ignored<br/>
> returns: the integer quotient of the first operand divided by the second one; the remainder is put on the stack.

> e.g:<br/>
>> `/,100 7` yields 14<br/>
>> `/(100 7 2)` yields 14 also<br/>
>> `$#quotient /,100 7 k` yields 2 : the division's remainder, and assigns 14 to variable "quotient".<br/>
>>> (The `k` operator pops an item from the stack.)

`%` Modulo (remainder)
> required operands: 2<br/>
> excess operands: ignored<br/>
> returns: the remainder of the integer division of the first operand by the second operand

> e.g:<br/>
>> `%7 3` yields 1<br/>
>> `%7.1 3.1` yields 0.9

`^` Exponent (power)
> required operands: 2<br/>
> excess operands: further exponentiation<br/>
> returns: the first operand raised to the power of the next operands

> e.g:<br/>
>> `^2 3` yields 8<br/>
>> `^(2 3 /1 2)` yields 2.828426, just like `^^2 3 /1 2`<br/>
>>> *Note: a non-integer exponent is not supported on negative numbers:<br/>
>>> `^~10 .5` yields an error.*

`l` Logarithm (lower case L)
> required operands: 2<br/>
> excess operands: ignored<br/>
> returns: the logarithm of the second operand in a base given by the first operand

> e.g:<br/>
>> `l10 1000` yields 3<br/>
>> `le 10` yields the natural logarithm of 10, having base `e`<br/>
>>> *Note: zero or negative operands are not supported*

`i` Integer value
> required operands: 1<br/>
> excess operands: ignored<br/>
> returns: the first operand truncated towards zero

> e.g:<br/>
>> `i4.7` yields 4<br/>
>> `i~3.8` yields -3<br/>
>> `i15` yields 15<br/>
>> `i~27` yields -27

`i,` Ceiling
> required operands: 1<br/>
> excess operands: ignored<br/>
> returns: the first operand inflated away from zero so as to be integer

> e.g:<br/>
>> `i,4.7` yields 5<br/>
>> `i,~3.8` yields -4<br/>
>> `i,15` yields 15<br/>
>> `i,~27` yields -27

`@` Round
> required operands: 1<br/>
> excess operands: ignored<br/>
> returns: the first operand truncated or filled towards nearest integer<br/>
> If a number is halfway two integers, rounds away from 0.

> e.g:<br/>
>> `@ 13.2` yields 13<br/>
>> `@ 13.7` yields 14<br/>
>> `@ 13.5` yields 14<br/>
>> `@ ~13.2` yields -13<br/>
>> `@ ~13.6` yields -14<br/>
>> `@ ~13.5` yields -14

`a` Absolute value
> required operands: 1<br/>
> excess operands: ignored<br/>
> returns: the absolute value of its first operand

> e.g:<br/>
>> `a~3` yields 3<br/>
>> `a15.9` yields 15.9

`s` Sign
> required operands: 1<br/>
> excess operands: considered also<br/>
> returns:<br/>
>> 1 if all operands are > 0,<br/>
>> -1 if all are < 0,<br/>
>> otherwise 0

> e.g:<br/>
>> `s~14` yields -1.<br/>
>> `s300` yields 1.<br/>
>> `s(3 5 20)` yields 1.<br/>
>> `s(~3 5)` yields 0.<br/>
>> `s(~3 +10~20)` yields -1.

`b` Input number base<br/>
> required operands: 1<br/>
> excess operands: ignored<br/>
> returns: the new base

> The `b` operator sets the number base for subsequent numbers in script or read from standard input or files.

> As base 1 only allows to express 0, it's useless and raises an error,<br/>
> so only bases from 2 up to any large integer are supported.

> If the specified base has a fractal part, it's truncated.

> Caution: in order to return to base 10 after a first `b` command,<br/>
> the 10 has to be expressed in the previously established base.

> e.g:<br/>
>> `b8 [c Using octal numbersfor input] b12 [c using base ten again, as "12" is ten in base 8.]`

>> `b2 +101 11` yields 8 and really prints "8.000000", as we only changed the input base, not the output base.

`b,` Output number base for subsequent output
> required operands: 1<br/>
> excess operands: ignored<br/>
> returns: the new base

> The `b,` operator sets the number base for output to standard output or files.

> In order to avoid confusion, when changing both the input and output base, it's less confusing to change the output base first: e.g.:<br/>
>> `b16 b,16` first sets the input base to 16 and next the output base to ... 22(base10)!<br/>
>> `b,16 b16` first sets the output base to 16 and next the input base to 16 also.

> e.g:<br/>

`n` Parses string to number<br/>
> required operands: 1<br/>
> excess operands: ignored<br/>
> returns: the parsed numeric value or error

> If the first operand is a string, the n operator converts it to a a number, if possible.<br/>
> If the first operand is empty, 0 will be returned.<br/>
> If the first operand is already a number, it will be returned as is.<br/>
> If the first operand is an error value, that error will be returned or thrown depending on Z#ign.

> The `n` operator takes the current input number base into account.

> If the string to be parsed is to contain a negative number, it can be preceded by either `-` or `~`.

> e.g:<br/>
>> `n[s28]` yields 28.<br/>
>> `n#28` yields 28.<br/>
>> `n28` yields 28 (easy, the first operand is already a number)<br/>
>> `n#-28` yields -28.<br/>
>> `n#~28` yields -28.<br/>
>> `b16 n#2E` yields 46(base10).<br/>
>> `b60 n[s2 1 0]` yields 7260(base10).<br/>
>> `b60 n[s210]` yields an error: NumberParsingFailure("Digit value too high for base of input number").<br/>
>> `n€` yields 0

## Trigonometric operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Example|Example<br/>yields|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|p|π or Pi|0|ignored|3.1415 etc.|`Cp`|-1 (= cosine(π)|
|°|degrees<br/>from<br/>radians|1|ignored|number|`°p`|180|
|°,|radians<br/>from<br/>degrees|1|ignored|number|`°,180`|3.1415 etc.|
|S|sine<br/>from<br/>radians|1|ignored|number|`S/p2`|1 (=sine(π/2)|
|S,|arcsine<br/>in<br/>radians|1|ignored|number|`S,1`|1.570795 etc.<br/>(= π/2)|
|S,,|hyperbolic<br/>sine|1|ignored|number|`S,,1`|1.7520 etc.|
|S,,,|inverse<br/>hyperbolic<br/>sine|1|ignored|number|`S,,,1`|0.88137 etc.|
|C|cosine<br/>from<br/>radians|1|ignored|number|`C0`|1|
|C,|arccosine<br/>in<br/>radians|1|ignored|number|`C,0`|1.570795 etc.<br/>(= π/2)|
|C,,|hyperbolic<br/>cosine|1|ignored|number|`C,,1`|1.54308 etc.|
|C,,,|inverse<br/>hyperbolic<br/>cosine|1|ignored|number|`C,,,1`|0|
|T|tangent<br/>from<br/>radians|1|ignored|number|`T°,45`|1|
|T,|arctangent<br/>in<br/>radians|1|ignored|number|`°T,1`|45|
|T,,|hyperbolic<br/>tangent|1|ignored|number|`T,,1`|0.761594 etc.|
|T,,,|inverse<br/>hyperbolic<br/>tangent|1|ignored|number|`T,,,.5`|0.549306 etc.|
|A|four quadrant<br/>arctangent<br/>in radians<br/>of 1st arg (y)<br/>and 2nd arg (x)|2|ignored|number|`°A 4 ~4`|135|

## Logical operators

Laconic has no boolean value type, however

as return value,
- 0 is false and
- 1 is true.

As input value, falsy are:
- 0,
- empty string,
- empty value and
- any error.

All other values are truthy:
- any non-zero number, negative ones included,
- any non-empty string.

|Operator|Description|Expected<br/>operands|Excess<br/>operands|Returns|Example|Example<br/>yields|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|!|not|1|are<br/>checked<br/>also|1 if all<br/>operands<br/>are falsy;<br/>else 0|`!0`<br/>`!5`<br/>`!(0 € -4 4 #)`<br/>`!(5 0 1)`|1<br/>0<br/>1<br/>0|
|&|and|2|are<br/>checked<br/>also|1 if all<br/>operands<br/>are truthy;<br/>else 0|`&29 #Hello`<br/>`&(45 1 ~7)`<br/>`&86 0`|1<br/>1<br/>0|
|\||or|2|are<br/>checked<br/>also|1 if 1 or more<br/>operands<br/>are truthy;<br/>else 0|`\|1 0`<br/>`\|0 0`<br/>`\|(0 #Ghent €)`|1<br/>0<br/>1|
|x|xor|2|are<br/>checked<br/>also|1 if only 1<br/>operand<br/>is truthy;<br/>else 0|`x(0 1 0)`<br/>`x0 €`<br/>`x74 ~12`|1<br/>0<br/>1|

## Comparison operators

For comparison's sake, all possible Laconic values live in one large continuum:<br/>
empty < any number < any string < any error<br/>

The < and > operators can be combined with the negation operator:<br/>
`!<` and `!>`<br/>
so as to obtain greater-or-equal or less-or-equal expressions with 2 operands.

The `=` (equals) operator takes a precision margin into account in order to mitigate the underlying f64 number type's precision limitations.

This precision margin is 0.000_000_01 by default, but can be set and changed again using the `Z#prec` operation. E.g.:

> `Z#prec .1 = .11 .12` yields 1: true.

*Note: `=` is always a comparison operator ("equals"), and never assigns. For assignments, see the `$` and `:` operators.*

|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Example|Example<br/>yields|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|=|equals|2|are<br/>compared<br/>also|1 if all<br/>operands<br/>are equal;<br/>else 0|`=27 ^3 3`<br/>`=#Καλημέρα [sΚαλημέρα]`<br/>`=(256 ^2 8 ^16 2)`<br/>`=€ 0`<br/>`$0T°,45 =1v0`|1<br/>1<br/>1<br/>0<br/>1|
|<|is less|2|are<br/>compared<br/>also|1 if all<br/>operands<br/>are an<br/>increasing<br/>series;<br/>else 0|`Z#ign 1 <(€ ~33 0 [sA] [sa] /5 0`)<br/>`<0 €`|1<br/>0|
|>|is greater|2|are<br/>compared<br/>also|1 if all<br/>operands<br/>are a<br/>decreasing<br/>series;<br/>else 0|`>(#Woof! 38 2)`<br/>`> +12.1 .3 13`<br/>`>#a #A`<br/>`>[sZorro y Perro] #Zorro`|1<br/>0<br/>1<br/>1|
|m|minimum|2|used|value of<br/>the smallest<br/>operand|`m Sp Cp`<br/>`m(38 77 3)`<br/>`m(#z #York 8)`|-1<br/>3<br/>8|
|M|maximum|2|used|value of<br/>the greatest<br/>operand|`M Sp Cp`<br/>`M(45 ~3 1_252)`<br/>`M## ###`|0<br/>1252<br/>"##"|

*Note: `###` yields "##" as the first `#` is the simple string prefix.*

## Constant operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Example|Example<br/>yields|
|:-:|:-:|:-:|:-:|:-:|:-|:-:|
|p|pi|0|ignored|3.141592 etc.|Sp|-1 (=sine(π))|
|e|Euler's<br/>number|0|ignored|2.718281 etc.|||
|¶|newline<br/>character<br/>=<br/>c#n|0|ignored|"\n", U+000A|`+,(72 ¶ 99)`|"72<br/>99"|
|€|empty<br/>value<br/>=<br/>c#empty|0|ignored|empty value|`?=v#crit 0 B1 €`|If<br/>variable<br/>"crit" is 0,<br/>break<br/>the loop,<br/>else<br/>do nothing.|
|c|named<br/>constant:|1|ignored|several,<br/>see below|||
|c#gold|golden<br/>ratio,<br/>calculated as<br/>(1 + sqrt(5))/2|0|ignored|1.618033 etc.|`$#height *v#width c#gold`|variable<br/>"height"<br/>has var. "width"<br/>x golden<br/>ratio|
|c#cogold|conjugate<br/>of golden<br/>ratio,<br/>calculated as<br/>(1 - sqrt(5))/2|0|ignored|-0.618033 etc.|||
|c#n|newline<br/>character<br/>=<br/>¶|0|ignored|"\n", U+000A|+(`$Decem- c#n #ber`)|"Decem-<br/>ber"|
|c#empty|empty<br/>value<br/>=<br/>€|0|ignored|empty value|||
|c#rtn|the running<br/>routine's<br/>name|0|ignored|"main" if not<br/>in routine;<br/>else the running<br/>routine's<br/>name|`R(`<br/>` #percent`<br/>` $0k`<br/>` $1k`<br/>` ?`<br/>`  !v1`<br/>`  U`<br/>`   +`<br/>`    c#rtn`<br/>`    [s: zero div.]`<br/>`  /*v0 100 v1`<br/>`)`|The "percent"<br/>routine<br/>includes<br/>its own name<br/>in an error<br/>message.|
|c#utc|UTC<br/>system<br/>time|0|ignored|Number<br/>of seconds<br/>elapsed<br/>since<br/>1970-1-1<br/>00:00:00|`c#utc`|1755978527<br/>   .526352|

## Variable-related operators

|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Example|Example<br/>yields|
|:-:|:-:|:-:|:-:|:-:|:-|:-:|
|$|assignment<br/>of 2nd<br/>operand<br/>to variable<br/>having 1st<br/>operand<br/>as name<br/>(number or text)|2|assigned to<br/>subsequent<br/>variables|assigned<br/>value|`$4 8 v4`<br/>`$#count 0 v#count`<br/><br/>`$(#tariff 3 10 25)`<br/>`  +,(v#tariff0 #; v#tariff2)`|8<br/>0<br/><br/><br/>3;25|
|v|value of<br/>variable<br/>having 1st<br/>operand<br/>as name<br/>(number or text)|1|ignored|any type of<br/>expression<br/>value<br/>(including<br/>empty and<br/>error)|See above||
|v,|value of<br/>variable<br/>having 1st<br/>operand<br/>as name<br/>(number or text);<br/>if empty,<br/>assigns and<br/>second operand|2|ignored|any type of<br/>expression<br/>value<br/>(including<br/>empty and<br/>error)|See above||
|:|like v<br/>but has result<br/>of parent operator<br/>assigned to that<br/>variable|1|ignored|like v|`$#count 0`<br/>`  +:#count 1`<br/>`  v#count`|<br/><br/>1|
|:,|like :<br/>but assigns<br/>and returns<br/>the second<br/>operand if the<br/>variable<br/>is empty|2|ignored|like :|`$#count €`<br/>`  +:,#count 100 1`<br/>`  v#count`|<br/><br/>101|

## Stack-related operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Example|Example<br/>yields|
|:-:|:-:|:-:|:-:|:-:|:-|:-:|
|K|push to<br/>LIFO<br/>stack|1|pushed<br/>also|value of<br/>last<br/>operand|`K155 K30 k`<br/>`K(155 30) k`|30<br/>30|
|K,|pushes its<br/>operands<br/>to LIFO<br/>stack<br/>in reverse<br/>order|1|pushed<br/>also|value of<br/>last<br/>operand|`K,(155 30) k`<br/>`K,155 K,30 k`|155<br/>30|
|K,,|clears<br/>the<br/>stack|0|ignored|number<br/>of stack<br/>items<br/>cleared|`K,, k`|€<br/>(empty<br/>value)|
|k|pop from<br/>LIFO<br/>stack|0|ignored|value of<br/>top stack<br/>item;<br/>empty<br/>if none|`K(#A 33) k k`|"A"|
|k,|stack<br/>height|0|ignored|number<br/>of stack<br/>items|`K,, K(10 20 30) k,`|3|

## Settings-related operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Example|Example<br/>yields|
|:-:|:-:|:-:|:-:|:-:|:-|:-:|
|Z|setting:<br/>assign value<br/>of 2nd<br/>operand<br/>to setting<br/>designated<br/>by 1st<br/>operand|2|ignored|setting<br/>value<br/>(2nd operand)|||
|Z#prec|comparison<br/>precision<br/>setting<br/>(Default=<br/>.000_000_01)|1|ignored|setting<br/>value<br/>(2nd operand)|`Z`<br/>`  #prec`<br/>`  .01`<br/>`=`<br/>`  .001`<br/>`  .002`|<br/><br/><br/><br/><br/>1|
|Z#loops|maximum<br/>number<br/>of loop<br/>iterations<br/>(Default=<br/>10,000)|1|ignored|setting<br/>value<br/>(2nd operand)|`Z#loops 1_000_000`||
|Z#ign|if not 0 ignore<br/>errors,<br/>else stop<br/>script<br/>execution<br/>on errors|1|ignored|setting<br/>value<br/>(2nd operand)|`Z#ign 1`||

## Named operations list

The below named operations have been implemented:

|Operator|Description|Required<br/>operands,<br/>including<br/>operation<br/>name|Excess<br/>operands|Returns|Examples|Example<br/>yields|
|:-:|:-:|:-:|:-:|:-:|:-|:-:|
|o#r|rounding<br/>(Same as @)|2|ignored|number<br/>truncated<br/>or filled<br/>towards<br/>nearest<br/>integer|`o#r 2.1`<br/>`o#r 2.5`<br/>`~2.5`|2<br/>3<br/>-3|
|o#version|version|1|A 2nd<br/>operand<br/>chooses the<br/>version part<br/>(1-3).<br/>0 chooses<br/>entire version<br/>again.|The<br/>Laconic<br/>interpreter's<br/>version|`o#version 0`<br/>`o#version 1`<br/>`o#version 2`<br/>`o#version 3`|"1.0.2"<br/>1<br/>0<br/>2|
|o#fib|Fibonacci<br/>number|2|ignored|Fibonacci<br/>number<br/>chosen<br/>by index (2nd<br/>operand)|`o#fib 0`<br/>`o#fib 1`<br/>`o#fib 2`<br/>`o#fib 3`<br/>`o#fib 4`<br/>`o#fib 5`|0<br/>1<br/>1<br/>2<br/>3<br/>5|
|o#uni|Unicode<br/>character|2|converted<br/>also|string<br/>having<br/>Unicode<br/>characters|`o#uni 65`<br/>`o(#uni 65 66 67)`<br/>`o#uni 936`<br/>`b16 o#uni [n3A8]`<br/>`b16 o(#uni [n3A8]`<br/>`  [n3C5][n3C7]`<br/>`  [n3AE])`|"A"<br/>"ABC"<br/>"Ψ"<br/>"Ψ"<br/><br/><br/>"Ψυχή"|
|o#ucv|Unicode<br/>value|2|3rd operand<br/>is used as<br/>character<br/>position.<br/>If absent,<br/>0 is used.|Unicode<br/>code point of<br/>character|`o#ucv #Dinant`<br/>`O#ucv #Dinant 1`|68<br/>105|
|o#len|length in<br/>characters<br/>(not in<br/>bytes)|2|length is<br/>added|total length<br/>of operands|`o#len #Dinant`<br/>`O#len #Dinant #Mons`<br/>`o#len #易經`|6<br/>10<br/>2|
|o#lower|lower<br/>case|2|ignored|lower case<br/>of 2nd<br/>operand|`o#lower [sThe Hague]`|"the hague"|
|o#upper|upper<br/>case|2|ignored|upper case<br/>of 2nd<br/>operand|`o#upper [sThe Hague]`|"THE HAGUE"|
|o#proper|proper<br/>case|2|ignored|proper case<br/>of 2nd<br/>operand|`o#proper [sADDIS ABEBA]`|"Addis Abeba"|
|o,#find|find 3rd operand<br/>as part of<br/>2nd operand<br/>starting from<br/>position passed<br/>as 4th operand|3|4th operand<br/>is start position.<br/>If missing,<br/>0 is used.|If found,<br/>start position<br/>of found<br/>substring.<br/>Else, empty.|see below||
|o,#sub|substring|3;<br/>see<br/>below|4th operand<br/>will be used<br/>as length|the substring|see below||
|O,,#repl|string<br/>replacement|4 or 7;<br/>see<br/>below|ignored|resulting<br/>string|see below||
|o,#split|split string|4;<br/>see<br/>below|ignored|number of<br/>segments|see below||
|o,#fmt|set number<br/>format|2 or 4;<br/>see<br/>below|ignored|empty|see below||
|o#leap|leap<br/>year|2|ignored|1 if number<br/>in 2st operand<br/>is a leap year,<br/>else 0|`o#leap 2004`<br/>`o#leap 2000`<br/>`o#leap 1900`<br/>`o#leap 2025`|1<br/>1<br/>0<br/>0|
|o,#dow|day of<br/>week|4 or 2:<br/>#dow,<br/>year,<br/>month<br/>and day<br/>--or--<br/>#dow,<br/>sequence<br/>number.|ignored|0 for<br/>saturday,<br/>1-6 for<br/>following<br/>days|`o,#dow 2025 1 1`<br/>`o(#dow 2025 1 4)`<br/>`o#dow 161517`|4 (wed.)<br/>0 (sat.)<br/>4 (wed.)|
|o,#greg|Gregorian<br/>day's<br/>sequence<br/>number|4:<br/>#greg,<br/>year,<br/>month<br/>and day.|ignored|1 for<br/>October<br/>15, 1582,<br/>etc.|`o,#greg 2025 7 1`|161698|
|o#gregy|year from<br/>Gregorian<br/>day's<br/>sequence<br/>number|2:<br/>#gregy<br/>and seq.nr.|ignored|year|`o#gregy 161698`|2025|
|o#gregm|month from<br/>Gregorian<br/>day's<br/>sequence<br/>number|2:<br/>#gregm<br/>and seq.nr.|ignored|month (1-12)|`o#gregm 161698`|7|
|o#gregd|day from<br/>Gregorian<br/>day's<br/>sequence<br/>number|2:<br/>#gregd<br/>and seq.nr.|ignored|day (1-31)|`o#gregd 161698`|1|
|o#gregt|date text<br/>from<br/>Gregorian<br/>day's<br/>sequence<br/>number|2:<br/>#gregt<br/>and seq.nr.|A 3rd.<br/>operand<br/>is used as<br/>separator|A YYYYsMMsDD<br/>string where<br/>s is a<br/>separator,<br/>if any|`o#gregt 161698`<br/>`O#gregt 161698 #-`<br/>&nbsp;|20250701TUE<br/>2025-07-01-TUE|
|o#timed|time<br/>to<br/>Gregorian<br/>day<br/>sequence<br/>number|2:<br/>#timed<br/>and UTC<br/>seconds|ignored|date<br/>sequence<br/>number<br/>like the<br/>ones<br/>returned<br/>by `o,greg`|`o#timed c#utc`|161751|
|o#timeh|time<br/>to<br/>hours in<br/>day|2:<br/>#timeh<br/>and UTC<br/>seconds|ignored|The hour<br/>(0-23)<br/>in<br/>Universal<br/>Time|`o#timeh c#utc`|20|
|o#timem|time<br/>to<br/>minutes in<br/>hour|2:<br/>#timem<br/>and UTC<br/>seconds|ignored|The minutes<br/>(0-59)<br/>in<br/>Universal<br/>Time|`o#timem c#utc`|9|
|o#times|time<br/>to<br/>seconds in<br/>minute|2:<br/>#times<br/>and UTC<br/>seconds|ignored|The seconds<br/>(0-59)<br/>in<br/>Universal<br/>Time|`o#times c#utc`|26|
|o#timef|time<br/>to<br/>seconds<br/>fraction in<br/>second|2:<br/>#timef<br/>and UTC<br/>seconds|ignored|The fraction<br/>of the second<br/>(0.nnn...)<br/>in<br/>Universal<br/>Time|`o#timef c#utc`|0.682822|
|o#timet|time<br/>to<br/>text|2:<br/>#timet<br/>and UTC<br/>seconds|A third<br/>operand<br/>is used<br/>as a<br/>separator|A "time-<br/>stamp"<br/>text<br/>in<br/>Universal<br/>Time|`o#timet c#utc`<br/>`O#timet c#utc #:`<br/><br/>`o#fmt 2`<br/>`  O#timet`<br/>`  c#utc [s ]`|201927.139002UTC<br/>20:20:15.635714:UTC<br/><br/><br/><br/>20 22 59.23 UTC|

&nbsp;

`o#find` Find substring in a string
> required operands: 3
>> - operation name: `#find`<br/>
>> - source string<br/>
>> - substring to find in source string

> excess operands: a 4th operand is used as zero-based starting position; if absent, 0 is assumed<br/>
> returns: if found, the 0-based start position of the substring, otherwise the empty value (`€`)

> e.g:<br/>
>> `O#find #Samovar #a` yields 1<br/>
>> `o,#find #Samovar #a 3` yields 5<br/>
>> `o(#find #Samovar #a 3)` yields 5<br/>
>> `O#find [sNo hay remedio] #mejor` yields €<br/>
>> `tO#find [sNo hay remedio] #mejor` yields 0 (= the type id of `€`)<br/>
>> `tO#find [sNo hay remedio] #hay` yields 1<br/>
>> `$#pos O#find #Brussels #s ?tv#pos [sHas s] [sHas no s]` yields "Has s"

`o,#sub` Substring
> required operands: 3<br/>
>> - #sub: the operator's name;<br/>
>> - the source string;<br/>
>> - the start position of the substring;<br/>
>> - (optional 4th) the length of the substring. If ommitted, the substring will be taken from the start position until the end of the source string.<br/>

> excess operands after 4th: ignored<br/>
> returns: the substring

> E.g.:<br/>
>> `O#sub [sLa Roche-en-Ardenne] 12` yields "Ardenne"<br/>
>> `o,#sub [sLa Roche-en-Ardenne] 3 5` yields "Roche"

> If the source string argument is not a string, but a number, then that number is first converted to a string according to the current `o#fmt` settings.

`o,#repl` and `O,,#repl`
> required operands: 4<br/>
>> - #repl : the operator's name;<br/>
>> - the source string;<br/>
>> - the substring to be replaced;<br/>
>> - the string to replace it with

> excess operands: 3 more are used:<br/>
>> - the number or name of the position variable;<br/>
>> - the number or name of the sequence variable;<br/>
>> - the number or name of the routine that can use both variables to decide if a replacement should happen.

> returns: the source string with the requested replacements applied.

> In other words,<br/>
> `O,,#repl sourceString substring replacement varPos varSeq conditionRoutineName`

> replaces an occurrence of substring in sourceString with replacement if the condition evaluates to a value that is truthy (not 0 (zero), empty string or € (the Empty value)).

> The condition has to be passed as the name of a routine defined previously using the R, operator - the variant that declares routines that can accesses the calling code's memory.

> The condition routine can make use of two variables, the identifiers (string or number) of which are passed in the varPos and varSeq operands.

> (The names varSeq and varPos are examples. You can choose any name or number.)

> If the condition routine hasn't been defined before, no replacement is performed.

> varPos :<br/>
> When the condition routine is called, varPos will hold the (zero-based) start of the occurrence of substring found in the original string.

> varSeq:<br/>
> When the condition routine is called, varSeq will hold the (zero-based) sequence number of the occurrence.

> If the varPos, varNum and condition operands are ommitted, all occurrences of substring will be replaced.

> The operator returns a string in which the requested replacements have been performed.

> E.g.<br/>
>> `$#replaced o,#repl #philadelphia #ph #f<`br/>
>> puts "filadelfia" in variable #replaced.

> `R,#replCond =v#seq 0`<br/>
>> `$#replaced O,,#repl #philadelphia #ph #f #pos #seq #replCond`<br/>
>> puts "filadelphia" in variable #replaced.

>> `R,#replCond >v#seq 0`<br/>
>> `$#replaced O,,#repl #philadelphia #ph #f #pos #seq #replCond`<br/>
>> puts "philadelfia" in variable #replaced.

>> `R,#replCond >v#seq 5`<br/>
>> `$#replaced O,,#repl #philadelphia #ph #f #pos #seq #replCond`<br/>
>> puts "philadelphia" in variable #replaced.

>> `R,#replCond =v#pos 0`<br/>
>> `$#replaced O,,#repl #philadelphia #ph #f #pos #seq #replCond`<br/>
>> puts "filadelphia" in variable #replaced.

>> `R,#replCond >v#pos 4`<br/>
>> `$#replaced O,,#repl #philadelphia #ph #f #pos #seq #replCond`<br/>
>> puts "philadelfia" in variable #replaced.

>> `R,#replCond 1`<br/>
>> `$#replaced O,,#repl #philadelphia #ph #f #pos $seq #replCond`<br/>
>> puts "filadelfia" in variable #replaced.

>> `R,#replCond 0`<br/>
>> `$#replaced O,,#repl #philadelphia #ph #f #pos #seq #replCond`<br/>
>> puts "philadelphia" in variable #replaced.

>> `R,#replCond =v#pos 0`<br/>
>> `$#myCity #philadelphia`<br/>
>> `O,,#repl :#myCity #p #P #pos #seq #replCond`<br/>
>> puts "Philadelphia" in variable #myCity (due to the : operator).

> (Note that all string operations are case-sensitive.)

> Note: as the R, operator returns the name of the routine itself, it can be used "in place" as the routine-name operand:

<pre>
    $
        #replaced
        O,,
            #repl
            #philadelphia
            #ph
            #f
            #pos
            #seq
            R,
                #replCond
                =v#pos 0
</pre>


`o,#split` Split a string
> required operands: 4<br/>
> excess operands: ignored<br/>
> returns: the number of segments found

> The `o,#split` operator takes 4 arguments:<br/>
> - #split : the operation's name;<br/>
> - the source string;<br/>
> - the segment separator, like #;<br/>
> - the prefix of the names of the variables the split fragments will be assigned to.

> E.g.:
<pre>
    $
        #nrCities
        o,
            #split
            [sBrugge,Arlon,Liège]
            #,
            #city
    F
        0
        v
            #nrCities
        1
        -
            #count
            1
        w
            +
                v
                    +,
                        #city
                        v#count
                ¶
</pre>
>> will first store the three city names in variables city0, city1 and city2 and next write the three city names in the original string on separate lines to standard output.

> If the separator is an empty string (# or \[s\]), every character of the source string will be copied to a variable.

> Note that the numbers after the varNamePrefix will have no leading zeroes, fractal part or interpunction.

> In combination with the r, operator, this allows for awk-like file processing as well as reading of CSV files.

`o,#fmt` Set number format for output
> required operands: 2<br/>
> - #fmt : the operation's name;<br/>
> - the number of fractal digits required

> excess operands: 2 more operands can be given:<br/>
> - the fractal separator (default = .);<br/>
> - the thousands grouping separator (default: no grouping).

> returns: the empty value.

> This number format is only used for<br/>
> - the final output of a script,<br/>
> - the output by the w operator<br/>
> - or the `+` (concatenation) operator.

> The `+,` (concatenation) operator variant does not use the settings established by an o,#fmt operation or its defaults!

> Default is `o,#fmt 6 #. #`

> E.g.:<br/>
>> `o,#fmt 2 #, #.` requests numbers to be output in European continental style and two fractal digits after the comma.<br/>
>> `o(#fmt 2 #, #.)` does the same<br/>
>> `o#fmt 3` requests output of numbers with 3 fractal digits, but no change of fractals or thousands separators.

## Flow-related operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Example|Example<br/>yields|
|:-:|:-:|:-:|:-:|:-:|:-|:-:|
|;|combines<br/>expressions|2|used|value of<br/>last one|`;158 28`<br/>`;;+52 8 °2.41 r52.1`<br/>`;(#A #B #C)`|28<br/>52<br/>"C"|
|?|if|2:<br/>1. condition<br/>2. operator<br/>executed<br/>if true<br/>3. operator<br/>executed<br/>if false|ignored|value of<br/>executed<br/>operator|see below||
|?,|try<br/>operand 1;<br/>if error<br/>return<br/>operand 2|2|if a 3rd<br/>operand<br/>is given,<br/>it's returned<br/>when no error.|see prev.<br/>columns|see below||
|V|value of<br/>1st operand<br/>of last ?,<br/>operator|0|ignored|Error value<br/>if operand1<br/>failed, else<br/>any value|see `?,`<br/>below||
|W|while|2:<br/>1. condition<br/>2. operator<br/>to be<br/>executed|executed<br/>also|value of<br/>last<br/>executed<br/>operator|see below||
|F|for|5:<br/>1. start count<br/>2. end count<br/>3. increment<br/>4. counter<br/>variable<br/>number<br/>or name<br/>5. operator<br/>to be<br/>executed<br/>in iterations|executed<br/>also|value of<br/>last<br/>executed<br/>operator|see below||
|B|break<br/>while<br/>or for<br/>loop|1:<br/>1 = current<br/>loop;<br/>higher = nth-1<br/>nesting<br/>loop|ignored|1st<br/>operand|see below||

`;` Operation combinator
> required operands: 1, expected: 2<br/>
> excess operands: used also<br/>
> returns: the value of its last operand

> The `;` operator, or a sequence of them, can be used to group multiple operations where only one is expected, as in the second operand of the `W` (while) operator, the 2nd or 3rd operands of the ? (if) operator, and others.

> e.g:<br/>
>> `;$0 ~18 v0` groups 2 operations as 1 and yields -18<br/>
>> `;;K7 K35 X#myRoutine` groups 3 operations as 1<br/>
>> `;(+:#count 1 +:#sum v#count ?=v#count 20 B1 €)` groups 3 operations as 1

`?` if
> required operands: 3<br/>
> excess operands: ignored<br/>
> returns: if the first operand has a truthy value, the second operand, otherwise the third.

> Depending on the first operand, either the second or the third operand is executed, and the resulting value is returned, thus making this operator a real *if-then-else* construct.

> e.g:<br/>
<pre>
    $#crit 10
    ?
        >v#crit 5
        ;
            /:#crit 2
            +#divs 1
        €
    v#crit
</pre>
>> assigns (`$`) 10 to variable "crit"; if (`?`) variable "crit" exceeds (`>`) 5, it's divided by 2 and (`;`) variable "divs" is increased, otherwise, nothing (€) happens. Afterwards, the value of variable "crit" is returned.

`switch ... case ... case ... default` - like constructs can also be coded using the `?` - operator:

<pre>
    R(
        #thanksWord

        $#language k

        ?=v#language #en      [c Test ? and first operand: the condition]
        [sthank you]          [c Second operand: return value if condition is true]
        ?=v#language #fr      [c Third operand: a subsequent test]
        #merci
        ?=v#language #de
        [sdanke schön]
        ?=v#language #nl
        [sdank u]
        ?=v#language #el
        #ευχαριστώ
        [sthank you]          [c Default value]
    )
</pre>
>> defines (`R`) a routine `thanksWord` that reads a language abbreviation string from the stack (`k`) and assigns (`$`) it to the `language` variable.
>> Next, it tests (`?`) several known abbreviations; if a match is found, the return value of the `?` operators is given as their second argument. Otherwise, a subsequent test is performed in the test's third operand. The last test operator (`?`) has a default value in its third operand.

`?,` try ... catch ...
> required operands: 2<br/>
> excess operands: a 3rd operand will be used to produce the return value if operand 1 executes without errors.<br/>
> returns: if the 1st operand executes without errors, its result value, else the value of the 2nd operand's execution.

> In other words:<br/>
> `?, opr1 oprIfError optional:oprIfOk`

> If operator opr1 is successful (no error),<br/>
> then its outcome is returned,<br/>
> else oprIfError's outcome.

> If a third operator oprIfOkis present, then that operator's outcome is returned if opr1 was successful.

> Both the operators oprIfOk and oprIfError can make use of a V operator, which will yield the result of the successful operation if called by oprIfOk, or the error value if called by oprIfError.

> e.g:<br/>
<pre>
    ?,
        +€ 7
        ;
            w
                +
                    +[sProblem: ] V
                    ¶
            0
</pre>
>> As this script tries (`?,`) to add to an empty operand (`+€ 7`), an error is raised. Therefore, the entire `?,` operation yields 0 as a fallback result after (`;`) having written (`w`) "Problem: EmptyOperand('+')\n" to standard output.

<pre>
    $0 200
    ?,(
        +v0 7
        ;
            w++[sProblem: ] V ¶
            0
        ;
            w+[sAddition succeeded] ¶
            V
    )
</pre>
>> This script first assigns 200 to variable 0. Next it tries (`?,`) and adds (`+`) this variable's value (`v0`) and 7. As this operation succeeds, and the `?,`operator has a third operand, "Addition succeeded\n" is written to standard output (`w`) and (`;`) the 1st operand's value (`V`) is returned - to wit, 207.

`W` while
> required operands: 2<br/>
> excess operands: executed also<br/>
> returns: the value of the last executed operator

> While operand 1 is truthy, operands 2 up to the last one are executed in loop iterations.

>> ("Truthy" means: not<br/>
>> - empty (`€`)<br/>
>> - 0<br/>
>> - empty string (`[s]`)<br/>
>> - error value.)

> If more than one statement is needed in operand 2, use the ; combinator operator or enclose the `W` operator's operands in parentheses.

> Besides finding a falsy value when re-evaluation its 1st operand, the `W` operator will also stop repeating iterations if
>> - the maximum number of iterations set by the `Z#loops` operation have been done (default = 10,000) - this is a protection against vicious loops;<br/>
>> - a `B` operation in the current or a nested loop requests a loop break.

> e.g:<br/>
<pre>
    $0 10
    $1 0
    W
        v0
        ;
            +:1v0
            -:0 1
    v1
</pre>
>> yields the summation of 10, to wit 55.

>>> *Note: a summation can be calculated more easily using `$#source 10 /* v#source + v#source 1 2`*

<pre>
    R(
        #factorial

        $#fact k
        $#res 1
        W
            >v#fact 0
            ;
                *:#res v#fact
                -:#fact 1
        v#res
    )

    X(#factorial 6)
</pre>
>> defines a routine "factorial" that calculates factorials, calls it while passing value 6 on the stack and returns 720.

<pre>
    Z#loops 500
    $#count 0
    W
        1
        +:#count 1
    v#count
</pre>
>> yields 500: as the 1st operand (`1`) of the `W` operator is always truthy, the maximal number of iteration set by the `Z#loops 500` operation is looped through, increasing the "count" variable every time by 1 (`+:#count 1`). As a result, that maximal number of iterations is returned (`v#count`).

> Breaking from a loop: see the `B` operator.

`F` for
> required operands: 5<br/>
> excess operands: executed also in iterations<br/>
> returns: the value of its last executed operand

> `F startValue endValue increment counterVariable statement`

> The `F` operator takes 5 operands :
> - the starting value of the loop counter
> - the end value of the loop counter
> - the increment step: the number the loop counter is incremented with after every iteration
> - the variable identifier (number or string) that will hold the current loop counter
> - an operator executed in each iteration; if there are more than 5 operands, these are executed too in each iteration.
> 
> The `F` operator
> 1. starts by initializing its loop counter with its 1st operand,
> 2. executes its 5th and subsequent operands
> 3. increases or decreases its loop counter with its 3rd operand and stores it in the variable indicated in the 4th operand
> 4. checks if the loop counter is still in the range between its 1st and 2nd operand, inclusive,
> 5. checks if no break has been set on itself using the `B` operator,
> 6. checks if the maximal number of iterations has not been reached,
> 7. if steps 4, 5 and 6 are positive, repeats from step 2.

> If you want to execute more than one operator, either
> - use the `;` operator
> - or use parentheses.

> e.g:<br/>
<pre>
    $
        0
        1
    F
        3
        11
        2
        1
        *
            :0
            v1
    v0
</pre>
>> calculates and returns the product of all odd numbers between 3 and 11, inclusive.

> The `F` operator can also count down: just have the 1st operand greater than the 2nd. The step operator should remain positive. E.g.:
<pre>
    R(
        #revertChars

        $#orig k
        $#nrFrag o,#split v#orig # #fragment
        $#reverse #
        F
            - v#nrFrag 1    [c First index]
            0               [c Last index]
            1               [c Step value; positive]
            #index          [c Id of variable that will hold loop index]
            +               [c Concatenate character in var. "fragment"index
                                with var. "reverse"]
                :#reverse
                v
                    +,
                        #fragment
                        v#index
        v#reverse
    )

    X(#revertChars #ABCDE)  [c Call routine "revertChars")
</pre>
>> This script creates a routine "revertChars" that reverses the characters of a string popped from the stack.<br/>
>> Next, the script calls it and yields the reverted string (as the call is the last operator).

> Breaking from a loop: see the `B` operator.

> Just like the `W` operator, the number of loops an `F` operator can iterate through is limited by the settings of the `Z#loops` operation, or its default: 10,000.
> If your script doesn't execute as many iterations as you thought it would, you might need to set the allowed number of iterations using this `Z#loops` operation. E.g.:

<pre>
    $#iterationsNeeded 1_000_000
    Z#loops v#iterationsNeeded
    F
        1
        v#iterationsNeeded
        1
        #count
        [c Do something a million times.]
</pre>

`B` break from a loop
> required operands: 1<br/>
> excess operands: ignored<br/>
> returns: its first operand

> The `B` operator breaks both 'W` (while) and `F` (for) loops.

> The `B` operator has been designed to both break a current loop as well as a parent loop of nested loops. Therefore, it takes 1 operand: the level of parent loops (if any) to break, where 1 is the current loop, 2 is the immediate parent loop, etc. E.g.:

<pre>
    Z#loops 10
    $#iters 0
    W
        1
        W
            1
            +:#iters 1
    v#iters
</pre>
>> Has no break operations, and as the maximal number of iterations is set to 10, the script will execute 10 times 10 iterations of a nested loop, so variable "iters" will hold 100 at the end.

<pre>
    Z#loops 10
    $#iters 0
    W
        1
        W
            1
            ;
                +:#iters 1
                B1
    v#iters
</pre>
>> Has a break operation in its nested loop, so these nested loops will only execute once. Therefore, this script will execute 10 times 1 iteration of a nested loop, so variable "iters" will hold 10 at the end.

<pre>
    Z#loops 10
    $#iters 0
    W
        1
        W
            1
            ;
                +:#iters 1
                B2
    v#iters
</pre>
>> Has a break operation in its nested loop that breaks the parent loop, so both the nested and parent loops will only execute once. Therefore, this script will execute 1 time 1 iteration of a nested loop, so variable "iters" will hold 1 at the end.

<pre>
    Z#loops 10
    $#iters 0
    W
        1
        W
            1
            ;(
                +:#iters 1
                B2
                B0
            )
    v#iters
</pre>
>> Has a break operation in its nested loop that breaks the parent loop. However the next operation is `B0`, which desactivates any breaks, so this script repeats both the parent and nested loop 10 times anyway. Therefore, variable "iters" will hold 100 at the end.

## Routine-related operators

For examples, see the section about routines above.

|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|
|:-:|:-:|:-:|:-:|:-:|
|R|definition<br/>of routine<br/>with new<br/>scope|2:<br/>1. routine<br/>name or<br/>number;<br/>2. operator<br/>to be<br/>executed|included<br/>in definition|name or<br/>number|
|R,|definition<br/>of routine<br/>using scope<br/>of caller|2:<br/>1. routine<br/>name or<br/>number;<br/>2. operator<br/>to be<br/>executed|included<br/>in definition|name or<br/>number|
|X|execute<br/>routine|1:<br/>routine<br/>name or<br/>number|pushed<br/>on stack as<br/>arguments|result of<br/>routine's<br/>last<br/>top-level<br/>operator|
|X,|execute<br/>routine|1:<br/>routine<br/>name or<br/>number|pushed<br/>on stack as<br/>arguments<br/>**in reverse<br/>order**|result of<br/>routine's<br/>last<br/>top-level<br/>operator|

## I/O operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Example|Example<br/>yields|
|:-:|:-:|:-:|:-:|:-:|:-|:-:|
|r|read<br/>from<br/>stdin|0|ignored|if input<br/>is a valid<br/>number,<br/>a number;<br/>else a<br/>string|see below||
|r,|read<br/>from<br/>file|1:<br/>file<br/>path|ignored|if file<br/>content<br/>is a valid<br/>number,<br/>a number;<br/>else a<br/>string|see below||
|w|write<br/>to<br/>stdout|1|written<br/>also|number<br/>of bytes<br/>written|see below||
|w,|write<br/>to<br/>file,<br/>overwriting<br/>it|2|ignored|nr. of<br/>characters<br/>written|see below||

`r` read input from standard input
> required operands: 0<br/>
> excess operands: ignored<br/>
> returns: if the input is a valid number, a number, otherwise a string. For input of a negative number, both the standard minus sign `-` as Laconic's unary minus operator `~` may be used.

> e.g:<br/>
>> `w[sEnter a number: ] r` yields 45 if the user entered "45".<br/>
>> `w[sEnter a number: ] r` yields -45 if the user entered "-45".<br/>
>> `w[sEnter a number: ] r` yields -45 if the user entered "~45".<br/>
>> `w[sEnter a number: ] r` yields "Ouagadougou" if the user entered "Ouagadougou".<br/>
>> `w[sEnter degrees: ]°,r` yields the number input understood as degrees and converted to radians (`°,`).<br/>
>> `w[sEnter your name: ] $#name r` stores the name entered by the user to variable "name" - next, input check can be performed by the code.<br/>
>> `w[sEnter a Laconic expression: ] Er` evaluates and executes (`E`) a string entered (`r`) as Laconic code, so if the user entered "+(22 8 12 7)", 49 would be returned.

`r,` read input from a file
> required operands: 1: the file path<br/>
> excess operands: ignored<br/>
> returns: the file's content as a string, even if that represents a valid number. To convert it to a number, use the `E` operator.

> The file path given may be absolute or relative to the user shell's current working directory.

> As the file path is a string, it can be composed and calculated like any other string.

> The file should consist entirely of valid UTF-8 characters. If not, an error is thrown.

> e.g:<br/>
>> `* 2 Er, [s/home/linda/constants/lemniscate.txt]` would yield the double of the Lemniscate constant if file /home/linda/constants/lemniscate.txt would contain ".8346268".<br/>

>> `Er, #routines.txt` would import all routines coded in file ./routines.txt using `R` or `R,` operators. These routines would be immediately callable from subsequent code.<br/>

>> *Note: another way of importing routines from a Laconic code file is by issuing the -i parameter when using the laconic executable.*

`w` write to standard output
> required operands: 1<br/>
> excess operands: written also<br/>
> returns: the number of bytes written

> Note that no newline or CR characters are inserted automatically. You can, however, easily add a newline to any text using the + and ¶ or c#n operators - see the example below.

> If a script's last operation is a `w`, the entire script's return value will be the number of bytes that write operation did write. To avoid this often useless output, insert a `Z#quiet 1` operation in the script, or else have the script end with an empty string: `[s]` or `#`.

> e.g:<br/>
<pre>
    Z#quiet 1
    w[sEnter radians: ]
    $#input r 
    ?
        =tv#input 1
        w([sDegrees: ] °v#input ¶)
        w([sHey, enter a number!] ¶)
</pre>
>> This script<br/>
>> - suppresses the output of the entire script's last operation's value (`Z#quiet 1`),
>> - writes a prompt for an input of radians (`w`),
>> - assigns the user's input (`r`) to variable "input" (`$`),
>> - tests (`?`) if the type (`t`) of that input (`v#input`) is numeric (`1`),
>> - and if so, outputs (`w`) this input converted to degrees (`°`)
>> - or else, displays an error message (`w`).

`w,` write to a file, overwriting it.
> required operands: 2<br/>
>> - file path
>> - content to be written

> excess operands: ignored<br/>
> returns: the number of bytes written

> The file path given may be absolute or relative to the user shell's current working directory.

> As the file path is a string, it can be composed and calculated like any other string.

> If the write operation fails, the `w,` operator returns an error message containing the file system's error message.

> e.g:<br/>
<pre>
    Z#quiet 1
    $#index 5
    $#currentData [sJust a file write test]
    ?,(
        w,
            +,
                #calcData
                v#index 
            v#currentData
        w(V ¶)
        w(q,V [s bytes written] ¶)
    )
</pre>
>> This script tries (`?,`) and writes (`w,`) data in variable (`v`) "currentData" to a file having as path the concatenation (`+,`) of "calcData" and a number or string in variable "index".<br/>
>> If the write operation fails, the error message (`V`) is written (`w`) to standard output.<br/>
>> If it succeeds, the number of bytes written (`V`) is written to standard output. (The `q,` operator converts that number to a string without decimal digits.)


## Other operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Example|Example<br/>yields|
|:-:|:-:|:-:|:-:|:-:|:-|:-:|
|q|quote:<br/>convert<br/>to string.<br/>Same as<br/>+# ...|1|ignored|string;<br/>numbers are<br/>formatted<br/>according to<br/>o,#fmt settings|`q21`<br/>`q[sA string]`<br/>`q€`<br/>`q/1 0`|"21.000000"<br/>"A string"<br/>""<br/>"DivideByZero('/')"|
|q,|quote:<br/>convert<br/>to string.<br/>Same as<br/>+,# ...|1|ignored|string;<br/>fractal parts<br/>of numbers are<br/>truncated<br/>towards zero|`q,21`<br/>`q,[sA string]`<br/>`q,€`<br/>`q,/1 0`|"21"<br/>"A string"<br/>""<br/>"DivideByZero('/')"|
|t|type|1|ignored|0 for empty,<br/>1 for number,<br/>2 for text,<br/>90 for error.|`t€`<br/>`t~55`<br/>`t+,[sRoom ] 24`<br/>t/1 0|0<br/>1<br/>2<br/>90|
|N|number<br/>of operands<br/>of preceding<br/>same-level<br/>operator.<br/>If that was<br/>a loop (W or F),<br/>iterations<br/>executed.|0|ignored|number|`*56.77 21 N`<br/><br/>`$`<br/>`  10`<br/>`  ;`<br/>`    F1 5 1 0 €`<br/>`    N`<br/>`v10`|2<br/><br/><br/><br/><br/><br/><br/>5|
|E|evaluates<br/>and<br/>executes<br/>the expression<br/>in 1st<br/>operand<br/>(should<br/>be string)|1|ignored|evaluation<br/>result|`E[s -70 8]`<br/><br/>`E[sR#double *2 k]`<br/>`X(#double 11)`|62<br/><br/><br/>22|
|U|user-<br/>coded<br/>error|1:<br/>error<br/>message|ignored|error|`U[sInput`<br/>`  should be`<br/>`  a number!]`|"UserDefinedError(<br/>`"Input should be a number!"`)"|

