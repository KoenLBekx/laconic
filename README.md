<!--style type="text/css">
    :not(pre) > code {
        border: .5px solid #A0A0A0;
    }
</style-->
<style>td:first-child{font-size: 1.5rem;}</style>
# **Laconic**

> *I got carried away. I needed a very concise expression interpreter, but Laconic nearly became a programming language:*

> *besides numeric and string operators, it does provide variables, tests, loops, routines, standard input & output, file I/O and error handling.*

*- Koen Bekx*

## Introduction

Laconic is a Polish expression interpreter :<br/>
> `*+4 2 3`<br/>
> evaluates to 18.

The Laconic crate provides both
- a library exposing `struct Interpreter`, which can be used by other applications;
```
    use laconic::Interpreter;
    let mut interpreter = Interpreter::new_stdio_filesys();

    let exe_result = interpreter.execute("*+4 2 3".to_string());

    assert!(exe_result.is_ok());
    assert_eq!(18_f64, exe_result.unwrap().numeric_value());
```
- an executable that can be called from the command line:

 > `...$ laconic '*+4 2 3'`

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

These precision limitations can be mitigated by setting an *orb* or precision allowance of comparison operators using a `Z§prec` expression in the script - see the operator's description below.

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

## Operators: general

All operators consist of a single character that precedes its operands.

***Note:** there are also "named operators" - see the O and o operators.*

See below for a detailed explanation of each operator.

There's never a need to separate an operator from preceding or following elements using a whitespace,
but it's permitted for readability.

Every operator has a default number of operands it expects. E.g., the `+` operator expects two operands :

> `+ 1 2` yields 3.

However, many operators can operate on more operands, too, but in order to deviate from the default number, parentheses have to be used:

> `+(1 2 3)` yields 6, just like<br/>
> `++1 2 3`

Many operators have a different behavior when followed by one or more *variant operator*s : `,`. E.g.:<br/>
> `°1` yields one radian as degrees (ca. 57.295779).<br/>
> `°,1` yields one degree as radians (ca. 0.017452).

## Value types

Every element, except whitespace, has a value. Likewise, every operator returns a value - it's the operator's value. The value of elements or operators can be operands of other operators.

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
> `Z§ign 1 t/33 0` yields 90: error (because of division by 0; the `Z§ign 1` operation is needed to prevent the error halting execution immediately)

## Error handling

A script's author or Laconic executable user can choose how Laconic handles errors: either by halting or ignoring.

> **Halting**: whenever an operator returns an error value, script execution is stopped immediately and an error value is returned. The string representation of this error value can be displayed;

> **Ignoring**: Laconic treats the error outcome as just another operand value and continues execution, having operators using the error value as operand return the same error value. However, this allows the very script to handle the error.

Halting is the default way of error handling.

In scripts:

> The error handling mode can be set (and changed again) in scripts using the `Z§ign` or `?,` operators:

>> `Z§ign 1` has Laconic ignore any errors until a `Z§ign 0` operation is met:

>>> `ta€` *type of absolute value of empty* should return value 90, because *absolute value of empty* returns an error. However, the `t` operator is never executed fully, as the `a` absolute value operator halts the script execution on finding an empty operand.

>>> `Z§ign 1 ta€` does yield 90: after `Z§ign 1` has Laconic ignore errors, the `a€` operation returns an error and the `t` type operator can evaluate it to find it has type 90: error.

>> The `?,` operator takes two operands and tries and evaluates the first one. If this first operand has a non-error value, `?,` returns that one. Otherwise, the value of the second operand is returned.

>>> `?,a€ §Oops!` yields the string "Oops!", as the `?,` operator tries to evaluate its first operand (`a€`: *absolute value of empty*), gets an error value, and therefore returns the value of its second operand: the string element `§Oops!`.

>> By the way, if the `?,` operator has a third operand, this operand's value is returned on successful execution of the first one:

>>> `?,(a72 §Oops! §Ok)` yields the string "Ok".

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
> `§` and whitespace<br/>
> `§` and `[`<br/>
> `§` and `(`<br/>
> `§` and `)`

***Note** that the `§` character doesn't terminate a simple string, so it can be part of one.*

Eg.:<br/>

> `[sKunji Namparshespa]` yields the string "Kunji Namparshespa".<br/>
> `§Petrov` yields the string "Petrov".<br/>
> `§易經` yields the string "易經", just like `b16 o(§uni 6613 [n7D93])`.

If any of their operands is a string, some numerical operators will perform string-related operations :

`+` and `+,` Concatenation, converting any numeric operand to a string:
> `+, [sTotal: ] 353`<br/>
> yields<br/>
> "Total: 353"

Other numerical operators having string operands, or other combinations of number and string operands,
might throw errors. Please refer to the individual operators' descriptions below.

String brackets support nesting, so<br/>
> `+ §!!! [s [s...]]`<br/>
> yields<br/>
> "!!! [s...]"

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

> `$§tau *2p` assigns the value of twice Pi to variable having identifier "tau".

> `$0 *2p` assigns the value of twice Pi to variable having identifier 0 - a number.

Identifier 0 is not the same as identifier "0":<br/>
> `$§0 *2p` assigns the value of twice Pi to variable having identifier "0" - a string.

One can even have spaces in variable identifiers:

> `$[sMax value] 200` assigns 200 to variable having identifier "Max value".

The value of a variable can be read by the `v` operator:

<pre>
$
    §diapason
    440
$
    §halftone
    ^2 /1 12
*
    v§diapason
    ^
        v§halftone
        2
</pre>
> yields the frequency of tone B<sub>4</sub> according to ISO 16.

There is a shorthand way of making an operator assign its return value to a variable that's one of its operands: replace the `v` operator with the `:` read-and-assign operator:

> `+:§index 1` increases the value of variable "index" with 1.

Variables can also hold pointers to other variables:

> `$§pointer 5 vv§pointer` returns the value of variable having identifier 5.

Variable identifiers can also be calculated:

> `$§month 1 $+,§daysInMonth v§month 31` assigns the value 31 to variable having identifier "daysInMonth1".

> Expressions like these, together with variables holding a pointer like v§month, allow for array-like constructs.

## Stack

Laconic's execution environment provides one Last-In-First-Out stack, the main use of which is to pass values to routines that don't share the calling operator's memory.

One can push values onto the stack using the `K` operator:

> `K 40` pushes the value 40 onto the stack.

One can pop and read items from the stack using the `k` operator:

> `K40 k` yields 40.

For easier passing arguments to routines, one can push operands in reverse order on the stack using the `K,` operand:

> `K,(9 7 5 3) >(kkkk)` yields 1 (the larger numbers were on top of the stack and popped first)

The `k,` operator returns the height of the stack: its number of values:

> `K(§A §B 25) k,` yields 3.

For clearing the stack, one can use the `K,,` operator, which would do the same as:

> `Wk,k` : While (`W`) the stack has items (`k,`), pop the topmost item (`k`).

## Routines

In order to reuse Laconic script code, it is possible to write routines that can be called repeatedly by other code.

Laconic offers two kinds of routines:
- routines that share - can read and write - the variables of the calling operator's environment;
- routines that have an isolated set of variables and can't access the ones of the calling environment.

These two kinds of routines are declared using two different operator variants:
- `R` declares routines that will run in an isolated environment;
- `R,` declares routines that will share the caller's variables.

> *Note: routines will always share the stack and routines known to their caller, and the caller will always have access to stack items pushed by a called routine or other routines declared by a called routine.*

The `R` and `R,` operators expect two operands:
- the routine's name or identifier, which can be any value type (empty, number or string);
- the operation to be performed.

If more than two arguments are given (using parentheses), all operands after the routine name will be executed when the routine is run.

> *Note: another way to have more operators executed is by combining them by the appropriate number of preceding `;` operators - see this operator's description.*

E.g., the below script declares a routine that calculates an average of the stack items, ignoring non-numeric values:

<pre>
R(
	§average
	$§count k,
	$§total 0
	W
		k,
		;
			$§next k
			?
				=1 tv§next
				+:§total v§next 
				-:§count 1
	?
		=0 v§count
		0
		/v§total v§count
)
</pre>

A routine can be called using the `X` (eXecute) operator. This operator expects only one operand: the routine's name.

Arguments can be passed to the routine by using the `K` operator to push them on the stack. E.g., given the above "average" routine being declared,

> `K(1 2 3 2) X§average` yields 2.

Alternatively, if more operands are given to the `X` operator, these are pushed on the stack for the routine to pop them (or not) as arguments. So the "average" routine can also be called as follows:

> `X(§average 1 2 3 2)`

> ***Note: as the stack is a last-in-first-out stack, stack items will be read by a routine in the reverse order they were pushed. One can avoid this using the `K,` or `X,` operators instead.***

As a routine's name is just another expression value, it's possible to use variables and stack items as pointers to routines.

Routines can be stored in script files. They can be declared to a main script in two ways:
- either by preceding calling code in the script by an Evaluate-after-reading operation: `Er,§<filename> X[sRoutine from file]`
- or else by including the script file by preceding the main code by an `-i` parameter when running the command-line Laconic interpreter: `...$ laconic -i myRoutines.lac 'X[sRoutine from file]'`

A routine's code has access to its own name or identifier using the `c§rtn` operation. When this operation is used outside of any routine, it returns "main".

*An example of a routine in a script file can be found in `scripts/average.lac`. This script file not only declares the "average" routine, but also an "average_unit_tests" routine that, when called, performs several unit tests on the "average" routine.*

## Arithmetic operators

|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|
|:-:|:-:|:-:|:-:|:-:|
|~|unary<br/>minus|1|ignored|number|
|+|addition<br/>if no string<br/>operands|2|added|number|
|+|concatenation<br/>if at least 1<br/>string<br/>operand|2|added|string;<br/>numbers are<br/>formatted<br/>as per<br/>o,§fmt command|
|+,|concatenation<br/>if at least 1<br/>string<br/>operand|2|added|string;<br/>numbers are<br/>truncated<br/>to integers|
|-|subtraction|2|subtracted|number|
|*|multiplication|2|multiplied|number|
|/|division|2|new<br/>division|number|
|/,|integer<br/>division;<br>remainder<br/>is pushed<br/>to stack|2|ignored|number:<br>quotient|
|%|modulo|2|modulo<br/>from<br/>modulo...|number|
|^|exponen-<br/>tiation|2|new<br/>exponen-<br/>tiation|number|
|i|integer|1|ignored|number<br/>truncated<br/>towards<br/>zero|
|i,|ceiling|1|ignored|number<br/>filled<br/>towards<br/>nearest<br/>integer<br/>away from<br/>zero|
|o§r|rounding|1|ignored|number<br/>truncated<br/>or filled<br/>towards<br/>nearest<br/>integer|
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
>> Numbers will be formatted according to the formatting settings requested using the o,§fmt operator or default settings.

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
>> `$§quotient /,100 7 k` yields 2 : the division's remainder, and assigns 14 to variable "quotient".<br/>
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

`o§r` Round
> required operands: 1 (technically, 2: `§r` is the first operand of the `o` operator)<br/>
> excess operands: ignored<br/>
> returns: the first operand truncated or filled towards nearest integer<br/>
> If a number is halfway two integers, rounds away from 0.

> e.g:<br/>
>> `o§r 13.2` yields 13<br/>
>> `o§r 13.7` yields 14<br/>
>> `o§r 13.5` yields 14<br/>
>> `o§r ~13.2` yields -13<br/>
>> `o§r ~13.6` yields -14<br/>
>> `o§r ~13.5` yields -14

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
> If the first operand is an error value, that error will be returned or thrown depending on Z§ign.

> The `n` operator takes the current input number base into account.

> If the string to be parsed is to contain a negative number, it can be preceded by either `-` or `~`.

> e.g:<br/>
>> `n[s28]` yields 28.<br/>
>> `n§28` yields 28.<br/>
>> `n28` yields 28 (easy, the first operand is already a number)<br/>
>> `n§-28` yields -28.<br/>
>> `n§~28` yields -28.<br/>
>> `b16 n§2E` yields 46(base10).<br/>
>> `b60 n[s2 1 0]` yields 7260(base10).<br/>
>> `b60 n[s210]` yields an error: NumberParsingFailure("Digit value too high for base of input number").<br/>
>> `n€` yields 0

## Trigonometric operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Empty<br/>operand<br/>used as|Error<br/>operand<br/>used as|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|p|Pi|0|ignored|number|n/a|n/a|
|°|degrees<br/>from<br/>radians|1|ignored|number|error|error|
|°,|radians<br/>from<br/>degrees|1|ignored|number|error|error|
|S|sine<br/>from<br/>radians|1|ignored|number|error|error|
|S,|arcsine<br/>in<br/>radians|1|ignored|number|error|error|
|S,,|hyperbolic<br/>sine|1|ignored|number|error|error|
|S,,,|inverse<br/>hyperbolic<br/>sine|1|ignored|number|error|error|
|C|cosine<br/>from<br/>radians|1|ignored|number|error|error|
|C,|arccosine<br/>in<br/>radians|1|ignored|number|error|error|
|C,,|hyperbolic<br/>cosine|1|ignored|number|error|error|
|C,,,|inverse<br/>hyperbolic<br/>cosine|1|ignored|number|error|error|
|T|tangent<br/>from<br/>radians|1|ignored|number|error|error|
|T,|arctangent<br/>in<br/>radians|1|ignored|number|error|error|
|T,,|hyperbolic<br/>tangent|1|ignored|number|error|error|
|T,,,|inverse<br/>hyperbolic<br/>tangent|1|ignored|number|error|error|
|A|four quadrant<br/>arctangent<br/>in radians<br/>of 1st arg (y)<br/>and 2nd arg (x)|2|ignored|number|error|error|

## Logical operators

There are no boolean values.<br/>
As return value, 0 is false and 1 is true.<br/>
As input value, falsy are: 0, empty string, empty value and any error.<br/>
All other values are truthy.

|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Empty<br/>operand<br/>used as|Error<br/>operand<br/>used as|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|!|not|1|are<br/>checked<br/>also|1 if all<br/>operands<br/>are falsy;<br/>else 0|falsy|falsy<br/>if Z§ign 1<br/>else error|
|&|and|1|are<br/>checked<br/>also|1 if all<br/>operands<br/>are truthy;<br/>else 0|falsy|falsy<br/>if Z§ign 1<br/>else error|
|\||or|1|are<br/>checked<br/>also|1 if 1 or more<br/>operands<br/>are truthy;<br/>else 0|falsy|falsy<br/>if Z§ign 1<br/>else error|
|x|xor|1|are<br/>checked<br/>also|1 if only 1<br/>operand<br/>is truthy;<br/>else 0|falsy|falsy<br/>if Z§ign 1<br/>else error|

## Comparison operators

empty < any number < any string < any error<br/>

The < and > operators can be combined with the negation operator:<br/>
!< and !><br/>
so as to obtain greater-or-equal or less-or-equal expressions with 2 operands.

|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Empty<br/>operand<br/>used as|Error<br/>operand<br/>used as|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|=|equals|2|are<br/>compared<br/>also|1 if all<br/>operands<br/>are equal;<br/>else 0|empty|depends<br/>on Z§ign|
|<|is less|2|are<br/>compared<br/>also|1 if all<br/>operands<br/>are an<br/>increasing<br/>series;<br/>else 0|empty|depends<br/>on Z§ign|
|>|is greater|2|are<br/>compared<br/>also|1 if all<br/>operands<br/>are a<br/>decreasing<br/>series;<br/>else 0|empty|depends<br/>on Z§ign|
|m|minimum|2|used|value of<br/>the smallest<br/>operand|empty|depends<br/>on<br/>Z§ign|
|M|maximum|2|used|value of<br/>the greates<br/>operand|empty|depends<br/>on<br/>Z§ign|

## Constant operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Empty<br/>operand<br/>used as|Error<br/>operand<br/>used as|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|p|pi|0|ignored|number|n/a|n/a|
|e|Euler's<br/>constant|0|ignored|number|n/a|n/a|
|¶|newline<br/>character<br/>=<br/>c§n|0|ignored|string|n/a|n/a|
|€|empty<br/>value<br/>=<br/>c§empty|0|ignored|Empty|n/a|n/a|
|c|named<br/>constant:|1|ignored|several|error|error|
|c§gold|golden<br/>ratio|0|ignored|several|error|error|
|c§cogold|conjugate<br/>of golden<br/>ratio|0|ignored|several|error|error|
|c§n|newline<br/>character<br/>=<br/>¶|0|ignored|several|error|error|
|c§empty|empty<br/>value<br/>=<br/>€|0|ignored|several|error|error|
|c§rtn|the running<br/>routine's<br/>name|0|ignored|"main" if not<br/>in routine;<br/>else the running<br/>routine's<br/>name|error|error|

## Variable-related operators

Note: variables are stored in a HashTable; their key or "name" can be a number or a string ("text")

|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Empty<br/>operand<br/>used as|Error<br/>operand<br/>used as|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|$|assignment<br/>of 2nd<br/>operand<br/>to variable<br/>having 1st<br/>operand<br/>as name<br/>(number or text)|2|assigned to<br/>subsequent<br/>variables|assigned<br/>value|error<br/>if 1st<br/>operand;<br/>empty if 2nd|error<br/>if 1st<br/>operand;<br/>if 2nd, depends<br/>on Z§ign|
|v|value of<br/>variable<br/>having 1st<br/>operand<br/>as name<br/>(number or text)|1|ignored|any type of<br/>expression<br/>value<br/>(including<br/>empty and<br/>error)|error|error|
|:|like v<br/>but has result<br/>of parent operator<br/>assigned to that<br/>variable|1|ignored|like v|error|error|

## Stack-related operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Empty<br/>operand<br/>used as|Error<br/>operand<br/>used as|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|K|push to<br/>LIFO<br/>stack|1|pushed<br/>also|value of<br/>last<br/>operand|empty|depends<br/>on Z§ign|
|K,|push to<br/>LIFO<br/>stack<br/>in reverse<br/>order|1|pushed<br/>also|value of<br/>last<br/>operand|empty|depends<br/>on Z§ign|
|K,,|clears<br/>the<br/>stack|0|ignored|number<br/>of stack<br/>items<br/>cleared|n/a|n/a|
|k|pop from<br/>LIFO<br/>stack|0|ignored|value of<br/>top stack<br/>item;<br/>empty<br/>if none|n/a|n/a|
|k,|stack<br/>height|0|ignored|number<br/>of stack<br/>items|n/a|n/a|

## Settings-related operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Empty<br/>operand<br/>used as|Error<br/>operand<br/>used as|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|Z|setting:<br/>assign value<br/>of 2nd<br/>operand<br/>to setting<br/>designated<br/>by 1st<br/>operand|2|ignored|setting<br/>value<br/>(2nd operand)|error|error|
|Z§prec|comparison<br/>precision<br/>setting|1|ignored|setting<br/>value<br/>(2nd operand)|error|error|
|Z§loops|maximum<br/>number<br/>of loop<br/>iterations|1|ignored|setting<br/>value<br/>(2nd operand)|error|error|
|Z§ign|if not 0 ignore<br/>errors,<br/>else stop<br/>script<br/>execution<br/>on errors|1|ignored|setting<br/>value<br/>(2nd operand)|error|error|

## Named operators

The number of characters to represent operators is limited.
Moreover, for most people, the beautiful 汉字 (Han zi) are difficult to enter using a keyboard. 

That's why there are also operators that are designated by a name; these are called "named operators".
The o and O operators take that name as their first operand in order to perform the operation of a named operator.

The difference between both operators is that<br/>
- the lower case o operator takes 2 operands : the named operator name and one operation operand;<br/>
- the upper case O operator takes 3 operands : the named operator name and two operation operands.

Furthermore, adding the comma operator to both the O and o operands increases their expected number of operands by (2 * number_of_commas), so

- the o, operator takes 4 operands;<br/>
- the O, operator takes 5 operands;<br/>
- the o,, operator takes 6 operands;<br/>
- the O,, operator takes 7 operands;<br/>
- etc.

Both operators, however, can have their number of operands overridden by the ( and ) operators,
in which case they can be used interchangeably and following commas don't affect the number of expected operands (they can still affect the behaviour, though).

The operator's name, which is the first operand of the o and O operators, can either be a number or a string.
For readability, however, strings are chosen for the implemented operators.

As stated, this string is the first operand and can be written either as<br/>
o §name<br/>
o§name<br/>
o\[sname\]<br/>
or<br/>
o \[sname\]

The below named operators have been implemented:

|Operator|Description|Required<br/>operands,<br/>inc.<br/>operator<br/>name|Excess<br/>operands|Returns|Empty<br/>operand<br/>used as|Error<br/>operand<br/>used as|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|o§r|rounding|2|ignored|number<br/>truncated<br/>or filled<br/>towards<br/>nearest<br/>integer|error|error|
|o§version|version|1|A 2nd<br/>operand<br/>chooses the<br/>version part<br/>(1-3).<br/>0 chooses<br/>entire version<br/>again.<br/>E.g.: 1.0.2|The<br/>Laconic<br/>interpreter's<br/>version|error|error|
|o§fib|Fibonacci<br/>number|2|ignored|Fibonacci<br/>number<br/>chosen<br/>by index (2nd<br/>operand)|error|error|
|o§uni|Unicode<br/>character|2|converted<br/>also|string<br/>having<br/>Unicode<br/>characters|error|error|
|o§ucv|Unicode<br/>value|2|3rd operand<br/>is used as<br/>character<br/>position.<br/>If absent,<br/>0 is used.|Unicode<br/>code point of<br/>character|error|error|
|o§len|length in<br/>characters|2|length is<br/>added|total length<br/>of operands|error|error|
|o§lower|lower<br/>case|2|ignored|lower case<br/>of 2nd<br/>operand|error|error|
|o§upper|upper<br/>case|2|ignored|upper case<br/>of 2nd<br/>operand|error|error|
|o§proper|proper<br/>case|2|ignored|proper case<br/>of 2nd<br/>operand|error|error|
|o,§find|find 3rd operand<br/>as part of<br/>2nd operand<br/>starting from<br/>position passed<br/>as 4th operand|3|4th operand<br/>is start position.<br/>If missing,<br/>0 is used.|If found,<br/>start position<br/>of found<br/>substring.<br/>Else, empty.|error|error|
|o,§sub|substring|3|4th operand<br/>will be used<br/>as length|the substring|error|error|
|O,,§repl|string<br/>replacement|4 or 7|ignored|resulting<br/>string|error|error|
|o,§split|split string|4|ignored|number of<br/>segments|error|error|
|o,§fmt|set number<br/>format|4|ignored|empty|error|error|
|o§leap|leap<br/>year|2|ignored|1 if number<br/>in 1st operand<br/>is a leap year,<br/>else 0|error|error|
|o,§dow|day of<br/>week|4:<br/>§dow,<br/>year,<br/>month<br/>and day.|ignored|0 for saturday,<br/>1-6 for<br/>following<br/>days|error|error|
|o,§greg|Gregorian<br/>day's<br/>sequence<br/>number|4:<br/>§dow,<br/>year,<br/>month<br/>and day.|ignored|1 for january 1,<br/>year 0000,<br/>etc.|error|error|
|o§gregy|year from<br/>Gregorian<br/>day's<br/>sequence<br/>number|2:<br/>§gregy<br/>and seq.nr.|ignored|year|error|error|
|o§gregm|month from<br/>Gregorian<br/>day's<br/>sequence<br/>number|2:<br/>§gregm<br/>and seq.nr.|ignored|month (1-12)|error|error|
|o§gregd|day from<br/>Gregorian<br/>day's<br/>sequence<br/>number|2:<br/>§gregd<br/>and seq.nr.|ignored|day (1-31)|error|error|
|o§gregt|date text<br/>from<br/>Gregorian<br/>day's<br/>sequence<br/>number|2:<br/>§gregt<br/>and seq.nr.|A 3rd.<br/>operand<br/>is used as<br/>separator|A YYYYsMMsDD<br/>string where<br/>s is a<br/>separator,<br/>if any|error|error|

Note: the **o,§sub** operator takes 4 arguments:<br/>
- §sub: the operator's name;<br/>
- the source string;<br/>
- the start position of the substring;<br/>
- (optional) the length of the substring. If ommitted, the substring will be taken from the
  start position until the end of the source string.

Note: the **o,§repl** or **O,,§repl** operator takes 4 or 7 arguments:<br/>
- §repl : the operator's name;<br/>
- the source string;<br/>
- the substring to be replaced;<br/>
- the string to replace it with;<br/>

And optionally:<br/>
- the number or name of the position variable;<br/>
- the number or name of the sequence variable;<br/>
- the number or name of the routine that can use both variables to decide if a replacement should happen.

Note: the **0,§split** operator takes 4 arguments:
- §split : the operator's name;<br/>
- the source string;<br/>
- the segment separator, like §;<br/>
- the prefix of the numbered variable names the split segments will be assigned to.

Note: the **o,§fmt** operator takes 4 arguments:<br/>
- §fmt : the operator's name;<br/>
- the number of fractal digits;<br/>
- the fractal separator (default = .);<br/>
- the thousands grouping separator (default: no grouping).

This number format is only used for the final output of a script,
or for output by the w or the +(concatenation) commands.

## Flow-related operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Empty<br/>operand<br/>used as|Error<br/>operand<br/>used as|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|?|if|2:<br/>1. condition<br/>2. operator<br/>executed<br/>if true<br/>3. operator<br/>executed<br/>if false|ignored|value of<br/>executed<br/>operator|empty|depends<br/>on Z§ign|
|?,|try<br/>operand 1;<br/>if error<br/>return<br/>operand 2|2|if a 3rd<br/>operand<br/>is given,<br/>it's returned<br/>when no error.|see prev.<br/>columns|empty|error|
|V|value of<br/>1st operand<br/>of last ?,<br/>operator|0|ignored|Error value<br/>if operand1<br/>failed, else<br/>any value|n/a|n/a|
|W|while|2:<br/>1. condition<br/>2. operator<br/>to be<br/>executed|executed<br/>also|value of<br/>last<br/>executed<br/>operator|empty|depends<br/>on Z§ign|
|F|for|5:<br/>1. start count<br/>2. end count<br/>3. increment<br/>4. counter<br/>variable<br/>number<br/>or name<br/>5. operator<br/>to be<br/>executed|executed<br/>also|value of<br/>last<br/>executed<br/>operator|empty|depends<br/>on Z§ign|
|B|break<br/>while<br/>or for<br/>loop|1:<br/>1 = current<br/>loop;<br/>higher = nth-1<br/>nesting<br/>loop|ignored|1st<br/>operand|error|error|
|R|definition<br/>of routine<br/>with new<br/>scope|2:<br/>1. routine<br/>name or<br/>number;<br/>2. operator<br/>to be<br/>executed|included<br/>in definition|name or<br/>number|error|error|
|R,|definition<br/>of routine<br/>using scope<br/>of caller|2:<br/>1. routine<br/>name or<br/>number;<br/>2. operator<br/>to be<br/>executed|included<br/>in definition|name or<br/>number|error|error|
|X|execute<br/>routine|1:<br/>routine<br/>name or<br/>number|pushed<br/>on stack as<br/>arguments|result of<br/>routine's<br/>last<br/>top-level<br/>operator|error|error|
|X,|execute<br/>routine|1:<br/>routine<br/>name or<br/>number|pushed<br/>on stack as<br/>arguments<br/>**in reverse<br/>order**|result of<br/>routine's<br/>last<br/>top-level<br/>operator|error|error|

## I/O operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Empty<br/>operand<br/>used as|Error<br/>operand<br/>used as|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|r|read<br/>from<br/>stdin|0|ignored|if input<br/>is a valid<br/>number,<br/>a number;<br/>else a<br/>string|n/a|n/a|
|r,|read<br/>from<br/>file|1:<br/>file<br/>path|ignored|if input<br/>is a valid<br/>number,<br/>a number;<br/>else a<br/>string|n/a|n/a|
|w|write<br/>to<br/>stdout|1|written<br/>also|number<br/>of bytes<br/>written|empty<br/>string|depends on<br/>Z§ign|
|w,|write<br/>to<br/>file,<br/>overwriting<br/>it|2|ignored|nr. of<br/>characters<br/>written|empty<br/>string|depends on<br/>Z§ign|

## Other operators
|Operator|Description|Required<br/>operands|Excess<br/>operands|Returns|Empty<br/>operand<br/>used as|Error<br/>operand<br/>used as|
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|;|combines<br/>expressions|2|used|value of<br/>last one|empty|error|
|q|quote:<br/>convert<br/>to string.<br/>Same as<br/>+§ ...|1|ignored|string;<br/>numbers are<br/>formatted<br/>according to<br/>o,§fmt settings|empty|error|
|q,|quote:<br/>convert<br/>to string.<br/>Same as<br/>+,§ ...|1|ignored|string;<br/>fractal parts<br/>of numbers are<br/>truncated<br/>towards zero|empty|error|
|t|type|1|ignored|0 for empty,<br/>1 for number,<br/>2 for text,<br/>90 for error.|empty|depends on<br/>Z§ign|
|N|number<br/>of operands<br/>of preceding<br/>same-level<br/>operator|0|ignored|number|n/a|n/a|
|E|evaluate<br/>the expression<br/>in 1st<br/>operand<br/>(should<br/>be string)|1|ignored|evaluation<br/>result|error|depends on<br/>Z§ign|
|U|user-<br/>coded<br/>error|1:<br/>error<br/>message|ignored|error|error|error|

