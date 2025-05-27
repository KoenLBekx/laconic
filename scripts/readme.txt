From the Bash command line, a script can be executed as follows :

	cat scripts/laconic_script.lac | target/release/laconic -i 


If needed, the commands in a script can be preceded by custom commands in a literal argument
enclosed by single quotes :

E.g.

	cat scripts/isPrime.lac) | target/release/laconic '$0 31' -i 

will calculate the primality or smallest divisor of number 31.
(If that's 1, the number is prime.)

E.g.

	cat scripts/write_fibonacci.lac | target/release/laconic '$0 12' -i 

will print the 12 first Fibonacci numbers.

If you want the script's statements to precede the statements in argument,
have the -i parameter precede these statements:

	cat scripts/laconic_script.lac | target/release/laconic -i '[c Statements in argument ...]'

And you can also have statements as arguments precede and follow the statements in the script file:

	cat scripts/laconic_script.lac | target/release/laconic '[c Statements in argument]' -i '[c More statements in argument ...]'

Moreover, multiple script files can be fed to standard input/

	cat script1.lac script2.lac script3.lac | laconic -i '[c Statements]'


