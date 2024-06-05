From the Bash command line, a script can be executed as follows :

	target/release/laconic "$(cat scripts/laconic_script.lac)"



Not enclosing the $(cat ...) argument in double quotes,
or enclosing it in single quotes,
will produce erroneous results.



If needed, the commands in a script can be preceded by custom commands in a literal argument
enclosed by single quotes :

E.g.

	target/release/laconic '$0 31' "$(cat scripts/isPrime.lac)"

will calculate the primality or smallest divisor of number 31.
(If that's 1, the number is prime.)

E.g.

	target/release/laconic '$0 12' "$(cat scripts/write_fibonacci.lac)"

will print the 12 first Fibonacci numbers.
