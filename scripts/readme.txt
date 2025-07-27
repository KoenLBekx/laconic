From the Bash command line, a script can be executed as follows :

	target/release/laconic -i scripts/laconic_script.lac


If needed, the commands in a script can be preceded by custom commands in a literal argument
enclosed by single quotes :

E.g.

	given a script isPrime.lac, that expects a preinitialized variable 0, the command

		target/release/laconic '$0 31' -i scripts/isPrime.lac

	might calculate the primality of number 31.

If you want the script's statements to precede the statements in argument,
have the -i parameter precede these statements:

	target/release/laconic -i scripts/laconic_script.lac '[c Statements in argument ...]'

And you can also have statements as arguments precede and follow the statements in the script file:

	target/release/laconic '[c Statements in argument]' -i scripts/laconic_script.lac '[c More statements in argument ...]'

Moreover, multiple script files can be included:

	target/release/laconic -i script1.lac -i script2.lac -i script3.lac '[c Statements]'

Script files are a great way to import routines to a script:

	target/release/laconic -i scripts/primeRoutine.lac 'K 101 X§isPrime'


