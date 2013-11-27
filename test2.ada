-- A program to test reading and writing of integers, writing strings, and
-- simple variable-to-variable assignment.
procedure prog is
	myFave : integer;
	yourFave : integer;
	begin

	put("Hi. What's your favorite integer?\n> ");
	get(yourFave);

	myFave := yourFave;

	put("What a coincidence! My favorite's ");
	put(myFave);
	put_line(", as well!");
end prog;
