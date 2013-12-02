-- A program to test reading and writing of integers, writing strings, and
-- simple variable-to-variable assignment.
procedure prog is
	a : constant integer := 100;
	b : constant integer := 200;
	c : integer;
	truth : boolean;
	begin
	put("2 - 1 + 2 = ");
	put_line(2 - 1 + 2);
	put("2 - (1 + 2) = ");
	put_line(2 - (1 + 2));
	put("100 + 200 * 2 = ");
	put_line(100 + 200 * 2);
	put("(100 + 200) * 2 = ");
	put_line((100 + 200) * 2);

	truth := true;
	put("truth = ");
	put_line(truth);

	put("not truth = ");
	put_line(not truth);

	put("truth and true = ");
	put_line(truth and true);

	put("truth and false = ");
	put_line(truth and false);

	put("truth or true = ");
	put_line(truth or true);
	
	put("truth or false = ");
	put_line(truth or false);

	put_line("=== truth := false; ===");

	truth := false;
	put("truth = ");
	put_line(truth);

	put("not truth = ");
	put_line(not truth);

	put("truth and true = ");
	put_line(truth and true);

	put("truth and false = ");
	put_line(truth and false);

	put("truth or true = ");
	put_line(truth or true);
	
	put("truth or false = ");
	put_line(truth or false);

	put("=== a = ");
	put(a);
	put("; b = ");
	put(b);
	put_line("; ===");

	put("a < b = ");
	put_line(a < b);
	put("a > b = ");
	put_line(a > b);
	put("(a = b) = ");
	put_line(a = b);

	put("Enter a number, 1 through 10:\n> ");
	get(c);
	if c < 1 then
		put_line("Your number was less than 1!");
	end if;
	if c > 10 then
		put_line("Your number was greater than 10!");
	end if;
	if (c > 1 or c = 1) and (c < 10 or c = 10) then
		put("Your number was: ");
		put_line(c);
	end if;
end prog;
