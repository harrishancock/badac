procedure prog is
	pi : constant real := 3.14159;
	r : real;
begin
	put_line(pi);
	put("Enter a radius.\n> ");
	get(r);
	put("circumference = ");
	put_line(pi * r * r);
	put("pi + r + r = ");
	put_line(pi + r + r);
	put("2 * 2 * 2 = ");
	put_line(2 * 2 * 2);
end prog;
