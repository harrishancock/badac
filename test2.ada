-- A program to test reading and writing of integers, writing strings, and
-- simple variable-to-variable assignment.
procedure prog is
	a : constant integer := 100;
	b : constant integer := 200;
	c : integer;
	begin
	c := a + b;
	put_line(c);
	c := a * b;
	put_line(c);
	c := a - b;
	put_line(c);
	c := a / b;
	put_line(c);
	c := a mod b;
	put_line(c);
end prog;
