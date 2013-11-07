-- Baby Ada program, with decls and stats. This program forces the parser
-- to execute every single production rule in the Baby Ada grammar.
-- Note: this program is not actually supposed to be meaningful.
procedure prog is
	a : constant boolean := false;
        b : real;
begin
	-- block statement
	begin
		-- assignment statement
		b := 3.14159;
	end;
	-- block statement with decl
	declare c : boolean; begin
		-- if statement
		if b > 0 then
			-- read statement
			get(c);
		end if;
	end;
	-- loop statement
	while not (b - 1 * a > 5) = true loop
		-- write statements
		put_line("Hello, world!");
		put(a - 5 + b);
	end loop;
end prog;
