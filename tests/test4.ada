-- Baby Ada program, with decls and stats. This program forces the parser
-- to execute every single production rule in the Baby Ada grammar.
-- Note: this program is not actually supposed to be meaningful.
procedure prog is
	a : constant boolean := false;
        b : real;
begin
	declare a : integer;
	        a : real; begin
		a := 5;
	end;

	begin
		b := 3.14159;
	end;

	-- block statement with decl
	declare c : boolean; begin
		c := false;
	end;
end prog;
