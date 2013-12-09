procedure prog is
	-- redeclaration of identifier (in program nonterminal)
	a : constant boolean := false;
        A : real;
begin
	-- use of undeclared identifier
	B := 1.7;

	-- redeclaration of identifier (in blockst nonterminal)
	declare scoped_id : real;
	        scoped_ID : real; begin
	end;

	-- use of undeclared identifier (out of scope)
	scoped_id := 5.0;
end prog;
