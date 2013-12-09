procedure prog is
	a : constant boolean := false;
        b : real;
begin
	-- case-insensitive reference
	B := 1.7;

	-- similar identifier names, shadowing identifier in enclosing scope
	declare a : integer;
	        aa : real;
	        aaa : boolean; begin
		A := 5;
		declare a : real; begin
			a := 123.456;
			-- reference to identifier in enclosing scope
			Aaa := false;
		end;
	end;

	-- blockst without declare, with a nested declaration
	begin
		b := 3.14159;
		declare a_really_long_identifier123 : real; begin
			a_Really_Long_Identifier123 := 0;
		end;
	end;

	declare c : boolean; begin
		c := false;
	end;

	-- legal "redeclaration" of an identifier in a different scope
	declare c : real; begin
		c := 1.0;
        end;
end prog;
