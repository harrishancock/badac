-- Test whether the compiler allows assignment to constant integer.

procedure prog is
anIntegerConstant : constant integer := 5; begin
	anIntegerConstant := 2;
end prog;
