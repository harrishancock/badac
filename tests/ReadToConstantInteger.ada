-- Test whether the compiler allows reading to a constant integer.

procedure prog is
anIntegerConstant : constant integer := 5; begin
	get(anIntegerConstant);
end prog;
