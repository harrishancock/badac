-- Test whether the compiler allows assignment to constant real.

procedure prog is
aRealConstant : constant real := 3.14; begin
	aRealConstant := 1.0;
end prog;
