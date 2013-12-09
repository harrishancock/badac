-- Test whether the compiler allows reading to a constant real.

procedure prog is
aRealConstant : constant real := 3.14; begin
	get(aRealConstant);
end prog;
