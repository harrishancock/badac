-- Test whether the compiler allows reading to a constant boolean.

procedure prog is
aBooleanConstant : constant boolean := false; begin
	get(aBooleanConstant);
end prog;
