-- Test whether the compiler allows assignment to constant boolean.

procedure prog is
aBooleanConstant : constant boolean := false; begin
	aBooleanConstant := true;
end prog;
