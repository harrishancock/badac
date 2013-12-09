-- Test if compiler allows mismatched types in a relop.

procedure prog is begin
	put(1 = 1.0);
end prog;
