-- Test if compiler allows mismatched types in an addop.

procedure prog is begin
	put(1 + 1.1);
end prog;
