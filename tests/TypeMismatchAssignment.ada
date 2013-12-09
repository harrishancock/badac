-- Test if compiler allows mismatched types in an assignment statement.

procedure prog is
a : integer;
b : boolean; begin
	a := b;
end prog;
