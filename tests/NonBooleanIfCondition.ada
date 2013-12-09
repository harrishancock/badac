-- Test whether compiler allows a non-boolean type in an 'if' condition.

procedure prog is begin
	if 1.1 then put_line("This ain't good"); end if;
end prog;
