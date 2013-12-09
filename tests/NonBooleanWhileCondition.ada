-- Test whether compiler allows a non-boolean type in an 'while' condition.

procedure prog is begin
	while 1.1 loop put_line("This ain't good"); end loop;
end prog;
