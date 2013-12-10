-- Prompt the user for a number, then compute the square root of that number
-- to within a hard-coded tolerance.
procedure sqrt is
tolerance : constant real := 0.001;
maxiterations : constant integer := 20;
i : integer;
square : real; 
root : real;
diff : real; begin
	put("Using tolerance = ");
	put(tolerance);
	put(" and maxiterations = ");
	put_line(maxiterations);

	put("sqrt of ? ");
	get(square);

	if square < 0.0 then
		put_line("sqrt on negative number!");
	end if;
	-- else
	if not (square < 0.0) then
		-- Compute an initial guess. Dividing by two is kind of lame, but will
		-- suffice.
		root := square / 2.0;
		put("Initial guess: ");
		put_line(root);

		-- Compute the absolute value of the difference between the
		-- square of the guess and the real square.
		diff := root * root - square;
		if diff < 0.0 then
			-- unary minus does not exist in Baby Ada
			diff := 0.0 - diff;
		end if;

		while diff > tolerance and i < maxiterations loop
			-- Newton's Method
			root := root - (root * root - square) / (2.0 * root);

			-- Babylonian Method (equivalent to Newton's, but not
			-- quite as exciting, syntax-wise)
			--root := 0.5 * (root + square / root);

			diff := root * root - square;
			if diff < 0.0 then
				diff := 0.0 - diff;
			end if;

			put("(");
			put(i);
			put(") Refined to: ");
			put(root);
			put_line(" ...");

			i := i + 1;
		end loop;

		-- Output the final value.
		put("Square root of ");
		put(square);
		put(" is ");
		put_line(root);
	end if;
end sqrt;
