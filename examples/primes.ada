procedure primes is
passed : boolean;
i : integer;
prime : integer;
maximum : integer; begin
	put("Generate primes through ? ");
	get(maximum);

	prime := 2;
	while prime < maximum or prime = maximum loop
		-- This is about as inefficient as it gets. :)
		i := prime - 1;
		passed := true;  -- assume it's a prime until proven otherwise
		while i > 1 loop
			-- without a break statement, we've got to do funny
			-- business to preserve any previous failure
			passed := passed and not (0 = (prime mod i));
			i := i - 1;
		end loop;
		if passed then
			put(prime);
			put(" ");
		end if;
		prime := prime + 1;
	end loop;
	put("\n");
end primes;
