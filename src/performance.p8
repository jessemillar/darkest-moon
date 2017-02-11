function show_performance()
	clip()
	local cpu=flr(stat(1)*100)
	local fps=-30/flr(-stat(1))
	local perf=
		cpu .. "% cpu @ " ..
		fps ..  " fps"
	print(perf,0,122,0)
	print(perf,0,121,fps==30 and 7 or 8)
end
