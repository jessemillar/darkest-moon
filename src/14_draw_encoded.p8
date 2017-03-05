function draw_encoded(x, y, width, height, image, transparent)
	local transparent=transparent or 0 -- default to black transparency

	for y_index=0,height-1 do
		for x_index=0,width-1 do
			local index=y_index*width+x_index+1
			local color=("0x"..sub(image, index, index))+0

			if color!=transparent then
				pset(x_index+x, y_index+y, color)
			end
		end
	end
end
