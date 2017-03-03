function renderHUD()
	-- print the day ("level")
	print_ol("day: "..day,5,5,2,0)

	-- wheat hud sprite
	spr(162,87,3)
	print_ol(player_inventory_harvested,98,5,2,0)

	-- seed hud sprite
	spr(144,108,3)
	print_ol(player_inventory_seeds,119,5,2,0)
end

-- print outlined text
function print_ol(s,_x,_y,text_color,outline_color)
	for x=-1,1 do
		for y=-1,1 do
			print(s,_x+x,_y+y,outline_color)
		end
	end

	print(s,_x,_y,text_color)
end
