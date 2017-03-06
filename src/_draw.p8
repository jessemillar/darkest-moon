function _draw()
	cls() -- clear the screen

	if game_state==0 then
		draw_encoded(15, 20, 97, 50, encoded_logo, 14) -- draw the title screen logo

		if t%25<18 then
			print_centered("press z to play", 82, 6)
		end

		print("copyright 2016", 36, 102, 5)
		print("jesse millar", 40, 110, 5)
	elseif game_state==1 then
		local line_height=9

		print_centered("movement", line_height, 9)
		print_centered("move with the arrow keys", line_height*2, 6)

		print_centered("planting", line_height*3, 9)
		print_centered("plant wheat by pressing z", line_height*4, 6)

		print_centered("harvesting", line_height*5, 9)
		print_centered("walk into wheat bundles", line_height*6, 6)
		print_centered("take bundles to the chest", line_height*7, 6)
		print_centered("chest is located south-east", line_height*8, 6)

		print_centered("sleeping", line_height*9, 9)
		print_centered("walk into your house", line_height*10, 6)
		print_centered("sleeping refills seeds", line_height*11, 6)

		if t%25<18 then
			print_centered("press z to start", line_height*12+flr(line_height/2), 7)
		end
	elseif game_state==2 then
		-- reset the palette
		palt()
		palt(0,false)

		-- clip to lit rectangle
		local xl,yt,xr,yb=lght:extents()
		clip(xl,yt,xr-xl+1,yb-yt+1) 

		-- store clipping coords globally to let us not draw certain objects
		clipbox=make_box(
			xl-8,yt,xr+8,yb+24
		)

		-- background from level map
		map(0,0,0,0,16,16) 

		-- under-entity "blob" shadows
		render_blob_shadows() 

		-- entities themselves
		render_entities()

		-- "foreground" layer of level (parts that need to be on top of entities)
		map(0,0,0,0,16,16,128) 

		-- apply lighting to all that
		lght:apply() 

		-- "real" polygonal shadows
		render_wall_shadows()

		clip(0,0,128,128) 

		renderHUD()
		tbox_draw() -- draw the message boxes (if any)

		-- debug_collisions()
		-- show_performance()
	end
end
