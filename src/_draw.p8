function _draw()
	cls() -- clear the screen

	if game_state==0 then
		draw_encoded(0, 15, 128, 70, encoded_logo, 14) -- draw the title screen logo

		if t%25<18 then
			print("press z to play", 34, 90, 6)
		end
	else
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
