function _draw()
	cls() -- clear the screen
	
	-- reset the palette
	palt()
	palt(0,false)

	-- clip to lit rectangle
	local xl,yt,xr,yb=lgt:extents()
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
	lgt:apply() 

	-- "real" polygonal shadows
	render_wall_shadows()

	debug_collisions()
	-- show_performance()
end
