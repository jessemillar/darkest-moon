function _init()
	t=0 -- the clock
	init_blending(6)
	init_palettes(16)

	build_room(0,0)
	process_walls()

	ply=indiana:new({
		pos=v(64,120),facing=3
	})

	lgt=light:new({
		pos=v(64,120),bri=1
	})

	watcher:new({
		pos=v(112,24)
	})
end
