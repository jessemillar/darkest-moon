function _init()
	t=0

	init_blending(6)
	init_palettes(16)

	build_room(0,0)
	process_walls()

	plyr=player:new({
		pos=v(22,42),facing=4
	})

	rtcl=reticle:new({
		pos=plyr.pos
	})

	lgt=light:new({
		pos=plyr.pos,bri=0.85
	})

	chst=chest:new({
		pos=v(61,20)
	})
end
