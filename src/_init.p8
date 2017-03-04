function _init()
	day=1 -- keep track of the "level"
	score=0

	init_blending(6)
	init_palettes(16)

	build_room(0,0)
	process_walls()

	plyr=player:new({
		pos=v(22,42),
		facing=4
	})

	mrdr=marauder:new({
		pos=v(flr(rnd(128)),150),
		facing=4
	})

	rtcl=reticle:new({
		pos=plyr.pos
	})

	lght=light:new({
		pos=plyr.pos,
		bri=0.85
	})

	hs=house:new({
		pos=v(30,12)
	})

	chst=chest:new({
		pos=v(61,20)
	})
end
