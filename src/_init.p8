function _init()
	t=0

	init_blending(6)
	init_palettes(16)

	build_room(0,0)
	process_walls()

	ply=player:new({
		pos=v(22,42),facing=4
	})

	lgt=light:new({
		pos=v(64,120),bri=1
	})

	chst=chest:new({
		pos=v(61,20)
	})

	tbox_messages={} -- the array for keeping track of text box overflows
	tbox("", "the chest contained spray cheese!")
	tbox("bernard", "he-hello...? this is bernard. is anyone there? over...")
	tbox("gregory", "yes! i am herrrre! over!")
	tbox("bernard", "cool! how are you? over.")
	tbox("gregory", "i'm good, man. how are you? over!")
	tbox("bernard", "so good. over.")
	tbox("lewis", "i am good toooooo guys!")
	tbox("gregory", "lewis? is that you? how did you get this frequency? over.")
	tbox("lewis", "i have my wayz.")
	tbox("bernard", "you're a crazy pong, lewis.")
	tbox("lewis", "why, thank you, sir bernard.")
end
