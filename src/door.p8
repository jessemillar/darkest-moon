door=kind({
	extends=entity,
	cbox=make_box(-1,9,6,11)
})

function door:walked_into(ob)
	if ob=="player" then
		player_sleeping=true
	end
end
