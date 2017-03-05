door=kind({
	extends=entity,
	cbox=make_box(-1,9,6,11)
})

function door:s_default(t)
	collide(self,"cbox",self.walked_into)
end

function door:walked_into(ob)
	player_sleeping=true
end
