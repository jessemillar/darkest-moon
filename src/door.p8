door=kind({
	extends=entity,
	cbox=make_box(-3,9,4,11)
})

function door:s_default(t)
	collide(self,"cbox",self.walked_into)
end

function door:walked_into(ob)
	player_sleeping=true
end

function door:render(t)
	local pos=self.pos

	spr(220,pos.x-16,pos.y-12,4,3) 
end
