house=kind({
	extends=entity,
	cbox=make_box(-16,-12,15,9)
})

function house:s_default(t)
	collide(self,"cbox",self.walked_into)
end

function house:walked_into(ob)
	player_sleeping=true
end

function house:render(t)
	local pos=self.pos

	spr(220,pos.x-16,pos.y-12,4,3) 
end
