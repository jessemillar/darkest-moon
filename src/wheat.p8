wheat=kind({
	extends=entity,
	cbox=make_box(-4,-4,3,3)
})

function wheat:s_default(t)
	collide(self,"cbox",self.walked_into)
end

function wheat:walked_into(ob)
	printh(time())
end

function wheat:render(t)
	local pos=self.pos

	spr(128,pos.x-4,pos.y-4) 
end
