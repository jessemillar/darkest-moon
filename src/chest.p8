chest=kind({
	extends=entity,
	shadow={x=0,y=-2,rx=8,ry=4},
	cbox=make_box(-8,-8,7,-4)
})

function chest:s_default(t)
	collide(self,"cbox",self.walked_into)
end

function chest:walked_into(ob)
	printh(time())
end

function chest:render(t)
	local pos=self.pos

	spr(160,pos.x-8,pos.y-16,2,2) 
end
