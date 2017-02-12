chest=kind({
	extends=entity,
	walked_into=true,
	shadow={x=0,y=-2,rx=8,ry=4},
	cbox=make_box(-8,-8,7,-4)
})

function chest:s_default(t)
	collide(self,"cbox",self.hit_object)
end

function chest:hit_object(ob)
	printh("impact")
end

function chest:render(t)
	local pos=self.pos

	spr(78,pos.x-8,pos.y-16,2,2) 
end
