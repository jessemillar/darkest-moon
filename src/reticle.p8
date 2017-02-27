grid_empty=true

reticle=kind({
	extends=entity,
	cbox=make_box(-4,-4,3,3)
})

function reticle:s_default(t)
	collide(self,"cbox",self.hit_object)
end

function reticle:hit_object(ob)
	grid_empty=false
end

function reticle:render(t)
	local pos=self.pos

	spr(143,pos.x-4,pos.y-4) 
end
