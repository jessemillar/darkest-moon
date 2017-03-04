chest=kind({
	extends=entity,
	cbox=make_box(-8,-8,7,-4)
})

function chest:s_default(t)
	collide(self,"cbox",self.walked_into)
end

function chest:walked_into(ob)
	if player_inventory_harvested>0 then
		sfx(10)
		score+=player_inventory_harvested*10
		player_inventory_harvested=0
	end
end

function chest:render(t)
	local pos=self.pos

	spr(160,pos.x-8,pos.y-16,2,2) 
end
