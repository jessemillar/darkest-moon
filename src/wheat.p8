wheat=kind({
	extends=entity,
	cbox=make_box(-4,-4,3,3),
	growth=0
})

function wheat:s_default(t)
	collide(self,"cbox",self.walked_into)

	if self.t>0 and self.t%25==0 then
		if self.growth<3 then
			self.growth+=1
		end
	end

	if self.growth==3 then
		self.pos.y=cos(self.t/50)/50+self.pos.y
	end
end

function wheat:walked_into(ob)
	if self.growth==3 then
		self.state="s_delete"
	end
end

function wheat:render(t)
	local pos=self.pos

	spr(128+self.growth,pos.x-4,pos.y-4) 
end

function wheat:s_delete()
	player_inventory_harvested+=1
	printh(player_inventory_harvested)
	return true
end
