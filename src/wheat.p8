wheat=kind({
	extends=entity,
	cbox=make_box(-4,-4,3,3),
	growth=0
})

function wheat:s_default(t)
	-- grow
	if self.t>0 and self.t%25==0 then -- define the growth rate with modulus
		if self.growth<3 then
			self.growth+=1
		end
	end
end

function wheat:walked_into(ob)
	if self.growth==3 then
		self.state="s_harvest"
	end
end

function wheat:render(t)
	local pos=self.pos
	local float_offset=0

	if self.growth==3 then
		float_offset=cos(self.t/50)/50
	end

	spr(128+self.growth,pos.x-4,pos.y-4+float_offset) 
end

function wheat:s_harvest()
	sfx(11)
	player_inventory_harvested+=1
	printh(player_inventory_harvested)
	return true
end
