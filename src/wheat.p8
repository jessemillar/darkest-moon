wheat_growth_rate=25

wheat=kind({
	extends=entity,
	cbox=make_box(-4,4,3,11),
	growth=0,
	solid=false
})

function wheat:s_default(t)
	-- grow
	if self.t>0 and self.t%wheat_growth_rate==0 then -- define the growth rate with modulus
		if self.growth<3 then
			self.growth+=1
		end
	end

	if player_sleeping or player_waking then
		self.state="s_destroy"
	end

	collide(self,"cbox",self.hit_object)
end

function wheat:double_plant()
	self.state="s_destroy"
end

function wheat:hit_object(ob)
	return event(ob,"double_plant")
end

function wheat:walked_into(ob)
	if self.growth==3 then
		if ob=="player" then
			self.state="s_harvest"
		else
			self.state="s_destroy"
		end
	end
end

function wheat:render(t)
	local pos=self.pos
	local float_offset=0

	if self.growth==3 then
		float_offset=cos(self.t/50)/50
	end

	spr(128+self.growth,pos.x-4,pos.y-4+float_offset,1,2) 
end

function wheat:s_destroy()
	sfx(13)
	return true
end

function wheat:s_harvest()
	sfx(11)
	player_inventory_harvested+=1
	return true
end
