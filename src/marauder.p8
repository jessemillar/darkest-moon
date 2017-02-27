marauder_speed=0.8

marauder_sprites={
	64,64,96,70
}

marauder=kind({
	extends=entity,
	frm=0,
	shadow={x=0,y=0,rx=8,ry=4},
	shoff=v(0,0),
	cbox=make_box(-3,-6,4,1)
})

marauder_shadow_locs={
	v(2,0),v(-2,0),v(0,0),v(0,-3)
}

function marauder:s_default(t)
	-- moving around
	local moving=false

	for i=0,3 do  
		if btn(i) then
			self.facing=i+1
			self.pos+=dirs[i+1]*marauder_speed
			moving=true
		end
	end 

	if moving then
		if t%6==0 then
			self.frm=(self.frm+1)%3
		end
	else
		self.frm=0
	end

	-- update shadow position
	set(self.shadow,marauder_shadow_locs[self.facing])
	-- collision detection
	collide(self,"cbox",self.hit_object)
end

function marauder:hit_object(ob)
	return event(ob,"walked_into")
end

function marauder:render()
	local pos=self.pos
	local sprite=marauder_sprites[self.facing]+self.frm*2

	spr(sprite,pos.x-8,pos.y-16,2,2,self.facing==1)
end
