player_speed=1

player_sprites={
	2,2,32,8
}

player=kind({
	extends=entity,
	frm=0,
	shadow={x=0,y=0,rx=8,ry=4},
	shoff=v(0,0),
	cbox=make_box(-3,-6,4,1)
})

player_shadow_locs={
	v(2,0),v(-2,0),v(0,0),v(0,-3)
}

function player:s_default(t)
	-- moving around
	local moving=false

	for i=0,3 do  
		if btn(i) then
			self.facing=i+1
			self.pos+=dirs[i+1]*player_speed
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
	set(self.shadow,player_shadow_locs[self.facing])
	-- collision detection
	collide(self,"cbox",self.hit_object)

	-- planting wheat in a grid
	local vertical_shift=0
	local horizontal_shift=0

	if self.facing==1 then
		horizontal_shift=-16
	elseif self.facing==2 then
		horizontal_shift=16
	elseif self.facing==3 then
		vertical_shift=-16
	else
		vertical_shift=8
	end

	local reticle_left=flr(self.pos.x/8)*8+horizontal_shift+4
	local reticle_top=flr(self.pos.y/8)*8+vertical_shift+4

	rtcl.pos=v(reticle_left,reticle_top)

	if btnp(4) then
		wheat:new({
			pos=v(reticle_left,reticle_top)
		})
	end
end

function player:hit_object(ob)
	return event(ob,"walked_into")
end

function player:render()
	local pos=self.pos
	local sprite=player_sprites[self.facing]+self.frm*2

	spr(sprite,pos.x-8,pos.y-16,2,2,self.facing==1)
end
