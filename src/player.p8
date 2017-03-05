player_speed=1
player_sleeping=false
player_waking=false
player_inventory_harvested=0
player_inventory_seeds_max=3
player_inventory_seeds=player_inventory_seeds_max
player_harvesting_streak=0

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

	if not player_sleeping and not player_waking then
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

		-- constrain the player to the screen
		if self.pos.x<0 then
			self.pos.x=0
		elseif self.pos.x>128 then
			self.pos.x=128
		end

		if self.pos.y<0 then
			self.pos.y=0
		elseif self.pos.y>128 then
			self.pos.y=128
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
			if player_inventory_seeds>0 then
				sfx(12)
				player_inventory_seeds-=1
				wheat:new({
					pos=v(reticle_left,reticle_top-8)
				})
			else
				sfx(13)
			end
		end
	else
		if player_sleeping then
			lght.bri-=0.05
		end

		if lght.bri<=0.05 then
			lght.bri=0.05
			player_sleeping=false
			player_waking=true
			mrdr.pos.x=flr(rnd(128))
			mrdr.pos.y=150
			self.facing=4
			player_inventory_seeds=player_inventory_seeds_max
		end

		if player_waking then
			lght.bri+=0.05
		end

		if lght.bri>=0.85 then
			light.bri=0.85
			player_waking=false
			day+=1
		end
	end
end

function player:hit_object(ob)
	return event(ob,"walked_into")
end

function player:render()
	local pos=self.pos
	local sprite=player_sprites[self.facing]+self.frm*2

	-- player sprite
	spr(sprite,pos.x-8,pos.y-16,2,2,self.facing==1)
end
