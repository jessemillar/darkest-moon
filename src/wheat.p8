wheat=kind({
	extends=entity,
	cbox=make_box(-4,-4,3,3),
	growth=0
})

function wheat:s_default(t)
	collide(self,"cbox",self.walked_into)

	if self.t>0 and self.t%200==0 then
		if self.growth<2 then
			self.growth+=1
		end
	end
end

function wheat:walked_into(ob)
	-- printh(time())
end

function wheat:render(t)
	local pos=self.pos

	spr(128+self.growth,pos.x-4,pos.y-4) 
end
