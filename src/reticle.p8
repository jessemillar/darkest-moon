reticle=kind({
	extends=entity
})

function reticle:render(t)
	local pos=self.pos

	if fget(mget(pos.x/8,pos.y/8))==64 then
		spr(143,pos.x-4,pos.y-4) 
	end
end
