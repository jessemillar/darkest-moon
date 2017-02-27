reticle=kind({
	extends=entity
})

function reticle:render(t)
	local pos=self.pos

	spr(143,pos.x-4,pos.y-4) 
end
