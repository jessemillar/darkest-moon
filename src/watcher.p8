-------------------------------
-- ghostly watcher
-------------------------------

watcher=kind({
	extends=entity,
	shadow={x=0,y=0,rx=8,ry=4}
})

function watcher:render(t)
	local z=sin(t*0.007)*3-3
	local p=self.pos-v(0,z)-v(8,24)

	spr(14,p.x,p.y,2,3) 
end
