light=kind({
	extends=entity,
	off=v(0,0),
	cbox=make_box(-1,-1,1,1)
})

light_offsets={
	v(-7,-2),v(7,-2),
	v(0,-9),v(0,6)
}

function light:s_default()
	-- anchor to avatar
	self.pos=plyr.pos
end

function light:range()
	return flr(42*self.bri)
end

function light:extents()
	local p,r=self.pos:ints(),

	self:range()

	return
		p.x-r,p.y-r,
		p.x+r,p.y+r
end

function light:apply()
	local p=self.pos:ints()

	local light_fill=fl_light(
		p.x,p.y,self.bri,
		blend_line
	)

	local xl,yt,xr,yb=
		self:extents()
		crect(xl,yt,xr,yb,
			light_fill)
end
