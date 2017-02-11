-- a box is just a rectangle with some helper methods
box=kind()

function box:translate(v)
	return make_box(
		self.xl+v.x,self.yt+v.y,
		self.xr+v.x,self.yb+v.y
	)
end
 
 function box:overlaps(b)
	return 
		self.xr>=b.xl and 
		b.xr>=self.xl and
		self.yb>=b.yt and 
		b.yb>=self.yt
 end
 
 function box:contains(pt)
	return pt.x>=self.xl and
		pt.y>=self.yt and
		pt.x<=self.xr and
		pt.y<=self.yb
 end
    
 function box:sepv(b)
	local candidates={
		v(b.xl-self.xr-1,0),
		v(b.xr-self.xl+1,0),
		v(0,b.yt-self.yb-1),
		v(0,b.yb-self.yt+1)
	}

	return min_of(candidates,vec.__len)   
 end

function make_box(xl,yt,xr,yb)
	if (xl>xr) xl,xr=xr,xl
	if (yt>yb) yt,yb=yb,yt

	return box:new({
		xl=xl,yt=yt,xr=xr,yb=yb
	})
end

function vec_box(v1,v2)
	return make_box(
		v1.x,v1.y,
		v2.x,v2.y
	)
end
