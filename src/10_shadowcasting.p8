-------------------------------
-- shadowcasting by walls
-------------------------------

function render_wall_shadows()
 local render_one=
  render_shadow_fn()
 for e in all(entities_with.walls) do
  foreach(e.walls,render_one)
 end
end

function render_shadow_fn()
 -- remember lighting info
 local p,rng=lgt.pos:ints(),lgt:range()
 local rngsq=rng*rng
 local black=fl_color(0)
 
 -- return function using it
 return function(wall)
	 local s,e=wall.s,wall.e
	 local dist=wall.d^(p-s) 
	 if (dist<=0) return 
	 local ds,de=s-p,e-p
	 if (#ds>rngsq and #de>rngsq) return
	 local horiz=wall.d.x~=0
	 
	 local cs,ce=
	  rng/max(abs(ds.x),abs(ds.y)),
	  rng/max(abs(de.x),abs(de.y))
	 local ps,pe=
	  ds*cs,de*ce
	 local pm=
	  v(abs(ps.x/pe.x)>1 and ps.x or pe.x,
	    abs(ps.y/pe.y)>1 and ps.y or pe.y)

  local pts={s,e,p+pe,p+pm,p+ps}
  if wall.h then
 	 holed_ngon(pts,wall.h,black)
 	else
 	 ngon(pts,black)
 	end
 end
end

-------------------------------
-- solids (light obscuring)
-------------------------------

gobs={
	sd_spire={
		sprite={n=67,w=2,h=4},
		tile=115,
		off=v(8,4),
		walls={
		 {-8,0,8,0,3},
		 {8,0,8,-15,1},
		 {8,-15,-8,-15,4},
		 {-8,-15,-8,0,2}
		},
		hole={-4,-32,3,-15},
		cbox=make_box(-8,-15,7,0)
	}
}

solid=kind({
 extends=entity
})

function solid:create()
 local def,pos=
  self.def,self.pos
 
 self.cbox=self.def.cbox
 local hole=self.def.hole
 if hole then
  hole={
   tl=pos+v(hole[1],hole[2]),
   br=pos+v(hole[3],hole[4])
  }
 end
 self.walls={}
 for wd in all(self.def.walls) do
  add(self.walls,{
   s=pos+v(wd[1],wd[2]),
   e=pos+v(wd[3],wd[4]),
   d=dirs[wd[5]],
   h=hole
  })
 end
 entity.create(self)
end

solid.walked_into=true

function solid:render()
 local s=self.def.sprite
 local spos=
  self.pos+v(-s.w*4,-s.h*8)  
 -- dynamic lighting
 dim_object(self)
 spr(s.n,spos.x,spos.y,s.w,s.h,self.flipped)
end
