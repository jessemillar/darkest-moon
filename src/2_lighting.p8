-------------------------------
-- boxes
-------------------------------

-- a box is just a rectangle
-- with some helper methods
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

-------------------------------
-- lighting
-------------------------------

-- determines how far each
-- level of light reaches
-- this is distance *squared*
-- due to the ordering here,
-- light level 1 is the
-- brightest, and 6 is the
-- darkest (pitch black)
light_rng={
 10*42,18*42,
 26*42,34*42,
 42*42,
}
-- special "guard" value
-- to ensure nothing can be
-- light level 0 without ifs
light_rng[0]=-1000

--  this is our "light" fill
-- function.
--  it operates by dimming
-- what's already there.
--  each horizontal line
-- is drawn by multiple
-- calls to another fill
-- function handling
-- the correct light level
-- for each segment.
light_fills={
 fl_none,fl_blend(2),fl_blend(3),
 fl_blend(4),fl_blend(5),fl_color(0)
}
brkpts={}
function fl_light(lx,ly,brightness,ln)
 local brightnessf,fills=
  brightness*brightness,
  light_fills
 return function(x1,x2,y)
  -- transform coordinates
  -- into light-space
  local ox,oy,oe=x1-lx,y-ly,x2-lx
  -- brightness range multiplier
  -- + per line flicker effect
  local mul=
   brightnessf*
    (rnd(0.16)+0.92)
  -- calculate light levels
  -- at left end, right end,
  local ysq=oy*oy
  local srng,erng,slv,elv=
   ysq+ox*ox,
   ysq+oe*oe
  for lv=5,0,-1 do
   local r=band(light_rng[lv]*mul,0xffff)
   if not slv and srng>=r then
    slv=lv+1
    if (elv) break
   end
   if not elv and erng>=r then
    elv=lv+1
    if (slv) break
   end
  end
  -- these will hold the
  -- lowest/highest light
  -- level within our line
  local llv,hlv=1,max(slv,elv)  
  -- calculate breakpoints
  -- (x coordinates at which
  --  light level changes,
  --  in light-space)
  -- and lowest(brightest)
  -- light level within line
  local mind=max(x1-lx,lx-x2)
  for lv=hlv-1,1,-1 do
   local brng=band(light_rng[lv]*mul,0xffff)
   local brp=_sqrt[brng-ysq]
   brkpts[lv]=brp
   if not brp or brp<mind then
    llv=lv+1
    break
   end
  end
  -- everything calculated,
  -- draw all segments now!
  local xs,xe=lx+ox
  -- from left bound to
  -- start of most-lit segment
  -- decreasing light lv
  -- (brightness increasing)
  for l=slv,llv+1,-1 do
   xe=lx-brkpts[l-1]
   fills[l](xs,xe-1,y)
   xs=xe
  end
  -- from most-lit zone
  -- to last break point
  -- increasing light lv
  -- (brightness decreasing)
  for l=llv,elv-1 do 
   xe=lx+brkpts[l]
   fills[l](xs,xe-1,y)
   xs=xe
  end
  -- last segment from
  -- last breakpoint to the
  -- right bound
  fills[elv](xs,x2,y)
 end
end

-------------------------------
-- palette effects
-------------------------------

function init_palettes(n)
 pals={}
 for p=1,n do
  pals[p]={}
  for c=0,15 do
   pals[p][c]=sget(p,c)
  end
 end
end

function reset_palette()
 pal()
 palt(3,true)
 palt(0,false)
end

function set_palette(no)
 for c,nc in pairs(pals[no]) do
  pal(c,nc)
 end
end

function dim_object(o)
 local lcoeff=(o.pos-lgt.pos).y/25
 if lcoeff>0 then
  local pt=mid(flr(lcoeff/0.1),0,6)
  set_palette(8+pt)
 end
end

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

-------------------------------
-- player object
-------------------------------

indiana=kind({
 extends=entity,
 frm=0,
 shadow={x=0,y=0,rx=8,ry=4},
 shoff=v(0,0),
 cbox=make_box(-3,-5,4,1)
})
ind_shadow_locs={
 v(2,0),v(-2,0),v(0,0),v(0,-3)
}

function indiana:s_default(t)
 -- moving around
 local moving=false
 for i=0,3 do  
  if btn(i) then
   if (not btn(4)) self.facing=i+1
   self.pos+=dirs[i+1]*0.6
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
 set(self.shadow,ind_shadow_locs[self.facing])
 -- collision detection
 collide(self,"cbox",self.hit_object)
end

function indiana:hit_object(ob)
 return event(ob,"walked_into")
end

ind_sprites={
 195,195,233,227 
}
function indiana:render()
 local pos=self.pos
 local sprite=
  ind_sprites[self.facing]+
   self.frm*2
 spr(sprite,pos.x-8,pos.y-16,2,2,self.facing==1)
end

-------------------------------
-- light object
-------------------------------

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
  --anchor to avatar
  self.pos=ply.pos
  --controlling the light
  if btn(4) and self.bri>0.2 then
   self.bri-=0.02
  end
  if btn(5) and self.bri<63/44 then
   self.bri+=0.02
  end
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
 
-------------------------------
-- ghostly watcher
-------------------------------

watcher=kind({
 extends=entity,
 shadow={x=0,y=0,rx=8,ry=4}
})
 function watcher:render(t)
  local z=sin(t*0.007)*3-3
  local p=self.pos-v(0,z)-
   v(8,24)
  spr(14,p.x,p.y,2,3) 
 end

-------------------------------
-- building a room
-------------------------------

wall=kind({
 walked_into=true,
 extends=entity
})
 
function build_room(mx,my)
 local obtab={}
 for k,gob in pairs(gobs) do
  obtab[gob.tile]=gob
 end
 
 for ty=0,15 do
  for tx=0,15 do
   local tile=mget(mx+tx,my+ty)
   local spawn=obtab[tile]
   if spawn then 
    solid:new({
     pos=v(tx,ty)*8+spawn.off,
     def=spawn
    })
    mset(tx,ty,128)
   else
    mset(tx,ty,tile)
   end
  end
 end 
end

function flags(pos,mask)
 local x,y=
  mid(pos.x,0,16),
  mid(pos.y,0,15)
 return band(fget(mget(x,y)),mask)
end

function process_walls()
 process_walls_with(v(0,1),v(1,0),4,3)
 process_walls_with(v(0,1),v(1,0),8,4)
 process_walls_with(v(1,0),v(0,1),1,1)
 process_walls_with(v(1,0),v(0,1),2,2)
 find_wall_fronts()
end

function process_walls_with(dout,din,mask,wdir)
 for row=0,15 do
  local l,c,bv,prv=
   dout*row-din*2,-2,0
  repeat
	  repeat
	   prv=bv
	   l+=din
	   c+=1
	   bv=flags(l,mask)
	  until c==16 or bv~=prv
	  if prv~=0 then
	   add_wall(sl,l,wdir)
	  end
	  sl=l
  until c==16
 end
end

wparams={
 {f=v(0,5),t=v(7,4),
  we=v(0,4)},
 {f=v(7,5),t=v(0,4),
  we=v(7,4)},
 {f=v(0,5),t=v(-1,13),
  we=v(-1,5),h=v(-1,1),wi=true},
 {f=v(0,12),t=v(-1,0),
  we=v(-1,12),h=v(-1,14)},
}
function add_wall(from,to,dr)
 local d,ps=dirs[dr],wparams[dr]
 local cs,ce,co=
  from*8+ps.f,
  to*8+ps.we,
  to*8+ps.t
 local wbox=make_box(cs.x,cs.y,co.x,co.y)
 local hole
 if ps.h then
  local ch=to*8+ps.h
  local hbox=make_box(cs.x,cs.y,ch.x,ch.y)
  hole={
   tl=v(hbox.xl,hbox.yt),
   br=v(hbox.xr,hbox.yb)
  }
 end
 wall:new({
  cbox=wbox,
  walls={
   {
    s=ps.wi and ce or cs,
    e=ps.wi and cs or ce,
    d=-d,
    h=hole   
   }
  }
 })
end

-------------------------------
-- front-facing walls
-------------------------------

-- front parts of walls are
-- drawn as entities to let
-- us darken them when they
-- should be in shadow
wallfront=kind({
 extends=entity
})
 function wallfront:render()
  dim_object(self)
  map(self.mx,self.my,
      self.pos.x,self.pos.y-16,
      self.mw,2)      
 end
 
 
function find_wall_fronts()
 for y=0,14 do
  local pc,c,sx=0
  for x=0,16 do
   c=flags(v(x,y),16)+
     flags(v(x,y+1),16)
   if c~=pc or c==16 then
    if pc==32 then
     w=wallfront:new({
      mx=sx,my=y,mw=x-sx,
      pos=v(sx,y+2)*8
     })
    end
    sx=x
   end
   pc=c
  end
 end
end
