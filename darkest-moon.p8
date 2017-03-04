pico-8 cartridge // http://www.pico-8.com
version 8
__lua__

function round(x)
	return flr(x+0.5)
end

-- copies props to obj
-- if obj is nil, a new object will be created, so set(nil,{...}) copies the object
function set(obj,props)
	obj=obj or {}

	for k,v in pairs(props) do
		obj[k]=v
	end

	return obj
end

-- used for callbacks into entities that might or might not have a method to handle an event
function event(ob,name,...)
	local cb=ob[name]

	return type(cb)=="function"
		and cb(ob,...)
		or cb
end

-- returns smallest element of seq, according to key function 
function min_of(seq,key)
	local me,mk=nil,32767

	for e in all(seq) do
		local k=key(e)

		if k<mk then
			me,mk=e,k
		end
	end

	return me
end

-- creates a "class" object with support for basic inheritance/initialization
function kind(kob)
	kob=kob or {}

	setmetatable(kob,{__index=kob.extends})

	kob.new=function(self,ob)
		ob=set(ob,{kind=kob})

		setmetatable(ob,{__index=kob})

		if (kob.create) ob:create()

		return ob
	end
	 
	return kob 
end

-- for some stuff, we want vector math - so we make a vector type with all the usual operations
vec={}

function vec.__add(v1,v2)
	return v(v1.x+v2.x,v1.y+v2.y)
end

function vec.__sub(v1,v2)
	return v(v1.x-v2.x,v1.y-v2.y)
end

function vec.__mul(v1,a)
	return v(v1.x*a,v1.y*a)
end

function vec.__mul(v1,a)
	return v(v1.x*a,v1.y*a)
end

function vec.__div(v1,a)
	return v(v1.x/a,v1.y/a)
end

-- we use the ^ operator to mean dot product
function vec.__pow(v1,v2)
	return v1.x*v2.x+v1.y*v2.y
end

function vec.__unm(v1)
	return v(-v1.x,-v1.y)
end

-- this is not really the length of the vector, but length squared - easier to calculate, and can be used for most of the same stuff
function vec.__len(v1)
	local x,y=v1:split()
	return x*x+y*y
end

-- normalized vector
function vec:norm()
	return self/sqrt(#self)
end

-- rotated 90-deg clockwise
function vec:rotcw()
	return v(-self.y,self.x)
end

-- force coordinates to integers
function vec:ints()
	return v(flr(self.x),flr(self.y))
end

-- used for destructuring, i.e.:  x,y=v:split()
function vec:split()
	return self.x,self.y
end

-- has to be there so our metatable works for both operators and methods
vec.__index = vec

-- creates a new vector
function v(x,y)
	local vector={x=x,y=y}
	setmetatable(vector,vec)
	return vector
end

-- vector for each cardinal direction, ordered the same way pico-8 d-pad is
dirs={
	v(-1,0),v(1,0),
	v(0,-1),v(0,1)
}

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

-- entity root type
entity=kind({
	t=0,
	state="s_default",
	solid=true
})

-- a big bag of all entities
entities={}

-- entities with some special props are tracked separately
entities_with={}

tracked_props={
	"render","cbox",
	"walls","shadow"
}

-- used to add/remove objects in the entities_with list
function update_with_table(e,fn)
	for prop in all(tracked_props) do
		if e[prop] then
			local lst=entities_with[prop] or {}

			fn(lst,e)
			entities_with[prop]=lst
		end
	end
end

-- all entities do common stuff when created - mostly register in lists
e_id=1

function entity:create()
	if not self.name then
		self.name=e_id..""
		e_id+=1
	end

	local name=self.name

	entities[name]=self

	update_with_table(self,add)
end

-- this is the core of our _update() method - update each entity in turn
function update_entities()
	for n,e in pairs(entities) do
		local update_fn=e[e.state]
		local result = update_fn and update_fn(e,e.t) or nil

		if result then
			if result==true then
				-- remove entity
				entities[n]=nil
				update_with_table(e,del)
			else
				-- set state
				set(e,{
					state=result,t=0
				})
			end
		else
			-- bump timer in this state
			e.t+=1
		end
	end
end

-- renders entities, sorted by y to get proper occlusion
function render_entities()
	ysorted={}

	for d in all(entities_with.render) do
		local y=d.pos and flr(d.pos.y) or 139

		ysorted[y]=ysorted[y] or {}
		add(ysorted[y],d)
	end

	for y=clipbox.yt,clipbox.yb do
		for d in all(ysorted[y]) do
			reset_palette()
			d:render(d.t)
		end

		reset_palette()
	end
end

function render_blob_shadows()
	local sh_fill=fl_blend(5)

	for e in all(entities_with.shadow) do
		local sh=e.shadow
		local p=e.pos+e.shadow

		if clipbox:contains(p) then
			cellipse(p.x,p.y,
			sh.rx,sh.ry,sh_fill)
		end
	end
end

function c_box(e)
	local b,p=e.cbox,e.pos
	return p
		and b:translate(p:ints())
		or b
end

cqueue={}

function collide(ent,prop,cb)
	add(cqueue,{e=ent,p=prop,cb=cb})
end

function do_collisions()
	for c in all(cqueue) do
		local e=c.e
		local eb=c_box(e)

		for o in all(entities_with[c.p]) do
			if o!=e then
				local ob=c_box(o)
				if eb:overlaps(ob) then
					local separate=c.cb(e,o)
					if separate and e.solid and o.solid then
						local sepv=eb:sepv(ob)
						e.pos+=sepv
						eb=eb:translate(sepv)
					end
				end
			end
		end
	end

	cqueue={} -- wipe the collision queue
end

function debug_collisions()
	for e in all(entities_with.cbox) do
		local eb=c_box(e)

		rect(eb.xl,eb.yt,eb.xr,eb.yb,14)
	end
end

--  all shapes accept a fill function which is responsible for actual drawing the functions just do calculations and clipping

-- draws a polygon from an array of points, using ln() to fill it points must be clockwise
function ngon(pts,ln)
 local xls,xrs=ngon_setup(pts)
 for y,xl in pairs(xls) do
  local xr=xrs[y]
  ln(xl,xr,y)
 end
end

-- like ngon, but with a rectangular hole (used to mask shadows)
dummy_hole={tl={y=16000},br={}}
function holed_ngon(pts,hole,ln)
 local xls,xrs=ngon_setup(pts)
 hole=hole or dummy_hole
 local htop,hbot,hl,hr=
  hole.tl.y,hole.br.y,
  hole.tl.x,hole.br.x
 for y,xl in pairs(xls) do
  local xr=xrs[y]
  if y<htop or y>hbot then
   ln(xl,xr,y)
  else
   local cl,cr=
    min(hl,xr),max(hr,xl)
   if xl<=cl then
    ln(xl,cl,y)
   end
   if cr<=xr then
    ln(cr,xr,y)
   end
  end
 end
end

-- sets up min/max x of each polygon line
function ngon_setup(pts)
 local xls,xrs={},{} 
 local npts=#pts
 for i=0,npts-1 do
  ngon_edge(
   pts[i+1],pts[(i+1)%npts+1],
   xls,xrs
  )
 end
 return xls,xrs
end

function ngon_edge(a,b,xls,xrs)
 local ax,ay=a.x,round(a.y)
 local bx,by=b.x,round(b.y)
 if (ay==by) return

 local x,dx=
  ax,(bx-ax)/abs(by-ay)
 if ay<by then
  for y=ay,by do
   xrs[y]=x
   x+=dx
  end
 else
  for y=ay,by,-1 do
   xls[y]=x
   x+=dx
  end
 end
end

-- draws a filled rectangle with a custom fill fn
function crect(x1,y1,x2,y2,ln)
 x1,x2=max(x1,0),mid(x2,127)
 y1,y2=max(y1,0),min(y2,127)
 if (x2<x1 or y2<y1) return
 for y=y1,y2 do
  ln(x1,x2,y)
 end
end

-- draws a filled ellipse with a custom fill fn
function cellipse(cx,cy,rx,ry,ln)
 cy,ry=round(cy),round(ry)
 local w=0
 local ryx,rxy=ry/rx,rx/ry
 local dy=(-2*ry+1)*rxy
 local dx=ryx
 local ddx=2*ryx
 local ddy=2*rxy
 local lim=rx*ry
 local v=ry*ry*rxy
 local my=cy+ry-1
 for y=cy-ry,cy-1 do
  -- creep w up until we hit lim
  while true do
   if v+dx<=lim then
    v+=dx
    dx+=ddx
    w+=1
   else
    break
   end
  end
  -- draw line
  if w>0 then
   local l,r=
    mid(cx-w,0,127),
    mid(cx+w-1,0,127)
   if (y>=0 and y<128) ln(l,r,y)
   if (my>=0 and my<128) ln(l,r,my)
  end
  -- go down
  v+=dy
  dy+=ddy
  my-=1
 end
end

-------------------------------
-- basic fills
-------------------------------

-- a fill function is just a function(x1,x2,y) that draws a horizontal line

-- returns a fill function that draws a solid color
function fl_color(c)
 return function(x1,x2,y)
  rectfill(x1,y,x2,y,c)
 end
end

-- used as fill function for ignored areas
function fl_none()
end

-------------------------------
-- fast blend fill
-------------------------------

-- sets up everything blend_line will need
function init_blending(nlevels)
 -- tabulate sqrt() for speed
 _sqrt={}
 for i=0,4096 do
  _sqrt[i]=sqrt(i)
 end

 -- populate look-up tables for blending based on palettes in sprite mem
 for lv=1,nlevels do
  -- light luts are stored in memory directly, table indexing is costly
  local addr=0x4300+lv*0x100
  local sx=lv-1
  for c1=0,15 do
   local nc=sget(sx,c1)
   local topl=shl(nc,4)
   for c2=0,15 do
    poke(addr,
     topl+sget(sx,c2))
    addr+=1
   end
  end
 end 
end

function fl_blend(l)
 local lutaddr=0x4300+shl(l,8)
	return function(x1,x2,y)
	 local laddr=lutaddr
	 local yaddr=0x6000+shl(y,6)
	 local saddr,eaddr=
	  yaddr+band(shr(x1+1,1),0xffff),
	  yaddr+band(shr(x2-1,1),0xffff)
	 -- odd pixel on left?
	 if band(x1,1.99995)>=1 then
	  local a=saddr-1
	  local v=peek(a)
	  poke(a,
	   band(v,0xf) +
	   band(peek(bor(laddr,v)),0xf0)
	  )
	 end
	 -- full bytes
	 for addr=saddr,eaddr do
	  poke(addr,
	   peek(
	    bor(laddr,peek(addr))
	   )
	  )
	 end
	 -- odd pixel on right?
	 if band(x2,1.99995)<1 then
	  local a=eaddr+1
	  local v=peek(a)
	  poke(a,
	   band(peek(bor(laddr,v)),0xf) +
	   band(v,0xf0)
	  )
	 end
	end
end

-- determines how far each level of light reaches this is distance *squared* due to the ordering here, light level 1 is the brightest, and 6 is the darkest (pitch black)
light_rng={
 10*42,18*42,
 26*42,34*42,
 42*42,
}

-- special "guard" value to ensure nothing can be light level 0 without ifs
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
  -- these will hold the lowest/highest light level within our line
  local llv,hlv=1,max(slv,elv)  
  -- calculate breakpoints (x coordinates at which light level changes, in light-space) and lowest(brightest) light level within line
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
  -- everything calculated, draw all segments now!
  local xs,xe=lx+ox
  -- from left bound to start of most-lit segment decreasing light lv (brightness increasing)
  for l=slv,llv+1,-1 do
   xe=lx-brkpts[l-1]
   fills[l](xs,xe-1,y)
   xs=xe
  end
  -- from most-lit zone to last break point increasing light lv (brightness decreasing)
  for l=llv,elv-1 do 
   xe=lx+brkpts[l]
   fills[l](xs,xe-1,y)
   xs=xe
  end
  -- last segment from last breakpoint to the right bound
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
	palt(14,true)
	palt(0,false)
end

function set_palette(no)
	for c,nc in pairs(pals[no]) do
		pal(c,nc)
	end
end

function dim_object(o)
	local lcoeff=(o.pos-lght.pos).y/25

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
 local p,rng=lght.pos:ints(),lght:range()
 local rngsq=rng*rng
 local black=fl_color(0)
 
 -- return function using it
 return function(wall)
	 local s,e=wall.s,wall.e
	 local dist=wall.d^(p-s) 
	 if (dist<=0) return 
	 local ds,de=s-p,e-p
	 if (#ds>rngsq and #de>rngsq) return
	 local horiz=wall.d.x!=0
	 
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

gobs={}

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
	  until c==16 or bv!=prv
	  if prv!=0 then
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

-- front parts of walls are drawn as entities to let us darken them when they should be in shadow
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
   if c!=pc or c==16 then
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

function _draw()
	cls() -- clear the screen
	
	-- reset the palette
	palt()
	palt(0,false)

	-- clip to lit rectangle
	local xl,yt,xr,yb=lght:extents()
	clip(xl,yt,xr-xl+1,yb-yt+1) 

	-- store clipping coords globally to let us not draw certain objects
	clipbox=make_box(
		xl-8,yt,xr+8,yb+24
	)

	-- background from level map
	map(0,0,0,0,16,16) 

	-- under-entity "blob" shadows
	render_blob_shadows() 

	-- entities themselves
	render_entities()

	-- "foreground" layer of level (parts that need to be on top of entities)
	map(0,0,0,0,16,16,128) 

	-- apply lighting to all that
	lght:apply() 

	-- "real" polygonal shadows
	render_wall_shadows()

	clip(0,0,128,128) 

	renderHUD()

	-- debug_collisions()
	-- show_performance()
end

function _init()
	day=1 -- keep track of the "level"
	score=0

	init_blending(6)
	init_palettes(16)

	build_room(0,0)
	process_walls()

	plyr=player:new({
		pos=v(22,42),
		facing=4
	})

	mrdr=marauder:new({
		pos=v(100,100),
		facing=4
	})

	rtcl=reticle:new({
		pos=plyr.pos
	})

	lght=light:new({
		pos=plyr.pos,
		bri=0.85
	})

	hs=house:new({
		pos=v(30,12)
	})

	chst=chest:new({
		pos=v(61,20)
	})
end

function _update()
	-- let all objects update
	update_entities()

	-- check for collisions
	-- collision callbacks happen here
	do_collisions()
end

chest=kind({
	extends=entity,
	cbox=make_box(-8,-8,7,-4)
})

function chest:s_default(t)
	collide(self,"cbox",self.walked_into)
end

function chest:walked_into(ob)
	if player_inventory_harvested>0 then
		sfx(10)
		score+=player_inventory_harvested*10
		player_inventory_harvested=0
	end
end

function chest:render(t)
	local pos=self.pos

	spr(160,pos.x-8,pos.y-16,2,2) 
end

house=kind({
	extends=entity,
	cbox=make_box(-16,-12,15,9)
})

function house:s_default(t)
	collide(self,"cbox",self.walked_into)
end

function house:walked_into(ob)
	player_sleeping=true
end

function house:render(t)
	local pos=self.pos

	spr(220,pos.x-16,pos.y-12,4,3) 
end

function renderHUD()
	-- print the score
	print_ol("score",5,5,6,0)
	print_ol(score,30,5,2,0)

	-- print the day ("level")
	print_ol("day",50,5,6,0)
	print_ol(day,67,5,2,0)

	-- wheat hud sprite
	spr(162,87,3)
	print_ol(player_inventory_harvested,98,5,2,0)

	-- seed hud sprite
	spr(144,108,3)
	print_ol(player_inventory_seeds,119,5,2,0)
end

-- print outlined text
function print_ol(s,_x,_y,text_color,outline_color)
	for x=-1,1 do
		for y=-1,1 do
			print(s,_x+x,_y+y,outline_color)
		end
	end

	print(s,_x,_y,text_color)
end

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

marauder_speed=0.6
marauder_move_chance=5
marauder_chasing=false

marauder_sprites={
	64,64,96,70
}

marauder=kind({
	extends=entity,
	frm=0,
	shadow={x=0,y=0,rx=8,ry=4},
	shoff=v(0,0),
	cbox=make_box(-3,-6,4,1)
})

marauder_shadow_locs={
	v(2,0),v(-2,0),v(0,0),v(0,-3)
}

function marauder:s_default(t)
	-- moving around
	local moving=false

	if rnd(100)<marauder_move_chance then
		marauder_chasing=not marauder_chasing
	end

	if marauder_chasing then
		if self.pos and plyr.pos then
			if flr(self.pos.x)!=flr(plyr.pos.x) then
				if self.pos.x<plyr.pos.x then
					self.facing=2
					self.pos.x+=marauder_speed/2
					moving=true
				elseif self.pos.x>plyr.pos.x then
					self.facing=1
					self.pos.x-=marauder_speed/2
					moving=true
				end
			end

			if flr(self.pos.y)!=flr(plyr.pos.y) then
				if self.pos.y<plyr.pos.y then
					self.facing=4
					self.pos.y+=marauder_speed/2
					moving=true
				elseif self.pos.y>plyr.pos.y then
					self.facing=3
					self.pos.y-=marauder_speed/2
					moving=true
				end
			end
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
	set(self.shadow,marauder_shadow_locs[self.facing])
	-- collision detection
	collide(self,"cbox",self.hit_object)
end

function marauder:hit_object(ob)
	return event(ob,"marauded")
end

function marauder:render()
	local pos=self.pos
	local sprite=marauder_sprites[self.facing]+self.frm*2

	spr(sprite,pos.x-8,pos.y-16,2,2,self.facing==1)
end

function show_performance()
	clip()
	local cpu=flr(stat(1)*100)
	local fps=-30/flr(-stat(1))
	local perf=
		cpu .. "% cpu @ " ..
		fps ..  " fps"
	print(perf,0,122,0)
	print(perf,0,121,fps==30 and 7 or 8)
end

player_speed=1
player_sleeping=false
player_waking=false
player_inventory_harvested=0
player_inventory_seeds=5

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

reticle=kind({
	extends=entity
})

function reticle:render(t)
	local pos=self.pos

	spr(143,pos.x-4,pos.y-4) 
end

wheat=kind({
	extends=entity,
	cbox=make_box(-4,4,3,11),
	growth=0,
	solid=false
})

function wheat:s_default(t)
	-- grow
	if self.t>0 and self.t%25==0 then -- define the growth rate with modulus
		if self.growth<3 then
			self.growth+=1
		end
	end

	collide(self,"cbox",self.hit_object)
end

function wheat:double_plant()
	self.state="s_destroy"
end

function wheat:hit_object(ob)
	return event(ob,"double_plant")
end

function wheat:walked_into(ob)
	if self.growth==3 then
		self.state="s_harvest"
	end
end

function wheat:render(t)
	local pos=self.pos
	local float_offset=0

	if self.growth==3 then
		float_offset=cos(self.t/50)/50
	end

	spr(128+self.growth,pos.x-4,pos.y-4+float_offset,1,2) 
end

function wheat:s_destroy()
	sfx(13)
	return true
end

function wheat:s_harvest()
	sfx(11)
	player_inventory_harvested+=1
	return true
end

__gfx__
0000000000000000eeeeee0000eeeeeeeeeeee0000eeeeeeeeeeee0000eeeeeeeeeeee00000eeeeeeeeeee00000eeeeeeeeeee00000eeeee0000000000000000
1110000011000000eeee00222200eeeeeeee00222200eeeeeeee00222200eeeeeeee002222200eeeeeee002222200eeeeeee002222200eee0000000000000000
2211000021100000eee0222444220eeeeee0222444220eeeeee0222444220eeeeee02224442220eeeee02224442220eeeee02224442220ee0000000000000000
3331100033110000ee02244444440eeeee02244444440eeeee02244444440eeeeee02442224420eeeee02442224420eeeee02442224420ee0000000000000000
4221100044221000ee024442222240eeee024442222240eeee024442222240eeee02422fff22420eee02422fff22420eee02422fff22420e0000000000000000
5511100055110000ee024420fff020eeee024420fff020eeee024420fff020eeee042f0fff0f240eee042f0fff0f240eee042f0fff0f240e0000000000000000
66d5100066dd5100ee0242f0fff020eeee0242f0fff020eeee0242f0fff020eeee042f0fff0f240eee042f0fff0f240eee042f0fff0f240e0000000000000000
776d100077776d51eee042ffffff20eeeee042ffffff20eeeee042ffffff20eeeee042fffff240eeeee042fffff240eeeee042fffff240ee0000000000000000
8822100088842100eeee04222222f0eeeeee04222222f0eeeeee04222222f0eeeeee0422222f20eeeeee0422222f20eeeeee0422222f20ee0000000000000000
9422100099942100eee024421245550eee0024421245550eeee029421245550eeee04422125550eeeee04422125550eeeee04422125550ee0000000000000000
a9421000aa994210eee0d22ccc2a7a0ee0f1d22ccc2a7a0eeee0119ccc2a7a0eeee022dccca7a0eeee0922dccca7a0eeeee01ffdcca7a0ee0000000000000000
bb331000bbb33100ee011d999a09990ee0f1dd999a09990eeee01ff99a09990eee01d29aaa9990eeee09d29aaa9990eeeee01ffaaa9990ee0000000000000000
ccd51000ccdd5100ee0ffddcccc5550eee000d22ccc5550eeee0dffcccc5550eee0fdddccc555c0eeee0dd44cc5550eeeee0ddccc45550ee0000000000000000
dd511000dd511000eee024ddcccc00eeeeee0244ccc000eeeee0ddddcc240eeeee0dddddcccccc0eeeee0244ccc00eeeeeee00dcc4420eee0000000000000000
ee421000ee444210eee02440002440eeeeee02440220eeeeeee0110000240eeeeee02440002440eeeeee0244000eeeeeeeeeee0004420eee0000000000000000
f9421000fff94210eeee000eee000eeeeeeee000e00eeeeeeeee00eeee00eeeeeeee000eee000eeeeeeee000eeeeeeeeeeeeeeeee000eeee0000000000000000
eeeeee00000eeeeeeeeeee00000eeeeeeeeeee00000eeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eeee001111100eeeeeee001111100eeeeeee001111100eee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee02211111110eeeee02211111110eeeee02211111110ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee02222111110eeeee02222111110eeeee02222111110ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
ee0242222111110eee0242222111110eee0242222111110e00000000000000000000000000000000000000000000000000000000000000000000000000000000
ee0444222211110eee0444222211110eee0444222211110e00000000000000000000000000000000000000000000000000000000000000000000000000000000
ee0444422211110eee0444422211110eee0444422211110e00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee04442222110eeeee04442222110eeeee04442222110ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee02444222110eeeee02444222110eeeee02444222110ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee0942252220eeeeee094225222210eeee0942252220eee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee0c2d5555240eeeee0c2d555521190eee0c2d5555240ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee0c994444450eeeee0c99444445990eee0c994444450ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
ee0cdd555555550eeee0ddd55555500eeee0ddd5555550ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
ee0dddd55555550eeeee022255000eeeeeee000552220eee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee02220002220eeeeee022200eeeeeeeeeeeee002220eee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eeee000eee000eeeeeeee000eeeeeeeeeeeeeeeee000eeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeee0000eeeeeeeeeeee0000eeeeeeeeeeee0000eeeeeeeeeeee00000eeeeeeeeeee00000eeeeeeeeeee00000eeeee00000000000000000000000000000000
eeee00222200eeeeeeee00222200eeeeeeee00222200eeeeeeee002222200eeeeeee002222200eeeeeee002222200eee00000000000000000000000000000000
eee0222444220eeeeee0222444220eeeeee0222444220eeeeee02224442220eeeee02224442220eeeee02224442220ee00000000000000000000000000000000
ee02244444440eeeee02244444440eeeee02244444440eeeeee02442224420eeeee02442224420eeeee02442224420ee00000000000000000000000000000000
ee024442222240eeee024442222240eeee024442222240eeee0242200022420eee0242200022420eee0242200022420e00000000000000000000000000000000
ee024420000020eeee024420000020eeee024420000020eeee0420000000240eee0420000000240eee0420000000240e00000000000000000000000000000000
ee024200000020eeee024200000020eeee024200000020eeee0420000000240eee0420000000240eee0420000000240e00000000000000000000000000000000
eee042000000200eeee04200000020eeeee04200000020eeeee04200000240eeeee04200000240eeeee04200000240ee00000000000000000000000000000000
eeee042222224060eeee0422222240eeeeee042222220eeeeeee042222240eeeeeee0422262420eeeeee042222240eee00000000000000000000000000000000
eee0244212440670ee00244217640eeeeee0294212440eeeeee04422126440eeeee04422176440eeeee04422122420ee00000000000000000000000000000000
eee0322bbb22070ee0f1322bbb760eeeeee0119bbb20eeeeeee0223bbb7620eeee09223bbb76f0eeeee01ff3bbb20eee00000000000000000000000000000000
ee0113999a90f0eee0f133999a07f0eeeee01ff99a90eeeeee01329aaa976f0eee09329aaa9ff0eeeee01ffaaaa90eee00000000000000000000000000000000
ee0ff33bbbbb0eeeee000322bbbff0eeeee03ffbbbb0eeeeee0f333bbbbbff0eeee03344bbbbb0eeeee033bbb4420eee00000000000000000000000000000000
eee02433bbbb0eeeeeee0244bbb00eeeeee03333bb240eeeee033333bbbbbb0eeeee0244bbb00eeeeeee003bb4420eee00000000000000000000000000000000
eee02440002440eeeeee02440220eeeeeee0110000240eeeeee02440002440eeeeee0244000eeeeeeeeeee0004420eee00000000000000000000000000000000
eeee000eee000eeeeeeee000e00eeeeeeeee00eeee00eeeeeeee000eee000eeeeeeee000eeeeeeeeeeeeeeeee000eeee00000000000000000000000000000000
eeeeee00000eeeeeeeeeee00000eeeeeeeeeee00000eeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eeee001111100eeeeeee001111100eeeeeee001111100eee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee02211111110eeeee02211111110eeeee02211111110ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee02222111110eeeee02222111110eeeee02222111110ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
ee0242222111110eee0242222111110eee0242222111110e00000000000000000000000000000000000000000000000000000000000000000000000000000000
ee0444222211110eee0444222211110eee0444222211110e00000000000000000000000000000000000000000000000000000000000000000000000000000000
ee0444422211110eee0444422211110eee0444422211110e00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee04442222110eeeee04442222110eeeee04442222110ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee02444222110eeeee02444222110eee0e02444222110ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eeee042252220eeeeeee04225222210e060e042252220eee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee0b235555240eeeee0b235555211900760b235555240ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee0b994444450eeeee0b99444445990e076f994444450ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
ee0b33555555550eeee033355555500eee0ff335555550ee00000000000000000000000000000000000000000000000000000000000000000000000000000000
ee0333355555550eeeee022255000eeeeee0000552220eee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eee02220002220eeeeee022200eeeeeeeeeeeee002220eee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eeee000eee000eeeeeeee000eeeeeeeeeeeeeeeee000eeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeee0eeee0eeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000099eeee99
eeeeeeeeeeeeeeee0a0ee090eeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000000000009eeeeee9
eeeeeeeeeeeeeeee0aa00990eeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000eeeeeeee
eeeeeeeeeeeeeeee0aa00a90eeee0eee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000eeeeeeee
eeeeeeeeeeeeee0e0a099aa0e0e0a00e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000eeeeeeee
eeeeeeeeeeeee0900a099aa0090a90900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000eeeeeeee
eeeeeeeee0eee0900aa90a90099aaa0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009eeeeee9
eeeeeeee0a0ee0900aa90a90e0aa9aa0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000099eeee99
e00eeeee0a000a900aa90a90e0aa9a90000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0ff0ee0e0a090a900aa90a90ee0a9a0e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0f0ee0f00a090a0e0aa90a0eee0240ee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e0ee0ff0e009f00ee0a9000ee02240ee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eee0e00e09a90a9009a90a90ee0a0eee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ee0f0eee09a00a9009a00a90e0aa90ee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e0ff0eeee0a00a0ee0a00a0e0909a0ee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ee00eeeeee0ee0eeee0ee0eee0e00eee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeee00e00e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ee000000000000eee09a09a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e02442444442420eee0aaaa000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0244444444444420ee0a990e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0442444444424440ee0240ee00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0422222222222240e0240eee00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
02444424442444200aa90eee00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0444444444444440e000eeee00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000dddd0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0444445dd54444400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
02424445524444200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
02222222222222200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
04442444444424400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
02444424444444200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e00000000000000e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbb4444444444444444bbbbbbbbbbbbbbbbbbbbbbbb04444420000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbb4444444444444444bbbb33bbb3bbbbbbbbbbbbbb02445420000000000000000000000000000000000000000000000000000000000000000000000000
bbbbb3bb4444444444444444bb3444444453bb3444334bbbb0445420000000000000000000000000000000000000000000000000000000000000000000000000
bb3b3bbb4444444444444444bb54444444444444444445bb02424420000000000000000000000000000000000000000000000000000000000000000000000000
b3bb3b3b4444444444444444bb44444444444444444444bb02424420000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbb4d11d1d444444444b344444444444444444444bb02424420000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbbd6666d6d44444444b34445d444444444444444bb0542450b000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbbd666666d44444444b34442444444444444d543bb04424540000000000000000000000000000000000000000000000000000000000000000000000000
d6666666666666666666666dbb34444444444444444443bbb04545400000000000000000000000000000000000000000099999999a9999999999999a99999990
16666666666666666666666dbb3444444444444444444bbb0244444000000000000000000000000000000000000000000999999999999999999a999949999990
d6666666666666666666666dbbb4444444444444444443bb02444440000000000000000000000000000000000000000009999f9f99a99a9999999f99999a9990
d66666666666666666666661bb44444444444444444445bb024544200000000000000000000000000000000000000000099994999999a99999aa9999999f9990
d66666666666666666666661b344444444445d44444444bb0245440b00000000000000000000000000000000000000000999999999aa9944499aa99a9a999990
16666666666666666666666db344444444455544444444bb0244242000000000000000000000000000000000000000000999999999a994444499aa9999999990
16666666666666666666666db344454444422244444444bb0244242000000000000000000000000000000000000000000999a9944a994444444499aa99994990
d6666666666666666666666dbb44444444444244444443bb3000000300000000000000000000000000000000000000000999f999aa9444774744499aa99a9990
bbbbbbbb1666666d00000000bb44444444444444d54444bb0000000000000000000000000000000000000000000000000999999aa9444777477444499a999f90
bbbbbbbb166d666100000000bb34444444444444524443bb00000000000000000000000000000000000000000000000009999aaa944477774777444499a99990
bbbbbbbb1ddd111100000000bb44444444445444444444bb0000000000000000000000000000000000000000000000000949aa994444777747777744999aa990
bbbbbbbb166d11d100000000bb44444444442444444443bb00000000000000000000000000000000000000000000000009aaa99444444444444444444999aa90
bbbbbbbb1d66d1d100000000bb34444444444444444443bb00000000000000000000000000000000000000000000000009a99947444444444444444444499a90
bbbbbbbb1dd1d1d100000000bbb54443b3544b3443445bbb00000000000000000000000000000000000000000000000009999447447445555555544747499990
bbbbbbbb11d1d1d100000000bbbbbbbbbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000004455555777445555555544747749440
bbbbbbbb51d5511500000000bbbbbbbbbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000004755555777445555555544774447740
0000000000000000444443bbbb344444444444444444444400000000000000000000000000000000000000000000000004755555777445555555544774747740
00000000000000004444445bb3444444445d44444444444400000000000000000000000000000000000000000000000004755555777445555555544774744740
000000000000000044444444b44444d4455544444555444400000000000000000000000000000000000000000000000004744774777445555555544774774740
00000000000000004444444444444444422244444422444400000000000000000000000000000000000000000000000004447774777445555555544774774440
0000000000000000444d444444444444444244444444444400000000000000000000000000000000000000000000000004477774777445555555544774774440
0000000000000000445544444444444444444453b444444400000000000000000000000000000000000000000000000004444444444445555555544444444440
00000000000000004442444444445544444444bbbb34444d00000000000000000000000000000000000000000000000004444444444445555555544444444440
00000000000000004444444444444244444443bbbb344444000000000000000000000000000000000000000000000000b000000000000000000000000000000b

__gff__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000005060000000000000000000000000000000000000000000000000000000000000000000000000f000000000000000000000000000000000000000000050404060000000000000000000000000908080a00000000000000000000000000000000
__map__
c6c0e0e0e0e0e0e0e0c3c4c5c6c0e0c6e0e0e0e0c6e0e0d6e0e0c6e0e0c0e0e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
d6e0e0e0e0e0c0e0e0d3c2d5d6e0e0d6e0e0c0e0d6e0e0e0e0e0d6e0c3c4c4c4000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e0e0e0e0e0e0e0e0c0d3c2f2c4c4c5c0e0e0e0e0e0e0c0e0c0e0e0e0d3c2c2c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c0e0e0e0e0c0e0c3c4f3c2c2c2c2f2c4c4c4c4c4c4c4c4c5e0e0e0e0d3c2c2c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e0e0c0e0c0e0e0d3c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2d5e0e0c0e0d3c2c2c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c4c4c4c4c4c4c4f3c2c2c2c2c2c2d4c2c2c2c2c2c2c2c2f2c4c4c4c4f3c2c2c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2d4c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2d4c2c2c2c2c2c2c2c2c2c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2c2c2d4c2c2c2c2c2c2c2c2c2c2d4c2c2c2c2c2c2c2c2c2c2d4c2c2c2c2c2c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2c2c2c2c2c2c2c2c2c2d4c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2c2c2c2c2c2c2c2c2c2c2c2c2f4e4e4e4e4e4e4e4e4e4e4f5c2c2c2c2d4c2c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2c2c2c2c2c2c2c2c2c2c2f4e4e5e0c0e0e0e0e0e0e0e0c0d3c2c2c2c2c2c2c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
f4e4e4e4e4e4e4f5c2c2f4e5e0e0c0e0e0e0e0e0e0c0e0e0d3c2c2d4c2c2c2c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e5e0c0e0e0c0e0e3e4e4e5e0c0e0e0e0e0e0c0e0e0e0e0e0e3e4e4e4e4e4e4e4000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
015a00201d7541c7541d7541a7541c754187541a7541a7541a7511a7551a7011d7541d7541f7541d7541c7541a754187541c7541d7541c7541a7541a7511a7551d2001320016200132001d200132001b2001a200
011f00200c1040c1000c1000c1000c1000c1000c1000c100081040810008100081000810008100081000810003104031000310003100031000310003100031000210402100021000210002100021000210002100
011f00101b200132001b200132001b200132001b200132001d200132001d200132001d200132001d2001320000000000000000000000000000000000000000000000000000000000000000000000000000000000
011f00201820000000182000000018200000001820000000182000000018200000001820000000182000000016200000001620000000162000000016200000001620000000162000000016200000001620000000
011f00100f20013200152001d2001b2001620022200292000e20013200162001b2001a200162001a2001b20000000000000000000000000000000000000000000000000000000000000000000000000000000000
011f00101860318603186031860318603186031860300000186031860418603186031860300000000001860400000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010100003c7343c7303c7353a000360003c000230000b00001000000002d000000000000030000000003200000000000003300000000330002f00029000220001c00000000000000000000000000000000000000
010100000017400170001750000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010400001862430001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010700000c420180030c4200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01060000247010c701000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010300003070300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011600003030530305303053030530305303053030500305003050030500305003050030500305003050030500305003050030500305003050030500305003050030500305003050030500305003050030500305
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
01 00014344
00 00014344
00 02030145
00 02030105
00 02030105
02 04050145
02 44454144
00 41424344
00 41424344
00 41424344
03 01424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344

