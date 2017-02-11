------------------------------
-- drawing shapes
------------------------------

--  all shapes accept a fill
-- function which is responsible
-- for actual drawing
--  the functions just do
-- calculations and clipping

-- draws a polygon from an
-- array of points, using
-- ln() to fill it
-- points must be clockwise
function ngon(pts,ln)
 local xls,xrs=ngon_setup(pts)
 for y,xl in pairs(xls) do
  local xr=xrs[y]
  ln(xl,xr,y)
 end
end

-- like ngon, but with a
-- rectangular hole (used
-- to mask shadows)
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

-- sets up min/max x of
-- each polygon line
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

-- draws a filled rectangle
-- with a custom fill fn
function crect(x1,y1,x2,y2,ln)
 x1,x2=max(x1,0),mid(x2,127)
 y1,y2=max(y1,0),min(y2,127)
 if (x2<x1 or y2<y1) return
 for y=y1,y2 do
  ln(x1,x2,y)
 end
end

-- draws a filled ellipse
-- with a custom fill fn
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

-- a fill function is just
-- a function(x1,x2,y) that
-- draws a horizontal line

-- returns a fill function
-- that draws a solid color
function fl_color(c)
 return function(x1,x2,y)
  rectfill(x1,y,x2,y,c)
 end
end

-- used as fill function
-- for ignored areas
function fl_none()
end

-------------------------------
-- fast blend fill
-------------------------------

-- sets up everything
-- blend_line will need
function init_blending(nlevels)
 -- tabulate sqrt() for speed
 _sqrt={}
 for i=0,4096 do
  _sqrt[i]=sqrt(i)
 end

 -- populate look-up tables
 -- for blending based on
 -- palettes in sprite mem
 for lv=1,nlevels do
  -- light luts are stored in
  -- memory directly, table
  -- indexing is costly
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
