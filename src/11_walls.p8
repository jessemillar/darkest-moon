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
