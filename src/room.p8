-------------------------------
-- room generation
-------------------------------

-- node
--  pos : vector
--  dir : vector/dirno
--  w : int

function generate_room()
 --clear
 g_rfill(make_box(0,0,18,19),156)
 --prepare
 local lx,ux,lw,uw=
  flr(rnd(12)+1),
  flr(15-rnd(11)),
  flr(rnd(0)+2),
  flr(rnd(0)+2)
  
 carve_corridor(
  {pos=v(lx,18),w=lw,d=v(0,-1)},
  {pos=v(ux,0),w=uw,d=v(0,1)}
 ) 
 --finishing
 g_connect_up(fpats)
 g_connect_up(cpats)
 g_randomize(reps)
 
 -- indiana
 ply=indiana:new({
  pos=v(lx*8+lw*4,120),facing=3
 })
end

function corr_box(n)
 local p,d,w=n.pos,n.d,n.w
 return vec_box(
  p,p+d*w+d:rotcw()*w
 )
end

function carve_corridor(n1,n2)
 n1,n2=set({},n1),set({},n2)
 local n,b1,b2
 -- extend towards each other
 while true do
  b1,b2=
   corr_box(n1),
   corr_box(n2)
  g_rfill(b1,128)
  g_rfill(b2,128)
  if ((n2.pos-n1.pos)^n1.d<=n2.w) break  
  n=rnd()>0.5 and n1 or n2
  n.pos+=n.d
 end
 -- connect together
 local cbox=make_box(
  min(b1.xl,b2.xl),
  min(b1.yt,b2.yt),
  max(b1.xr,b2.xr),
  max(b1.yb,b2.yb)
 )
 g_rfill(cbox,128)
end

function g_rfill(box,tile)
 for x=box.xl,box.xr-1 do
  for y=box.yt,box.yb-1 do
   mset(x,y,tile)
  end
 end
end

fpats={
 --fronts
 {0,0,32,0,1,0, 187},
 {0,0,32,0,2,0, 171},
}
cpats={
 --fronts
 ---right
 {0,0,16,0,1,16,1,0,0, 174},
 {0,0,16,1,0,0, 190},
 ---left
 {0,0,16,0,1,16,-1,0,0, 173},
 {0,0,16,-1,0,0, 189},
 --walls
 ---inner corners
 {0,0,32,1,0,32,0,1,32,1,1,16, 132},
 {0,0,32,-1,0,32,0,1,32,-1,1,16, 134},
 {0,0,32,1,0,0,0,1,32,1,1,32, 164},
 {0,0,32,-1,0,0,0,1,32,-1,1,32, 166},
 ---outer corners
 {0,0,32,0,1,16,1,0,0, 185},
 {0,0,32,0,1,16,-1,0,0, 184},
 {0,0,0, 0,1,32, -1,1,0, 168}, 
 {0,0,0, 0,1,32, 1,1,0, 169}, 
 ---straights
 {0,0,32,0,1,16, 133},
 {0,0,32,1,0,0, 148},
 {0,0,32,1,0,16, 148},
 {0,0,32,-1,0,0, 150},
 {0,0,32,-1,0,16, 150},
 {0,0,0, 0,1,32, 165} 
}
function g_cmatch(x,y,m)
 x,y=mid(x,0,15),mid(y,0,15)
 local v=band(fget(mget(x,y)),0x30)
 return v==m
end

function g_connect_up(pats)
 for x=0,15 do
  for y=0,15 do
   for p in all(pats) do
    local match=true
    for n=3,#p,3 do
     if not g_cmatch(x+p[n-2],y+p[n-1],p[n]) then
      match=false
      break
     end 
    end
    if match then
     mset(x,y,p[#p])
     break
    end
   end
  end
 end 
end

reps={
 r128={128,144,160,176},
 r187={187,187,188},
 r171={171,171,172}
}
function g_randomize(reps)
 for x=0,15 do
  for y=0,15 do
   local r=reps["r"..mget(x,y)]
   if r then 
    mset(x,y,r[flr(rnd(#r)+1)])
   end
  end
 end
end
