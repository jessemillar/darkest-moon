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
