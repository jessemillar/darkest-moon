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
