------------------------------
-- utilities
------------------------------

function round(x)
 return flr(x+0.5)
end

-- copies props to obj
-- if obj is nil, a new
-- object will be created,
-- so set(nil,{...}) copies
-- the object
function set(obj,props)
 obj=obj or {}
 for k,v in pairs(props) do
  obj[k]=v
 end
 return obj
end

-- used for callbacks into
-- entities that might or
-- might not have a method
-- to handle an event
function event(ob,name,...)
 local cb=ob[name]
 return type(cb)=="function"
  and cb(ob,...)
  or cb
end

-- returns smallest element
-- of seq, according to key
-- function 
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
