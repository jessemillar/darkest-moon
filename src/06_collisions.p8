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
