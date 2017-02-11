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
