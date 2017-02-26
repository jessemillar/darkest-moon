-- entity root type
entity=kind({
	t=0,
	state="s_default"
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
