function _update()
	t=t+1 -- increment the clock

	tbox_interact()

	-- let all objects update
	update_entities()
	-- check for collisions
	-- collision callbacks happen here
	do_collisions()
end
