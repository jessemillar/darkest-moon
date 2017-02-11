function _update()
	-- let all objects update
	update_entities()
	-- check for collisions
	-- collision callbacks happen
	-- here
	do_collisions()
end
