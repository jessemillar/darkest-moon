function _update()
	t=t+1 -- increment the text box timer

	if #tbox_messages==0 then
		-- let all objects update
		update_entities()

		-- check for collisions
		-- collision callbacks happen here
		do_collisions()
	end

	tbox_interact()
end
