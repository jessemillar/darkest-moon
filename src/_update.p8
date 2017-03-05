function _update()
	t=t+1 -- increment the text box timer

	if game_state==0 then 
		if btnp(4) then
			game_state=1
		end
	else
		if #tbox_messages==0 then
			-- let all objects update
			update_entities()

			-- check for collisions
			-- collision callbacks happen here
			do_collisions()
		end

		tbox_interact()
	end
end
