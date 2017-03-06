function _update()
	t=t+1 -- increment the text box timer

	if game_state<2 then 
		if btnp(4) then
			sfx(10)
			game_state+=1
		end
	elseif game_state==2 then
		if #tbox_messages==0 then
			if game_over then
				sfx(-1) -- stop sfx
				music(0)
				score=0
				day=1
				plyr.pos.x=22
				plyr.pos.y=42
				plyr.facing=4
				mrdr.pos.x=flr(rnd(128))
				mrdr.pos.y=150
				player_sleeping=false
				player_waking=false
				player_inventory_seeds_max=3
				player_inventory_harvested=0
				player_inventory_seeds=player_inventory_seeds_max
				player_harvesting_streak=0
				t=0
				game_over=false
				game_state=0
			end

			-- let all objects update
			update_entities()

			-- check for collisions
			-- collision callbacks happen here
			do_collisions()
		end

		tbox_interact()
	end
end
