chest=kind({
	extends=entity,
	cbox=make_box(-8,-8,7,-4)
})

function chest:walked_into(ob)
	if ob=="player" and player_inventory_harvested>0 then
		if player_inventory_harvested==player_inventory_seeds_max and player_inventory_seeds==0 then
			player_harvesting_streak+=1

			tbox("", "perfect streak up! score bonus set to "..50*player_harvesting_streak.."!")

			if player_harvesting_streak%2==0 then
				player_inventory_seeds_max+=1
				tbox("", "seed count increased by one!")
			end
		else
			if player_harvesting_streak>0 then
				tbox("", "perfect streak ruined...")
			end

			player_harvesting_streak=0
		end

		sfx(10)
		score+=player_inventory_harvested*10+50*player_harvesting_streak
		player_inventory_harvested=0
	end
end

function chest:render(t)
	local pos=self.pos

	spr(160,pos.x-8,pos.y-16,2,2) 
end
