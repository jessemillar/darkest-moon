-- add a new text box
function tbox(speaker, message)
	local linebreak=1 -- keeps track of the last linebreak position

	if #tbox_messages>0 and #tbox_messages%2==1 then -- add an empty line for a "new" dialogue box if a previous message exists in the queue
		tbox_line(speaker, "")
	end

	for i=0,flr(#message/26) do -- search through the message and break it into words
		local line=sub(message, linebreak, linebreak+26) -- lines are 26 characters but grab 26 to do a lookahead check

		if #line==27 and #message>linebreak+26 then -- if we're not near the end of the message
			for j=#line,0,-1 do -- look backward for the first whitespace character to determine the linebreak
				if sub(line,j,j)==" " then
					local lookahead=0

					if j==#line then -- if the line ends perfectly at the end of a word...
						lookahead=1
					end

					tbox_line(speaker, sub(line, 0, j+lookahead)) -- add the word to the array
					linebreak+=j
					break
				end
			end
		else
			tbox_line(speaker, line) -- add the rest of the message to the text boxes array
			break -- only add the message once
		end
	end
end

-- a utility function for easily adding a line to the messages array
function tbox_line(speaker, line)
	local line={speaker=speaker, line=line, animation=0}
	add(tbox_messages, line)
end

-- check for button presses so we can clear text box messages
function tbox_interact()
	if #tbox_messages>0 and btnp(4) then
		sfx(0) -- play a sound effect

		if #tbox_messages>1 then
			del(tbox_messages, tbox_messages[1])
		end

		del(tbox_messages, tbox_messages[1])
	end
end

-- check if a text box is currently visible (useful if the dialogue clear button is used for other actions as well)
function tbox_active()
	if #tbox_messages>0 then
		return true
	else
		return false
	end
end

-- draw the text boxes (if any)
function tbox_draw()
	if #tbox_messages>0 then -- only draw if there are messages
		rectfill(3, 103, 124, 123, 7) -- draw border rectangle
		rectfill(5, 106, 122, 121, 1) -- draw fill rectangle
		line(5, 105, 122, 105, 6) -- draw top border shadow 
		line(3, 124, 124, 124, 6) -- draw bottom border shadow 

		-- draw the speaker portrait
		if #tbox_messages[1].speaker>0 then
			local speaker_width=#tbox_messages[1].speaker*4

			if speaker_width>115 then
				speaker_width=115
			end

			rectfill(3, 96, speaker_width+9, 102, 7) -- draw border rectangle
			rectfill(5, 99, speaker_width+7, 105, 1) -- draw fill rectangle
			line(5, 98, speaker_width+7, 98, 6) -- draw top border shadow 

			print(sub(tbox_messages[1].speaker, 0, 28), 7, 101, 7)
		end

		-- print the message
		if tbox_messages[1].animation<#tbox_messages[1].line then
			sfx(1)
			tbox_messages[1].animation+=1
		elseif tbox_messages[2].animation<#tbox_messages[2].line then
			sfx(1)
			tbox_messages[2].animation+=1
		end
			
		print(sub(tbox_messages[1].line, 0, tbox_messages[1].animation), 7, 108, 7) 
		if #tbox_messages>1 then -- only draw a second line if one exist
			print(sub(tbox_messages[2].line, 0, tbox_messages[2].animation), 7, 115, 7) 
		end
		
		-- draw and animate the arrow
		if t%10<5 then
			spr(1, 116, 116)
		else
			spr(1, 116, 117)
		end
	end
end
