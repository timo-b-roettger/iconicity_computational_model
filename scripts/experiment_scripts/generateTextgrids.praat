# RENAMING SOUNDS SCRIPT
# This script renames sounds by removing the last two numbers in the filename.
# It then concatenates all sounds and generates a text grid.
# Begin by selecting all sounds to be renamed and concatenated in GUI, prior to running the script.
# Last changes made: 2021-05-10
# To be added: 
#	- form that prompts user to select folder with sounds
#	- form that prompts user to type talker id (talker$), later used for saving concatenated sound and textgrid.
#	- add save long sound file + textgrid
#	- add select all sounds initially

nSounds=numberOfSelected("Sound")
for i from 1 to nSounds
        sels[i] = selected("Sound",i)
endfor
for i from 1 to nSounds
        selectObject: sels[i]
	filename$=selected$("Sound")
# Change numbers$ to , 2 if filename ends with two digits
	numbers$ = right$ (filename$, 1)
	newFilename$ = filename$ - numbers$
	Rename: newFilename$
endfor
select all
Concatenate recoverably
