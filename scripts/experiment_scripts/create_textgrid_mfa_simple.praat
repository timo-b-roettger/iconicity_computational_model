# create_textgrid_mfa_simple.praat
# Written by E. Chodroff
# Oct 23 2018

# This script takes as input a directory containing wav files and 
# outputs TextGrids with a single tier called "utt" (for utterance) 
# and a single interval with the text of the transcript.
# The first boundary is currently set at 20 ms from the start.
# The second and final boundary is currently set at 50 ms from the end.

### CHANGE ME!
# directory with wav files that need TextGrids
dir$ = "/Users/Eleanor/Desktop/align_input/"

# text of transcript
text$ = "Here is the full text of my transcript. Please change me!"
###

## Maybe change me
# insert initial boundary 200 ms from start of file
boundary_start = 0.200

# insert final boundary 500 ms from end of file
boundary_end = 0.500
##

Create Strings as file list: "files", dir$ + "*.wav"
nFiles = Get number of strings

for i from 1 to nFiles
	selectObject: "Strings files"
	filename$ = Get string: i
	basename$ = filename$ - ".wav"
	Read from file: dir$ + basename$ + ".wav"
	dur = Get total duration
	To TextGrid: "utt", ""
	Insert boundary: 1, boundary_start
	Insert boundary: 1, dur-boundary_end
	Set interval text: 1, 2, text$
	Save as text file: dir$ + basename$ + ".TextGrid"
	select all
	minusObject: "Strings files"
	Remove
endfor