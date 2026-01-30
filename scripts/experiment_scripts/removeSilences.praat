# --- SETTINGS ---
# Set default values
pitch = 100
time_step = 0
thresh = -25
min_sil = 0.1
min_sound = 0.05

# Ask the user for the folder containing the sounds
form Process folder
    sentence Input_folder /Users/name/Desktop/sounds/
    sentence Output_folder /Users/name/Desktop/trimmed/
endform

# Create the output folder if it doesn't exist (Praat command)
createDirectory: output_folder$

# Get a list of all wav files in the folder
Create Strings as file list: "fileList", input_folder$ + "*.wav"
num_files = Get number of strings

# Loop through all files and trim
for i from 1 to num_files
    selectObject: "Strings fileList"
    file_name$ = Get string: i
    
    # Read the sound
    Read from file: input_folder$ + file_name$
    sound_id = selected("Sound")
    base_name$ = selected$("Sound")

    # Generate textgrid to identify silences
    # [Pitch], [Time Step], [Silence Thresh], [Min Sil], [Min Sound], [Sil Label], [Sound Label]
    To TextGrid (silences): pitch, time_step, thresh, min_sil, min_sound, "silent", "sounding"
    tg_id = selected("TextGrid")

    # Find sounding intervals
    n_intervals = Get number of intervals: 1
    first = 0
    last = 0
    for j from 1 to n_intervals
        label$ = Get label of interval: 1, j
        if label$ = "sounding"
            if first = 0
                first = j
            endif
            last = j
        endif
    endfor

    # Extract and Save
    if first > 0
        t_start = Get start time of interval: 1, first
        t_end = Get end time of interval: 1, last
        
        selectObject: sound_id
        Extract part: t_start, t_end, "rectangular", 1, "no"
        
        # Save to the output folder
        Save as WAV file: output_folder$ + file_name$
        
        # Clean up the trimmed version from list
        Remove
    else
        appendInfoLine: "No sound detected in: ", file_name$
    endif

    # Clean up original Sound and TextGrid before next file
    selectObject: tg_id
    Remove
    selectObject: sound_id
    Remove
endfor

# Clean up the file list
selectObject: "Strings fileList"
Remove

appendInfoLine: "Done! All files processed."