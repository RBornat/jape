#!/bin/bash
# claims to be, in bash, the way to find source directory.
# from https://stackoverflow.com/questions/59895/how-can-i-get-the-source-directory-of-a-bash-script-from-within-the-script-itsel/60157372#60157372
ps
full_path_to_script="$(realpath -s "$0")"

# You can then also get the full path to the directory, and the base
# filename, like this:
script_directory="$(dirname "$full_path_to_script")"
script_filename="$(basename "$full_path_to_script")"

# Now print it all out
echo "full_path_to_script = \"$full_path_to_script\""
echo "script_directory    = \"$script_directory\""
echo "script_filename     = \"$script_filename\""
