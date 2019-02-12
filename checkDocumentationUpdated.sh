#!/usr/bin/env bash
# Check if the documentation in the man directory matches what should be in it
# based on what is in the headers of the R scripts.
echo "Starting documentation check."

# Concatenate all the files in the man dir into one long string and md5sum it.
orig_sum=$(find man -type f | sort -u | xargs cat | md5sum)

# Rebuild the documentation.
R -e "devtools::document()"

# Concatenate all the files in the man dir into one long string and md5sum it.
new_sum=$(find man -type f | sort -u | xargs cat | md5sum)

# echo $orig_sum
# echo $new_sum

if [ "$orig_sum" != "$new_sum" ]; then
  echo "Your committed manual files (man/*.Rd) are out of sync with the documentation in the R files."
  echo "Run roxygenise() locally then commit again."
  exit 1
else
  echo "Documentation up to date."
  exit 0
fi
