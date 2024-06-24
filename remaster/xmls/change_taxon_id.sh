#!/bin/bash

# Prompt the user to enter the path to the XML file
read -p "Enter the path to the XML file: " xml_file

# Check if the file exists
if [ ! -f "$xml_file" ]; then
    echo "File not found!"
    exit 1
fi

# Create a file to save extracted taxon IDs
extracted_ids_file="extracted_taxon_ids.txt"

# Clear contents of the file if it already exists
> "$extracted_ids_file"

# Temporary file for storing modified XML content
temp_file=$(mktemp)

# Read each line of the XML file
while IFS= read -r line; do
    # Check if the line starts with "<date value="
    if [[ $line == \<date\ value=* ]]; then
        # Skip this line if it starts with "<date value="
        continue
    fi
    
    # Check if the line contains "<taxon id="
    if [[ $line == *"<taxon id="* ]]; then
        # Extract taxon ID from the line
        taxon_id=$(sed -n 's/.*<taxon id="\([^"]*\)".*/\1/p' <<< "$line")

        # Check if the line contains a taxon ID
        if [ ! -z "$taxon_id" ]; then
            # Extract the part before the second underscore
            part_before_second_underscore=$(awk -F'_' '{print $1"_"$2}' <<< "$taxon_id")

            # Save the extracted taxon ID to the file
            echo "$part_before_second_underscore" >> "$extracted_ids_file"

            # Replace the taxon ID with the extracted part
            modified_line=$(sed "s/$taxon_id/$part_before_second_underscore/" <<< "$line")

            # Append the modified line to the temporary file
            echo "$modified_line" >> "$temp_file"
        else
            # If the line doesn't contain a taxon ID, just append it to the temporary file
            echo "$line" >> "$temp_file"
        fi
    else
        # If the line doesn't contain "<taxon id=", just append it to the temporary file
        echo "$line" >> "$temp_file"
    fi
done < "$xml_file"

# Overwrite the original XML file with the modified content
mv "$temp_file" "$xml_file"

echo "Taxon IDs replaced successfully."
echo "Extracted taxon IDs saved to: $extracted_ids_file"
