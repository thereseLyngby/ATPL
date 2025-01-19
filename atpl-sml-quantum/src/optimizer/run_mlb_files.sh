#!/bin/bash

# Check if mlkit is installed
if ! command -v mlkit &> /dev/null; then
  echo "Error: mlkit is not installed or not in the PATH."
  exit 1
fi

# Directory containing the .mlb files
MLB_DIR="./test_files"  # Update to point to the test_files directory

# Iterate over each .mlb file in the directory
for file in "$MLB_DIR"/*.mlb; do
  # Check if there are no .mlb files
  if [ ! -e "$file" ]; then
    echo "No .mlb files found in $MLB_DIR"
    exit 1
  fi

  echo "Processing file: $file"
  
  # Compile the .mlb file
  echo "Compiling $file..."
  mlkit "$file" >/dev/null
  
  # Check if compilation produced a 'run' executable
  if [ ! -f "./run" ]; then
    echo "Error: Compilation of $file failed. No 'run' executable found."
    continue
  fi

  # Time the execution of the compiled program
  echo "Running the compiled program..."
  START=$(date +%s.%N)
  ./run
  END=$(date +%s.%N)
  
  # Calculate elapsed time
  DURATION=$(echo "$END - $START" | bc)
  echo "Execution time for $file: $DURATION seconds"
  echo "------------------------------------"

  # Clean up the generated 'run' file
  echo "Cleaning up..."
  rm -f ./run
done