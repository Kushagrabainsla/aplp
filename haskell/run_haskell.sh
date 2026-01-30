#!/bin/bash
# Usage: ./run_haskell.sh <filename.hs>
# Compiles and runs the given Haskell file

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <filename.hs>"
  exit 1
fi

FILE="$1"
BASENAME=$(basename "$FILE" .hs)
DIRNAME=$(dirname "$FILE")

# Compile the Haskell file
ghc -o "$DIRNAME/$BASENAME" "$FILE"
if [ $? -ne 0 ]; then
  echo "Compilation failed."
  exit 2
fi
# Compilation successful message and separator
echo "Compilation successful. Running the program..."
echo "-----------------------------------"

# Run the compiled executable
"$DIRNAME/$BASENAME"
