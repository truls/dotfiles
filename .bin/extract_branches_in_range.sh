#!/bin/bash

# Usage: ./extract_branches_in_range.sh <source-branch>
# Example: ./extract_branches_in_range.sh main

set -euo pipefail

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <source-branch>"
    exit 1
fi

SOURCE_BRANCH="$1"

# Get decorated log from SOURCE_BRANCH..HEAD
git log --oneline --decorate=full "$SOURCE_BRANCH"..HEAD |
    grep -oP '\(.*?\)' |   # Extract everything inside parentheses
    tr -d '()' |           # Remove parentheses
    tr ',' '\n' |          # Split by comma
    sed -E 's/^[[:space:]]+//' | # Trim leading space
    grep -E 'refs/heads/' | # Only include local branches
    sed 's|refs/heads/||' |
    sed 's/HEAD -> //g'
