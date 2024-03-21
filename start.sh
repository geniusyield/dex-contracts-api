#!/bin/bash
echo "======[geniusyield-server]======"
echo "Startup checks...."
# Check if SERVER_CONFIG environment variable is set
if [ -z "./SERVER_CONFIG" ]; then
    echo "Error: SERVER_CONFIG environment variable is not set." >&2
    exit 1 # Exit code 1 for unset variable
fi
if [ -z "./CORE_MAESTRO_API_KEY" ]; then
    echo "Error: CORE_MAESTRO_API_KEY environment variable is not set." >&2
    exit 1 # Exit code 1 for unset variable
fi
if [ -z "./MAESTRO_API_KEY" ]; then
    echo "Error: MAESTRO_API_KEY environment variable is not set." >&2
    exit 1 # Exit code 1 for unset variable
fi
if [ -z "./SERVER_API_KEY" ]; then
    echo "Error: SERVER_API_KEY environment variable is not set." >&2
    exit 1 # Exit code 1 for unset variable
fi

# Check if yq is installed
if ! command -v yq &> /dev/null; then
    echo "Error: yq is not installed. Please install yq to validate YAML content." >&2
    exit 2 # Exit code 2 for yq not installed
fi

# Attempt to parse SERVER_CONFIG as YAML
echo "$SERVER_CONFIG" | yq eval . - > /dev/null 2>&1
if [ $? -ne 0 ]; then
    echo "Error: SERVER_CONFIG does not contain a valid YAML document." >&2
    exit 3 # Exit code 3 for invalid YAML content
fi

# If the script reaches this point, SERVER_CONFIG is both set and valid
echo "SERVER_CONFIG is set and contains a valid YAML document."
echo "===================================="
echo "Replace placeholders...."
export SERVER_CONFIG=$(echo "$SERVER_CONFIG" | sed "s%<<CORE_MAESTRO_API_KEY>>%$CORE_MAESTRO_API_KEY%g")
export SERVER_CONFIG=$(echo "$SERVER_CONFIG" | sed "s%<<MAESTRO_API_KEY>>%$MAESTRO_API_KEY%g")
export SERVER_CONFIG=$(echo "$SERVER_CONFIG" | sed "s%<<SERVER_API_KEY>>%$SERVER_API_KEY%g")
echo "[OK] Done. Replaced placeholders."
echo "===================================="
echo "Starting geniusyield-server..."
set -x
cabal run geniusyield-server -- serve
