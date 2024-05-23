#!/bin/bash
echo "======[geniusyield-server]======"
echo "Startup checks...."
# Check if SERVER_CONFIG environment variable is set
if [ -z "$SERVER_CONFIG" ]; then
    echo "Error: SERVER_CONFIG environment variable is not set." >&2
    exit 1 # Exit code 1 for unset variable
fi

if [[ "$SERVER_CONFIG" == *"<<CORE_MAESTRO_API_KEY>>"* ]]; then
    if [ -z "$CORE_MAESTRO_API_KEY" ]; then
        echo "Error: CORE_MAESTRO_API_KEY environment variable is not set." >&2
        exit 1 # Exit code 1 for unset variable
    fi
fi

if [[ "$SERVER_CONFIG" == *"<<MAESTRO_API_KEY>>"* ]]; then
    if [ -z "$MAESTRO_API_KEY" ]; then
        echo "Error: MAESTRO_API_KEY environment variable is not set." >&2
        exit 1 # Exit code 1 for unset variable
    fi
fi

if [ -z "$SERVER_API_KEY" ]; then
    echo "Error: SERVER_API_KEY environment variable is not set." >&2
    exit 1 # Exit code 1 for unset variable
fi

if [ -z "$SEED_PHRASE" ]; then
    echo "Error: SEED_PHRASE environment variable is not set." >&2
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
echo "$SERVER_CONFIG" > ./server_config.yaml
sed -i "s|<<CORE_MAESTRO_API_KEY>>|${CORE_MAESTRO_API_KEY}|" server_config.yaml
sed -i "s|<<MAESTRO_API_KEY>>|${MAESTRO_API_KEY}|" server_config.yaml
sed -i "s|<<SERVER_API_KEY>>|${SERVER_API_KEY}|" server_config.yaml
sed -i "s|<<SEED_PHRASE>>|${SEED_PHRASE}|" server_config.yaml
export SERVER_CONFIG=$(cat server_config.yaml)
echo "[OK] Done. Replaced placeholders."

# Attempt to parse SERVER_CONFIG as YAML after replacing the placholde
echo "$SERVER_CONFIG" | yq eval . - > /dev/null 2>&1
if [ $? -ne 0 ]; then
    echo "Error: SERVER_CONFIG does not contain a valid YAML document after replacing the placeholders ." >&2
    exit 4 # Exit code 4 for invalid YAML content after replacing the placeholders
fi
echo "===================================="
echo "Starting geniusyield-server..."
set -x
/DEX/geniusyield-server -- serve
