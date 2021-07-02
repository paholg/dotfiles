#!/usr/bin/env bash

set -euo pipefail

# This script gets the external IP of your systems then connects to the Gandi
# LiveDNS API and updates your dns record with the IP.

API_KEY=$(cat API_KEY)

SUBDOMAIN="home"
DOMAIN="paholg.com"

# Get external IP address
EXT_IP=$(curl -s ifconfig.me)
CUR_IP=$(dig +short home.paholg.com)


if [ "$EXT_IP" = "$CUR_IP" ]; then
    echo "IP $CUR_IP current, exiting."
    exit 0
fi

echo "home.paholg.com is $CUR_IP, should be $EXT_IP. Updating."

#Get the current Zone for the provided domain
CURRENT_ZONE_HREF=$(curl -s -H "X-Api-Key: $API_KEY" https://dns.api.gandi.net/api/v5/domains/$DOMAIN | jq -r '.zone_records_href')

# Update the A Record of the subdomain using PUT
curl -D- -X PUT -H "Content-Type: application/json" \
        -H "X-Api-Key: $API_KEY" \
        -d "{\"rrset_name\": \"$SUBDOMAIN\",
             \"rrset_type\": \"A\",
             \"rrset_ttl\": 1200,
             \"rrset_values\": [\"$EXT_IP\"]}" \
        $CURRENT_ZONE_HREF/$SUBDOMAIN/A
