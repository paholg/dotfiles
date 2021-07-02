#!/usr/bin/env bash

# This script gets the external IP of your systems then connects to the Gandi
# LiveDNS API and updates your dns record with the IP.

API_KEY=`cat API_KEY`

SUBDOMAIN="home"
DOMAIN="paholg.com"

# Get external IP address
EXT_IP=$(curl -s ifconfig.me)

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
