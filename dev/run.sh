#!/bin/bash

apps () {
cat << EOF
allocation-service
app-backend
atlas-gateway
atlas-transport
driver-tracking-healthcheck
mock-fcm
mock-registry
mock-sms
parking-bap
mock-parking-bpp
public-transport-bap
public-transport-search-consumer
search-result-aggregator
transporter-scheduler
scheduler-example-app
scheduler-example-scheduler
driver-offer-bpp
EOF
}

map () { while read -r line; do $1 "$line"; done; }

to_log_filename () { echo "$1.log"; }

to_run_cmd () { echo "stack exec $1-exe 2>&1 | tee $(to_log_filename $1)"; }

rm -f $(apps | map to_log_filename)
apps | map to_run_cmd | parallel -j $(apps | wc -l)
