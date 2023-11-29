#!/usr/bin/env -S just --justfile
# ^ A shebang isn't required, but allows a justfile to be executed
#   like a script, with `./justfile test`, for example.

export ENDPOINT := "http://localhost:8081"
# export ENDPOINT := "https://bikes.cfeeley.org"

status STATION_ID='7001':
    curl --location "https://toronto.publicbikesystem.net/customer/gbfs/v2/en/station_status" | jq '.data.stations[] | select(.station_id=="{{STATION_ID}}")'

info STATION_ID='7001':
    curl --location "https://toronto.publicbikesystem.net/customer/gbfs/v2/en/station_information" | jq '.data.stations[] | select(.station_id=="{{STATION_ID}}")'

integrals STATION_ID='7001':
    curl --location "{{ENDPOINT}}/data/station-status/integral?station-id={{STATION_ID}}" | jq .

version:
    curl --location "{{ENDPOINT}}/version" | jq .

factors STATION_ID='7001':
    curl --location "{{ENDPOINT}}/data/station-status/factor?station-id={{STATION_ID}}" | jq .

performance-csv:
    wget --content-disposition "{{ENDPOINT}}/data/system-status/performance/csv"

performance-csv-nov:
    wget --content-disposition "{{ENDPOINT}}/data/system-status/performance/csv?start-time=2023-11-01T00%3A00&end-time=2023-12-01T00%3A00"

performance-csv-station STATION_ID='7001':
    wget --content-disposition "{{ENDPOINT}}/data/system-status/performance/csv?station-id={{STATION_ID}}"

sums:
    curl "https://tor.publicbikesystem.net/customer/gbfs/v2/en/station_status" | \
        jq --raw-output '.data.stations[] | [.vehicle_types_available[].count] | @tsv' | \
        awk '{j[1]="Boost"; j[2]="Iconic"; j[3]="E-Fit"; j[4]="E-Fit G5"; for (i=1; i<=NF; i++) sum[i]+=$i} END {for (i in sum) {print j[i] ": " sum[i]}}'

test:
    cabal test --test-show-details=direct

test-one PATTERN:
    cabal test --test-show-details=direct --test-options='--pattern /{{PATTERN}}/'
