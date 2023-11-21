#!/usr/bin/env -S just --justfile
# ^ A shebang isn't required, but allows a justfile to be executed
#   like a script, with `./justfile test`, for example.

# export ENDPOINT := "{{ENDPOINT}}"
export ENDPOINT := "https://bikes.cfeeley.org"

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

performance-csv-station STATION_ID='7001':
    wget --content-disposition "{{ENDPOINT}}/data/system-status/performance/csv?station-id={{STATION_ID}}"
