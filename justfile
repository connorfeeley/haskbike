#!/usr/bin/env -S just --justfile
# ^ A shebang isn't required, but allows a justfile to be executed
#   like a script, with `./justfile test`, for example.

status STATION_ID='7001':
    curl --location "https://toronto.publicbikesystem.net/customer/gbfs/v2/en/station_status" | jq '.data.stations[] | select(.station_id=="{{STATION_ID}}")'

info STATION_ID='7001':
    curl --location "https://toronto.publicbikesystem.net/customer/gbfs/v2/en/station_information" | jq '.data.stations[] | select(.station_id=="{{STATION_ID}}")'

integrals STATION_ID='7001':
    curl --location "http://localhost:8081/data/station-status/integral?station-id={{STATION_ID}}" | jq .

factors STATION_ID='7001':
    curl --location "http://localhost:8081/data/station-status/factor?station-id={{STATION_ID}}" | jq .

performance-csv:
    wget --content-disposition "http://localhost:8081/data/system-status/performance/csv"

performance-csv-station STATION_ID='7001':
    wget --content-disposition "http://localhost:8081/data/system-status/performance/csv?station-id={{STATION_ID}}"
