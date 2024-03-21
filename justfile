#!/usr/bin/env -S just --justfile
# ^ A shebang isn't required, but allows a justfile to be executed
#   like a script, with `./justfile test`, for example.

set shell := ["bash", "-c"]

set positional-arguments := true

export CABAL := "cabal --with-gcc=clang --with-ld=clang -O0"
# To benchmark:
# {{CABAL}} v2-run --enable-profiling exes -- --plain visualize -v --log-database +RTS -p

export OLD_STATUS_COLS := "station_id, last_reported, num_bikes_available, num_bikes_disabled, num_docks_available, num_docks_disabled, is_charging_station, status, is_installed, is_renting, is_returning, traffic, vehicle_docks_available, vehicle_types_available_boost, vehicle_types_available_iconic, vehicle_types_available_efit, vehicle_types_available_efit_g5"
export OLD_INFO_COLS := "id, station_id, name, physical_configuration, lat, lon, altitude, address, capacity, is_charging_station, rental_methods, is_valet_station, is_virtual_station, groups, obcn, nearby_distance, bluetooth_id, ride_code_support, rental_uris, active"

export NEW_STATUS_COLS := ""
export NEW_INFO_COLS := "id, station_id, name, physical_configuration, lat, lon, altitude, address, capacity, is_charging_station, rental_methods, is_valet_station, is_virtual_station, groups, obcn, nearby_distance, bluetooth_id, ride_code_support, rental_uris, active, reported"

export ENDPOINT := "http://localhost:8081"
# export ENDPOINT := "https://bikes.cfeeley.org"

status STATION_ID='7001':
    curl --location "https://toronto.publicbikesystem.net/customer/gbfs/v2/en/station_status" | jq '.data.stations[] | select(.station_id=="{{STATION_ID}}")'

info STATION_ID='7001':
    curl --location "https://toronto.publicbikesystem.net/customer/gbfs/v2/en/station_information" | jq '.data.stations[] | select(.station_id=="{{STATION_ID}}")'

integrals STATION_ID='7001':
    curl --location "{{ENDPOINT}}/data/station-status/integral?station-id={{STATION_ID}}" | jq .

errors-latest AMOUNT='1':
    curl --location "{{ENDPOINT}}/debug/errors/latest/{{AMOUNT}}" | jq .

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

# Sum the number of available ebikes (v1 API)
sum-ebike-v1:
    curl --location "https://toronto.publicbikesystem.net/customer/ube/gbfs/v1/en/station_status" | jq '[.data.stations[].num_bikes_available_types.ebike] | add'

test PACKAGE='all':
    {{CABAL}} test {{PACKAGE}} --test-show-details=direct

test-one PACKAGE PATTERN:
    {{CABAL}} test {{PACKAGE}} --test-show-details=direct --test-options='--pattern /{{PATTERN}}/'

bench:
    {{CABAL}} bench

poll *ARGS:
    {{CABAL}} run haskbike -- --plain poll {{ARGS}} -v

reset *ARGS:
    #!/usr/bin/env bash
    source ./.env.local
    {{CABAL}} run haskbike -- --plain reset --reset-only -v --log-database {{ARGS}}

migrate *ARGS:
    #!/usr/bin/env bash
    source ./.env.local
    {{CABAL}} run haskbike -- --plain debug --enable-migrations -v --log-database

visualize *ARGS:
    {{CABAL}} v2-run --enable-profiling --profiling-detail all-functions exes -- --plain visualize -v {{ARGS}}

export-rds-table TABLE:
    #!/usr/bin/env bash
    PGPASSWORD=$HASKBIKE_PASSWORD psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" \
        -d haskbike \
        -c "SELECT * FROM public.{{TABLE}}" \
        --csv -P csv_fieldsep="^" > "./scripts/database-dumps/rds/{{TABLE}}-$(date '+%Y-%m-%d-%H-%M').csv"

export-local-table TABLE:
    PGPASSWORD=$HASKBIKE_PASSWORD psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" \
        -d haskbike \
        -c "SELECT * FROM public.{{TABLE}}" \
        --csv -P csv_fieldsep="^" > "./scripts/database-dumps/{{TABLE}}-$(date '+%Y-%m-%d-%H-%M').csv"

watch:
    fd . --extension=nix --extension=hs | entr -a just build-clang

build-clang:
    {{CABAL}} --with-gcc=clang --with-ld=clang --ghc-options=-fllvm build

build *ARGS:
    {{CABAL}} build {{ARGS}}

redate:
    git-privacy redate origin/master

module-timings:
    cabal clean
    cabal build --ghc-options "-O0 -ddump-to-file -ddump-timings"
    nix run "nixpkgs#time-ghc-modules"


import-local-table TABLE CSVFILE:
    #!/usr/bin/env bash
    source ./.env.local
    PGPASSWORD=$HASKBIKE_PASSWORD psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" \
        -d haskbike \
        -c "COPY {{TABLE}} FROM '{{CSVFILE}}' WITH (FORMAT csv, DELIMITER '^', HEADER true)"

# import-local-station-info CSVFILE:
#     #!/usr/bin/env bash
#     source ./.env.local
#     PGPASSWORD=$HASKBIKE_PASSWORD psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" \
#         -d haskbike \
#         -c "COPY station_information (id, station_id, name, physical_configuration, lat, lon, altitude, address, capacity, is_charging_station, rental_methods, is_valet_station, is_virtual_station, groups, obcn, nearby_distance, bluetooth_id, ride_code_support, rental_uris, active) FROM '{{CSVFILE}}' WITH (FORMAT csv, DELIMITER '^', HEADER true)"

import-local-station-info CSVFILE:
    #!/usr/bin/env bash
    source ./.env.local
    PGPASSWORD=$HASKBIKE_PASSWORD psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" \
        -d haskbike \
        -c "COPY station_information ({{NEW_INFO_COLS}}) FROM '{{CSVFILE}}' WITH (FORMAT csv, DELIMITER '^', HEADER true)"

profile:
    {{CABAL}} v2-run --enable-profiling exes -- poll -v +RTS -p
