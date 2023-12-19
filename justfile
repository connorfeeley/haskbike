#!/usr/bin/env -S just --justfile
# ^ A shebang isn't required, but allows a justfile to be executed
#   like a script, with `./justfile test`, for example.

set shell := ["bash", "-c"]

export CABAL := "cabal --with-gcc=clang --with-ld=clang --ghc-options=-fllvm"

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
    {{CABAL}} test --test-show-details=direct

test-one PATTERN:
    {{CABAL}} test --test-show-details=direct --test-options='--pattern /{{PATTERN}}/'

poll:
    {{CABAL}} run haskbike -- --plain poll -v # --log-database

visualize:
    {{CABAL}} run haskbike -- --plain visualize -v # --log-database

export-rds-table TABLE:
    #!/usr/bin/env bash
    source ./.env.awsrds.ADMIN
    PGPASSWORD=$HASKBIKE_PASSWORD psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" \
        -d haskbike \
        -c "SELECT * FROM public.{{TABLE}}" \
        --csv -P csv_fieldsep="^" > "./scripts/database-dumps/{{TABLE}}-$(date '+%Y-%m-%d-%H-%M').csv"

export-local-table TABLE:
    PGPASSWORD=$HASKBIKE_PASSWORD psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" \
        -d haskbike \
        -c "SELECT * FROM public.{{TABLE}}" \
        --csv -P csv_fieldsep="^" > "./scripts/database-dumps/{{TABLE}}-$(date '+%Y-%m-%d-%H-%M').csv"

watch:
    fd . --extension=nix --extension=hs | entr -a just build-clang

build-clang:
    {{CABAL}} --with-gcc=clang --with-ld=clang --ghc-options=-fllvm build

build:
    {{CABAL}} build

redate:
    git-privacy redate origin/master

module-timings:
    cabal clean
    cabal build --ghc-options "-O0 -ddump-to-file -ddump-timings"
    nix run "nixpkgs#time-ghc-modules"


# Not working.
# import-local-table TABLE CSVFILE:
#     source ./.env.local

#     PGPASSWORD=$HASKBIKE_PASSWORD psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" \
#         -d haskbike \
#         -c "COPY {{TABLE}} FROM '{{CSVFILE}}' WITH (FORMAT csv, DELIMITER '^', HEADER false)"
