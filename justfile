#!/usr/bin/env -S just --justfile
# ^ A shebang isn't required, but allows a justfile to be executed
#   like a script, with `./justfile test`, for example.

set shell := ["bash", "-c"]
set positional-arguments := true
set export := true

export HOSTNAME := "$(hostname)"

export CABAL_OPTS := "--with-gcc=clang --with-ld=clang --ghc-options='-fllvm -O0'"
export CABAL := "cabal ${CABAL_OPTTS}"

# To benchmark:
# {{CABAL}} v2-run --enable-profiling exes -- --plain visualize -v --log-database +RTS -p

export OLD_STATUS_COLS := "station_id, last_reported, num_bikes_available, num_bikes_disabled, num_docks_available, num_docks_disabled, is_charging_station, status, is_installed, is_renting, is_returning, traffic, vehicle_docks_available, vehicle_types_available_boost, vehicle_types_available_iconic, vehicle_types_available_efit, vehicle_types_available_efit_g5"
export OLD_INFO_COLS := "id, station_id, name, physical_configuration, lat, lon, altitude, address, capacity, is_charging_station, rental_methods, is_valet_station, is_virtual_station, groups, obcn, nearby_distance, bluetooth_id, ride_code_support, rental_uris, active"
export NEW_STATUS_COLS := ""
export NEW_INFO_COLS := "id, station_id, name, physical_configuration, lat, lon, altitude, address, capacity, is_charging_station, rental_methods, is_valet_station, is_virtual_station, groups, obcn, nearby_distance, bluetooth_id, ride_code_support, rental_uris, active, reported"
export ENDPOINT := "http://localhost:8081"

# export ENDPOINT := "https://bikes.cfeeley.org"

status STATION_ID='7001':
    curl --location "https://toronto.publicbikesystem.net/customer/gbfs/v2/en/station_status" | jq '.data.stations[] | select(.station_id=="{{ STATION_ID }}")'

info STATION_ID='7001':
    curl --location "https://toronto.publicbikesystem.net/customer/gbfs/v2/en/station_information" | jq '.data.stations[] | select(.station_id=="{{ STATION_ID }}")'

integrals STATION_ID='7001':
    curl --location "{{ ENDPOINT }}/data/station-status/integral?station-id={{ STATION_ID }}" | jq .

errors-latest AMOUNT='1':
    curl --location "{{ ENDPOINT }}/debug/errors/latest/{{ AMOUNT }}" | jq .

version:
    curl --location "{{ ENDPOINT }}/version" | jq .

factors STATION_ID='7001':
    curl --location "{{ ENDPOINT }}/data/station-status/factor?station-id={{ STATION_ID }}" | jq .

performance-csv:
    wget --content-disposition "{{ ENDPOINT }}/data/system-status/performance/csv"

performance-csv-nov:
    wget --content-disposition "{{ ENDPOINT }}/data/system-status/performance/csv?start-time=2023-11-01T00%3A00&end-time=2023-12-01T00%3A00"

performance-csv-station STATION_ID='7001':
    wget --content-disposition "{{ ENDPOINT }}/data/system-status/performance/csv?station-id={{ STATION_ID }}"

sums:
    curl "https://tor.publicbikesystem.net/customer/gbfs/v2/en/station_status" | \
        jq --raw-output '.data.stations[] | [.vehicle_types_available[].count] | @tsv' | \
        awk '{j[1]="Boost"; j[2]="Iconic"; j[3]="E-Fit"; j[4]="E-Fit G5"; for (i=1; i<=NF; i++) sum[i]+=$i} END {for (i in sum) {print j[i] ": " sum[i]}}'

# Sum the number of available ebikes (v1 API)
sum-ebike-v1:
    curl --location "https://toronto.publicbikesystem.net/customer/ube/gbfs/v1/en/station_status" | jq '[.data.stations[].num_bikes_available_types.ebike] | add'

test PACKAGE='all':
    set -euo pipefail
    {{ CABAL }} test {{ PACKAGE }} --test-show-details=direct

run *ARGS:
    {{ CABAL }} run haskbike-cli -- --plain -v {{ ARGS }}

test-one PACKAGE PATTERN:
    {{ CABAL }} test {{ PACKAGE }} --test-show-details=direct --test-options='--pattern=/{{ PATTERN }}/'

# Benchmark and compare against a baseline.
bench PACKAGE='all':
    {{ CABAL }} bench {{ PACKAGE }} --benchmark-options="--baseline=bench-baseline-{{HOSTNAME}}.csv --fail-if-slower=10 --fail-if-faster=10"

# Save benchmark results as a benchline.
bench-baseline PACKAGE='all':
    {{ CABAL }} bench {{ PACKAGE }} --benchmark-options="--csv=bench-baseline-{{HOSTNAME}}.csv --fail-if-slower=10 --fail-if-faster=10"

bench-one PACKAGE PATTERN:
    {{ CABAL }} bench {{ PACKAGE }} --benchmark-options="--baseline=bench-baseline-{{HOSTNAME}}.csv --fail-if-slower=10 --fail-if-faster=10 --pattern=/{{ PATTERN }}/"

# pg_stat_statements helpers for HTTP/SQL benchmark correlation.
#
# Prerequisite (one-time, requires Postgres restart):
#   echo "shared_preload_libraries = 'pg_stat_statements'" >> $PGDATA/postgresql.conf
#   pg_ctl restart
#
# Then `just pg-stats-setup` creates the extension on the haskbike DB.
# Workflow during a benchmark run:
#   just pg-stats-setup        # one-time
#   just pg-stats-reset        # zero counters before the run
#   just bench-server          # (Task #7) drive load via k6
#   just pg-stats-snapshot bench-results/<ts>/pg_stat_statements.csv

pg-stats-setup:
    #!/usr/bin/env bash
    set -euo pipefail
    source ./.env.local
    PSQL=( psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" -d haskbike )
    export PGPASSWORD="$HASKBIKE_PASSWORD"

    # Persist the shared_preload_libraries setting (no-op if it already
    # contains pg_stat_statements). Requires SUPERUSER.
    "${PSQL[@]}" -v ON_ERROR_STOP=1 -c "ALTER SYSTEM SET shared_preload_libraries = 'pg_stat_statements'"

    # Check whether the library is actually loaded yet. If not, the operator
    # has to restart Postgres before CREATE EXTENSION will succeed.
    loaded=$("${PSQL[@]}" -tAX -c "SHOW shared_preload_libraries" | tr -d '[:space:]')
    case ",${loaded}," in
      *,pg_stat_statements,*)
        "${PSQL[@]}" -v ON_ERROR_STOP=1 -c "CREATE EXTENSION IF NOT EXISTS pg_stat_statements"
        echo "pg_stat_statements is loaded and the extension is created."
        ;;
      *)
        {
          echo "shared_preload_libraries does not yet include pg_stat_statements."
          echo "ALTER SYSTEM has staged the change; restart Postgres so the library"
          echo "loads, then re-run 'just pg-stats-setup'. Examples:"
          echo ""
          echo "  Homebrew:    brew services restart postgresql@<version>"
          echo "  pg_ctl:      pg_ctl -D \"\$PGDATA\" restart"
          echo "  systemd:     sudo systemctl restart postgresql"
          echo "  macOS.app:   restart from the Postgres.app menu"
          echo ""
          echo "Current value: ${loaded:-<empty>}"
        } >&2
        exit 1
        ;;
    esac

pg-stats-reset:
    #!/usr/bin/env bash
    set -euo pipefail
    source ./.env.local
    PGPASSWORD=$HASKBIKE_PASSWORD psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" \
        -d haskbike \
        -c "SELECT pg_stat_statements_reset()"

# Dump pg_stat_statements ordered by total_exec_time DESC. Pass an absolute or
# project-relative output path; parent directory is created if missing.
pg-stats-snapshot OUTFILE:
    #!/usr/bin/env bash
    set -euo pipefail
    source ./.env.local
    mkdir -p "$(dirname "{{ OUTFILE }}")"
    PGPASSWORD=$HASKBIKE_PASSWORD psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" \
        -d haskbike \
        -c "SELECT queryid, calls, total_exec_time, mean_exec_time, rows, shared_blks_hit, shared_blks_read, shared_blks_dirtied, temp_blks_read, temp_blks_written, query FROM pg_stat_statements ORDER BY total_exec_time DESC LIMIT 200" \
        --csv > "{{ OUTFILE }}"

# Capture SQL emitted by handlers for use with `EXPLAIN ANALYZE` in psql.
# Run the server with --log-database (already wired through serve) and grep
# the stderr stream for `[sql]` lines; copy a query into psql wrapped in
# `EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON) <query>` to inspect a plan.

# Run a single k6 scenario. SCENARIO is the JS file path under bench/k6/
# (relative or absolute). BASE_URL defaults to $ENDPOINT (defined above).
# Pass extra k6 args via ARGS, e.g. RATE=10 DURATION=10m via env.
#
# Examples:
#   just k6 bench/k6/scenarios/smoke.js
#   RATE=10 DURATION=10m just k6 bench/k6/scenarios/steady.js
#   ENDPOINT=data-station-status-integral SPAN_DAYS=7 just k6 bench/k6/scenarios/per-endpoint/single.js
k6 SCENARIO *ARGS:
    BASE_URL="${BASE_URL:-{{ ENDPOINT }}}" k6 run {{ ARGS }} {{ SCENARIO }}

# Run the full benchmark sweep against an already-running server.
# Server should be started in a separate terminal: `just serve --log-database`.
# Outputs land in bench-results/<timestamp>/.
#
# Snapshots pg_stat_statements before and after each scenario for diffing.
bench-server:
    #!/usr/bin/env bash
    set -uo pipefail
    ts="$(date +%Y%m%dT%H%M%S)"
    out="bench-results/${ts}"
    mkdir -p "${out}"
    BASE_URL="${BASE_URL:-{{ ENDPOINT }}}"
    export BASE_URL
    echo "Output: ${out}"
    echo "Target: ${BASE_URL}"
    just pg-stats-reset
    just pg-stats-snapshot "${out}/pg_stat_statements.before.csv"

    # Run each scenario; capture exit but don't abort the pipeline. k6 exits
    # 99 on threshold violation, which is informative (not a hard failure).
    declare -A rc
    for sc in smoke steady burst; do
        echo "--- ${sc} ---"
        k6 run --out "json=${out}/${sc}.ndjson" --summary-export="${out}/${sc}.json" "bench/k6/scenarios/${sc}.js"
        rc[$sc]=$?
        just pg-stats-snapshot "${out}/pg_stat_statements.after_${sc}.csv"
    done

    echo
    echo "Scenario exit codes:"
    for sc in smoke steady burst; do
        printf "  %-6s rc=%s%s\n" "${sc}" "${rc[$sc]}" "$([[ ${rc[$sc]} -eq 99 ]] && echo ' (threshold violation)' || true)"
    done
    echo "Done. Results in ${out}"
    echo "Run: just bench-correlate ${out}"

    # Exit non-zero if any scenario hard-failed (rc != 0 and != 99).
    overall=0
    for sc in smoke steady burst; do
        if [[ ${rc[$sc]} -ne 0 && ${rc[$sc]} -ne 99 ]]; then overall=1; fi
    done
    exit "${overall}"

# Correlate a bench-results/<timestamp> directory: pair k6 latency tags with
# pg_stat_statements deltas to produce a single per-endpoint summary table.
bench-correlate RUN_DIR:
    python3 scripts/correlate-bench.py {{ RUN_DIR }}

poll *ARGS:
    {{ CABAL }} run haskbike-cli -- --plain poll {{ ARGS }} -v

reset *ARGS:
    #!/usr/bin/env bash
    source ./.env.local
    {{ CABAL }} run haskbike-cli -- --plain reset --reset-only -v --log-database {{ ARGS }}

migrate *ARGS:
    #!/usr/bin/env bash
    source ./.env.local
    {{ CABAL }} run haskbike-cli -- --plain -v --log-database database migrate

# Pre-fill the station_occupancy cache for canonical rolling windows
# anchored at the current UTC time, floored to the cache bucket boundary.
# Run periodically (cron / systemd timer) so the request path always hits
# the cache.
#
# Defaults to spans 1d / 7d / 30d. Override:
#   just warm-occupancy --spans 1,7,30,90
warm-occupancy *ARGS:
    #!/usr/bin/env bash
    set -eo pipefail
    source ./.env.local
    {{ CABAL }} run haskbike-cli -- --plain -v database warm-occupancy {{ ARGS }}

serve *ARGS:
    {{ CABAL }} v2-run --enable-profiling --profiling-detail all-functions haskbike-cli -- --plain serve {{ ARGS }} -v

export-rds-table TABLE:
    #!/usr/bin/env bash
    PGPASSWORD=$HASKBIKE_PASSWORD psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" \
        -d haskbike \
        -c "SELECT * FROM public.{{ TABLE }}" \
        --csv -P csv_fieldsep="^" > "./scripts/database-dumps/rds/{{ TABLE }}-$(date '+%Y-%m-%d-%H-%M').csv"

export-local-table TABLE:
    PGPASSWORD=$HASKBIKE_PASSWORD psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" \
        -d haskbike \
        -c "SELECT * FROM public.{{ TABLE }}" \
        --csv -P csv_fieldsep="^" > "./scripts/database-dumps/{{ TABLE }}-$(date '+%Y-%m-%d-%H-%M').csv"

watch:
    fd . --extension=nix --extension=hs | entr -a just build-clang

cabal *ARGS:
    {{ CABAL }} {{ ARGS }}

build-clang:
    {{ CABAL }} build

build *ARGS:
    {{ CABAL }} build {{ ARGS }}

clean *ARGS:
    {{ CABAL }} clean {{ ARGS }}

redate:
    git-privacy redate origin/master

module-timings:
    {{ CABAL }} clean
    {{ CABAL }} build --ghc-options "-O0 -ddump-to-file -ddump-timings"
    nix run "nixpkgs#time-ghc-modules"

import-local-table TABLE CSVFILE:
    #!/usr/bin/env bash
    source ./.env.local
    PGPASSWORD=$HASKBIKE_PASSWORD psql -h "$HASKBIKE_PGDBHOST" -p "$HASKBIKE_PGDBPORT" -U "$HASKBIKE_USERNAME" \
        -d haskbike \
        -c "COPY {{ TABLE }} FROM '{{ CSVFILE }}' WITH (FORMAT csv, DELIMITER '^', HEADER true)"

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
        -c "COPY station_information ({{ NEW_INFO_COLS }}) FROM '{{ CSVFILE }}' WITH (FORMAT csv, DELIMITER '^', HEADER true)"

profile:
    {{ CABAL }} v2-run --enable-profiling exes -- poll -v +RTS -p

sha384-integrity FILE:
    cat {{ FILE }} | openssl dgst -sha384 -binary | openssl base64 -A
