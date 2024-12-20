# Revision history for haskbike

## 0.0.0.1 -- 2023-03-18

* First version. Released on an unsuspecting world.

## 1.1.0 -- 2023-11-21

* Added support for export of performance data as CSV.

## 1.2.0 -- 2023-11-29

* Added database support for system information.

## 1.3.0 -- 2023-12-12

* Log query results (success, errors) to table.
* Replaced the messy polling implementation that I hated.

## 1.3.1 -- 2023-12-12

* Fix tests broken by prior refactoring.

## 1.3.2 -- 2023-12-13

* Show latest time the bike share Toronto API was queried in the sidebar.

## 2.0.0 -- 2023-12-20

* Rework station information database schema:
  * Insert new station information records (when changed).
  * Use station information serial ID column as station status foreign key.

## 2.1.1 -- 2023-12-20

* Track nixpkgs-unstable.

## 2.1.2 -- 2023-12-20

* Track nixos-unstable; fix build failures with latest nixpkgs.

## 3.0.0 -- 2023-12-28

* Add station status lookup table.

## 3.1.0 -- 2024-01-03

* Optimize various database operations.
* Add charging infrastructure statistics to system status page.

## 3.1.1 -- 2024-01-03

* Optimize query used for station list rendering.

## 3.2.0 -- 2024-01-05

* Add query to query how much time a station spends empty.
* Show more station information on station status visualization page.

## 3.3.0 -- 2024-03-12

* Major optimizations for station empty/full query.
* Add station empty/full list page.
* Fix number of database connections in pool: only open same number of connections as number of capabilities (threads).
* Open station status page using same time range as the empty/full station list.
* Add `station_status_changes` table to store only station status records where the status is different from the preceding record (per station).
* Use GridJS (searchable, sortable tables) for rendering station empty/full list.
* Use ADT pattern for generating form inputs.
* Fixed query for single-station empty/full calculation.
* Added empty/full values to performance CSV.

## 3.3.1 -- 2024-03-12

* Add CLI option for populating `station_status_changes` table from the existing `station_status` table.
* Refactored `queryRowCount` to return an easier type to use.

## 3.4.0 -- 2024-03-22

* Fixes for ghc 9.6 compatibility.
* Generic constraints are now utilized in environment monads.
* API:
  * Refactor of API implementation including endpoint implementations for vehicle_types and versions.
  * Addition of system_regions and system_pricing_plans to the API.
  * Addition of 'Fit' to TorontoVehicleType definitions.
* CLI:
  * Add logInfo and logDebug logging functionality.
  * Run tests in parallel.
  * Add PollClientEnv.
  * Add polling test.
* Client: Run tests in parallel.
* Database:
  * Introduce library for db test utilities.
  * Add TestDatabaseRoundtrip functionality.
  * Database tests have been improved with better exception logging.
* Server:
  * Show cabal version in side bar.
* Nix:
  * Add configuration for treefmt.
  * Implement multi-package build fixes.
  * Update tracking to nixpkgs-unstable branch.
* Test:
  * Assertion added for successful response decoding.
  * Adjustments to tests when run under nix platform.
* CI:
  * Add licensing check

## 3.4.1 -- 2024-03-27

* Fix station empty/full calculation.
* Add support for importing and exporting compressed database dumps.
* Added `FromJSON` and `ToJSON` instances for `StationInformation` (database version).
* Fix `StationInformation` (API client version) `vehicle_types_available` decoding.
* Move `reset` CLI command to new `database` subcommand.
* Move CLI options to submodules.
* Updated benchmark baselines using M1 MBP.

## 3.5.0 -- 2024-03-29

* Database
  * Added `Oddities` module for various queries which analyze odd patterns in the station data.
  * Fixed station occupancy calculation.
  * Use `Data.Text` more pervasively, instead of `String`.
* CLI
  * Fixed abort caused by `undefined` in database dispatch.
* Server
  * Load assets asynchronously using HTML's `defer`
  * Added `<meta>` `description`.
  * Fixed rendering on mobile.
  * Use GridJS for the station list table.
  * Consolidate station list and station occupancy tables.
  * Use DD:HH:MM:SS format for station occupancy time formats.
  * Make selection form label optional.

## 3.6.1 -- 2024-04-03

* Database
  * Add `station_occupancy `table to store cached occupancy query results.
  * Rename ops to StationOccupancy
  * Move EmptyFull and EmptyFullRecord to StationOccupancy
  * Print migrations compactly
  * Add foreign key constraint
  * Rename station occupancy expr
  * Add function to cache occupancy
  * Fix table names
  * Move currentTimestampUtc_ to custom expr module
  * Add fn to insert cached occupancy
  * Add occupancy insertion test
  * Unused imports
  * Run migrations in transaction
  * Don't export schema versions, only final tables
  * Insert from SELECT w/ on conflict update
  * Remove calculated from PK
  * Make empty and full columns nullable
  * Defer runInsertReturningList to callsite
  * Use cached occupancy query
  * Remove unused
  * Add operation for pg_sleep
  * Add operation to lock table and sleep
  * Always use cached occupancy table
  * Update benchmark baseline

* Client
  * Fix incorrect JSON schema when re-serializing API responses.

* CLI
  * Add `database migrate` command
  * Remove unused option
  * Add mock-server command
  * Manual test tree instead of autodiscover
  * Query mock server
  * Add test for poll queue insertion
  * Add log before attempting db insert
  * Move thread creation to function
  * Use bounded queue
  * Rename function and cleanup
  * Consolidate logging for response wrapper
  * Simplify and cleanup

* Client
  * Use BaseUrl from environment
  * Simplify vehicle types
  * Convert to NamedRoutes
  * Add mock pricing plans
  * Add mock PBSC API server
  * Add mock versions data
  * Add mock vehicle type data
  * Add mock station info
  * Add mock station status
  * Add putDataField to HasDataField class
  * Use putDataField in ResponseWrapper ToJSON
  * Use test tree instead of tasty-discover

* Server
  * Refactor server to launch using natural transformation function

* Nix
  * Use fork of beam packages with support for `insertOnlyOnConflict`
  * Fix module graph generation script

## 3.6.2 -- 2024-04-04

* Server
  * Cache station occupancy when querying `DataAPI` (`/data/`)
  * Propagate query parameters from station occupancy list to `DataAPI` query

## 3.7.0 -- 2024-06-21

* Server
  * Add hidden debug page.

## 3.7.1 -- 2024-06-25

* Server
  * Fix charging infrastructure component link on System Status page.

## 3.7.2 -- 2024-06-25

* Server
  * Tweak index page style to align content in the center with a maximum width.
