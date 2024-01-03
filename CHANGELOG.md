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
