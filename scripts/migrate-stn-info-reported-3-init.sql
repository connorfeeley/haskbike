BEGIN;
-- \i ~/source/haskbike/scripts/migrate-stn-info-reported-3-init.sql

-- Drop all tables.
DROP TABLE IF EXISTS public.station_status;
DROP TABLE IF EXISTS public.station_information;
DROP TABLE IF EXISTS public.system_information;
DROP TABLE IF EXISTS public.system_information_count;
DROP TABLE IF EXISTS public.queries;
DROP TABLE IF EXISTS public.beam_version;
DROP TABLE IF EXISTS public.beam_migration;
DROP TYPE IF EXISTS public."endpoint_queried";
DROP SCHEMA IF EXISTS partman CASCADE;
DROP EXTENSION IF EXISTS pg_partman CASCADE;


CREATE SCHEMA partman;
CREATE EXTENSION pg_partman WITH SCHEMA partman;

CREATE TABLE public.beam_migration (
    id int4 NOT NULL,
    "commitId" varchar NOT NULL,
    "date" timestamp NOT NULL,
    CONSTRAINT beam_migration_pkey PRIMARY KEY (id)
);

CREATE TABLE public.beam_version (
    "version" int4 NOT NULL,
    CONSTRAINT beam_version_pkey PRIMARY KEY (version)
);

CREATE TYPE public."endpoint_queried" AS ENUM (
    'station_information',
    'station_status',
    'system_information'
);

CREATE TABLE public.queries (
    id serial4 NOT NULL,
    "time" timestamptz NOT NULL,
    endpoint public."endpoint_queried" NOT NULL,
    success bool NOT NULL,
    error_msg text NULL,
    error_json jsonb NULL,
    CONSTRAINT queries_pkey PRIMARY KEY (id)
);

CREATE TABLE public.station_information (
    id serial4 NOT NULL,
    station_id int4 NOT NULL,
    "name" varchar(100) NOT NULL,
    physical_configuration text NULL,
    lat float8 NOT NULL,
    lon float8 NOT NULL,
    altitude float8 NULL,
    address varchar(100) NULL,
    capacity int4 NOT NULL,
    is_charging_station bool NOT NULL,
    rental_methods _text NULL,
    is_valet_station bool NOT NULL,
    is_virtual_station bool NOT NULL,
    "groups" _varchar NULL,
    obcn varchar(100),
    nearby_distance float8 NOT NULL,
    bluetooth_id varchar(100),
    ride_code_support bool NOT NULL,
    rental_uris _varchar NULL,
    active bool NOT NULL,
    CONSTRAINT station_information_id_key UNIQUE (id),
    CONSTRAINT station_information_pkey PRIMARY KEY (station_id)
);

CREATE TABLE public.system_information (
    id serial4 NOT NULL,
    reported timestamptz NOT NULL,
    build_hash text NULL,
    build_label text NULL,
    build_number text NULL,
    build_version text NULL,
    "language" text NULL,
    mobile_head_version int4 NOT NULL,
    mobile_minimum_supported_version int4 NOT NULL,
    "name" text NULL,
    system_id text NULL,
    timezone text NULL,
    CONSTRAINT system_information_id_key UNIQUE (id),
    CONSTRAINT system_information_pkey PRIMARY KEY (id, reported)
);

CREATE TABLE public.system_information_count (
    id serial4 NOT NULL,
    reported timestamptz NOT NULL,
    station_count int4 NOT NULL,
    mechanical_count int4 NOT NULL,
    ebike_count int4 NOT NULL,
    CONSTRAINT system_information_count_id_key UNIQUE (id),
    CONSTRAINT system_information_count_pkey PRIMARY KEY (id, reported)
);

CREATE TABLE public.station_status (
    station_id int4 NOT NULL,
    last_reported timestamptz NOT NULL,
    num_bikes_available int4 NOT NULL,
    num_bikes_disabled int4 NOT NULL,
    num_docks_available int4 NOT NULL,
    num_docks_disabled int4 NOT NULL,
    is_charging_station bool NOT NULL,
    status text NULL,
    is_installed bool NOT NULL,
    is_renting bool NOT NULL,
    is_returning bool NOT NULL,
    traffic varchar(100) NULL,
    vehicle_docks_available int4 NOT NULL,
    vehicle_types_available_boost int4 NULL,
    vehicle_types_available_iconic int4 NULL,
    vehicle_types_available_efit int4 NULL,
    vehicle_types_available_efit_g5 int4 NULL,
    CONSTRAINT station_status_pkey PRIMARY KEY (station_id, last_reported),
    CONSTRAINT station_status_station_id_fkey FOREIGN KEY (station_id) REFERENCES public.station_information (station_id) ON UPDATE CASCADE
)
PARTITION BY RANGE (last_reported);

CREATE INDEX station_status_last_reported_idx ON ONLY public.station_status USING btree (last_reported);
CREATE INDEX IF NOT EXISTS station_status_station_id_last_reported_idx ON ONLY public.station_status USING btree (station_id, last_reported);

SELECT partman.create_parent(
      p_parent_table := 'public.station_status'
    , p_control := 'last_reported'
    , p_interval := 'daily'
    , p_type := 'native'
    , p_premake := 30
    , p_start_partition := '2023-10-01'
);

-- Reset station_information 'id' serial counter.
SELECT pg_catalog.setval(pg_get_serial_sequence('public.station_information', 'id'), (SELECT MAX(id) FROM public.station_information)+1);

-- Populate station_information.
COPY station_information (id, station_id, name, physical_configuration, lat, lon, altitude, address, capacity, is_charging_station, rental_methods, is_valet_station, is_virtual_station, GROUPS, obcn, nearby_distance, bluetooth_id, ride_code_support, rental_uris, active)
FROM
    '/Users/cfeeley/source/haskbike/scripts/database-dumps/station_information-2023-12-19-13-45.csv' WITH (
        FORMAT csv,
        DELIMITER '^',
        HEADER TRUE);

SELECT pg_catalog.setval(pg_get_serial_sequence('public.station_information', 'id'), (SELECT MAX(id) FROM public.station_information)+1);

-- Populate station_status.
COPY station_status (station_id, last_reported, num_bikes_available, num_bikes_disabled, num_docks_available, num_docks_disabled, is_charging_station, status, is_installed, is_renting, is_returning, traffic, vehicle_docks_available, vehicle_types_available_boost, vehicle_types_available_iconic, vehicle_types_available_efit, vehicle_types_available_efit_g5)
FROM
    '/Users/cfeeley/source/haskbike/scripts/database-dumps/station_status-2023-12-19-12-56.csv' WITH (
        FORMAT csv,
        DELIMITER '^',
        HEADER TRUE);

COMMIT;

VACUUM ANALYZE public.station_status;
