BEGIN;
-- Note: run in emacs with '\e', then paste into buffer, save, kill buffer
-- \i ~/source/haskbike/scripts/migrate-stn-info-reported-3.sql

-- Disable triggers.
SET session_replication_role = 'replica';

-- Rename tables to match the new naming conventions where necessary.
-- This is not needed in this case as the table names remain unchanged.

-- Alter the data types of existing columns to match the new schema.
ALTER TABLE public.station_information ALTER COLUMN address TYPE text;
ALTER TABLE public.station_information ALTER COLUMN rental_methods TYPE _text;
ALTER TABLE public.station_information ALTER COLUMN rental_methods SET NOT NULL;
ALTER TABLE public.station_information ALTER COLUMN groups TYPE _text;
ALTER TABLE public.station_information ALTER COLUMN groups SET NOT NULL;
ALTER TABLE public.station_information ALTER COLUMN obcn TYPE text;
ALTER TABLE public.station_information ALTER COLUMN bluetooth_id TYPE text;

ALTER TABLE public.station_status ALTER COLUMN traffic TYPE text;

-- Add new columns with default values where applicable.
ALTER TABLE public.station_information ADD COLUMN reported timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL;

-- Generate a new serial sequence for "info_id" since it's required in new station_status structure.
ALTER TABLE public.station_status ADD COLUMN info_id serial;

-- Reset station_information 'id' serial counter.
-- SELECT MAX(id) FROM public.station_information;
-- SELECT nextval('public.system_information_id_seq');
-- BEGIN;
-- -- protect against concurrent inserts while you update the counter
-- LOCK TABLE public.station_information IN EXCLUSIVE MODE;
-- -- Update the sequence
-- SELECT setval('public.system_information_id_seq', COALESCE((SELECT MAX(id)+1 FROM public.station_information), 1), false);
-- COMMIT;

-- Update station_status 'info_id' to point to current station_information record.
UPDATE public.station_status SET info_id = si.id
FROM public.station_information si
WHERE si.station_id = public.station_status.station_id;

-- Now drop the old primary key constraint to add the new composite primary key.
ALTER TABLE public.station_status DROP CONSTRAINT station_status_pkey;

-- Add the foreign key constraint before creating the new primary key to ensure referential integrity.
ALTER TABLE public.station_status ADD CONSTRAINT fk_station_information FOREIGN KEY (info_id) REFERENCES public.station_information (id) ON UPDATE CASCADE;

-- Add the new composite primary key constraint as per new schema.
ALTER TABLE public.station_status ADD PRIMARY KEY (info_id, last_reported);

-- Change station_information primary key to be the new serial column.
ALTER TABLE public.station_status DROP CONSTRAINT station_status_station_id_fkey;
ALTER TABLE public.station_information DROP CONSTRAINT station_information_pkey;
ALTER TABLE public.station_information ADD PRIMARY KEY (id);

SELECT pg_catalog.setval(pg_get_serial_sequence('public.station_information', 'id'), (SELECT MAX(id) FROM public.station_information) + 1);

-- Extend the enum type with new values.
ALTER TYPE public."endpoint_queried" ADD VALUE IF NOT EXISTS 'versions';
ALTER TYPE public."endpoint_queried" ADD VALUE IF NOT EXISTS 'vehicle_types';
ALTER TYPE public."endpoint_queried" ADD VALUE IF NOT EXISTS 'system_regions';
ALTER TYPE public."endpoint_queried" ADD VALUE IF NOT EXISTS 'system_pricing_plans';

-- Create status index for (station_id, last_reported).
CREATE INDEX IF NOT EXISTS station_status_station_id_last_reported_idx ON ONLY public.station_status USING btree (station_id, last_reported);

-- Re-enable triggers.
SET session_replication_role = 'origin';

COMMIT;

-- Regenerate indexes (note: can't VACUUM inside transaction).
VACUUM (ANALYZE) station_status;

VACUUM (ANALYZE) station_information;
