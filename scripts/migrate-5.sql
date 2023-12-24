BEGIN;

-- Disable triggers.
SET session_replication_role = 'replica';

-- Get the `MIN(id)` for each `station_id` and `reported` group. This `id` will become the new reference in the `station_status` table.
-- SELECT station_id, reported, MIN(id) as min_id
-- FROM station_information
-- GROUP BY station_id, reported
-- HAVING COUNT(*) > 1;

SELECT count(*)
FROM station_status ss
JOIN (
    SELECT si.station_id, si.reported, MIN(si.id) as min_id
    FROM station_information si
    WHERE (si.station_id, si.reported) IN (
        SELECT station_id, reported
        FROM station_information
        GROUP BY station_id, reported
        HAVING COUNT(*) > 1
    )
    GROUP BY si.station_id, si.reported
) AS dupes ON ss.info_id != dupes.min_id
JOIN station_information si ON si.id = ss.info_id 
AND si.station_id = dupes.station_id 
AND si.reported = dupes.reported;

-- DELETE DUP STATUS
-- Step 1: Find the duplicate station_information records to be deleted
WITH duplicates_to_remove AS (
    SELECT si.id
    FROM station_information si
    WHERE (si.station_id, si.reported) IN (
        SELECT station_id, reported
        FROM station_information
        GROUP BY station_id, reported
        HAVING COUNT(*) > 1
    )
    AND si.id NOT IN (
        -- Exclude the minimum id for each group
        SELECT MIN(id)
        FROM station_information
        GROUP BY station_id, reported
    )
)
-- Step 2: Select the station_status records referencing the duplicates
, status_to_delete AS (
    SELECT ss.info_id
    FROM station_status ss
    INNER JOIN duplicates_to_remove dtr ON ss.info_id = dtr.id
)
-- Step 3: Delete the station_status records
DELETE FROM station_status 
WHERE info_id IN (SELECT info_id FROM status_to_delete);



-- For each duplicate row in `station_information`, update `station_status.info_id` to the `min_id` retrieved in the previous step. This can be done with an `UPDATE` query joined with the above select.
UPDATE station_status
SET info_id = subquery.min_id
FROM (
    SELECT si.station_id, si.reported, MIN(si.id) as min_id
    FROM station_information si
    GROUP BY si.station_id, si.reported
    HAVING COUNT(*) > 1
) AS subquery, station_information
WHERE station_status.info_id = station_information.id
AND station_information.station_id = subquery.station_id
AND station_information.reported = subquery.reported
AND station_information.id NOT IN (subquery.min_id);


-- Once the references in `station_status` are correctly updated, you can delete the duplicates from `station_information` using the initial selection query, with slight modification to reference the duplicates directly.
DELETE FROM station_information
WHERE id IN (
    SELECT si.id
    FROM station_information si
    INNER JOIN (
        SELECT station_id, reported, MIN(id) as min_id
        FROM station_information
        GROUP BY station_id, reported
        HAVING COUNT(*) > 1
    ) AS subquery
    ON si.station_id = subquery.station_id AND si.reported = subquery.reported
    WHERE si.id != subquery.min_id
);
 

-- DELETED DUP INFO REFERENCED BY DUP STATUS

DELETE FROM station_information
WHERE id IN (
    SELECT si.id
    FROM station_information si
    WHERE (station_id, reported) IN (
        SELECT station_id, reported
        FROM station_information
        GROUP BY station_id, reported
        HAVING COUNT(*) > 1
    )
    AND si.id NOT IN (
        SELECT MIN(si2.id)
        FROM station_information si2
        GROUP BY si2.station_id, si2.reported
    )
);

-- Step 1: Add the new UNIQUE constraint to the `station_information` table
ALTER TABLE public.station_information
ADD CONSTRAINT station_information_id_key UNIQUE (id);

-- Step 2: Create temporary column to hold `station_id` during transition
ALTER TABLE public.station_status
ADD COLUMN info_station_id INT;
ALTER TABLE public.station_status
ADD COLUMN info_reported timestamptz;

-- Step 3: Populate the new temporary column with corresponding `station_id` from `station_information`
UPDATE public.station_status
SET info_station_id = s.station_id,
    info_reported = s.reported
FROM public.station_information s
WHERE station_status.info_id = s.id;

-- Step 4: Drop the old foreign key constraint from `station_status`
ALTER TABLE public.station_status
DROP CONSTRAINT station_status_info_id_fkey;

-- Step 5: Rename columns in `station_status` as needed and drop old primary key constraint
ALTER TABLE public.station_status
DROP CONSTRAINT station_status_pkey;

-- Step 6: Add new composite primary key to `station_information`
-- Before we apply the change, ensure that the new primary key combination is unique
-- This query must return 0 rows, or you'll need to manually resolve duplicate keys
SELECT station_id, reported, COUNT(*)
FROM public.station_information
GROUP BY station_id, reported
HAVING COUNT(*) > 1;

-- If the result is 0 rows, we can proceed
ALTER TABLE public.station_information
DROP CONSTRAINT station_information_pkey;
ALTER TABLE public.station_information
ADD CONSTRAINT station_information_pkey PRIMARY KEY (station_id, reported);

-- Step 7: Add new foreign key relationship in `station_status`
ALTER TABLE public.station_status
ADD CONSTRAINT fk_station_information
FOREIGN KEY (info_station_id, info_reported)
REFERENCES public.station_information(station_id, reported)
ON UPDATE CASCADE;

-- Step 8: Drop the old primary key constraint from `station_information`
ALTER TABLE public.station_information
DROP CONSTRAINT IF EXISTS station_information_id_key;

-- Re-enable triggers.
SET session_replication_role = 'origin';

ROLLBACK;
