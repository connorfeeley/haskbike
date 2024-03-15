--
-- Station status has a bunch of rows where 'info_station_id' and 'info_reported' don't correspond to a valid station_information row.
-- station_status was missing a foreign key constraint on station_information.
-- We need to fix this by updating the 'info_reported' column in station_status to the closest (but not newer) reported time for each station.
-- We can do this by joining station_status with station_information and using the 'reported' column from station_information
-- to update 'info_reported' in station_status.
--

-- Create a function to handle duplicate primary key errors on station_status
CREATE OR REPLACE FUNCTION handle_dup_pkey()
RETURNS TRIGGER AS $$
BEGIN
  -- Check if update results in a duplicate key
  IF EXISTS (
    SELECT 1
    FROM station_status
    WHERE info_station_id = NEW.info_station_id
    AND last_reported = NEW.last_reported
    AND info_reported = NEW.info_reported
  ) THEN
    -- If so, set info_reported to 2000-01-01 00:00:00+00
    NEW.info_reported = '2000-01-01 00:00:00+00';
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create a trigger to handle duplicate primary key errors on station_status
CREATE OR REPLACE TRIGGER check_station_status_update
BEFORE UPDATE ON station_status
FOR EACH ROW
EXECUTE FUNCTION handle_dup_pkey();


-- Lock only required rows
CREATE OR REPLACE FUNCTION public.fnc_work(_start_time timestamptz, _end_time timestamptz, _reported_upper_limit timestamptz = NULL)
  RETURNS text
  LANGUAGE plpgsql AS
$func$
BEGIN
   -- optionally assert that the steering row exists
   PERFORM FROM station_status
   WHERE info_reported = '2023-12-20 05:20:38.925686+00'
   AND last_reported BETWEEN _start_time AND _end_time
   FOR KEY SHARE SKIP LOCKED;
   IF NOT FOUND THEN
      RAISE EXCEPTION 'station_status = % not found or blocked!', (_start_time, _end_time);
   END IF;

   -- lock rows
   PERFORM FROM station_status
   WHERE info_reported = '2023-12-20 05:20:38.925686+00'
   AND last_reported BETWEEN _start_time AND _end_time
   FOR NO KEY UPDATE SKIP LOCKED;

   IF NOT FOUND THEN
      -- we made sure the row exists, so it must be locked
      RETURN 'running';
   END IF;

   ----- code for big work HERE -----
    -- Update info_reported to the latest reported time for each station
    WITH latest_reported AS (
      SELECT
        info_station_id,
        MAX(last_reported) AS last_reported
      FROM
        station_status
      WHERE
        info_reported = '2023-12-20 05:20:38.925686+00'
      AND
        last_reported BETWEEN _start_time AND _end_time
      GROUP BY
        info_station_id
    ),

    latest_info AS (
      SELECT
        si.station_id,
        MAX(reported) AS reported
      FROM
        station_information si
      INNER JOIN
        latest_reported lr
        ON si.station_id = lr.info_station_id
      WHERE
        CASE WHEN _reported_upper_limit IS NULL THEN (si.reported <= lr.last_reported) ELSE (si.reported <= _reported_upper_limit) END
      GROUP BY
        si.station_id
    )

    UPDATE
      station_status ss
    SET
      info_reported = li.reported
    FROM
      latest_info li
    WHERE
        ss.info_station_id = li.station_id
      AND
        ss.last_reported BETWEEN _start_time AND _end_time
      AND
        ss.info_reported = '2023-12-20 05:20:38.925686+00';
   RETURN '';

EXCEPTION WHEN OTHERS THEN
   RETURN SQLERRM;

END
$func$;

SELECT public.fnc_work('2024-03-28', '2024-04-01', NULL);
SELECT public.fnc_work('2024-03-22', '2024-03-28', NULL);
SELECT public.fnc_work('2024-03-15', '2024-03-22', NULL);
SELECT public.fnc_work('2024-03-08', '2024-03-15', NULL);
SELECT public.fnc_work('2024-03-01', '2024-03-08', NULL);

SELECT public.fnc_work('2024-02-28', '2024-03-01', NULL);
SELECT public.fnc_work('2024-02-22', '2024-02-28', NULL);
SELECT public.fnc_work('2024-02-15', '2024-02-22', NULL);
SELECT public.fnc_work('2024-02-08', '2024-02-15', NULL);
SELECT public.fnc_work('2024-02-01', '2024-02-08', NULL);

SELECT public.fnc_work('2024-01-28', '2024-02-01', NULL);
SELECT public.fnc_work('2024-01-22', '2024-01-28', NULL);
SELECT public.fnc_work('2024-01-15', '2024-01-22', NULL);
SELECT public.fnc_work('2024-01-08', '2024-01-15', NULL);
SELECT public.fnc_work('2024-01-01', '2024-01-08', NULL);

SELECT public.fnc_work('2023-12-28', '2024-01-01', NULL);
SELECT public.fnc_work('2023-12-22', '2023-12-28', NULL);
SELECT public.fnc_work('2023-12-15', '2023-12-22', NULL);
SELECT public.fnc_work('2023-12-08', '2023-12-15', NULL);
SELECT public.fnc_work('2023-12-01', '2023-12-08', NULL);

SELECT public.fnc_work('2023-11-28', '2023-12-01', NULL);
SELECT public.fnc_work('2023-11-22', '2023-11-28', NULL);
SELECT public.fnc_work('2023-11-15', '2023-11-22', NULL);
SELECT public.fnc_work('2023-11-08', '2023-11-15', NULL);
SELECT public.fnc_work('2023-11-01', '2023-11-08', NULL);

SELECT public.fnc_work('2023-10-28', '2023-11-01', NULL);
SELECT public.fnc_work('2023-10-22', '2023-10-28', NULL);
SELECT public.fnc_work('2023-10-15', '2023-10-22', NULL);
SELECT public.fnc_work('2023-10-08', '2023-10-15', NULL);
SELECT public.fnc_work('2023-10-01', '2023-10-08', NULL);

SELECT public.fnc_work('2023-09-28', '2023-10-01', NULL);
SELECT public.fnc_work('2023-09-22', '2023-09-28', NULL);
SELECT public.fnc_work('2023-09-15', '2023-09-22', NULL);
SELECT public.fnc_work('2023-09-08', '2023-09-15', NULL);
SELECT public.fnc_work('2023-09-01', '2023-09-08', NULL);

-- First records actually predate the earliest station_information records, so set the upper limit to the earliest station_information manually.
SELECT public.fnc_work('2023-09-15', '2023-09-16', '2023-09-24 13:58:22.000 -0400');




SELECT * FROM station_status
WHERE info_reported = '2000-01-01 00:00:00+00';

DELETE FROM station_status
WHERE info_reported = '2000-01-01 00:00:00+00';

UPDATE
  station_status ss
SET
  info_reported = '2023-09-24 17:58:22+00'
WHERE
    ss.info_station_id = 7759
  AND
    ss.info_reported = '2023-12-20 05:20:38.925686+00';


SELECT
    ss.station_id, count(ss.station_id) as NUM
FROM
    public.station_status AS ss
LEFT JOIN
    public.station_information AS si
    ON ss.info_station_id = si.station_id
    AND ss.info_reported = si.reported
WHERE
    (si.station_id IS NULL
    OR si.reported IS NULL)  GROUP BY ss.station_id;

SELECT
    ss.station_id, ss.last_reported, ss.info_reported
FROM
    public.station_status AS ss
LEFT JOIN
    public.station_information AS si
    ON ss.info_station_id = si.station_id
    AND ss.info_reported = si.reported
WHERE
    (si.station_id IS NULL
    OR si.reported IS NULL);

ALTER TABLE public.station_status
ADD CONSTRAINT fk_station_information FOREIGN KEY (info_station_id, info_reported) REFERENCES public.station_information(station_id, reported) ON UPDATE CASCADE;


DROP TRIGGER IF EXISTS check_station_status_update ON station_status CASCADE;

DROP FUNCTION IF EXISTS handle_dup_pkey CASCADE;
DROP FUNCTION IF EXISTS public.fnc_work;
