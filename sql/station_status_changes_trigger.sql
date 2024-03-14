--
-- Create a trigger to insert new records into station_status_changes when a new record is inserted into station_status.
--

BEGIN;

CREATE OR REPLACE FUNCTION public.station_status_changes_trigger() RETURNS trigger AS $$
DECLARE
    prev_record public.station_status%ROWTYPE;
BEGIN
    SELECT INTO prev_record
        station_id, last_reported, num_bikes_available, num_bikes_disabled,
        num_docks_available, num_docks_disabled, is_charging_station,
        status, is_installed, is_renting, is_returning, traffic, vehicle_docks_available,
        vehicle_types_available_boost, vehicle_types_available_iconic,
        vehicle_types_available_efit, vehicle_types_available_efit_g5,
        info_station_id, info_reported  -- put these columns last to match station_status structure
    FROM public.station_status_changes
    WHERE station_id = NEW.station_id
    ORDER BY last_reported :: timestamp with time zone DESC
    LIMIT 1;

    -- If there's no previous record OR if each field other than last_reported is different, insert the new record into station_status_changes
    IF prev_record IS NULL OR
       (NEW.info_station_id != prev_record.info_station_id OR
        NEW.station_id != prev_record.station_id OR
        NEW.num_bikes_available != prev_record.num_bikes_available OR
        NEW.num_bikes_disabled != prev_record.num_bikes_disabled OR
        NEW.num_docks_available != prev_record.num_docks_available OR
        NEW.num_docks_disabled != prev_record.num_docks_disabled OR
        NEW.is_charging_station != prev_record.is_charging_station OR
        NEW.status != prev_record.status OR
        NEW.is_installed != prev_record.is_installed OR
        NEW.is_renting != prev_record.is_renting OR
        NEW.is_returning != prev_record.is_returning OR
        NEW.traffic != prev_record.traffic OR
        NEW.vehicle_docks_available != prev_record.vehicle_docks_available OR
        NEW.vehicle_types_available_boost != prev_record.vehicle_types_available_boost OR
        NEW.vehicle_types_available_iconic != prev_record.vehicle_types_available_iconic OR
        NEW.vehicle_types_available_efit != prev_record.vehicle_types_available_efit OR
        NEW.vehicle_types_available_efit_g5 != prev_record.vehicle_types_available_efit_g5)
    THEN
        INSERT INTO public.station_status_changes (
            info_station_id, info_reported, station_id, last_reported,
            num_bikes_available, num_bikes_disabled, num_docks_available,
            num_docks_disabled, is_charging_station, status,
            is_installed, is_renting, is_returning, traffic,
            vehicle_docks_available, vehicle_types_available_boost,
            vehicle_types_available_iconic, vehicle_types_available_efit,
            vehicle_types_available_efit_g5
        )
        VALUES (
            NEW.info_station_id, NEW.info_reported :: timestamp with time zone, NEW.station_id, NEW.last_reported :: timestamp with time zone,
            NEW.num_bikes_available, NEW.num_bikes_disabled, NEW.num_docks_available,
            NEW.num_docks_disabled, NEW.is_charging_station, NEW.status,
            NEW.is_installed, NEW.is_renting, NEW.is_returning, NEW.traffic,
            NEW.vehicle_docks_available, NEW.vehicle_types_available_boost,
            NEW.vehicle_types_available_iconic, NEW.vehicle_types_available_efit,
            NEW.vehicle_types_available_efit_g5
        );
    END IF;

    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER station_status_changes_after_insert
AFTER INSERT ON public.station_status
FOR EACH ROW EXECUTE PROCEDURE public.station_status_changes_trigger();

COMMIT;
