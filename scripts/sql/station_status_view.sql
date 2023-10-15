-- public.station_status_diff source

CREATE OR REPLACE VIEW public.station_status_diff
AS SELECT sub.station_id,
    sub.last_reported,
    sub.is_charging_station,
    sub.vehicle_types_available_iconic,
    sub.vehicle_types_iconic_diff,
    sub.vehicle_types_available_efit,
    sub.vehicle_types_efit_diff,
    sub.vehicle_types_available_efit_g5,
    sub.vehicle_types_efit_g5_diff,
    sum(
        CASE
            WHEN sub.vehicle_types_iconic_diff > 0 THEN sub.vehicle_types_iconic_diff
            ELSE 0
        END) OVER (PARTITION BY sub.station_id ORDER BY sub.last_reported) AS total_iconic_dockings,
    sum(
        CASE
            WHEN sub.vehicle_types_iconic_diff < 0 THEN - sub.vehicle_types_iconic_diff
            ELSE 0
        END) OVER (PARTITION BY sub.station_id ORDER BY sub.last_reported) AS total_iconic_undockings,
    sum(
        CASE
            WHEN sub.vehicle_types_efit_diff > 0 THEN sub.vehicle_types_efit_diff
            ELSE 0
        END) OVER (PARTITION BY sub.station_id ORDER BY sub.last_reported) AS total_efit_dockings,
    sum(
        CASE
            WHEN sub.vehicle_types_efit_diff < 0 THEN - sub.vehicle_types_efit_diff
            ELSE 0
        END) OVER (PARTITION BY sub.station_id ORDER BY sub.last_reported) AS total_efit_undockings,
    sum(
        CASE
            WHEN sub.vehicle_types_efit_g5_diff > 0 THEN sub.vehicle_types_efit_g5_diff
            ELSE 0
        END) OVER (PARTITION BY sub.station_id ORDER BY sub.last_reported) AS total_efit_g5_dockings,
    sum(
        CASE
            WHEN sub.vehicle_types_efit_g5_diff < 0 THEN - sub.vehicle_types_efit_g5_diff
            ELSE 0
        END) OVER (PARTITION BY sub.station_id ORDER BY sub.last_reported) AS total_efit_g5_undockings
   FROM ( SELECT station_status.station_id,
            station_status.last_reported,
            station_status.is_charging_station,
            station_status.vehicle_types_available_iconic,
            station_status.vehicle_types_available_efit,
            station_status.vehicle_types_available_efit_g5,
            COALESCE(station_status.vehicle_types_available_iconic - lag(station_status.vehicle_types_available_iconic) OVER (PARTITION BY station_status.station_id ORDER BY station_status.last_reported), station_status.vehicle_types_available_iconic) AS vehicle_types_iconic_diff,
            COALESCE(station_status.vehicle_types_available_efit - lag(station_status.vehicle_types_available_efit) OVER (PARTITION BY station_status.station_id ORDER BY station_status.last_reported), station_status.vehicle_types_available_efit) AS vehicle_types_efit_diff,
            COALESCE(station_status.vehicle_types_available_efit_g5 - lag(station_status.vehicle_types_available_efit_g5) OVER (PARTITION BY station_status.station_id ORDER BY station_status.last_reported), station_status.vehicle_types_available_efit_g5) AS vehicle_types_efit_g5_diff
           FROM station_status) sub;
