const grid = new gridjs.Grid({...tableBase,
  columns: [
    {
      name: "ID",
      formatter: constructLink,
    },
    "Name",
    {
      name: "Type",
      formatter: formatStationType,
    },
    "# Docks",
    {name: 'Bikes Available', columns: ['Mechanical', 'E-Fit', 'E-Fit G5']},
    {name: 'Disabled', columns: ['Bikes', 'Docks']},
    { name: "Duration (DD:HH:MM:SS)", columns: [
      { name: "Empty", formatter: (cell) => `${cell?.toDDHHMMSS() || ''}`, attributes: { 'style':  'text-align: right'}},
      { name: "Full",  formatter: (cell) => `${cell?.toDDHHMMSS() || ''}`, attributes: { 'style':  'text-align: right'}},
    ] },
  ],
  server: {
    url: "/data/station-occupancy",
    then: (data) =>
      data.map((card) => [
        card.station_information.station_id,
        card.station_information.name,
        card.station_information.is_charging_station ? "Charging" : "Regular",
        card.station_information.capacity,
        card.station_status.vehicle_types_available[1]['count'],
        card.station_status.vehicle_types_available[2]['count'],
        card.station_status.vehicle_types_available[3]['count'],
        card.station_status.num_bikes_disabled,
        card.station_status.num_docks_disabled,
        card.durations.empty,
        card.durations.full,
      ]),
  },
});
