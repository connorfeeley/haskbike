// GridJS table parameters.
const grid = new gridjs.Grid({
  columns: [
    "Endpoint",
    { name: 'All Queries',        columns: [ {name: '# Queries',    },
                                             {name: 'Avg Interval', },
                                             {name: 'Latest',       },
                                           ] },
    { name: 'Successful Queries', columns: [ {name: '# Queries',    },
                                             {name: 'Avg Interval', },
                                             {name: 'Latest',       },
                                           ] },
    { name: 'Failed Queries',     columns: [ {name: '# Queries',    },
                                             {name: 'Avg Interval', },
                                             {name: 'Latest',       },
                                           ] },
  ],
  server: {
    url: "/debug/query-logs/history/all",
    then: (data) =>
      data.map((card) => [
        card.endpoint,

        card.total["num-queries"],
        card.total["avg-interval"],
        card.total["latest-query"],

        card.successful["num-queries"],
        card.successful["avg-interval"],
        card.successful["latest-query"],

        card.failed["num-queries"],
        card.failed["avg-interval"],
        card.failed["latest-query"],
      ]),
  },
  style: {
    table: {
      'line-height': 'normal',
      'white-space': 'nowrap',
    },
  },
  sort: true,
  multiColumn: false,
  search: true,
  fixedHeader: true,
  pagination: false,
});


document.addEventListener(
  "DOMContentLoaded",
  function () {
    grid.render(document.getElementById("query-history-table"));
  },
  false,
);
