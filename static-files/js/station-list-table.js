// Convert a string representing seconds to the shortest representation of 'DD:HH:MM:SS'.
String.prototype.toDDHHMMSS = function () {
    var sec_num = parseInt(this, 10);
    var days = Math.floor(sec_num / (3600 * 24));
    var hours = Math.floor((sec_num - (days * 3600 * 24)) / 3600);
    var minutes = Math.floor((sec_num - (days * 3600 * 24) - (hours * 3600)) / 60);
    var seconds = sec_num - (days * 3600 * 24) - (hours * 3600) - (minutes * 60);

    if (days < 10) {days = "0" + days;}
    if (hours < 10) {hours = "0" + hours;}
    if (minutes < 10) {minutes = "0" + minutes;}
    if (seconds < 10) {seconds = "0" + seconds;}

    if (days > 0)         {fmt = days + ':' + hours + ':' + minutes + ':' + seconds;}
    else if (hours > 0)   {fmt =              hours + ':' + minutes + ':' + seconds;}
    else if (minutes > 0) {fmt =                            minutes + ':' + seconds;}
    else if (seconds > 0) {fmt =                                            seconds;}
    else                  {fmt =                                                 '';}
    return fmt;
}

Number.prototype.toDDHHMMSS = function () {
    var sec_num = this;
    var days = Math.floor(sec_num / (3600 * 24));
    var hours = Math.floor((sec_num - (days * 3600 * 24)) / 3600);
    var minutes = Math.floor((sec_num - (days * 3600 * 24) - (hours * 3600)) / 60);
    var seconds = sec_num - (days * 3600 * 24) - (hours * 3600) - (minutes * 60);

    if (days < 10) {days = "0" + days;}
    if (hours < 10) {hours = "0" + hours;}
    if (minutes < 10) {minutes = "0" + minutes;}
    if (seconds < 10) {seconds = "0" + seconds;}

    if (days > 0)         {fmt = days + ':' + hours + ':' + minutes + ':' + seconds;}
    else if (hours > 0)   {fmt =              hours + ':' + minutes + ':' + seconds;}
    else if (minutes > 0) {fmt =                            minutes + ':' + seconds;}
    else if (seconds > 0) {fmt =                                            seconds;}
    else                  {fmt =                                                 '';}
    return fmt;
}

function constructLink(stationId) {
    // We get the current URL of the document.
    const currentUrl = new URL(window.location.href);

    // Initialized the URLSearchParams constructor with current URL's search params
    // which can be directly modified for our new URL.
    let urlParameters = new URLSearchParams(currentUrl.search);

    // Now, we'll add the 'station-id' parameter to our new set of URL parameters.
    urlParameters.set('station-id', stationId);

    // Define our base URL.
    const baseUrl = "/visualization/station-status";

    // Construct the new URL.
    const newUrl = `${baseUrl}?${urlParameters}`;

    // Construct the anchor element.
    const anchor = document.createElement('a');
    anchor.href = newUrl;
    anchor.textContent = stationId;

    // Return the created anchor element.
    return gridjs.html(anchor.outerHTML);
}

function formatStationType(stationType) {
  // Construct the anchor element.
  const span = document.createElement('span');
  span.textContent = stationType;
  span.style.fontWeight = stationType === 'Charging' ? 'bold' : 'normal';
  // span.style.color = stationType === 'Charging' ? 'green' : 'black';

  // Return the created anchor element.
  return gridjs.html(span.outerHTML);
}


const gridStyle = {
  table: {
    'line-height': 'normal',
    'white-space': 'nowrap',
  },
};

// Common GridJS table parameters. Consumers need to fill in 'server' and 'columns'.
const tableBase = {
  sort: true,
  multiColumn: false,
  search: true,
  fixedHeader: true,
  pagination: false,
  style: gridStyle,
};

document.addEventListener(
  "DOMContentLoaded",
  function () {
    grid.render(document.getElementById("station-list-table"));
  },
  false,
);
