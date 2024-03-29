// This function converts a duration string into seconds.
function parseDuration(durationString) {
  // Define regular expressions for days, hours, minutes, and seconds.
  const dayRegex = /(\d+)(d)/;
  const hourRegex = /(\d+)(h)/;
  const minuteRegex = /(\d+)(m)/;
  const secondRegex = /(\d+)(s)/;

  // Extract each unit from the duration string.
  const days =
    durationString.match(dayRegex) != null
      ? parseInt(durationString.match(dayRegex)[1])
      : 0;
  const hours =
    durationString.match(hourRegex) != null
      ? parseInt(durationString.match(hourRegex)[1])
      : 0;
  const minutes =
    durationString.match(minuteRegex) != null
      ? parseInt(durationString.match(minuteRegex)[1])
      : 0;
  const seconds =
    durationString.match(secondRegex) != null
      ? parseInt(durationString.match(secondRegex)[1])
      : 0;

  // Compute the total duration in seconds.
  const totalSeconds = seconds + minutes * 60 + hours * 3600 + days * 86400;

  // Return total duration in seconds.
  return totalSeconds;
}

function durationComparator(duration1, duration2) {
  // Convert each duration string to an equivalent number of seconds.
  const duration1InSeconds = parseDuration(duration1);
  const duration2InSeconds = parseDuration(duration2);

  // Compare the two durations and return the comparison result.
  if (duration1InSeconds > duration2InSeconds) {
    return 1;
  } else if (duration2InSeconds > duration1InSeconds) {
    return -1;
  } else {
    return 0;
  }
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
