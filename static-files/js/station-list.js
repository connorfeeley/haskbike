
const filterColumns = ['station-id-col', 'station-name-col', 'station-address-col'];
const table = document.getElementById('station-list-table');
const filterInput = document.getElementById('station-filter-input');
const stationTypeRadios = document.getElementsByName('station-type-radio');
const urlParams = new URLSearchParams(window.location.search);

filterInput.addEventListener('input', filterStationsTable);
for (let radio of stationTypeRadios) {
    radio.addEventListener('change', filterStationsTable);
}

function filterStationsTable() {
    const filterValue = filterInput.value.toLowerCase();
    const selectedStationType = getSelectedStationType();

    for (let i = 1; i < table.rows.length; i++) {
        let row = table.rows[i];
        let shouldShow = false;

        for (let column of filterColumns) {
            let cell = row.querySelector(`[data-column-id="${column}"]`);
            if (cell && cell.textContent.toLowerCase().includes(filterValue)) {
                shouldShow = true;
                break;
            }
        }

        let stationTypeCell = row.querySelector('[data-column-id="station-type-col"]');
        let stationType = stationTypeCell ? stationTypeCell.textContent : '';

        if (selectedStationType !== 'all' && stationType !== selectedStationType) {
            shouldShow = false;
        }

        row.style.display = shouldShow ? '' : 'none';
    }
}

function getSelectedStationType() {
    for (let radio of stationTypeRadios) {
        if (radio.checked) {
            // Update the URL's search params
            urlParams.set('station-type', radio.dataset.stationType);
            window.history.replaceState({}, '', `${window.location.pathname}?${urlParams}`);

            return radio.dataset.stationType;
        }
    }
}

// Call the filterStationsTable function at load time
filterStationsTable();
