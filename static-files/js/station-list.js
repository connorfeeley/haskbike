
const filterColumns = ['station-id-col', 'station-name-col', 'station-address-col'];

const table = document.getElementById('station-list-table');

const filterInput = document.getElementById('station-filter-input');
const stationTypeRadios = document.getElementsByName('station-type-radio');

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

        if (selectedStationType !== 'All' && stationType !== selectedStationType) {
            shouldShow = false;
        }

        row.style.display = shouldShow ? '' : 'none';
    }
}

function getSelectedStationType() {
    for (let radio of stationTypeRadios) {
        if (radio.checked) {
            return radio.dataset.stationType;  // we assume each radio button has a 'data-station-type' attribute
        }
    }
}
