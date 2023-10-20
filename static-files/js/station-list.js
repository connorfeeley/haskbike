const filterInput = document.getElementById('station-filter-input');
const table = document.getElementById('station-list-table');
const filterColumns = ['station-id-col', 'station-name-col', 'station-address-col'];

filterInput.addEventListener('input', function() {
    const filterValue = this.value.toLowerCase();
    const allRadio = document.getElementById('station-type-radio-all');
    const regularRadio = document.getElementById('station-type-radio-regular');

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

        let stationType = row.querySelector('[data-column-id="station-type-col"]').textContent;
        if (allRadio.checked) {
            // show all stations
        } else if (regularRadio.checked && stationType !== 'Regular') {
            shouldShow = false;
        } else if (!regularRadio.checked && stationType !== 'Charging') {
            shouldShow = false;
        }

        row.style.display = shouldShow ? '' : 'none';
    }
});
