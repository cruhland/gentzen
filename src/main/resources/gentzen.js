
(function () {
    var duplicateLineButton = document.getElementById("duplicateLine");
    var saveProofButton = document.getElementById("saveButton");

    duplicateLineButton.addEventListener("click", duplicateFocusedRow);
    saveProofButton.addEventListener("click", saveProof);

    var proofTable = document.getElementById("theTable");
    var escapingDiv = document.createElement("div");

    // Replace HTML entities with their Unicode chars after edits are made
    proofTable.addEventListener("change", function (changeEvent) {
        // TODO May be a security risk -- also annoying to type HTML entities
        escapingDiv.innerHTML = changeEvent.target.value;
        changeEvent.target.value = escapingDiv.innerHTML;
    });

    var proofTBody = proofTable.createTBody();
    var proofRows = [];

    var startLine = makeProofLine();
    moveAbove(startLine, -1);

    function makeTableRow(id, formula, reason) {
        var row = document.createElement("tr");

        var idCell = row.insertCell();
        idCell.innerText = id;

        var formulaInput = document.createElement("input");
        formulaInput.className = "formula";
        formulaInput.value = formula;
        formulaInput.addEventListener("focus", function () {
            lastFocusedInput = formulaInput;
        });
        row.insertCell().appendChild(formulaInput);

        var reasonInput = document.createElement("input");
        reasonInput.value = reason;
        reasonInput.addEventListener("focus", function () {
            lastFocusedInput = reasonInput;
        });
        row.insertCell().appendChild(reasonInput);

        return row;
    }

    function makeProofLine(formulaText, reasonText) {
        formulaText = formulaText || "";
        reasonText = reasonText || "";

        var newId = proofRows.length;
        var row = makeTableRow(newId, formulaText, reasonText);
        proofRows[newId] = row;

        return newId;
    }

    function moveAbove(topRow, botRow) {
        proofTBody.insertBefore(proofRows[topRow], proofRows[botRow]);
    }

    var lastFocusedInput = null;

    function duplicateFocusedRow() {
        var focused = lastFocusedInput;
        if (focused.nodeName !== "INPUT") return;

        var row = focused.parentElement.parentElement;
        if (row.nodeName !== "TR") return;

        var rowId = Number(row.children[0].innerText);
        if (Number.isNaN(rowId)) return;

        var formula = row.children[1].children[0].value;
        var reason = row.children[2].children[0].value;
        var newId = makeProofLine(formula, reason);

        moveAbove(newId, rowId);
    }

    function extractProof() {
        var rows = proofTBody.children;
        var proof = [];
        for (var i = 0; i < rows.length; i++) {
            var cols = rows[i].children;
            var id = cols[0].innerText;
            var formula = cols[1].firstElementChild.value;
            var reason = cols[2].firstElementChild.value;
            proof.push([id, formula, reason]);
        }

        return proof;
    }

    function saveProof() {
        var proof = extractProof();

        var proofJson = JSON.stringify(proof);
        var blob = new Blob(
            [proofJson], {type: "application/json;charset=utf-8"}
        );

        var saveFile = document.getElementById("saveFile").value;
        if (saveFile) {
            saveAs(blob, saveFile);
        } else {
            console.log("Invalid file name for saving proof");
        }
    }

    function loadProof(event) {
        var file = event.target.files[0];
        if (!file) return;

        var fileReader = new FileReader();

        fileReader.onload = function () {
            // TODO error handling
            var proofData = JSON.parse(fileReader.result);

            proofTBody.innerHTML = '';
            proofRows = [];
            for (var i = 0; i < proofData.length; i++) {
                var rowData = proofData[i];
                var id = Number(rowData[0]);
                var formula = rowData[1];
                var reason = rowData[2];

                var row = makeTableRow(id, formula, reason);
                proofTBody.appendChild(row);
                proofRows[id] = row;
            }
        };

        fileReader.onerror = function () {
            var error = fileReader.error;
            alert("A " + error.name + " occurred when reading " + file.name +
                  ": " + error.message);
        };

        fileReader.readAsText(file);
    }

    var loadFile = document.getElementById("loadFile");
    loadFile.addEventListener("change", loadProof);
})();
