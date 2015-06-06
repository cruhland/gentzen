
// TODO Don't use a global
var proofRows = [];

// TODO Don't use a global
var proofTBody = document.getElementById("theTable").createTBody();

function makeProofLine(formulaText, reasonText) {
    formulaText = formulaText || "";
    reasonText = reasonText || "";
    
    var newId = proofRows.length;
    var row = document.createElement("tr");
    proofRows[newId] = row;

    var idCell = row.insertCell();
    idCell.innerText = newId;

    var formulaInput = document.createElement("input");
    formulaInput.className = "formula";
    formulaInput.value = formulaText;
    row.insertCell().appendChild(formulaInput);

    var reasonInput = document.createElement("input");
    reasonInput.value = reasonText;
    row.insertCell().appendChild(reasonInput);

    return newId;
}

function moveAbove(topRow, botRow) {
    proofTBody.insertBefore(proofRows[topRow], proofRows[botRow]);
}

function setFormula(row, formulaHtml) {
    proofRows[row].children[1].innerHTML = formulaHtml;
}

function setReason(row, reasonHtml) {
    proofRows[row].children[2].innerHTML = reasonHtml;
}
