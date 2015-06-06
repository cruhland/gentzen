
// TODO Don't use a global
var proofRows = [];

// TODO Don't use a global
var proofTBody = document.getElementById("theTable").createTBody();

function makeProofLine(formulaHtml, reasonHtml) {
    formulaHtml = formulaHtml || "";
    reasonHtml = reasonHtml || "";
    
    var newId = proofRows.length;
    var row = document.createElement("tr");
    proofRows[newId] = row;

    appendTextInput(row.insertCell(), newId);
    appendTextInput(row.insertCell(), formulaHtml);
    appendTextInput(row.insertCell(), reasonHtml);

    return newId;
}

function appendTextInput(element, value) {
    var input = document.createElement("input");
    input.value = value;
    element.appendChild(input);
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
