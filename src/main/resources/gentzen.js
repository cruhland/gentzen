
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

    var idCell = row.insertCell();
    idCell.innerText = newId;

    var formulaCell = row.insertCell();
    formulaCell.innerHTML = formulaHtml;

    var reasonCell = row.insertCell();
    reasonCell.innerHTML = reasonHtml;

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
