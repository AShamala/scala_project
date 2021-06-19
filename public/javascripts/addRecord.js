function recordType() { 
    var recordType = document.getElementById('type').value;
    if (recordType == 'Хранение') {
        document.getElementById('storage').style.display = 'block';
    } else if (recordType == 'Ремонт') {
        document.getElementById('transfer').style.display = 'block';
    } else if (recordType == 'Передача') {
        document.getElementById('repairs').style.display = 'block';
    } else if (recordType == 'Списание') {
        document.getElementById('write-off').style.display = 'block';
    }
}