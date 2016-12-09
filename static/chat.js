var stopReload = false; // for debugging

function requestChatLog() {
    var xhr = new XMLHttpRequest();
    xhr.addEventListener("load", _ =>
            putChatLog(xhr.responseText) );
    xhr.open("GET", "/messages");
    xhr.send();
}

function putChatLog(chatlog) {
    document.getElementById("messages").textContent = chatlog;

    if (!stopReload) {
        setTimeout(requestChatLog, 250);
    }
}

function sendChatInput() {
    var msg = document.getElementById("chatinput").value;
    document.getElementById("chatinput").value = "";

    var xhr = new XMLHttpRequest();
    xhr.open("GET", "/say?c="+encodeURIComponent(msg) );
    xhr.send();
}

function init() {
    document.getElementById("chatinput")
        .addEventListener("keypress", function (e) {
            if (e.keyCode == 13) {
                sendChatInput();
            }
        });

    requestChatLog();
}

window.addEventListener("load",init);
