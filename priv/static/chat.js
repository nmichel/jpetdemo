var jjpet = require('jjpet');
var websocket;

function connect() {
    wsHost = $("#server").val()
    websocket = new WebSocket(wsHost);
    showScreen('<b>Connecting to: ' +  wsHost + '</b>'); 
    websocket.onopen = function(evt) { onOpen(evt) }; 
    websocket.onclose = function(evt) { onClose(evt) }; 
    websocket.onmessage = function(evt) { onMessage(evt) }; 
    websocket.onerror = function(evt) { onError(evt) }; 
};  

function disconnect() {
    websocket.close();
}; 

function toggle_connection() {
    if (websocket.readyState == websocket.OPEN){
        disconnect();
    } else {
        connect();
    };
};

function subscribe_room() {
    var room = $("#subscribe_room").val();
    var msg = {
        ctrl: {
            subscribe: ["room", room]
        }
    };
    websocket.send(JSON.stringify(msg));
}

function unsubscribe_room() {
    var room = $("#subscribe_room").val();
    var msg = {
        ctrl: {
            unsubscribe: ["room", room]
        }
    };
    websocket.send(JSON.stringify(msg));
}

function subscribe_topic() {
    var topic = $("#subscribe_topic").val();
    var msg = {
        ctrl: {
            subscribe: ["topic", topic]
        }
    };
    websocket.send(JSON.stringify(msg));
}

function unsubscribe_topic() {
    var topic = $("#subscribe_topic").val();
    var msg = {
        ctrl: {
            unsubscribe: ["topic", topic]
        }
    };
    websocket.send(JSON.stringify(msg));
}

function sendTxt() {
    if (websocket.readyState == websocket.OPEN){
        var name = $("#name").val();
        var room = $("#send_to_room").val();
        var txt = $("#send_txt").val();
        var msg = {from: name,
                   room: room,
                   msg: txt};
        websocket.send(JSON.stringify(msg));
    } else {
        showScreen('websocket is not connected'); 
    };
};

function onOpen(evt) { 
    var matcher = jjpet.compile('**/{"answer":_}');
    showScreen('<span style="color: green;">CONNECTED </span>'); 
    $("#connected").fadeIn('slow');
};  

function onClose(evt) { 
    showScreen('<span style="color: red;">DISCONNECTED </span>');
    $("#connected").fadeOut('slow');
};  

function onMessage(evt) { 
    var data = evt.data,
    json = JSON.parse(data);
    
    showScreen('<span style="color: blue;">[' + json.from + '@' + json.room + '] ' + json.msg + '</span>'); 
};  

function onError(evt) {
    showScreen('<span style="color: red;">ERROR: ' + evt.data + '</span>');
};

function showScreen(txt) { 
    $('#output').prepend('<p>' + txt + '</p>');
};

function clearScreen() 
{ 
    $('#output').html("");
};

$(function() {
    $("#connected").hide(); 

    if(!("WebSocket" in window)){  
        showScreen('<p><span style="color: red;">websockets are not supported </span></p>');
        $("#navigation").hide();  
    } else {
        connect();
    };
});
