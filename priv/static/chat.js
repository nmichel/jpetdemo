$(function() {
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
        if (websocket && websocket.readyState == websocket.OPEN){
            disconnect();
        } else {
            connect();
        };
    };
    
    function subscribe_room() {
        var room = $("#subscribe_room_text").val();
        var msg = {
            ctrl: {
                subscribe: ["room", room]
            }
        };
        websocket.send(JSON.stringify(msg));
    };

    function unsubscribe_room() {
        var room = $("#subscribe_room_text").val();
        var msg = {
            ctrl: {
                unsubscribe: ["room", room]
            }
        };
        websocket.send(JSON.stringify(msg));
    };

    function subscribe_topic() {
        var topic = $("#subscribe_topic_text").val();
        var msg = {
            ctrl: {
                subscribe: ["topic", topic]
            }
        };
        websocket.send(JSON.stringify(msg));
    };

    function unsubscribe_topic() {
        var topic = $("#subscribe_topic_text").val();
        var msg = {
            ctrl: {
                unsubscribe: ["topic", topic]
            }
        };
        websocket.send(JSON.stringify(msg));
    };
    
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
        showScreen('<div class="alert alert-success">connected</div>');
        $("#navigation").slideDown();
        $('#subscriptions').slideDown();
    };  
    
    function onClose(evt) { 
        showScreen('<div class="alert alert-danger">disconnected</div>');
        $("#navigation").slideUp();
        $('#subscriptions').slideUp();
    };  
    
    function onMessage(evt) { 
        var data = evt.data;
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

    $("#navigation").hide(); 
    $("#subscriptions").hide(); 

    $('#toggle_connection').click(function() {
        toggle_connection();
    });
    $('#send_text').click(function() {
        sendTxt();
    });
    $('#clear_screen').click(function() {
        clearScreen();
    });
    $('#subscribe_topic').click(function() {
        subscribe_topic();
    });
    $('#unsubscribe_topic').click(function() {
        unsubscribe_topic();
    });
    $('#subscribe_room').click(function() {
        subscribe_room();
    });
    $('#unsubscribe_room').click(function() {
        unsubscribe_room();
    });

    $('#name').tooltip({'trigger':'focus', 'placement':'right', 'title': 'message sender\'s name'});
    $('#send_to_room').tooltip({'trigger':'focus', 'title': 'room name to which the message must be sent'});
    $('#send_txt').tooltip({'trigger':'focus', 'title': 'text to send. Use # prefix to indicate a topic. e.g. #json'});
    $('#subscribe_room_text').tooltip({'trigger':'focus', 'title': 'the name of the room you want to have messages'});
    $('#subscribe_topic_text').tooltip({'trigger':'focus', 'title': 'the name of the topic you want to have messages'});

    if(!("WebSocket" in window)){  
        showScreen('<p><span style="color: red;">websockets are not supported </span></p>');
        $("#navigation").hide();  
    };
});
