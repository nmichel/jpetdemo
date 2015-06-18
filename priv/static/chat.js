$(function() {
    var jjpet = require('jjpet'),
        jChat = $('#chats'),
        patmap = {},
        websocket

    function unsubscribe_room(p) {
        var msg = {
            ctrl: {
                unsubscribe: ["room", p]
            }
        }
        websocket.send(JSON.stringify(msg))
    }

    function unsubscribe_topic(p) {
        var msg = {
            ctrl: {
                unsubscribe: ["topic", p]
            }
        }
        websocket.send(JSON.stringify(msg))
    };

    function register(p, w, cb) {
        var e = patmap[p]
        if (!e) {
            var jpm = jjpet.compile(p),
                e = {jpm: jpm, cb: cb, subs: []}
            patmap[p] = e
        }
        e.subs.push(w)
    }

    function unregister(p, w) {
        var e = patmap[p]
        if (!e) {
            return // <==
        }

        e.subs = e.subs.filter(function(v) {
            v !== w
        })

        if (e.subs.length == 0) {
          e.cb()
          delete patmap[p]
        }
    }

    function dispatch(n) {
        for (var k in patmap) {
            var e = patmap[k],
                mr = jjpet.run(n, e.jpm)

            if (mr.status) {
                e.subs.forEach(function(w) {
                    w.add(n)
                })
            }
        }
    }

    function buildTextLine(n) {
      return '<div class="line">'
        + '<span class="from">' + n.from + '</span>'
        + '<span class="room">@' + n.room + '</span>'
        + '<span class="text"> | ' + n.msg + '</span>'
        + '</div>'
    }

    function buildChatWindow(name) {
        var name = name,
            jWindow = $('<div class="col-lg-6"></div>'),
            jPanel = $('  <div class="panel panel-primary">'
                       + '  <div class="panel-heading clearfix">'
                       + '    <h2 class="panel-title pull-left">' + name + '</h2>'
                       + '    <div class="btn-group pull-right">'
                       + '      <a class="btn btn-default btn-sm">close</a>'
                       + '    </div>'
                       + '  </div>'
                       + '</div>'),
            jBody = $('<div class="block panel-body"></div>')

        jPanel.append(jBody)
        jWindow.append(jPanel)

        var w = {}

        function bind(w) {
          jPanel.find('a').click(function() {
            unregister(name, w)
            w.unlink()
          })
        }

        w.link = function(r) {
          bind(w)
          r.append(jWindow)
        }

        w.unlink = function(r) {
          jWindow.remove()
        }

        w.add = function(n) {
          jBody.append(buildTextLine(n))
        }

        return w
    }

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
        var room = $("#subscribe_room_text").val(),
            msg = {
                ctrl: {
                    subscribe: ["room", room]
                }
            },
            p = '{"room":"' + room + '"}',
            w = buildChatWindow(p)

        w.link(jChat)
        register(p, w, function() {
          unsubscribe_room(room)
        })
        websocket.send(JSON.stringify(msg));
    };

    function subscribe_topic() {
        var topic = $("#subscribe_topic_text").val(),
            msg = {
                ctrl: {
                    subscribe: ["topic", topic]
                }
            },
            p = '{"msg":#"#' + topic + '"}',
            w = buildChatWindow(p)

        w.link(jChat)
        register(p, w, function() {
          unsubscribe_topic(topic)
        })
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
        //var matcher = jjpet.compile('<!{"answer":_}!>');
        showScreen('<div class="alert alert-success">connected</div>');
        $("#navigation").slideDown();
        $('#subscriptions').slideDown();
        $('#chats').slideDown();
    };

    function onClose(evt) {
        showScreen('<div class="alert alert-danger">disconnected</div>');
        $("#navigation").slideUp();
        $('#subscriptions').slideUp();
        $('#chats').slideUp();
    };

    function onMessage(evt) {
        var data = evt.data,
            json = JSON.parse(data)
        dispatch(json)
    };

    function onError(evt) {
        showScreen('<span style="color: red;">ERROR: ' + evt.data + '</span>');
    };

    function showScreen(txt) {
        $('#output').prepend('<p>' + txt + '</p>');
    };

    function clearScreen() {
        $('#output').html("");
    };

    $("#navigation").hide();
    $("#subscriptions").hide();
    $("#chats").hide();

    $('#toggle_connection').click(function() {
        toggle_connection();
    });
    $('#send_text').click(function() {
        sendTxt();
    });
    $('#clear').click(function() {
        clearScreen();
    });
    $('#subscribe_topic').click(function() {
        subscribe_topic();
    });
    $('#subscribe_room').click(function() {
        subscribe_room();
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
