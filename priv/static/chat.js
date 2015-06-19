$(function() {
    $('#server').val('ws://' + window.location.host + '/websocket')

    var jjpet = require('jjpet'),
        jChat = $('#chats'),
        patmap = {},
        websocket

    // -----

    function send(node) {
        websocket.send(JSON.stringify(node))
    }

    function subscribe_room(room) {
      send({
        ctrl: {
          subscribe: ["room", room]
        }
      })
    }

    function unsubscribe_room(p) {
      send({
        ctrl: {
          unsubscribe: ["room", p]
        }
      })
    }

    function subscribe_topic(topic) {
      send({
        ctrl: {
          subscribe: ["topic", topic]
        }
      })
    }

    function unsubscribe_topic(p) {
      send({
        ctrl: {
          unsubscribe: ["topic", p]
        }
      })
    }

    function subscribe_any(e) {
      send({
        ctrl: {
          subscribe: ["any", e]
        }
      })
    }

    function unsubscribe_any(e) {
      send({
        ctrl: {
          unsubscribe: ["any", e]
        }
      })
    }

    function send_msg(from, to, txt) {
      send({from: from,
            room: to,
            msg: txt})
    }

    // -----

    function clean() {
        for (var k in patmap) {
            var e = patmap[k]
            e.subs.forEach(function(w) {
                w.unlink()
            })
            delete patmap[k]
        }
    }

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

    function isRegistered(p) {
        return !!patmap[p]
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

    // -----

    function buildTextLine(n) {
      return '<div class="line">'
        + '<span class="from">' + n.from + '</span>'
        + '<span class="room">@' + n.room + '</span>'
        + '<span class="text"> | ' + n.msg + '</span>'
        + '</div>'
    }

    function buildChatWindow(name, lfn) {
        lfn = lfn || buildTextLine

        var name = name,
            jWindow = $('<div class="col-lg-6"></div>'),
            jPanel = $('  <div class="panel panel-primary">'
                       + '  <div class="panel-heading clearfix">'
                       + '    <h2 class="panel-title pull-left">' + name + '</h2>'
                       + '    <div class="btn-group pull-right">'
                       + '      <button bid="1" class="btn btn-danger">close</button>'
                       + '      <button bid="2" class="btn btn-success">clear</button>'
                       + '    </div>'
                       + '  </div>'
                       + '</div>'),
            jBody = $('<div class="block panel-body"></div>')

        jPanel.append(jBody)
        jWindow.append(jPanel)

        var w = {}

        function bind(w) {
          jPanel.find('button[bid="1"]').click(function() {
            unregister(name, w)
            w.unlink()
          })
          jPanel.find('button[bid="2"]').click(function() {
            jBody.html('')
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
          jBody.append(lfn(n))
        }

        return w
    }

    // -----

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

    function onOpen(evt) {
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
        clean()
    };

    function onMessage(evt) {
        dispatch(JSON.parse(evt.data))
    };

    function onError(evt) {
        showScreen('<span style="color: red;">ERROR: ' + evt.data + '</span>');
    };

    // -----

    function click_subscribe_room() {
        var room = $("#subscribe_room_text").val(),
            p = '{"room":"' + room + '"}'
        if (isRegistered(p)) {
          // TODO : some animation ? Some log ?
          return // <==
        }

        var w = buildChatWindow(p)
        w.link(jChat)
        register(p, w, function() {
          unsubscribe_room(room)
        })

        subscribe_room(room)
    }

    function click_subscribe_topic() {
        var topic = $("#subscribe_topic_text").val(),
            p = '{"msg":#"#' + topic + '"}'
        if (isRegistered(p)) {
          // TODO : some animation ? Some log ?
          return // <==
        }

        var w = buildChatWindow(p)
        w.link(jChat)
        register(p, w, function() {
          unsubscribe_topic(topic)
        })

        subscribe_topic(topic)
    };

    function click_subscribe_any() {
        var p = $("#subscribe_any_text").val()
        if (isRegistered(p)) {
          // TODO : some animation ? Some log ?
          return // <==
        }

        var w = buildChatWindow(p, function(n) {
            return '<div>' + JSON.stringify(n) + '</div>'
        })
        w.link(jChat)
        register(p, w, function() {
          unsubscribe_any(p)
        })

        subscribe_any(p)
    };

    function click_sendMessage() {
        if (websocket.readyState != websocket.OPEN){
            showScreen('websocket is not connected')
            return // <==
        }

        var name = $("#name").val(),
            room = $("#send_to_room").val(),
            txt = $("#send_txt").val()
        send_msg(name, room, txt)
    }

    function showScreen(txt) {
        $('#output').prepend('<p>' + txt + '</p>');
    };

    function clearScreen() {
        $('#output').html("");
    };

    // -----

    $('#toggle_connection').click(toggle_connection)
    $('#send_text').click(click_sendMessage)
    $('#clear').click(clearScreen)
    $('#subscribe_topic').click(click_subscribe_topic)
    $('#subscribe_room').click(click_subscribe_room)
    $('#subscribe_any').click(click_subscribe_any)

    $("#navigation").hide();
    $("#subscriptions").hide();
    $("#chats").hide();

    $('#name').tooltip({'trigger':'focus', 'placement':'right', 'title': 'message sender\'s name'});
    $('#send_to_room').tooltip({'trigger':'focus', 'title': 'room name to which the message must be sent'});
    $('#send_txt').tooltip({'trigger':'focus', 'title': 'text to send. Use # prefix to indicate a topic. e.g. #json'});
    $('#subscribe_room_text').tooltip({'trigger':'focus', 'title': 'the name of the room you want to have messages'});
    $('#subscribe_topic_text').tooltip({'trigger':'focus', 'title': 'the name of the topic you want to have messages'});
    $('#subscribe_any_text').tooltip({'trigger':'focus', 'title': 'any valid jpet expression'});

    if(!("WebSocket" in window)){
        showScreen('<p><span style="color: red;">websockets are not supported </span></p>');
        $("#navigation").hide();
    };
});
