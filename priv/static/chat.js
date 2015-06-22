$(function() {
    $('#server').val('ws://' + window.location.host + '/websocket')

    var jjpet = require('jjpet'),
        jChat = $('#chats'),
        patmap = {},
        websocket

    var hb = (function() {

        var delayInSec = 30,
            delay = delayInSec * 1000,
            count = 0,
            max = (60/delayInSec * 10),
            tid = null

        function tick() {
            if (count++ === max) {
                console.log('count !', count)
                return
            }
            
            rawSend(["hb"])
            schedule()
        }

        function schedule() {
            tid = setTimeout(tick, delay)
        }

        return {
            ping: function() {
                if (tid) {
                    clearTimeout(tid)
                }

                schedule()
                count = 0
            },

            stop: function() {
                if (tid) {
                    clearTimeout(tid)
                }
                tid = null
            }
        }
    })()

    function hash(s) {
        for(var ret = 0, i = 0, len = s.length; i < len; i++) {
            ret = (31 * ret + s.charCodeAt(i)) << 0;
        }
        return ret
    }

    // -----

    $('h4.ejpet').each(function(i) {
        var jThis = $(this),
            t = jThis.text()
        
        $('<div class="try"><h4 class="ejpet">'+t+'</h4><h4 class="do">subscribe</h4></div>')
            .click(function() {
                do_subscribe_any(t)
                var e = $('div[title="' + hash(t) + '"]')
                e.focus()
                e.addClass('new')
                window.setTimeout(function() {e.removeClass('new')}, 500)
            })
            .replaceAll(jThis)
    })

    // -----

    function log(node) {
        var t = JSON.stringify(node)
        showScreen('<span class="info"> send: ' + t + '</span>');
    }

    function rawSend(node) {
        log(node)
        websocket.send(JSON.stringify(node))
    }

    function send(node) {
        hb.ping()
        rawSend(node)
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
                    w.window.addClass('received')
                    window.setTimeout(function() {w.window.removeClass('received')}, 500)
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
            jWindow = $('<div tabindex="1" title="' + hash(name) + '" class="window col-lg-6"></div>'),
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

        var w = {
            window: jWindow
        }

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
        showScreen('<span class="info">connecting to ' +  wsHost + '</span>');
        websocket.onopen = function(evt) { onOpen(evt) };
        websocket.onclose = function(evt) { onClose(evt) };
        websocket.onmessage = function(evt) { onMessage(evt) };
        websocket.onerror = function(evt) { onError(evt) };
        hb.ping()
    };

    function disconnect() {
        websocket.close()
    };

    function toggle_connection() {
        if (websocket && websocket.readyState == websocket.OPEN){
            disconnect();
        } else {
            connect();
        };
    };

    function onOpen(evt) {
        showScreen('<span class="success">connected</span>');
        $("#navigation").slideDown();
        $('#subscriptions').slideDown();
        $('#raw').slideDown();
        $('#chats').slideDown();
    };

    function onClose(evt) {
        showScreen('<span class="error">disconnected</span>');
        $("#navigation").slideUp();
        $('#subscriptions').slideUp();
        $('#raw').slideUp();
        $('#chats').slideUp();
        hb.stop()
        clean()
    };

    function onMessage(evt) {
        dispatch(JSON.parse(evt.data))
    };

    function onError(evt) {
        showScreen('<span class="error">ERROR: ' + evt.data + '</span>');
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

    function do_subscribe_any(p) {
        p = p.trim()

        if (!p.length) {
            return // <== 
        }

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
    }

    function click_subscribe_any() {
        do_subscribe_any($("#subscribe_any_text").val())
    };

    function click_sendMessage() {
        if (websocket.readyState != websocket.OPEN){
            showScreen('<span class="error">websocket is not connected</span>')
            return // <==
        }

        var name = $("#name").val(),
            room = $("#send_to_room").val(),
            txt = $("#send_txt").val()
        send_msg(name, room, txt)
    }

    function click_send_any() {
        var txt = $("#any_json").val()
        try {
            send(JSON.parse(txt))
        }
        catch (e) {
            showScreen('<span class="error">ERROR: Invalid JSON ' + txt + '</span>');
        }
    }

    function showScreen(txt) {
        var d = new Date()
        $('#output').prepend('<span>' + d.getHours()
                             + ':' + d.getMinutes() 
                             + ':' + d.getSeconds() + '</span> | ' + txt + '<br />');
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
    $('#send_json').click(click_send_any)

    $("#navigation").hide();
    $("#subscriptions").hide();
    $("#raw").hide();
    $("#chats").hide();

    $('#name').tooltip({'trigger':'focus', 'placement':'right', 'title': 'message sender\'s name'});
    $('#send_to_room').tooltip({'trigger':'focus', 'title': 'room name to which the message must be sent'});
    $('#send_txt').tooltip({'trigger':'focus', 'title': 'text to send. Use # prefix to indicate a topic. e.g. #json'});
    $('#subscribe_room_text').tooltip({'trigger':'focus', 'title': 'the name of the room you want to have messages'});
    $('#subscribe_topic_text').tooltip({'trigger':'focus', 'title': 'the name of the topic you want to have messages'});
    $('#subscribe_any_text').tooltip({'trigger':'focus', 'title': 'any valid jpet expression'});
    $('#any_json').tooltip({'trigger':'focus', 'title': 'any valid json expression'});

    if(!("WebSocket" in window)){
        showScreen('<span class="error">ERROR: websockets are not supported</span>')
        $("#navigation").hide();
    };
});
