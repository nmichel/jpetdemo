#jpetdemo : a basic ejpet demo


#What

`jpetdemo` is a messenging server based on websockets and JSON messenging.

The server is very simple and illustrates how JSON nodes can be routed using `ejpet` pattern matching mechanism.

#Quickstart
Clone 

```bash
$ git clone git@github.com:nmichel/jpetdemo.git
$ cd jpetdemo
```

Build

``` bash
$ ./rebar get-deps
$ ./rebar compile
```

Start

``` bash
$ ./run.sh
```

#Use

Point your browser at [http://localhost:9000](http://localhost:9000)[^1].

Open several times the URL to simulate several connected users.

The web UI allows to 

* send messages to "rooms"
* subscribe to "rooms"
* subscribe to "topics"

To indicate topics in text messages, use hastag markers # (e.g. #topic)

[^1]: possibly, replace `localhost` with a specific IP; e.g. if (like me) you are hosting the demo server in a VM.

