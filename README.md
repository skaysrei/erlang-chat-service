# A simple Erlang chat service: 

This is a chat server built on top of the Erlang/OTP framework. It allows for multiple
TCP connections to take place and communication happens via a very simple protocol. 

### Features

Login:
-----

    LOGIN:"UserName"

Server protocol:
-----

    LOGOUT:"UserName"
    LOGIN:

    WHOAMI:
    WHEREAMI:

    LISTROOM:
    NEWROOM:"RoomName"
    DELROOM:"RoomName"
    JOINROOM:"RoomName"
    EXITROOM:"RoomName"


### OTP Supervision tree: 

<p align="left">
  <img src="https://github.com/skaysrei/docs-diagrams/blob/main/OTP%20Supervision%20tree_updated.jpg" />
</p>

Setting up kerl:

Step -1

Install git:
sudo apt install git-all


kerl config option ssl 1.1 on older otp builds (<24.2)
export KERL_CONFIGURE_OPTIONS="--without-javac --with-ssl=/usr/local/opt/openssl@1.1"

chat_service
=====

An OTP application

Build
-----

    $ rebar3 compile

TLSv1.3

# rebar3 version 3.20.0

---

### Known issues:
1. If the service gets terminated, especially while one or more clients are still 
connected, it might take a few more seconds for the sockets to free up. 
Wait ~1 minute before restarting the service.
2. translation_layer worker not closing when connection closes because it is not
sending an exit signal. Sending it manually now but needs a rework. Also error
when connecting multiple clients, service already started.
(FIXED: Changed start_link/4 to start_link/3, issue was duplicate local name when
the server tried to start a new translation_layer worker).
3. Currently there is a bug when the header that the client receives is sometimes
doubled. This happend at random and the current workaround is implemented in the
client via trimming the prefix twice. Need further investigation.
4. TODO: When user disconnects the chat_controller state keeps holding onto its data,
should be an easy fix
5. TODO: Sending a direct message to non existing user crashes the controller, again,
easy fix via adding a check
6. TODO: Improve the output of LISTROOM:, its basic af (just like erlang string 
manipualtion capabilities)
7. TODO: Implement private room

---

### TODO: Implement Protobuf communication
### TODO: Implement RedisDB message storage
<br></br>
### References used during the development: 

[Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/content)

[The ABCs of OTP - Jesse J. Anderson](https://www.youtube.com/watch?v=4SCwubzqsVU)

[Erlang Master Class 1: Functional Programming](https://youtube.com/playlist?list=PLR812eVbehlwEArT3Bv3UfcM9wR3AEZb5)

[Erlang Master Class 2: Concurrent Programming](https://youtube.com/playlist?list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn)

[Erlang Master Class 3: OTP Behaviours and Releases](https://youtube.com/playlist?list=PLR812eVbehlx6vgWGf2FLHjkksAEDmFjc)

[The simplest Erlang TCP server ever](https://dmathieu.com/articles/development/erlang-tcp-server/)

[erl-chat-server: A simple chat server written in Erlang](https://github.com/luisgabriel/erl-chat-server)
<br></br>
### Useful related documentation: 

[Erlang/OTP Development Environment for concurrent programming](https://www.erlang.org/doc/)

[Kerl, Easy building and installing of Erlang/OTP instances](https://github.com/kerl/kerl)

[Rebar3, The official build tool for Erlang](https://rebar3.org/docs/)

[Erlang/OTP: gen_server](https://www.erlang.org/doc/man/gen_server.html)

[Erlang/OTP: gen_tcp](https://www.erlang.org/doc/man/gen_tcp.html)