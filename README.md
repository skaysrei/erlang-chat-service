# Erlang Chat Service
### _An Erlang/OTP based TCP Messaging server_

This is a chat server built on top of the Erlang/OTP framework. It allows for multiple
TCP connections to take place and communication happens via a very simple text protocol. 

### Features
- Supports multiple concurrent users via TCP connection
- Login and Logout, status check and list active chat rooms
- Create / Delete chat rooms, aswell as Join and Exit room
- Direct Messaging to other users
###### Still in development:
- Set room Visibility to Public or Private
- List all active users, list all room partecipants

### Available commands
| Command | Parameter (without < >) | Function |
| - | - | - |
| LOGIN:| <user_name> | Allows to login with a Username after connection |
| LOGOUT: | <user_name> | Allows to logout with a Username after connection |
| WHOAMI: | n/a | Returns a message with the Username of the current logged user |
| WHEREAMI: | n/a | Returns a message with the Room name the User is currently inside of |
| | |
| SAY: | <user_name> : <message> | Send a private message to a single User |
| | |
| LISTROOM: | n/a | Returns a message with the list of all active chat Rooms |
| NEWROOM: | <room_name> | Creates a chat Room with the name passed as Parameter |
| DELROOM: | <room_name> | Same as above, but if you are the owner of a room, it deletes it |
| JOINROOM: | <room_name> | Join a room (must be Public), takes the Room name as Parameter |
| EXITROOM: | <room_name> | Exit the Room the user is currently inside of |
| | |
| ROOM: | <room_name> : <message> | Send a message to a specific room that will be broadcasted to all Partecipants |

##### _Some examples:_
From the Client's CLI:
```
$ LOGIN:Braeburned

$ Logged in successfully OwO
```
```
$ WHOAMI:

$ You are currently logged in as: Braeburned.
```

# How to install
#### Requirements
In order to run the application you will need the following software:
| Required software | Function |
| - | - |
| Erlang/OTP 22.3 Installation | The erlang environment to run the software |
| Rebar3 | Optional. Rebar3 is required in order to generate a release |
| Git | Pretty sure you already know what this is :3 |

##### Step 1
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
##### Step 2

##### Step 3

##### Step 4

## Software structure
### OTP Supervision tree: 
<p align="left">
  <img src="https://github.com/skaysrei/docs-diagrams/blob/main/OTP%20Supervision%20tree_updated.jpg" />
</p>

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