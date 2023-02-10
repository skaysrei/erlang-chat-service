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

## [Chat Service Client](https://github.com/skaysrei/chat-service-client)
Here you will find the repository of the `client` you can user for testing!

### Available commands
| Command | Parameter (no ' ') | Function |
| - | - | - |
| LOGIN:| 'user_name' | Allows to login with a Username after connection |
| LOGOUT: | 'user_name' | Allows to logout with a Username after connection |
| WHOAMI: | n/a | Returns a message with the Username of the current logged user |
| WHEREAMI: | n/a | Returns a message with the Room the User is currently inside of |
| | |
| SAY: | 'user_name' : 'message' | Send a private message to a single User |
| | |
| LISTROOM: | n/a | Returns a message with the list of all active chat Rooms |
| NEWROOM: | 'room_name' | Creates a chat Room with the name passed as Parameter |
| DELROOM: | 'room_name' | Same as above, but if you are the owner of a room, it deletes it |
| JOINROOM: | 'room_name' | Join a room (must be Public), takes the Room name as Parameter |
| EXITROOM: | 'room_name' | Exit the Room the user is currently inside of |
| SETPRIVATE: | 'room_name' | Sets the visibility of a room to private |
| SETPUBLIC: | 'room_name' | Sets the visibility of a room to public |
| INVITEROOM: | 'room_name' : 'user_name' | Invites another user to a private room |
| | |
| ROOM: | 'room_name' : 'message' | Send a message to a specific room that will be broadcasted to all Partecipants |
| | |
| !exit | n/a | Terminates the connection |

##### _Some examples:_
From the Client's CLI:
```
$ LOGIN:Seasalt

$ Logged in successfully OwO


$ SAY:Seasalt:Hey! What's puppin?

$ direct@Seasalt: Hey! What's puppin?
```
```
$ WHOAMI:

$ You are currently logged in as: Brae.
```

> By default the server starts on port `1337`, make sure its available. The client will run on port 1337 aswell 
> but can be configured for a different `IP` and `PORT` on startup.

# How to install
#### Requirements
In order to run the application you will need the following software:
| Required software | Function |
| - | - |
| [Erlang/OTP 22.3](https://www.erlang.org/patches/otp-22.3) | The erlang environment to run the software. You can use [Kerl](https://github.com/kerl/kerl) to make this process easier |
| [rebar3 3.20](https://rebar3.org/docs/getting-started/) | Optional. Rebar3 is required in order to generate a release |
| [Git](https://git-scm.com/) | Pretty sure you already know what this is :3 |

#### Set up your Erlang/OTP instance with Kerl
Kerl is a build tool which will allow us to set up a specific install version of Erlang/OTP instances. 
You will need a working Erlang/OTP 22.3 install on your machine, one easy way to get it is by using Kerl. [Learn more here](https://github.com/kerl/kerl)
> In case your build fails on newer versions of linux, check your `OpenSSL` package version, it comes preloaded on 
> most distros and it needs to be of version `<= 1.1.1` to successfully build. In the `./scripts` folder you will
> find a bash script you can use to build and install `OpenSSL 1.1.1b` (which has support for TLSv1.3) and also
> automagically run the kerl build script!

#### Install rebar3
rebar3 provides a lot of options for managing Erlang/OTP projects, and in our case specifically, will be used to create a release awe can run. [Learn more here](https://rebar3.org/docs/getting-started/)

#### Clone the repository
```
git clone https://github.com/skaysrei/erlang-chat-service.git
```

#### Build a release and try it out!
You can now compile to bytecode and create a release by running:
```
rebar3 compile
```
```
rebar3 release
```
Finally use the [Extended Start Script](https://rebar3.org/docs/deployment/releases/#extended-start-script) to run it.
You can visit the web page to see the available commands, for our purpouses `console` will be sufficent, 
running the app in `console` mode will give us an interactive shell to keep an eye on the server stdout.
```
./_build/default/rel/chat_service/bin/chat_service console
```
> Do note: Remember to make sure your `Erlang` instance is active and the `rebar3` path is set in the shell, the Script will need it to start the app.

Install git:
sudo apt install git-all


kerl config option ssl 1.1 on older otp builds (<24.2)
export KERL_CONFIGURE_OPTIONS="--without-javac --with-ssl=/usr/local/opt/openssl@1.1"

<br></br>

## Software structure
### OTP Supervision tree: 
<p align="left">
  <img src="https://github.com/skaysrei/docs-diagrams/blob/main/OTP%20Supervision%20tree_updated.jpg" width="800"/>
</p>

### Dataflow logic: 
<p align="left">
  <img src="https://github.com/skaysrei/docs-diagrams/blob/main/Untitled%20Diagram.jpg" width="800"/>
</p>

<br></br>
<br></br>

## Known issues:
1. If the service gets terminated, especially while one or more clients are still 
connected, it might take a few more seconds for the sockets to free up. 
Wait ~1 minute before restarting the service.

2. translation_layer worker not closing when connection closes because it is not
sending an exit signal. Sending it manually now but needs a rework. Also error
when connecting multiple clients, service already started.
(FIXED: Changed start_link/4 to start_link/3, issue was duplicate local name when
the server tried to start a new translation_layer worker). Maybe remove header?

3. Currently there is a bug when the header that the client receives is sometimes
doubled. This happens at random and the current workaround is implemented in the
client via trimming the prefix twice. Needs further investigation. 


> Do note: A crash event on the `chat_controller` may cause the server to lose its state.
> If a crash occours at this time it is recommended to restart the client and `login` again.

## TODO (Short with time):
1. Refactor, refactor, refactor again... code redundancy can be improved, common action
such as state fetch and update could be grouped togheter (within chat_controller), and replace the cast functions
with custom wrapped ones that are shorte maybe. Especially when updating user state such as room and login position,
it would make sure the state in the translation_layer gets refreshed every time :3c neat uh?

2. TODO: When user disconnects the chat_controller state keeps holding onto its data,
should be an easy fix. Implement cleanup function. 

<br></br>

---

### Future development plans:
-  ##### Implement Protobuf communication option on translation_layer
-  ##### Implement RedisDB message storage and caching

<br></br>

### References used during development: 

[Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/content)

[The ABCs of OTP - Jesse J. Anderson](https://www.youtube.com/watch?v=4SCwubzqsVU)

[Erlang Master Class 1: Functional Programming](https://youtube.com/playlist?list=PLR812eVbehlwEArT3Bv3UfcM9wR3AEZb5)

[Erlang Master Class 2: Concurrent Programming](https://youtube.com/playlist?list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn)

[Erlang Master Class 3: OTP Behaviours and Releases](https://youtube.com/playlist?list=PLR812eVbehlx6vgWGf2FLHjkksAEDmFjc)

[The simplest Erlang TCP server ever](https://dmathieu.com/articles/development/erlang-tcp-server/)

[erl-chat-server: A simple chat server written in Erlang](https://github.com/luisgabriel/erl-chat-server)

### Useful related documentation: 

[Erlang/OTP Development Environment for concurrent programming](https://www.erlang.org/doc/)

[Kerl, Easy building and installing of Erlang/OTP instances](https://github.com/kerl/kerl)

[Rebar3, The official build tool for Erlang](https://rebar3.org/docs/)

[Erlang/OTP: gen_server](https://www.erlang.org/doc/man/gen_server.html)

[Erlang/OTP: gen_tcp](https://www.erlang.org/doc/man/gen_tcp.html)
