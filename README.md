# A concurrent Erlang chat service: 
### OTP Supervision tree: 

<p align="center">
  <img src="https://github.com/skaysrei/docs-diagrams/blob/main/OTP%20Supervision%20tree.jpg" />
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

# rebar3 version 3.20.0

### TODO: Implement Protobuf communication
### TODO: Implement RedisDB message storage
<br></br>
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
<br></br>
### Useful general documentation: 

[Erlang/OTP Development Environment for concurrent programming](https://www.erlang.org/doc/)

[Kerl, Easy building and installing of Erlang/OTP instances](https://github.com/kerl/kerl)

[Rebar3, The official build tool for Erlang](https://rebar3.org/docs/)