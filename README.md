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

# TODO: Implement Protobuf middleware to manage Client-server comms
