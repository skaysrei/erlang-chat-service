# Clones OpenSSL source
cd $HOME/Downloads
git clone https://github.com/openssl/openssl
cd openssl
git checkout OpenSSL_1_1_1b

# Creates a build in 'HOME/Apps/openssl/1.1.1b'
./Configure shared --prefix=$HOME/Apps/openssl/1.1.1b  --openssldir=$HOME/Apps/openssl/1.1.1b linux-x86_64
make
make install

# Disables Java compiler && sets the updated OpenSSL path for the shell
export KERL_CONFIGURE_OPTIONS="--without-javac --with-ssl=$HOME/Apps/openssl/1.1.1b"

# Executes Kerl build -> [OTP build version] [target folder name]
kerl build 22.3 erlang-22.3