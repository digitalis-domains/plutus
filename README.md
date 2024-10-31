# Digitalis Plutus Smart Contracts

## Prepare Environment

Install dependencies (apt-get)

```bash
sudo apt-get update
sudo apt-get install -y --no-install-recommends \
    autoconf \
    automake \
    build-essential \
    ca-certificates \
    chrony \
    dpkg-dev \
    gcc \
    gnupg \
    g++ \
    hlint \
    libc6-dev \
    libncursesw5 \
    libffi-dev \
    libgmp-dev \
    liblzma-dev \
    libnuma-dev \
    libpq-dev \
    libssl-dev \
    libsystemd-dev \
    libtinfo-dev \
    libtool \
    netbase \
    pkg-config \
    procps \
    tmux \
    xz-utils \
    zlib1g-dev
```

Install LIBSODIUM

```bash
git clone https://github.com/input-output-hk/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install
sudo ldconfig
```

Install SEPC256K1

```bash
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git checkout ac83be33d0956faf6b7f61a60ab524ef7d6a473a
./autogen.sh
./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental
make
sudo make install
sudo ldconfig
```

Install BLST

```bash
: ${BLST_VERSION:='v0.3.11'}
git clone --depth 1 --branch ${BLST_VERSION} https://github.com/supranational/blst
cd blst
./build.sh
cat > libblst.pc << EOF
prefix=/usr/local
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: Multilingual BLS12-381 signature library
URL: https://github.com/supranational/blst
Version: ${BLST_VERSION#v}
Cflags: -I\${includedir}
Libs: -L\${libdir} -lblst
EOF
sudo cp libblst.pc /usr/local/lib/pkgconfig/
sudo cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp  /usr/local/include/
sudo cp libblst.a /usr/local/lib
sudo chmod u=rw,go=r /usr/local/{lib/{libblst.a,pkgconfig/libblst.pc},include/{blst.{h,hpp},blst_aux.h}}
```

Also The [CI workflow](https://github.com/geniusyield/atlas/blob/fe204293b9a72b83cfc0e9789c9f1221e8753e6a/.github/workflows/haskell.yml) of Atlas project can be helpful to guide through preparing the environment as well.

## Build the Project

Installing the Haskell environment

```bash
ghcup install ghc 9.6.5
ghcup install cabal 3.10.1.0
ghcup set ghc 9.6.5
ghcup set cabal 3.10.1.0
```

To check that you will use the GHCup tools (and not any other installation on the system), you can execute

```bash
which cabal
```

Use Cabal to build the project:

    ```sh
    cd digitalis-contracts/
    cabal build
    ```