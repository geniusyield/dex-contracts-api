FROM debian:buster-slim

ENV LANG C.UTF-8

# common haskell + stack dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        dpkg-dev \
        git \
        gcc \
        gnupg \
        g++ \
        libc6-dev \
        libffi-dev \
        libgmp-dev \
        libnuma-dev \
        libtinfo-dev \
        make \
        netbase \
        xz-utils \
        zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

ARG CABAL_INSTALL=3.10.1.0

RUN set -eux; \
    cd /tmp; \
    ARCH="$(dpkg-architecture --query DEB_BUILD_GNU_CPU)"; \
    CABAL_INSTALL_TAR="cabal-install-$CABAL_INSTALL-$ARCH-linux-deb10.tar.xz"; \
    CABAL_INSTALL_URL="https://downloads.haskell.org/~cabal/cabal-install-$CABAL_INSTALL/$CABAL_INSTALL_TAR"; \
    curl -fSL "$CABAL_INSTALL_URL" -o cabal-install.tar.gz; \
    tar -xf cabal-install.tar.gz -C /usr/local/bin; \
    rm -rf /tmp/*; \
    cabal --version

ARG GHC=9.2.8

RUN set -eux; \
    cd /tmp; \
    ARCH="$(dpkg-architecture --query DEB_BUILD_GNU_CPU)"; \
    GHC_URL="https://downloads.haskell.org/~ghc/$GHC/ghc-$GHC-$ARCH-deb10-linux.tar.xz"; \
    curl -sSL "$GHC_URL" -o ghc.tar.xz; \
    GNUPGHOME="$(mktemp -d)"; export GNUPGHOME; \
    tar xf ghc.tar.xz; \
    cd "ghc-$GHC-$ARCH-unknown-linux"; \
    ./configure --prefix "/opt/ghc/$GHC"; \
    make install; \
    # remove profiling support to save space
    find "/opt/ghc/$GHC/" \( -name "*_p.a" -o -name "*.p_hi" \) -type f -delete; \
    rm -rf /tmp/*; \
    "/opt/ghc/$GHC/bin/ghc" --version

ENV PATH /root/.cabal/bin:/root/.local/bin:/opt/ghc/${GHC}/bin:$PATH
# ==================================[ BUILDER ]========================================

ENV LANG C.UTF-8

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        autoconf \
        automake \
        build-essential \
        chrony \
        libncursesw5 \
        liblzma-dev \
        libpq-dev \
        libssl-dev \
        libsystemd-dev \
        libtool \
        pkg-config \
        procps \
        snapd \
        wget \
        tmux && \
    rm -rf /var/lib/apt/lists/*

# Update and install wget and ca-certificates to download yq
RUN wget https://github.com/mikefarah/yq/releases/download/v4.6.1/yq_linux_amd64 -O /usr/local/bin/yq && \
    chmod +x /usr/local/bin/yq && \
    rm -rf /var/lib/apt/lists/*

# Libsodium:
RUN git clone https://github.com/input-output-hk/libsodium && \
    cd libsodium && \
    git checkout dbb48cc && \
    ./autogen.sh && \
    ./configure && \
    make && \
    make install

# Libsecp256k1:
RUN git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    git checkout ac83be33d0956faf6b7f61a60ab524ef7d6a473a && \
    ./autogen.sh && \
    ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
    make && \
    make install

ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

# ==================================[ BUILD ]========================================
WORKDIR /DEX

# TODO: first build only dependencies

COPY . .
RUN cabal update
RUN cabal build all --enable-tests --enable-benchmarks

# =============================[ SERVER ]================================
LABEL org.opencontainers.image.source="https://github.com/geniusyield/dex-contracts-api"

ENTRYPOINT ["/bin/bash", "./start.sh"]
