FROM debian:bullseye-slim

## CONFIG ############################
ARG CABAL_VERSION=3.10.1.0
ARG CABAL_VERSION_RELEASE_KEY=A970DF3AC3B9709706D74544B3D9F94B8DCAE210
ARG GHC=9.2.8
ARG GHC_RELEASE_KEY=88B57FCF7DB53B4DB3BFA4B1588764FBE22D19C4
ARG LLVM_VERSION=12
ARG LLVM_KEY=6084F3CF814B57C1CF12EFD515CF4D18AF4F7421
######################################

ENV LANG C.UTF-8

# common haskell + stack dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        autoconf \
        automake \
        build-essential \
        ca-certificates \
        chrony \
        curl \
        dpkg-dev \
        git \
        gcc \
        gnupg \
        g++ \
        jq \
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
        make \
        netbase \
        pkg-config \
        procps \
        tmux \
        wget \
        xz-utils \
        zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

# Install Cabal
RUN set -eux; \
    cd /tmp; \
    ARCH="$(dpkg-architecture --query DEB_BUILD_GNU_CPU)"; \
    CABAL_VERSION_TAR="cabal-install-$CABAL_VERSION-$ARCH-linux-deb10.tar.xz"; \
    CABAL_VERSION_URL="https://downloads.haskell.org/~cabal/cabal-install-$CABAL_VERSION/$CABAL_VERSION_TAR"; \
    CABAL_VERSION_SHA256SUMS_URL="https://downloads.haskell.org/~cabal/cabal-install-$CABAL_VERSION/SHA256SUMS"; \
    # sha256 from https://downloads.haskell.org/~cabal/cabal-install-$CABAL_VERSION/SHA256SUMS
    case "$ARCH" in \
        'aarch64') \
            CABAL_VERSION_SHA256='d9acee67d4308bc5c22d27bee034d388cc4192a25deff9e7e491e2396572b030'; \
            ;; \
        'x86_64') \
            CABAL_VERSION_SHA256='9c89f7150a6d09e653ca7d08d22922be2d9f750d0314d9a0a7e2103fac021fac'; \
            ;; \
        *) echo >&2 "error: unsupported architecture '$ARCH'"; exit 1 ;; \
    esac; \
    curl -fSL "$CABAL_VERSION_URL" -o cabal-install.tar.gz; \
    echo "$CABAL_VERSION_SHA256 cabal-install.tar.gz" | sha256sum --strict --check; \
    \
    curl -sSLO "$CABAL_VERSION_SHA256SUMS_URL"; \
    curl -sSLO "$CABAL_VERSION_SHA256SUMS_URL.sig"; \
    GNUPGHOME="$(mktemp -d)"; export GNUPGHOME; \
    \
    tar -xf cabal-install.tar.gz -C /usr/local/bin; \
    \
    rm -rf /tmp/*; \
    \
    cabal --version

# GHC 8.10 requires LLVM version 9 - 12 on aarch64

RUN set -eux; \
    if [ "$(dpkg-architecture --query DEB_BUILD_GNU_CPU)" = "aarch64" ]; then \
        GNUPGHOME="$(mktemp -d)"; export GNUPGHOME; \
        mkdir -p /usr/local/share/keyrings/; \
        gpg --batch --keyserver keyserver.ubuntu.com --recv-keys "$LLVM_KEY"; \
        gpg --batch --armor --export "$LLVM_KEY" > /usr/local/share/keyrings/apt.llvm.org.gpg.asc; \
        echo "deb [ signed-by=/usr/local/share/keyrings/apt.llvm.org.gpg.asc ] http://apt.llvm.org/buster/ llvm-toolchain-buster-$LLVM_VERSION main" > /etc/apt/sources.list.d/llvm.list; \
        apt-get update; \
        apt-get install -y --no-install-recommends llvm-$LLVM_VERSION; \
        gpgconf --kill all; \
        rm -rf "$GNUPGHOME" /var/lib/apt/lists/*; \
    fi

RUN set -eux; \
    cd /tmp; \
    ARCH="$(dpkg-architecture --query DEB_BUILD_GNU_CPU)"; \
    GHC_URL="https://downloads.haskell.org/~ghc/$GHC/ghc-$GHC-$ARCH-deb10-linux.tar.xz"; \
    # sha256 from https://downloads.haskell.org/~ghc/$GHC/SHA256SUMS
    case "$ARCH" in \
        'aarch64') \
            GHC_SHA256='645433359d8ad9e7b286f85ef5111db1b787ee3712c24c5dfde7c62769aa59a4'; \
            ;; \
        'x86_64') \
            GHC_SHA256='10d1be25487bcf99ac8eb77beaa220c85e69f8c1106f4219ce019206ecc0ac51'; \
            ;; \
        *) echo >&2 "error: unsupported architecture '$ARCH'" ; exit 1 ;; \
    esac; \
    curl -sSL "$GHC_URL" -o ghc.tar.xz; \
    echo "$GHC_SHA256 ghc.tar.xz" | sha256sum --strict --check; \
    \
    GNUPGHOME="$(mktemp -d)"; export GNUPGHOME; \
    curl -sSL "$GHC_URL.sig" -o ghc.tar.xz.sig; \
#    gpg --batch --keyserver keyserver.ubuntu.com --receive-keys "$GHC_RELEASE_KEY"; \
#    gpg --batch --verify ghc.tar.xz.sig ghc.tar.xz; \
#    gpgconf --kill all; \
    \
    tar xf ghc.tar.xz; \
    cd "ghc-$GHC"; \
    ./configure --prefix "/opt/ghc/$GHC"; \
    make install; \
    # remove profiling support to save space
    find "/opt/ghc/$GHC/" \( -name "*_p.a" -o -name "*.p_hi" \) -type f -delete; \
    # remove some docs
    rm -rf "/opt/ghc/$GHC/share/"; \
    \
    rm -rf /tmp/*; \
    \
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

# TODO: Fix revision information [Broken revision information in the bot backend #28]
RUN git init && \
    git config --global user.email "ci@github.com" && \
    git config --global user.name "CI" && \
    git add . && \
    git commit -m "Dummy commit"
RUN cabal update
RUN cabal build all --enable-tests --enable-benchmarks

# =============================[ SERVER ]================================
LABEL org.opencontainers.image.source="https://github.com/geniusyield/dex-contracts-api"

ENTRYPOINT ["/bin/bash", "./start.sh"]
