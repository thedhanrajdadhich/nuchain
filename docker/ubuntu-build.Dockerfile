FROM nuchain-base:ubuntu-16.04

COPY ./stack-docker.yaml /nuchain/stack.yaml
COPY ./submodules/ /nuchain/submodules/
COPY ./nuchain.cabal /nuchain/nuchain.cabal

RUN cd /nuchain && stack build --only-snapshot && stack build --only-dependencies

COPY ./Setup.hs /nuchain/Setup.hs
COPY ./conf /nuchain/conf
COPY ./demo /nuchain/demo
COPY ./executables /nuchain/executables
COPY ./nuchainclient.sh /nuchain/nuchainclient.sh
COPY ./demo /nuchain/demo
COPY ./src /nuchain/src
COPY ./tests /nuchain/tests
COPY ./LICENSE /nuchain/LICENSE

ARG flag

RUN bash -c "mkdir -p /nuchain/log && \
    cd && source ./build-exports && \
    cd /nuchain && \
    stack install $flag"

RUN mkdir -p /ubuntu-16.04 && \
    cp /nuchain/bin/genconfs /ubuntu-16.04 && \
    cp /nuchain/bin/nuchainserver /ubuntu-16.04 && \
    cp /nuchain/bin/nuchainclient /ubuntu-16.04

CMD ["/bin/bash"]
