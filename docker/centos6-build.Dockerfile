FROM nuchain-base:centos-6.8

COPY ./stack-docker.yaml /nuchain/stack.yaml
COPY ./submodules/ /nuchain/submodules/
COPY ./nuchain.cabal /nuchain/nuchain.cabal

RUN source /home/build-exports && ldconfig && cd /nuchain && stack build --only-snapshot

RUN source /home/build-exports && ldconfig && cd /nuchain && stack build --only-dependencies

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
    cd && source /home/build-exports && ldconfig && \
    cd /nuchain && \
    stack install $flag"


RUN mkdir -p /centos-6.8 && \
    cp nuchain/bin/genconfs /centos-6.8 && \
    cp nuchain/bin/nuchainserver /centos-6.8 && \
    cp nuchain/bin/nuchainclient /centos-6.8

CMD ["/bin/bash"]
