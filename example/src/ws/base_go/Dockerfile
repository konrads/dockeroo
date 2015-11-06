# Version 0.0.1
FROM ubuntu

RUN apt-get update
RUN apt-get -y install curl wget git vim lsof tcpdump iptables golang
RUN mkdir /tmp/server_go
WORKDIR /tmp/server_go
ENV GOPATH /tmp/server_go
ENV PATH $PATH:/tmp/server_go/bin

# install go tools
RUN go get -u github.com/motemen/gore
RUN go get -u github.com/nsf/gocode
RUN go get -u github.com/k0kubun/pp # or github.com/davecgh/go-spew

ENTRYPOINT ["bash"]
