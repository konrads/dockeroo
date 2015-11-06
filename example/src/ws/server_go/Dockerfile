# Version 0.0.1
FROM ws/base_go

# install a sample http server, as in http://thenewstack.io/building-a-web-server-in-go/
ADD fs/tmp /tmp/
RUN go install github.com/user/http_server

EXPOSE 8888
VOLUME ["/dev/host"]
ENTRYPOINT ["/tmp/server_go/bin/http_server"]
