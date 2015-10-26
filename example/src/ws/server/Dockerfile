# Version 0.0.1
FROM ws/base

VOLUME ["/dev/host"]
ENTRYPOINT ["erl"]
ENV LIBS=/dev/host/apps/erl/server/deps
CMD ["-pa", "/dev/host/apps/erl/server/ebin", "-s", "ws_client"]
