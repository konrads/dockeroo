# Version 0.0.1
FROM ws/base_erl

RUN ln -s /opt/kerl/releases/18.1/activate /etc/profile.d/kerl_erlang.sh   # done in specific containers
ENV PATH=$PATH:/opt/kerl/releases/18.1/bin

VOLUME ["/dev/host-client"]
ENV ERL_LIBS=/dev/host-client/deps
ENTRYPOINT ["erl", "-pa", "/dev/host-client/ebin", "-noshell", "-run"]
