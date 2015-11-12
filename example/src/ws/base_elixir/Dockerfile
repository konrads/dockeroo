# Version 0.0.1
FROM ws/base_erl

RUN locale-gen en_US.UTF-8
RUN update-locale LC_ALL=en_US.UTF-8

ENV LANG=en_US.UTF-8
ENV PATH=$PATH:/opt/kerl/releases/18.1/bin:/opt/elixir/bin

RUN cd /opt && git clone https://github.com/elixir-lang/elixir.git
RUN cd /opt/elixir && make clean test

VOLUME ["/dev/host"]
ENTRYPOINT ["bash"]
