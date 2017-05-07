# Version 0.0.1
FROM ubuntu

# add some dockeroo utils
RUN mkdir -p /opt/dockeroo
ADD fs/opt/dockeroo/dockeroo_utils.sh /opt/dockeroo/dockeroo_utils.sh
ADD fs/opt/dockeroo/folsom_query.escript /opt/dockeroo/folsom_query.escript
RUN chmod uo+x /opt/dockeroo/*
RUN ln -s /opt/dockeroo/dockeroo_utils.sh /etc/profile.d/dockeroo_utils.sh   # useful for terminal usage
RUN echo "\n. /etc/profile.d/dockeroo_utils.sh" >> /etc/bash.bashrc


# install minial linux and python packages
RUN apt-get update
RUN apt-get -y install curl wget python-dev python-pip build-essential git vim lsof tcpdump iptables openssl libncurses-dev libssl-dev
# RUN apt-get -y install libfreetype6-dev libpng12-dev pkg-config
RUN pip install --upgrade pip
RUN pip install pdbpp ipython nose
# RUN pip install numpy pandas matplotlib

# setup kerl
RUN mkdir -p /opt/kerl
RUN curl -o /opt/kerl/kerl https://raw.githubusercontent.com/spawngrid/kerl/master/kerl
RUN chmod ugo+x /opt/kerl/kerl
RUN ln -s /opt/kerl/kerl /usr/bin/kerl

# install OTP versions
# RUN . /opt/dockeroo/dockeroo_utils.sh && kerl_install R16B03
# RUN . /opt/dockeroo/dockeroo_utils.sh && kerl_install 17.5
RUN . /opt/dockeroo/dockeroo_utils.sh && kerl_install 18.1
# RUN ln -s /opt/kerl/releases/<otp_ver>/activate /etc/profile.d/kerl_erlang.sh   # done in specific containers
# ENV PATH=$PATH:/opt/kerl/releases/18.1/bin

VOLUME ["/dev/host"]
ENTRYPOINT ["bash"]
