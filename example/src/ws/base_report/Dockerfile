# Version 0.0.1
FROM ubuntu:latest

# add plot_csv
RUN mkdir -p /opt/dockeroo
ADD fs/opt/dockeroo/plot_csv.py /opt/dockeroo/plot_csv.py
RUN chmod uo+x /opt/dockeroo/*

# install minial linux and python packages
RUN apt-get update
RUN apt-get -y install curl wget vim python-dev python-pip build-essential libfreetype6-dev libpng12-dev pkg-config
RUN pip install pdbpp ipython nose numpy pandas matplotlib

VOLUME ["/dev/host-report"]
ENTRYPOINT ["/opt/dockeroo/plot_csv.py"]
