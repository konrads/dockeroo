# Version 0.0.1
FROM ubuntu

RUN apt-get update
RUN apt-get -y install curl wget git vim lsof tcpdump iptables default-jre
RUN cd /tmp && wget http://www.scala-lang.org/files/archive/scala-2.11.7.deb
RUN cd /tmp && wget https://bintray.com/artifact/download/sbt/debian/sbt-0.13.9.deb
RUN dpkg -i /tmp/scala-2.11.7.deb
RUN dpkg -i /tmp/sbt-0.13.9.deb
