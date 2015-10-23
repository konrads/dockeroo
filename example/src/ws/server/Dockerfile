# FIXME: REMOVE - bootstrapping test

# Version 0.0.1
FROM ws/base

ADD fs/usr/bin/bootstrap.sh /usr/bin/bootstrap.sh
RUN chmod uo+x /usr/bin/bootstrap.sh

VOLUME ["/dev/host"]
ENTRYPOINT ["/usr/bin/bootstrap.sh"]
