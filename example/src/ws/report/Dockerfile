# Version 0.0.1
FROM ws/base_report

# add python tools
RUN mkdir -p /opt/dockeroo
ADD fs/opt/dockeroo/*.py /opt/dockeroo/
RUN chmod uo+x /opt/dockeroo/*.py

VOLUME ["/dev/host-report"]
ENTRYPOINT ["python"]
