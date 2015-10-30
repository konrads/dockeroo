Dockeroo
========

Docker based tests. Organized in:
* bin - holds runnable framework scripts
* example - sample usage.  Holds docker based WS and HTTP tests
  * bin - executable scripts, utilize dockeroo bin
  * src - Dockerfiles and file system artefacts for every container
  * apps - source code for tested apps (erl/scala server/clients)

Run the example
---------------
```bash
> cd example

> # if re-running
> bin/ws_harness cluster delete
> bin/ws_harness images delete --all

> # remove dangling containers
> bin/ws_harness cluster cleanup

> # create all images, including the base ones
> bin/ws_harness images create --all

> # run tests
> bin/ws_harness test erl-ws-iptables
> # or
> bin/ws_harness http erl-http-iptables

> # generate report named 'HTTP'
> bin/ws_harness http report HTTP
> open report/report.html  # on mac
```

Legend
------
* images = docker images
* cluster = set of docker containers
* base_* images = infrastructure images, containing heavy pieces such as OTP or matplotlib, which take a while to build
* `--all` = flag to indicate deletion/creation of the base_* images
