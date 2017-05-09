Dockeroo
========
Shell based scripts for setup and running of docker based integration functional (performance...?) tests. Consists of core scripts residing in `bin`, and best practice suggestions in `example`.  Requires [docker](https://github.com/docker/docker) only.

Philosophy
----------
This project was born out of the need to test failure scenarios (machine down|net split) in distributed environments. Will such failures cause data loss, data duplication, overall failure or eventual healing? To set this up I utilize docker for image setup and container start/stop/pause/unpause, other tools such as [docker-compose](https://github.com/docker/compose) and [weave](https://github.com/weaveworks/weave) were evaluated, but deemed overkill for this purpose. Net splits are implemented via iptables.

The main dockeroo scripts:
* `bin/harness` - facilitates operations on the docker image|container as well as forcing of net failure|healing via iptables
* `bin/test_commons.sh` - unit test like asserts, used in test assertions in the `example`

Suggested best practices are encapsulated in the `example` dir:
* `example/bin/ws_harness` - extension of the dockeroo harness, adds a concept of a 'cluster' (set of nodes used in tests), as well targets for testing, manual setup/cleanup, reporting
* `example/apps` - contains software to be tested. In real life this will be self sufficient projects, installed from external git|debian|pip|... repos.
* `example/src` - consits of Dockerfiles and their filesystem artefacts, stored in the dir structure <namespace>/<image_name>. This directory structure is used by the dockeroo `harness` and references all images with <namespace>/<image_name>. <image_name>'s prefixed with `base_` are designated for slow building, infrequently created|refreshed - as such they are enabled with the use of `--all` flag in the dockeroo scripts, eg. `bin/harness images create --all`. The flag also adds the safety against accidental erasure of such images. Note: dockeroo only supports 1 <namespace>.

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

> # make apps used in the tests, note: need erl18, rebar, scala2.11.7, sbt
> pushd apps && make && popd

> # run tests and generate reports from the docker cluster
> bin/ws_harness test erl-ws-iptables
> bin/ws_harness report WS
> open report/WS/report.html  # on mac
> # or
> bin/ws_harness test erl-http-iptables
> bin/ws_harness report HTTP
> open report/HTTP/report.html  # on mac
```

You can now view the test report in your browser:
![sample report](/../screenshots/screenshots/report.screenshot.png?raw=true "Sample Report")


Terminology
-----------
* images = docker images
* node = docker container
* cluster = set of docker containers (aka nodes)
* base_* images = infrastructure images, containing heavy pieces such as OTP or matplotlib, which take a while to build
* `--all` = flag to indicate deletion|creation of the base_* images
