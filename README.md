# eGanglia
[**Ganglia Monitoring System**](http://ganglia.sourceforge.net/) for Erlang

## Overview
**eGanglia** lets users connect with the [**Ganglia Monitoring System**](http://ganglia.sourceforge.net/) using [**gmetric**](http://linux.die.net/man/1/gmetric) or [**Ganglia's C interface**](http://sourceforge.net/apps/trac/ganglia/wiki/ganglia_gmond_c_modules).

## Usage
Use `gmetric:announce/X` functions to interact directly with *gmetric*.
Use `gmetric:announce_every/X` functions to start a `gen_server` that periodically announces metrics using *gmetric*.
Implement `gen_metric` behavior to define new custom metrics. 
