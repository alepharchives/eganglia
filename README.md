# eGanglia
[**Ganglia Monitoring System**](http://ganglia.sourceforge.net/) for Erlang

## Overview
**eGanglia** lets users connect with the [**Ganglia Monitoring System**](http://ganglia.sourceforge.net/) using [**gmetric**](http://linux.die.net/man/1/gmetric)

## Usage
Use `gmetric:announce/X` functions to interact directly with *gmetric*.
Implement `gen_metric` behavior to define new custom metrics. Use `metric_group` to announce them. 

## Future Work
For this first version we're using gmetric for everything. Next versions should include a port for [**Ganglia's C interface**](http://sourceforge.net/apps/trac/ganglia/wiki/ganglia_gmond_c_modules)
`gen_metric` should let you post more than one metric at a time