# eGanglia
[**Ganglia Monitoring System**](http://ganglia.sourceforge.net/) for Erlang

## Overview
**eGanglia** lets users connect with the [**Ganglia Monitoring System**](http://ganglia.sourceforge.net/) using [**gmetric**](http://linux.die.net/man/1/gmetric)

## Usage
Use `gmetric:announce/X` functions to interact directly with *gmetric*.
Implement `gen_metric` behavior to define new custom metrics. Use `metric_group` to announce them.
Start the application using `eganglia:start/0` to start the metric groups defined in the application configuration

## Configuration
**eganglia** uses Erlang config files to set its environmental variables. The allowed options are:

* `groups :: [metric_group()]`: Default metric groups to load on application start.
                                Each metric_group is represented by a tuple that may have one of the following formats:
  * `{Name, CollectEvery, TimeThreshold, Metrics}`
  * `{CollectEvery, TimeThreshold, Metrics}`
  * `{CollectEvery, Metrics}`
  These parameters behave as follows:
  * `Name :: {local|global, atom()}`: The metric group process will be registered with this name
  * `CollectEvery :: once | pos_integer()`: Metrics will be collected either once or every *CollectEvery* seconds
  * `TimeThreshold :: pos_integer()`: If no *ValueThreshold* is triggered within *TimeThreshold* seconds, the entire metric group is sent. The default value for this field is 3600
  * `Metrics :: [metric()]`: List of metrics to include in the group. Each metric is represented by a tuple that may have one of the following formats:
      * `{Module, InitArgs}`
      * `{Name, Module, InitArgs}`
      * `{Name, Module, InitArgs}`
      * `{Name, Module, InitArgs, Options}`
      * `{Name, Module, InitArgs, Options, Threshold}`
    
    The parameters behave exactly as in `metric_group:add_metric/6`
    To fully understand each parameter, you should check the original [Ganglia Configuration docs](http://sourceforge.net/apps/trac/ganglia/wiki/Gmond%203.1.x%20General%20Configuration#collection_group). 

## Future Work
For this first version we're using gmetric for everything. Next versions should include a port for [**Ganglia's C interface**](http://sourceforge.net/apps/trac/ganglia/wiki/ganglia_gmond_c_modules)
`gen_metric` should let you post more than one metric at a time