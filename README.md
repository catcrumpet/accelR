# accelR

Install my terrible code using the `devtools` package like so: `devtools::install_github("catcrumpet/accelR")`

## A short and painful tutorial

Install and load the package.
```R
devtools::install_github("catcrumpet/accelR")
library(accelR)
```

Read in accelerometer data from an Actigraph device (i.e., a \*.agd file). In this case, the data was collected in Los Angeles, so the timezone is set appropriately.
```R
good_data <- read_agd("catcrumpet_data.agd", tz = "America/Los_Angeles")
```

The data should look something like this:
```
# A tsibble: 34,554 x 4 [10s] <America/Los_Angeles>
   timestamp           axis1 axis2 axis3
   <dttm>              <int> <int> <int>
 1 2017-11-29 16:00:00     0     0     0
 2 2017-11-29 16:00:10     0     0     0
 3 2017-11-29 16:00:20     0     0     0
 4 2017-11-29 16:00:30     0     0     0
 5 2017-11-29 16:00:40     0     0     0
 6 2017-11-29 16:00:50     0     0     0
 7 2017-11-29 16:01:00     0     0     0
 8 2017-11-29 16:01:10     0     0     0
 9 2017-11-29 16:01:20     0     0     0
10 2017-11-29 16:01:30     0     0     0
# … with 34,544 more rows
```

Add a column for nonwear, we will use a modified Troiano approach that is more strict (i.e., no spikes).
```R
better_data <- add_nonwear_troiano(good_data, spike_tolerance = 0)
```

Add a column for physical activity category. The default is to use Troiano cutpoints. In this case, the person is 12 years old.
```R
best_data <- add_pa_category(better_data, age = 12)
```
