<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `accelR` is to provide a simple and easy interface to
processing accelerometer data for physical activity analyses.

Everything is still being developed so anything can change at anytime.
In fact, things are changing all the time.

## Installation

Installation requires the `devtools` package, install it with:

``` r
install.packages("devtools")
```

Then install `accelR`:

``` r
devtools::install_github("catcrumpet/accelR")
```

# A short and painful tutorial

## Load the package.

``` r
library(accelR)
```

## Read in accelerometer data from an ActiGraph (\*.agd) file .

In this case, the data was collected in Los Angeles, so the
locale/timezone is set appropriately.

``` r
good_data <- read_agd("./inst/extdata/catcrumpet_data.agd", tz = "America/Los_Angeles")
good_data
```

    ## # A tsibble: 34,554 x 10 [10s] <America/Los_Angeles>
    ##    timestamp           axis1 axis2 axis3 steps   lux inclineoff inclinestanding
    ##    <dttm>              <int> <int> <int> <int> <int>      <int>           <int>
    ##  1 2017-11-30 00:00:00     0     0     0     0     0          0              10
    ##  2 2017-11-30 00:00:10     0     0     0     0     0          0              10
    ##  3 2017-11-30 00:00:20     0     0     0     0     0          0              10
    ##  4 2017-11-30 00:00:30     0     0     0     0     0          0              10
    ##  5 2017-11-30 00:00:40     0     0     0     0     0          0              10
    ##  6 2017-11-30 00:00:50     0     0     0     0     0          0              10
    ##  7 2017-11-30 00:01:00     0     0     0     0     0          0              10
    ##  8 2017-11-30 00:01:10     0     0     0     0     0          0              10
    ##  9 2017-11-30 00:01:20     0     0     0     0     0          0              10
    ## 10 2017-11-30 00:01:30     0     0     0     0     0          0              10
    ## # … with 34,544 more rows, and 2 more variables: inclinesitting <int>,
    ## #   inclinelying <int>

The print out displays how many observations/epochs there are (34554)
and the counts for the three axes (`axis1`, `axis2`, and `axis3`). Not
all ActiGraph accelerometers collect data on three axes. Also, we can
see the timezone/locale (America/Los\_Angeles) and the epoch length (10
seconds).

Data need to pass several checks: 1. The data is ordered by timestamp
(earliest to latest). 2. Each observation is regularly spaced by a
constant time period (i.e., the epoch length). 3. No observations are
duplicates. 4. The data spans the entire specified data collection time
period. 5. The data contains no gaps in observations. 6. The epoch
length of the data must an exact divisor of 60.

For better display of the following operations, we will remove the
unnecessary columns, preserving only the accelerometer counts columns.

``` r
good_data <- good_data[, c("timestamp", "axis1", "axis2", "axis3")]
good_data
```

    ## # A tsibble: 34,554 x 4 [10s] <America/Los_Angeles>
    ##    timestamp           axis1 axis2 axis3
    ##    <dttm>              <int> <int> <int>
    ##  1 2017-11-30 00:00:00     0     0     0
    ##  2 2017-11-30 00:00:10     0     0     0
    ##  3 2017-11-30 00:00:20     0     0     0
    ##  4 2017-11-30 00:00:30     0     0     0
    ##  5 2017-11-30 00:00:40     0     0     0
    ##  6 2017-11-30 00:00:50     0     0     0
    ##  7 2017-11-30 00:01:00     0     0     0
    ##  8 2017-11-30 00:01:10     0     0     0
    ##  9 2017-11-30 00:01:20     0     0     0
    ## 10 2017-11-30 00:01:30     0     0     0
    ## # … with 34,544 more rows

## Accelerometer-based calculations

All accelerometer-based calculations or methods do not assume which
column of data contains the count data to be processed. Most Actigraph
accelerometers capture count data in three axes: `axis1`, `axis2`, and
`axis3`. In order for most, if not all, of the following calculations to
work, the proper axis (or magnitude column) must be provided.

### Add a column for nonwear.

The default approach for nonwear detection is the Troiano approach. We
will use a modified Troiano approach that is more strict with no spikes.
This can be done by setting the argument `spike_tolerance = 0`.

``` r
better_data <- add_nonwear(good_data, counts = axis1, algorithm = nonwear_troiano, spike_tolerance = 0)
better_data
```

    ## # A tsibble: 34,554 x 5 [10s] <America/Los_Angeles>
    ##    timestamp           axis1 axis2 axis3 nonwear
    ##    <dttm>              <int> <int> <int> <lgl>  
    ##  1 2017-11-30 00:00:00     0     0     0 TRUE   
    ##  2 2017-11-30 00:00:10     0     0     0 TRUE   
    ##  3 2017-11-30 00:00:20     0     0     0 TRUE   
    ##  4 2017-11-30 00:00:30     0     0     0 TRUE   
    ##  5 2017-11-30 00:00:40     0     0     0 TRUE   
    ##  6 2017-11-30 00:00:50     0     0     0 TRUE   
    ##  7 2017-11-30 00:01:00     0     0     0 TRUE   
    ##  8 2017-11-30 00:01:10     0     0     0 TRUE   
    ##  9 2017-11-30 00:01:20     0     0     0 TRUE   
    ## 10 2017-11-30 00:01:30     0     0     0 TRUE   
    ## # … with 34,544 more rows

The first argument is the data that is being worked on, `good_data`. The
`counts` argument is the column in `good_data` to calculate nonwear on.
In this case it is set to `axis1` and is *unquoted* (i.e., it is `axis1`
and not `"axis1"`). The algorithm used is `nonwear_troiano` and is
unquoted as it is a nonwear function in the `accelR` package. The last
argument is an additional nonwear algorithm specification. In this case,
`spike_tolerance` is set to `0` so that the algorithm looks for spans of
0s with no breaks. By default, the new column for nonwear values is
named “nonwear” but this can be changed by setting the `nonwear`
argument to some other string like `nonwear = "hello"`.

This approach, which we (the lab I work in) refer to as the “modified
Troiano” approach is so commonly used that there is a separate algorithm
for it included in the package. The following should provide the same
results.

``` r
better_data_alternate <- add_nonwear(good_data, counts = axis1, algorithm = nonwear_troiano_modified)
# test for equality (should be TRUE)
all(better_data$nonwear == better_data_alternate$nonwear)
```

    ## [1] TRUE

The name for the new nonwear column can be customized by setting the
`nonwear` argument.

### Add a column for physical activity category.

The default is to use Troiano age-based cutpoints. In this case, the
person is 12 years old and the appropriate age-based cutpoints are
chosen automatically. Again, note that the `counts` argument is the
*unquoted* name of the column we want to categorize physical activity.
By default, the new physical activity column is named “pa”. This can be
changed using the `pa` argument.

``` r
best_data <- add_pa(better_data, counts = axis1, age = 12)
best_data
```

    ## # A tsibble: 34,554 x 6 [10s] <America/Los_Angeles>
    ##    timestamp           axis1 axis2 axis3 nonwear pa   
    ##    <dttm>              <int> <int> <int> <lgl>   <ord>
    ##  1 2017-11-30 00:00:00     0     0     0 TRUE    sed  
    ##  2 2017-11-30 00:00:10     0     0     0 TRUE    sed  
    ##  3 2017-11-30 00:00:20     0     0     0 TRUE    sed  
    ##  4 2017-11-30 00:00:30     0     0     0 TRUE    sed  
    ##  5 2017-11-30 00:00:40     0     0     0 TRUE    sed  
    ##  6 2017-11-30 00:00:50     0     0     0 TRUE    sed  
    ##  7 2017-11-30 00:01:00     0     0     0 TRUE    sed  
    ##  8 2017-11-30 00:01:10     0     0     0 TRUE    sed  
    ##  9 2017-11-30 00:01:20     0     0     0 TRUE    sed  
    ## 10 2017-11-30 00:01:30     0     0     0 TRUE    sed  
    ## # … with 34,544 more rows
