<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `accelR` is to provide a simple and easy interface to
processing accelerometer data for physical activity analyses.

Currently, only Actigraph data files are supported. These data files can
be \*.agd or \*.csv files and they must be generated by Actigraph
software.

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

## Read in accelerometer data from an ActiGraph (\*.agd) file

In this case, the data was collected in Los Angeles, so the
locale/timezone is set appropriately.

``` r
good_data <- read_agd("./inst/extdata/catcrumpet_data.agd", tz = "America/Los_Angeles")
good_data
```

    ## # A tsibble: 34,554 x 10 [10s] <America/Los_Angeles>
    ##    timestamp           axis1 axis2 axis3 steps   lux inclineoff inclinestanding
    ##    <dttm>              <int> <int> <int> <int> <int>      <int>           <int>
    ##  1 2017-11-29 16:00:00     0     0     0     0     0          0              10
    ##  2 2017-11-29 16:00:10     0     0     0     0     0          0              10
    ##  3 2017-11-29 16:00:20     0     0     0     0     0          0              10
    ##  4 2017-11-29 16:00:30     0     0     0     0     0          0              10
    ##  5 2017-11-29 16:00:40     0     0     0     0     0          0              10
    ##  6 2017-11-29 16:00:50     0     0     0     0     0          0              10
    ##  7 2017-11-29 16:01:00     0     0     0     0     0          0              10
    ##  8 2017-11-29 16:01:10     0     0     0     0     0          0              10
    ##  9 2017-11-29 16:01:20     0     0     0     0     0          0              10
    ## 10 2017-11-29 16:01:30     0     0     0     0     0          0              10
    ## # … with 34,544 more rows, and 2 more variables: inclinesitting <int>,
    ## #   inclinelying <int>

The print out displays how many observations/epochs there are (34554)
and the counts for the three axes (`axis1`, `axis2`, and `axis3`). Not
all ActiGraph accelerometers collect data on three axes. Also, we can
see the timezone/locale (America/Los\_Angeles) and the epoch length (10
seconds).

Data need to pass several checks:

1.  The data is ordered by timestamp (earliest to latest).
2.  Each observation is regularly spaced by a constant time period
    (i.e., the epoch length).
3.  No observations are duplicates.
4.  The data spans the entire specified data collection time period.
5.  The data contains no gaps in observations.
6.  The epoch length of the data must an exact divisor of multiple of
    60.

For better display of the following operations, we will remove the
unnecessary columns, preserving only the accelerometer counts columns.

``` r
# method 1, using base R
good_data <- good_data[, c("timestamp", "axis1", "axis2", "axis3")]

# method 2, using dplyr
good_data <- dplyr::select(good_data, timestamp, axis1, axis2, axis3)

# result
good_data
```

    ## # A tsibble: 34,554 x 4 [10s] <America/Los_Angeles>
    ##    timestamp           axis1 axis2 axis3
    ##    <dttm>              <int> <int> <int>
    ##  1 2017-11-29 16:00:00     0     0     0
    ##  2 2017-11-29 16:00:10     0     0     0
    ##  3 2017-11-29 16:00:20     0     0     0
    ##  4 2017-11-29 16:00:30     0     0     0
    ##  5 2017-11-29 16:00:40     0     0     0
    ##  6 2017-11-29 16:00:50     0     0     0
    ##  7 2017-11-29 16:01:00     0     0     0
    ##  8 2017-11-29 16:01:10     0     0     0
    ##  9 2017-11-29 16:01:20     0     0     0
    ## 10 2017-11-29 16:01:30     0     0     0
    ## # … with 34,544 more rows

## Accelerometer-based calculations

All accelerometer-based calculations or methods do not assume which
column of data contains the count data to be processed. Most Actigraph
accelerometers capture count data in three axes: `axis1`, `axis2`, and
`axis3`. In order for most, if not all, of the following calculations to
work, the proper axis (or magnitude column) must be provided.

### Add a column for nonwear

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
    ##  1 2017-11-29 16:00:00     0     0     0 TRUE   
    ##  2 2017-11-29 16:00:10     0     0     0 TRUE   
    ##  3 2017-11-29 16:00:20     0     0     0 TRUE   
    ##  4 2017-11-29 16:00:30     0     0     0 TRUE   
    ##  5 2017-11-29 16:00:40     0     0     0 TRUE   
    ##  6 2017-11-29 16:00:50     0     0     0 TRUE   
    ##  7 2017-11-29 16:01:00     0     0     0 TRUE   
    ##  8 2017-11-29 16:01:10     0     0     0 TRUE   
    ##  9 2017-11-29 16:01:20     0     0     0 TRUE   
    ## 10 2017-11-29 16:01:30     0     0     0 TRUE   
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

### Add a column for physical activity category

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
    ##  1 2017-11-29 16:00:00     0     0     0 TRUE    sed  
    ##  2 2017-11-29 16:00:10     0     0     0 TRUE    sed  
    ##  3 2017-11-29 16:00:20     0     0     0 TRUE    sed  
    ##  4 2017-11-29 16:00:30     0     0     0 TRUE    sed  
    ##  5 2017-11-29 16:00:40     0     0     0 TRUE    sed  
    ##  6 2017-11-29 16:00:50     0     0     0 TRUE    sed  
    ##  7 2017-11-29 16:01:00     0     0     0 TRUE    sed  
    ##  8 2017-11-29 16:01:10     0     0     0 TRUE    sed  
    ##  9 2017-11-29 16:01:20     0     0     0 TRUE    sed  
    ## 10 2017-11-29 16:01:30     0     0     0 TRUE    sed  
    ## # … with 34,544 more rows

## Accelerometer summaries

Summarizing accelerometer data is such an important feature that it
deserves its own section. The `accelR` package contains two functions
for summarizing accelerometer data: `summarise_day` and
`summarise_window`.

Note that these functions use the British/European spelling
(“summarise”) and not the American spelling (“summarize”). This is
to somewhat follow the naming convention of `dplyr` functions (e.g.,
`summarise`).

### Add a column data validity

Before data can be summarized, it must have a column for physical
activity (generated by `add_pa`) and a column of logical values for
whether the data is valid. For this example, we will define validity as
fulfilling all of the following criteria:

1.  The participant must be wearing the device (`!nonwear`).
2.  The physical activity values must be within valid ranges (for the
    included Troiano cutpoints, this means `pa != "ext"`).

These criteria are used to create a new column, `valid`.

``` r
# method 1, using base R
bestest_data <- best_data
bestest_data$valid <- !bestest_data$nonwear & bestest_data$pa != "ext"

# method 2, using base R
bestest_data <- best_data
bestest_data$valid <- with(bestest_data, !nonwear & pa != "ext")

# method 3, using dplyr
bestest_data <- dplyr::mutate(best_data, valid = !nonwear & pa != "ext")

# result
bestest_data
```

    ## # A tsibble: 34,554 x 7 [10s] <America/Los_Angeles>
    ##    timestamp           axis1 axis2 axis3 nonwear pa    valid
    ##    <dttm>              <int> <int> <int> <lgl>   <ord> <lgl>
    ##  1 2017-11-29 16:00:00     0     0     0 TRUE    sed   FALSE
    ##  2 2017-11-29 16:00:10     0     0     0 TRUE    sed   FALSE
    ##  3 2017-11-29 16:00:20     0     0     0 TRUE    sed   FALSE
    ##  4 2017-11-29 16:00:30     0     0     0 TRUE    sed   FALSE
    ##  5 2017-11-29 16:00:40     0     0     0 TRUE    sed   FALSE
    ##  6 2017-11-29 16:00:50     0     0     0 TRUE    sed   FALSE
    ##  7 2017-11-29 16:01:00     0     0     0 TRUE    sed   FALSE
    ##  8 2017-11-29 16:01:10     0     0     0 TRUE    sed   FALSE
    ##  9 2017-11-29 16:01:20     0     0     0 TRUE    sed   FALSE
    ## 10 2017-11-29 16:01:30     0     0     0 TRUE    sed   FALSE
    ## # … with 34,544 more rows

### Summarize by day

The simplest summary to perform is on the day level.

``` r
day_summary <- summarise_day(bestest_data, counts = axis1, pa = pa, valid = valid)
day_summary
```

    ## # A tibble: 5 x 20
    ## # Groups:   date [5]
    ##   date       timestamp_start     timestamp_stop      epochlength total_e total_m
    ##   <date>     <dttm>              <dttm>                    <int>   <int>   <dbl>
    ## 1 2017-11-29 2017-11-29 16:00:00 2017-11-30 00:00:00          10    2880     480
    ## 2 2017-11-30 2017-11-30 00:00:00 2017-12-01 00:00:00          10    8640    1440
    ## 3 2017-12-01 2017-12-01 00:00:00 2017-12-02 00:00:00          10    8640    1440
    ## 4 2017-12-02 2017-12-02 00:00:00 2017-12-03 00:00:00          10    8640    1440
    ## 5 2017-12-03 2017-12-03 00:00:00 2017-12-03 15:59:00          10    5754     959
    ## # … with 14 more variables: total_c <int>, valid_e <int>, valid_m <dbl>,
    ## #   valid_c <int>, sed_m <dbl>, lig_m <dbl>, mod_m <dbl>, vig_m <dbl>,
    ## #   ext_m <dbl>, sed_c <dbl>, lig_c <dbl>, mod_c <dbl>, vig_c <dbl>,
    ## #   ext_c <dbl>

This summary provides total minutes for each category (`*_m`) of
physical activity as well as an aggregate sum of activity counts by
category (`*_c`) of physical activity.

### Summarize by time window

Summarizing around a given time is more advanced but is useful when
trying to integrate accelerometer data with data from ecological
momentary assessment (EMA) studies. Data in EMA studies is typically
organized around a timestamp for when a survey is administered.
Summarizing accelerometer data in a specified window around this
timestamp can provide a metric of activity that occurred around the time
at survey.

The `summarise_window` uses two parameters to determine the window:

1.  An anchor time, which acts as the zero point (position 0).
2.  One or two numeric values to signify the relative window position
    (more details following).

The window values can take one or two positive or negative numeric
values. If one value is provided, then the window assumes that one of
the boundaries is the zero point. The following shows different window
parameters using different input values (`input_value`).

    ## # A tibble: 6 x 4
    ##   input_value window_left window_right window_length
    ##         <dbl>       <dbl>        <dbl>         <int>
    ## 1         -30         -30            0            30
    ## 2         -20         -20            0            20
    ## 3         -10         -10            0            10
    ## 4          10           0           10            10
    ## 5          20           0           20            20
    ## 6          30           0           30            30

Using two values allow specification of both left and right window
positions.

    ## # A tibble: 5 x 4
    ##   input_value window_left window_right window_length
    ##   <chr>             <dbl>        <dbl>         <int>
    ## 1 c(-10, 10)          -10           10            20
    ## 2 c(-20, 20)          -20           20            40
    ## 3 c(-30, 30)          -30           30            60
    ## 4 c(-60, -30)         -60          -30            30
    ## 5 c(30, 60)            30           60            30

Typically many timestamps are processed, so the `summarise_window`
function can process multiple anchor times and windows at once. The same
set of windows is reused for each anchor time.

``` r
# anchor time will use lubridate to convert a timestamp string to a datetime object
anchor_timestamps <- 
  lubridate::ymd_hms(c("2017-11-30 10:00:00", 
                       "2017-11-30 13:00:00"),
                     tz = "America/Los_Angeles")

windows_boundaries <- list(-30, -10, 10, 30, c(-15, 15), c(30, 60))

summarise_window(bestest_data, 
                 anchor_times = anchor_timestamps, 
                 windows = windows_boundaries,
                 counts = axis1, 
                 pa = pa, 
                 valid = valid)
```

    ## # A tibble: 12 x 25
    ##    anchor_time         window_left window_right window_length
    ##    <dttm>                    <dbl>        <dbl>         <int>
    ##  1 2017-11-30 10:00:00         -30            0            30
    ##  2 2017-11-30 10:00:00         -10            0            10
    ##  3 2017-11-30 10:00:00           0           10            10
    ##  4 2017-11-30 10:00:00           0           30            30
    ##  5 2017-11-30 10:00:00         -15           15            30
    ##  6 2017-11-30 10:00:00          30           60            30
    ##  7 2017-11-30 13:00:00         -30            0            30
    ##  8 2017-11-30 13:00:00         -10            0            10
    ##  9 2017-11-30 13:00:00           0           10            10
    ## 10 2017-11-30 13:00:00           0           30            30
    ## 11 2017-11-30 13:00:00         -15           15            30
    ## 12 2017-11-30 13:00:00          30           60            30
    ## # … with 21 more variables: window_start <dttm>, window_stop <dttm>,
    ## #   timestamp_start <dttm>, timestamp_stop <dttm>, epochlength <int>,
    ## #   total_e <int>, total_m <dbl>, total_c <int>, valid_e <int>, valid_m <dbl>,
    ## #   valid_c <int>, sed_m <dbl>, lig_m <dbl>, mod_m <dbl>, vig_m <dbl>,
    ## #   ext_m <dbl>, sed_c <dbl>, lig_c <dbl>, mod_c <dbl>, vig_c <dbl>,
    ## #   ext_c <dbl>
