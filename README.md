# accelR

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of accelR is to provide a simple and easy interface to processing accelerometer data for physical activity analyses.

Everything is still being developed so anything can change at anytime.

## Installation

Install my terrible code using the `devtools` package like so: 
``` r
devtools::install_github("catcrumpet/accelR")
```

## A short and painful tutorial

### Install and load the package.
```r
devtools::install_github("catcrumpet/accelR")
library(accelR)
```

### Read in accelerometer data from an ActiGraph (\*.agd) file . 
In this case, the data was collected in Los Angeles, so the timezone is set appropriately.
```r
good_data <- read_agd("catcrumpet_data.agd", tz = "America/Los_Angeles")
```

The `good_data` should look something like this:
```
# A tsibble: 1,081 x 4 [10s] <America/Los_Angeles>
   timestamp           axis1 axis2 axis3
   <dttm>              <int> <int> <int>
 1 2017-11-30 00:00:00   257   109    21
 2 2017-11-30 00:00:10   153   197   181
 3 2017-11-30 00:00:20   263   154   319
 4 2017-11-30 00:00:30   513   282   262
 5 2017-11-30 00:00:40   407   193   252
 6 2017-11-30 00:00:50   338   310   290
 7 2017-11-30 00:01:00   534   241   303
 8 2017-11-30 00:01:10   283   179   249
 9 2017-11-30 00:01:20   558   264   326
10 2017-11-30 00:01:30   505   227   245
# … with 1,071 more rows
```

The print out displays how many observations/epochs there are (`1081`) and the counts for the three axes (`axis1`, `axis2`, and `axis3`). Not all ActiGraph accelerometers collect data on three axes. Also, we can see the timezone (`America/Los_Angeles`) and the epoch length (`10s`).

Data need to pass several checks:
  1. The data is ordered by timestamp (earliest to latest).
  2. Each observation is regularly spaced by a constant time period (i.e., the epoch length).
  3. No observations are duplicates.
  4. The data spans the entire specified data collection time period.
  5. The data contains no gaps in observations.
  6. The epoch length of the data must an exact divisor of 60.

Currently there are no tools to fix these issues, but these are in development.

### Add a column for nonwear.
We will use a modified Troiano approach that is more strict with no spikes. This can be done by setting the argument `spike_tolerance = 0`.
```r
better_data <- add_nonwear_troiano(good_data, spike_tolerance = 0)
```

`better_data` now has a new column for nonwear (`nonwear`):
```
# A tsibble: 1,081 x 5 [10s] <America/Los_Angeles>
   timestamp           axis1 axis2 axis3 nonwear
   <dttm>              <int> <int> <int> <lgl>  
 1 2017-11-30 00:00:00   257   109    21 FALSE  
 2 2017-11-30 00:00:10   153   197   181 FALSE  
 3 2017-11-30 00:00:20   263   154   319 FALSE  
 4 2017-11-30 00:00:30   513   282   262 FALSE  
 5 2017-11-30 00:00:40   407   193   252 FALSE  
 6 2017-11-30 00:00:50   338   310   290 FALSE  
 7 2017-11-30 00:01:00   534   241   303 FALSE  
 8 2017-11-30 00:01:10   283   179   249 FALSE  
 9 2017-11-30 00:01:20   558   264   326 FALSE  
10 2017-11-30 00:01:30   505   227   245 FALSE  
# … with 1,071 more rows
```

### Add a column for physical activity category.
The default is to use Troiano age-based cutpoints. In this case, the person is 12 years old and the appropriate age-based cutpoints are chosen automatically.
```r
best_data <- add_pa_category(better_data, age = 12)
```

Finally, `best_data` should look something like this:
```
# A tsibble: 1,081 x 6 [10s] <America/Los_Angeles>
   timestamp           axis1 axis2 axis3 nonwear pa   
   <dttm>              <int> <int> <int> <lgl>   <ord>
 1 2017-11-30 00:00:00   257   109    21 FALSE   lig  
 2 2017-11-30 00:00:10   153   197   181 FALSE   lig  
 3 2017-11-30 00:00:20   263   154   319 FALSE   lig  
 4 2017-11-30 00:00:30   513   282   262 FALSE   mod  
 5 2017-11-30 00:00:40   407   193   252 FALSE   mod  
 6 2017-11-30 00:00:50   338   310   290 FALSE   lig  
 7 2017-11-30 00:01:00   534   241   303 FALSE   mod  
 8 2017-11-30 00:01:10   283   179   249 FALSE   lig  
 9 2017-11-30 00:01:20   558   264   326 FALSE   mod  
10 2017-11-30 00:01:30   505   227   245 FALSE   mod  
# … with 1,071 more rows
```

### Summarize the data.
This is still a work in progress. Currently, two functions are provided:
- `summarise_day`
- `summarise_window`

`summarise_day` will summarize (note the 's' in the function) the data by day automatically.

```r
day_summary <- summarise_day(best_data)
```

The results provide a lot of different values in a table, so `dplyr::glimpse` can be used to get quick idea of what's going on.
```r
dplyr::glimpse(day_summary)
```

```
Observations: 1
Variables: 20
$ date          <date> 2017-11-30
$ timestamp_min <dttm> 2017-11-30
$ timestamp_max <dttm> 2017-11-30 03:00:00
$ epoch_length  <int> 10
$ total_e       <int> 1081
$ total_m       <dbl> 180.1667
$ total_c       <int> 77687
$ valid_e       <int> 449
$ valid_m       <dbl> 74.83333
$ valid_c       <int> 0
$ sed_m         <dbl> 74.83333
$ lig_m         <dbl> 0
$ mod_m         <dbl> 0
$ vig_m         <dbl> 0
$ ext_m         <dbl> 0
$ sed_c         <dbl> 0
$ lig_c         <dbl> 0
$ mod_c         <dbl> 0
$ vig_c         <dbl> 0
$ ext_c         <dbl> 0
```

`summarise_window` is a much more specialized function which takes in 2 necessary arguments in addition to the accelerometer data: `anchor_time` and `window`.

`anchor_time` is the base time around which `window` (in minutes) operates around. `anchor_time` must be a POSIXct object, which is typically used to represent datetime objects. `lubridate::ymd_hms` can easily parse datetime strings into POSIXct objects.

`window` denotes the window around `anchor_time` in whole minutes (no fractions). `window` must be a numeric vector of length 1 or 2 and can contain negative numbers. A window of a single negative number will cover the period of those minutes prior to the `anchor_time` up to the `anchor_time`. A window of a single positive number will cover the period from the `anchor_time` to the specified minutes. A window of two numbers will cover the period from the left side to the right side.

For example, the following uses an `anchor_time` of 2017-11-30 01:30:00 (1:30 AM on November 30, 2017) in Los Angeles time and a window of 5 minutes before and 5 minutes after this time:
```r
window_summary <- 
   summarise_window(best_data, 
                    anchor_time = lubridate::ymd_hms("2017-11-30 01:30:00", 
                                                     tz = "America/Los_Angeles"), 
                    window = c(-5, 5))

dplyr::glimpse(window_summary)
```

```
Observations: 1
Variables: 25
$ anchor_time   <dttm> 2017-11-30 01:30:00
$ window_start  <dbl> -5
$ window_stop   <dbl> 5
$ window_length <int> 10
$ time_start    <dttm> 2017-11-30 01:25:00
$ time_stop     <dttm> 2017-11-30 01:35:00
$ timestamp_min <dttm> 2017-11-30 01:25:00
$ timestamp_max <dttm> 2017-11-30 01:34:50
$ epoch_length  <int> 10
$ total_e       <int> 60
$ total_m       <dbl> 10
$ total_c       <int> 694
$ valid_e       <int> 18
$ valid_m       <dbl> 3
$ valid_c       <int> 0
$ sed_m         <dbl> 3
$ lig_m         <dbl> 0
$ mod_m         <dbl> 0
$ vig_m         <dbl> 0
$ ext_m         <dbl> 0
$ sed_c         <dbl> 0
$ lig_c         <dbl> 0
$ mod_c         <dbl> 0
$ vig_c         <dbl> 0
$ ext_c         <dbl> 0
```

One final note on this, if the window period falls outside the data entirely, then the function will bypass calculating the summaries for the PA categories.

## Final notes
- I'm still learning how to document this, so references will be added in. I borrowed heavily from the work of other people and their attributions are due.