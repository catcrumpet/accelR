# accelR

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of accelR is to provide a simple and easy interface to processing accelerometer data for physical activity analyses.

Everything is still being developed so anything can change at anytime.

In fact major things have changed.

## Installation

Install my terrible code using the `devtools` package like so: 
``` r
devtools::install_github("catcrumpet/accelR")
```

# A short and painful tutorial

## Install and load the package.
```r
devtools::install_github("catcrumpet/accelR")
library(accelR)
```

## Read in accelerometer data from an ActiGraph (\*.agd) file . 
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

## Accelerometer-based calculations
All accelerometer-based calculations or methods \[now\] do not assume which column of data contains the count data to be processed. Most Actigraph accelerometers capture count data in three axes: `axis1`, `axis2`, and `axis3`. In order for most, if not all, of the following calculations to work, the proper axis (or magnitude column) must be provided.

### Add a column for nonwear.
The default approach for nonwear detection is the Troiano approach. We will use a modified Troiano approach that is more strict with no spikes. This can be done by setting the argument `spike_tolerance = 0`.
```r
better_data <- add_nonwear(good_data, axis1, spike_tolerance = 0)
```
Note that the first argument is the dataset that is being worked on, `good_data`. The second argument, `axis1`, is the column to calculate nonwear for. Also note that `axis1` is unquoted (i.e., it is `axis1` and not `"axis1"`), this is by design to make it more convenient to use. Most functions in this package should be able to accept unquoted arguments as the column to work on. If this bothers you, functions should also be able to accept quoted arguments as well.

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
The name for the new nonwear column can be customized by setting the `nonwear` argument.

### Add a column for physical activity category.
The default is to use Troiano age-based cutpoints. In this case, the person is 12 years old and the appropriate age-based cutpoints are chosen automatically.
```r
best_data <- add_pa(better_data, axis1, age = 12)
```
Again, note that the second argument is the unquoted name of the column we want to categorize physical activity.

Now, we have a new column of physical activity category data with the default name of "`pa`".
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
