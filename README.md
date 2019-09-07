# accelR

Install my terrible code using the `devtools` package like so: 
```R
devtools::install_github("catcrumpet/accelR")
```

## A short and painful tutorial

### Install and load the package.
```R
devtools::install_github("catcrumpet/accelR")
library(accelR)
```

### Read in accelerometer data from an Actigraph device (i.e., a \*.agd file). In this case, the data was collected in Los Angeles, so the timezone is set appropriately.
```R
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

### Add a column for nonwear, we will use a modified Troiano approach that is more strict (i.e., no spikes).
```R
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

### Add a column for physical activity category. The default is to use Troiano cutpoints. In this case, the person is 12 years old.
```R
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

## Final notes
- I'm still learning how to document this, so references will be added in. I borrowed heavily from the work of other people and their attributions are due.
- Using the tidy verbs `mutate` or `select` will remove the necessary attribute data necessary for some of the calculations. If you need to run `mutate`, you can use the internal function `accelR:::mutate_acc_` which will preserve these attributes. This is not recommended, however.
