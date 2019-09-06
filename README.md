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

Add a column for nonwear, we will use a modified Troiano approach that is more strict (i.e., no spikes).
```R
better_data <- add_nonwear_troiano(good_data, spike_tolerance = 0)
```

Add a column for physical activity category. The default is to use Troiano cutpoints. In this case, the person is 12 years old.
```R
best_data <- add_pa_category(better_data, age = 12)
```
