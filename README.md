# bcgg
Business Charts for ggplot2

# Motivation
The purpose of bcgg is to allow R users to create charts that are commonly used in business, but not easily created within ggplot2. These charts include:
* [Waterfall charts](https://en.wikipedia.org/wiki/Waterfall_chart), useful for visualizing sources of variance between two values
* [Mosaic plots](https://en.wikipedia.org/wiki/Mosaic_plot), also known as Marimekko charts, Mekko charts, variable width bar plots, or variwide charts, useful for showing relative size of items in a two-dimensional fashion

# Installation
To install bcgg, simply paste the code into you R console:
```R
devtools::install_github("Prometheus77/bcgg")
```

If that doesn't work, check to make sure that the devtools package is installed. You can install it with:
```R
install.packages("devtools")
```

# Usage
Here is some example code that will create a simple waterfall plot:
```R
library(bcgg)

mydata <- data.frame(Reason = c("2018", "Mix shift", "PC", "Mac", "Linux", "Mobile", "2019"),
                     Clickthrough_rate_bps = c(50, -10, 2, 4, 1, 3, 0))

bc_waterfall(mydata, Reason, Clickthrough_rate_bps)
```
