# ggbiplot

Function to create biplots with ggplot2. To load it in an R session, source this script by running the following code:

```r
 devtools::source_url("https://raw.githubusercontent.com/EdwinTh/ggbiplot/master/ggbiplot.R")
```

# Example

```r
ggpbiplot(prcomp(mtcars), components = c(3, 4))
```

