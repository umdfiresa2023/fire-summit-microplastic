Team Name’s FIRE Summit Presentation
================
Team Members

## Research Question

Write research question here.

(Optional) Insert an image if it helps motivate the research question.

## Data Wrangling

**Outcome variable**

Write description, data source, and wrangling methodology. Show a map if
you have made one.

If you have loops, do not run the entire thing. Instead, run one sample
loop or just display your codes without making it run by adding **\#\|
eval: false** at the top of the chunk.

You can also suppress warnings by adding **\#\| warning: false**.

``` r
library("tidyverse")
library("terra")
library("simplermarkdown")
```

**Treatment variable**

Write description, data source, and wrangling methodology. Show a map if
you have one.

**Control variables**
```{r}
cmd<-vect("Shapefiles/tl_2020_24_county10.shp")
cva<-vect("Shapefiles/tl_2020_51_county10.shp")

r<-rast("201001.nc4")
rp<-project(r[[1]], crs(cmd))

plot(rp)
plot(cmd, add=TRUE)
plot(cva, add=TRUE)
```
![image](https://github.com/umdfiresa2023/fire-summit-microplastic/assets/125389355/d4114d00-7e8d-4b7d-b2ef-db0c4adef010)

To take into account total precipitation and average storm-water runoff
data we used data from NASA Earth Data. Our data came from GLDAS Noah
Land Surface Model.


## Preliminary Results

Display a figure showing how the treatment variable impacted the outcome
variable.
