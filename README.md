---
editor_options: 
  markdown: 
    wrap: 72
---

# Team Microplastic's FIRE Summit Presentation

Sammi, Neha, Lasya, Sriya, Chayanika

## Research Question

What are the effects of the plastic bag ban on microplastic pollution in
the Chesapeake Bay?

How does emissions from battery recycling plants impact water pollution
in the Chesapeake Bay?

## Data Wrangling

**Outcome variable**

Our outcome variable is water quality from each county adjacent to the
Chesapeake Bay. Specific water parameters that we are interested in
include total nitrogen, total carbon, and total organic nitrogen.

This data is obtained from
[https://datahub.chesapeakebay.net](https://datahub.chesapeakebay.net/)
which reports water quality from each county each day.

```{r}
#| warning: false
install.packages("tidyverse")
install.packages("terra")
install.packages("simplermarkdown")
install.packages("ggplot2")

library("tidyverse")
library("terra")
library("simplermarkdown")
library("gglopt2")

df<- read.csv("WaterQualityFIPS.csv") %>%
  rename(fips=FIPS)

fips<-read.csv("state_and_county_fips_master.csv")

dfcounty<-merge(df, fips, by="fips", all.x=TRUE) %>%
  mutate(SampleDate=as.Date(SampleDate, format='%m/%d/%Y')) %>%
  mutate(Month=month(SampleDate), Year=year(SampleDate)) %>%
  group_by(Parameter, Unit, fips, name, state, Month, Year) %>%
  summarize(MeasureValue=mean(MeasureValue))
```

**Treatment variable**

Our treatment variable is an indicator of whether there is a plastic bag
ban or tax in each county. This data set came
from <https://www.bagtheban.com/in-your-state/>.

```{r}
#| warning: false

library("tidyverse")
library("terra")
library("simplermarkdown")

df2<- read.csv("PlasticBagLegislation.csv") %>%
  rename(name=Location) %>%
  rename(state=State) %>%
  mutate(state=ifelse(state=="Maryland", "MD", "VA")) %>%
  mutate(monthBan=ifelse(Ban==1, Month, "N/A")) %>%
  mutate(yearBan=ifelse(Ban==1, Year, "N/A")) %>%
  mutate(monthTax=ifelse(Tax==1, Month, "N/A")) %>%
  mutate(yearTax=ifelse(Tax==1, Year, "N/A"))
```

**Control variables**

To take into account total precipitation and average storm-water runoff
data we used data from NASA Earth Data. Our data came from GLDAS Noah
Land Surface Model.
```{r}
cmd<-vect("Shapefiles/tl_2020_24_county10.shp")
cva<-vect("Shapefiles/tl_2020_51_county10.shp")

r<-rast("201001.nc4")
rp<-project(r[[1]], crs(cmd))

plot(rp)
plot(cmd, add=TRUE)
plot(cva, add=TRUE)
```
![image](https://github.com/umdfiresa2023/fire-summit-microplastic/assets/125389355/460000a6-defc-4a82-b374-34dee5bb72e7)


## Preliminary Results


