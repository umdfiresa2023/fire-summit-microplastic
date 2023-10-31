
# Team Microplastic’s FIRE Summit Presentation

Sammi, Neha, Lasya, Sriya, Chayanika

## Research Question

What are the effects of the plastic bag ban on microplastic pollution in
the Chesapeake Bay? \<\<\<\<\<\<\< HEAD

=======

``` r
knitr::include_graphics("7F259009-BD11-47D9-8DE5-60E40B50B84E.jpeg")
```

![](7F259009-BD11-47D9-8DE5-60E40B50B84E.jpeg)

# \>\>\>\>\>\>\> fd548f17025832967e31fc760cb03b2394052205

What are the effects of the plastic bag ban on microplastic pollution in
the Chesapeake Bay? \>\>\>\>\>\>\>
8c0ca2b6ffc921f309d8c2a4918a3349beec47bc

How does emissions from battery recycling plants impact water pollution
in the Chesapeake Bay? ======= \>\>\>\>\>\>\>
9a136a4a8f6f3f4438a3892698c9a8f3029ad000

## Data Wrangling

**Outcome variable**

Our outcome variable is water quality from each county adjacent to the
Chesapeake Bay. Specific water parameters that we are interested in
include total nitrogen, total carbon, and total organic nitrogen.

This data is obtained from
[https://datahub.chesapeakebay.net](https://datahub.chesapeakebay.net/)
which reports water quality from each county each day.

``` r
install.packages("tidyverse")
install.packages("terra")
install.packages("simplermarkdown")
install.packages("ggplot2")

library("tidyverse")
library("terra")
library("simplermarkdown")
library("ggplot2")

df<- read.csv("WaterQualityFIPS.csv") %>%
  rename(fips=FIPS)

fips<-read.csv("state_and_county_fips_master.csv")

dfcounty<-merge(df, fips, by="fips", all.x=TRUE) %>%
  mutate(SampleDate=as.Date(SampleDate, format='%m/%d/%Y')) %>%
  mutate(Month=month(SampleDate), Year=year(SampleDate)) %>%
  group_by(Parameter, Unit, fips, name, state, Month, Year) %>%
  summarize(MeasureValue=mean(MeasureValue))
head(dfcounty)
```

    # A tibble: 6 × 8
    # Groups:   Parameter, Unit, fips, name, state, Month [1]
      Parameter Unit   fips name                state Month  Year MeasureValue
      <chr>     <chr> <int> <chr>               <chr> <dbl> <dbl>        <dbl>
    1 PIC       MG/L  24003 Anne Arundel County MD        1  2012        0.03 
    2 PIC       MG/L  24003 Anne Arundel County MD        1  2013        0.03 
    3 PIC       MG/L  24003 Anne Arundel County MD        1  2014        0.03 
    4 PIC       MG/L  24003 Anne Arundel County MD        1  2017        0.114
    5 PIC       MG/L  24003 Anne Arundel County MD        1  2018        0.03 
    6 PIC       MG/L  24003 Anne Arundel County MD        1  2020        0.03 

**Treatment variable**

Our treatment variable is an indicator of whether there is a plastic bag
ban or tax in each county. This data set came
from <https://www.bagtheban.com/in-your-state/>.

``` r
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

## Preliminary Results

``` r
cmd<-vect("Shapefiles/tl_2020_24_county10.shp")
cva<-vect("Shapefiles/tl_2020_51_county10.shp")

r<-rast("201001.nc4")
rp<-project(r[[1]], crs(cmd))

plot(rp)
plot(cmd, add=TRUE)
plot(cva, add=TRUE)
```

![](README_files/figure-commonmark/unnamed-chunk-4-1.png)

``` r
df3<- read.csv("PlasticBagLegislation.csv") %>%
  rename(name=Location) %>%
  rename(state=State) %>%
  mutate(state=ifelse(state=="Maryland", "MD", "VA")) 

df4<-df3 %>%
  mutate(monthBan=ifelse(Ban==1, Month, NA)) %>%
  mutate(yearBan=ifelse(Ban==1, Year, NA)) %>%
  mutate(monthTax=ifelse(Tax==1, Month, NA)) %>%
  mutate(yearTax=ifelse(Tax==1, Year, NA)) 

df4<-df4[,-1:-4]

df4county<-merge(df4, dfcounty, by=c("name", "state"), all.y=TRUE) %>%
  mutate(monthBan = ifelse(monthBan=="January", 1, 
                    ifelse(monthBan=="February", 2, 
                    ifelse(monthBan=="March", 3, 
                    ifelse(monthBan=="April", 4, 
                    ifelse(monthBan=="May", 5, 
                    ifelse(monthBan=="June", 6, 
                    ifelse(monthBan=="July", 7,
                    ifelse(monthBan=="August", 8,
                    ifelse(monthBan=="September", 9, 
                    ifelse(monthBan=="October", 10, 
                    ifelse(monthBan=="November", 11, 
                    ifelse(monthBan=="December", 12, NA))))))))))))) %>%
  mutate(monthTax = ifelse(monthTax=="January", 1, 
                    ifelse(monthTax=="February", 2, 
                    ifelse(monthTax=="March", 3, 
                    ifelse(monthTax=="April", 4, 
                    ifelse(monthTax=="May", 5, 
                    ifelse(monthTax=="June", 6, 
                    ifelse(monthTax=="July", 7,
                    ifelse(monthTax=="August", 8,
                    ifelse(monthTax=="September", 9, 
                    ifelse(monthTax=="October", 10, 
                    ifelse(monthTax=="November", 11, 
                    ifelse(monthTax=="December", 12, NA))))))))))))) %>%
  mutate(Ban = ifelse((Year > yearBan), 1, 0)) %>%
  mutate(Ban = ifelse((Year == yearBan & Month>monthBan), 1, Ban)) %>%
  mutate(Tax = ifelse((Year > yearTax), 1, 0)) %>%
  mutate(Tax = ifelse((Year == yearTax & Month>monthTax), 1, Tax))

md_nasa_data <- read.csv("md_nasa_data.csv")
va_nasa_data <- read.csv("va_nasa_data.csv")

#Renaming the names column in order to merge the datasets
md_nasa_data <- md_nasa_data %>%
  rename(name = NAMELSAD10)

va_nasa_data <- va_nasa_data %>%
  rename(name = NAMELSAD10)

# Merging with state_and_county_fips_master to get the state column
md_nasa <- merge(md_nasa_data, fips, by.x = "name")

va_nasa <- merge(va_nasa_data, fips, by.x = "name")

# Creating the month column using the file column
md_nasa <- md_nasa %>%
  mutate(Month = as.numeric(substr((md_nasa$file),5,6)))
va_nasa <- va_nasa %>%
  mutate(Month = as.numeric(substr((va_nasa$file),5,6)))

# Creating the year column
md_nasa <- md_nasa %>%
  mutate(Year = as.numeric(substr((md_nasa$file),1,4)))
va_nasa <- va_nasa %>%
  mutate(Year = as.numeric(substr((va_nasa$file),1,4)))

# Combining va_nasa and md_nasa
all <- rbind(md_nasa, va_nasa)

finaldf <- merge(df4county, all, by=c("name","state", "Month", "Year", "fips"), all.x = TRUE) %>%
  filter(!is.na(Evap_tavg)) %>%
  filter(!is.na(Qs_tavg)) %>%
  filter(!is.na(Rainf_f_tavg)) %>%
  filter(!is.na(Tair_f_tavg))
```

``` r
# Make a df with the areas with ban, data on before and after water quality(Chesapeake hub bay water)

before_after <- finaldf %>%
  # Getting rid of counties that do not have a ban
  filter(!is.na(monthBan) & !is.na(yearBan)) %>%
  
  #Creating a before column and after column
  mutate(before = ifelse(Year < yearBan & Month < monthBan, finaldf$MeasureValue, NA)) %>%
  mutate(after = ifelse(Year >= yearBan & Month >= monthBan, finaldf$MeasureValue, NA))
  

beforeAverages <- before_after %>%
  filter(!is.na(before)) %>%
  filter(Parameter == "PIC") %>%
  mutate(beforeMean = mean(before))

afterAverages <- before_after %>%
  filter(!is.na(after)) %>%
  filter(Parameter == "PIC") %>%
  mutate(afterMean = mean(after))

dfToGraph <- merge(beforeAverages, afterAverages, by="name", all.x = TRUE)
```