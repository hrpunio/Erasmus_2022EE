## CRASH COURSE in STATISTICAL CHARTING
## Prerequsites
## -------------
## start recording !
## How to start with R
## install R
## Install Rstudio
## Hadley Wickham (developer) 
## Tufte (chart guru)
## Nightingale (at the end)

##
## CROSS SECTIONAL
## Nominal

### New version
### LAND_COVER_07042022183204495.csv
### https://stats.oecd.org/Index.aspx?DataSetCode=LAND_COVER
library("tidyverse")
library(RColorBrewer)

countries <- c('POL', 'TUR', 'ALB', 'HRV', 'PSE')

d0 <- read.csv("LAND_COVER_07042022183204495.csv", stringsAsFactor=T,
              sep = ',', dec = ".",  header=T, na.string="NA");

names(d)
factor(d$VARIABLE)
levels(d$MEAS)
###

d <- d0 %>% select(COU, Country, MEAS, VARIABLE, Year, Value) %>%
  filter(Year == 2019 & MEAS == 'PCNT') %>%
  filter (COU %in% countries)

##########################
## univariate POL ########

d.pl <- d %>% filter (COU == 'POL')

## 1. Piechart
##
d.pl <- d.pl %>%
  arrange(desc(VARIABLE)) %>%
  mutate(ypos = cumsum(Value) - 0.5* Value)

mycols <- brewer.pal(12, "Set3")

p.pl.1 <- ggplot(d.pl, aes(x = "", y = Value, fill = VARIABLE)) +
  ##geom_bar(width = 1, stat = "identity", color = "white") +
  geom_bar(width = 1, position = "fill", stat="identity") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = mycols) +
  labs(fill='Land use') +
  theme_void()

p.pl.1

## 2. Piechart with numbers

p.pl.2 <- ggplot(d.pl, aes(x = "", y = Value, fill = VARIABLE)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = ypos, label = sprintf("%.1f", Value) ), color = "black")+
  scale_fill_manual(values = mycols) +
  labs(fill='Land use') +
  theme_void()
p.pl.2

## 3. Barchart (better)

p.pl.3 <- ggplot(d.pl, aes(x = reorder(VARIABLE, Value), y=Value ) ) +
  ggtitle("Land use") +
  theme(legend.position="top") +
  xlab(label="") + 
  ylab(label="") + 
  geom_bar(position="stack", stat="identity", fill='blue') +
  coord_flip()
p.pl.3

## All countries
##
d <- d %>%
  group_by(Country) %>%
  arrange(desc(VARIABLE)) %>%
  mutate(ypos = cumsum(Value) - 0.5* Value)

## W/o labels
## Pie-chart
## We assume pie-chart = population, there is no something like part of population
## or multi-population pie-chart

## 4. Piechart panel (wo numbers)
p.1 <- ggplot(d, aes(x = "", y = Value, fill = VARIABLE)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ##geom_text(aes(y = ypos, label = sprintf("%.1f", Value) ), color = "black")+
  scale_fill_manual(values = mycols) +
  labs(fill='Land use') +
  facet_wrap(~Country) +
  theme_void()
p.1

## 5. Pie chart panel (with numbers)

p.2 <- ggplot(d, aes(x = "", y = Value, fill = VARIABLE)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = ypos, label = sprintf("%.1f", Value) ), color = "black")+
  scale_fill_manual(values = mycols) +
  labs(fill='Land use') +
  facet_wrap(~Country) +
  theme_void()
p.2

## 6. Grouped barchart
p.4 <- ggplot(d, aes(x = reorder(VARIABLE, Value), y=Value, fill=Country ) ) +
  ggtitle("Land use") +
  theme(legend.position="top") +
  xlab(label="") + 
  ylab(label="") + 
  geom_bar(position="dodge", stat="identity") +
  coord_flip()
p.4

## 7. Stacked barchart
p.5 <- ggplot(d, aes(x = reorder(Country, Value), y=Value, fill=VARIABLE ) ) +
  ggtitle("Land use") +
  theme(legend.position="top") +
  xlab(label="") + 
  ylab(label="") + 
  geom_bar(position="stack", stat="identity") +
  coord_flip()
p.5

## Best:
## 8. Barchart Panel (not panel data!)
p.6 <- ggplot(d, aes(x = reorder(VARIABLE, Value), y=Value) ) +
  ggtitle("Land use") +
  theme(legend.position="top") +
  xlab(label="") + 
  ylab(label="") + 
  facet_wrap(~Country) +
  geom_bar(position="stack", stat="identity", fill='blue') +
  coord_flip()
p.6

## NUMERIC

## ########################################################################
## Population by community 2020 (gmina)
p0 <- read.csv("LUDN_2137_CREL_20220407210714.csv", 
               #colClasses = c('character', 'character', 'factor', 'factor', 'numeric', 'numeric', ));
               colClasses = c(Kod='character'),
               stringsAsFactor=F,
               sep = ';', dec = ",",  header=T, na.string="NA")  %>%
    select (Kod, Nazwa, sex=Płeć, pop=Wartosc) %>%
  filter (sex == 'ogółem');

##
## rozwody (divorces) 2020
r0 <- read.csv("LUDN_1872_CREL_20220407210059.csv", 
               #colClasses = c('character', 'character', 'factor', 'factor', 'numeric', 'numeric', ));
               colClasses = c(Kod='character'),
               stringsAsFactor=F,
               sep = ';', dec = ",",  header=T, na.string="NA") %>%
  select (Kod, divorce=Wartosc);

## Join population with divorces
## Compute ratio per 10,000
## Compute province identifier (first 2 digits of full TERYT id)
p <- left_join(p0, r0, by='Kod') %>% 
  mutate (divp = divorce/pop * 10000) %>%
  mutate (woj = substr(Kod,1,2), typ=substr(Kod, 7, 7))

summary(p)

###
### Divorces per 10 ths by province
### 9. boxplot
ggplot(p, aes(x=woj, y=divp )) + 
  geom_boxplot(fill='green') + ylab("years") + xlab("");


## Marimekko plot (Useful for **rates**)
## BTW difference between ratio and rate (in English)
## a ratio is a comparison of two numbers as a fraction a/b, 
## whereas a rate is a comparison of two quantities
## so divorces / 10000 is a rate
## pop = width ; divp = height of marimekko bars

## province aggregate
p.prov <- p %>%
  group_by(woj) %>% 
  summarise(pop=sum(pop), div=sum(divorce)) %>%
  ungroup() %>%
  mutate(divp = div/pop * 10000) %>%
  ## column with 'height' of bars has to be arranged
  arrange(desc(divp))

## Some extra computations
p.x <- p.prov %>%
  mutate ( 
  popc = cumsum(pop),
  wm = popc - pop, 
  wt = wm + (popc - wm)/2)
p99  <- ggplot(p.x, aes(ymin = 0))
p991 <- p99 + geom_rect(aes(xmin = wm, xmax = popc, ymax = divp,
                            fill = woj), color='black')
p991
p992 <- p991 + geom_text(aes(x = wt, y = divp * 0.5, label = woj)) +
  xlab("population") +
  ylab("divorces/10000") +
  ggtitle("Divorces in Poland 2020")
p992
## TERYT 14 -- mazowieckie; 12 -- małopolskie; 30 -- wielkopolskie
## 22 -- pomorskie

## Note: vertical dimension: rate / horizontal: denominator / area: total value

## 10. histogram
h1 <- ggplot(p, aes(x = divp)) + geom_histogram(binwidth = 1)
h1

## Comparison problem
## 11. panel of histograms
h2 <- ggplot(p, aes(x = divp)) + geom_histogram(binwidth = 1) +
  facet_wrap(~typ) 
h2

## 12. Kernel density (sort of histogram)
h3 <- ggplot(p, aes(x=divp)) + geom_density() +
  facet_wrap(~typ) 
h3


## TIME SERIES
## Univariate

## OECD database #######################################################
## Gdp
## Our countries
## https://data.oecd.org/ (new interface)
## https://data.oecd.org/gdp/gross-domestic-product-gdp.htm
## https://stats.oecd.org/
#
#
#
#g0 <- read.csv("DP_LIVE_08042022061028455.csv", stringsAsFactor=T,
#               sep = ',', dec = ".",  header=T, na.string="NA");
#g <- g0 %>% select(LOCATION, TIME, Value) %>%
#  filter (LOCATION %in% countries)
#
# World Bank
# https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD (total PPP)
# https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD (pc PPPP)

## Remove header manually
## GDP PPP (total)
# csvfile <- 'API_NY.GDP.MKTP.PP.CD_DS2_en_csv_v2_3732219.csv'
## GDP per capita PPP
csvfile <- 'API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_3731320.csv'

g0 <- read.csv(csvfile, stringsAsFactor=T,
        sep = ',', dec = ".",  header=T, na.string="NA") %>%
  select (code=Country.Code, name=Country.Name,  X1960:X2020) %>%
  pivot_longer(cols=starts_with("X"), names_to = 'year', values_to = 'value') %>%
  mutate (year = substr (year,2,6), year=as.numeric(year))

names(g0)

gdp.oc <- g0 %>% filter (code %in% countries)


gdp.pl <- gdp.oc %>% filter (code == 'POL')

## 13. line plot for univariate data (Time series PL)

p10 <- ggplot (gdp.pl, aes(x=year, y=value)) +
  geom_line(size=.8, color='red')
p10

## Problems/improvements:
## change scale to millions of USD
## remove NA

## 14. multiline

p11 <- ggplot (gdp.oc, aes(x=year, y=value, color=code)) +
  geom_line(size=.8)
p11

## Problems:
## in which year the difference between TUR and HRV is max?
## bw version 

## remove rows with missing values
gdp.oc.clean <- gdp.oc %>% drop_na()

## 15. bar plot (multibar version)
p12 <- ggplot (gdp.oc.clean, aes(x=year, 
                             y=value, 
                             group=code,
                             fill=code) ) +
  #geom_bar(width=.5, position=position_dodge(.6), stat="identity")
  geom_bar(position=position_dodge(), stat="identity")
p12

## Note: Software error (N/A values change bar width | should not be ZERO)
## Problems: not very readible

ggsave(plot = p12, filename = "gdpPPP.png", width=11)


## add More countries
more.countries <- c('')

## Problems: colors are indistinguishable


## BIVARIATE #######################################################
 
## Cross-sectional 

## BDL -- bank danych lokalnych
## Life expectancy at birth (F/M)
csvfile1 <- 'LUDN_2730_CREL_20220408092901.csv'
## Gdp current prices pc
csvfile2 <- 'RACH_3499_CREL_20220408095919.csv'

le0 <- read.csv(csvfile1, colClasses = c(Kod='character'),
               stringsAsFactor=F,
               sep = ';', dec = ",",  header=T, na.string="NA") %>%
  select (Kod, Nazwa, sex=Płeć, le=Wartosc, Rok, Lokalizacje) 

names(le0)

## males in urban/rural in total (Lokalizacje=='ogółem')
le <- le0 %>% filter (sex == 'mężczyźni' & Lokalizacje=='ogółem');

## GDP
gdp0 <- read.csv(csvfile2, colClasses = c(Kod='character'),
                stringsAsFactor=F,
                sep = ';', dec = ",",  header=T, na.string="NA")  %>%
  select (Kod, Nazwa, gdp=Wartosc, Rok) 

## Join LE + GDP
le.gdp <- left_join(le, gdp0, by=c('Kod', 'Rok'))

# 16. scatter-plot (GDP vs LE for polish provinces)


p20pl <- le.gdp %>% filter (Rok == 2020) %>%
  ggplot (aes(x=gdp, y=le)) +
  geom_point(size=.8, color="black") +
  geom_text(aes(y = le, label = sprintf("%2.2s", Kod) ), 
            color = "red", size=3, alpha=.45)+
  geom_smooth(method="lm")
p20pl

## absolutely no association
## Pearson correlation coefficient:
le.gdp.2020 <- le.gdp %>% filter (Rok == 2020) 
cor(le.gdp.2020$le, le.gdp.2020$gdp, method = c("pearson"))

## but
## TIME SERIES

le.gdp.pl <- le.gdp %>% filter (Kod == '0000000')

p20pl.ts <- le.gdp.pl %>%
  ggplot (aes(x=gdp, y=le)) +
  geom_point(size=.8, color="black") +
  geom_smooth(method="lm")
p20pl.ts

le.gdp.pl <- le.gdp.pl %>% drop_na()


cor(le.gdp.pl$le, le.gdp.pl$gdp, method = c("pearson"))
## Very high this time
## It is very easy to high higly correlated times-series
## but unrelated

# Another example: World bank

# lE vs GDP (gdp already read into g0 frame)
# life expectancy
# https://data.worldbank.org/indicator/SP.DYN.LE00.IN

csvfile6 <- 'API_SP.DYN.LE00.IN_DS2_en_csv_v2_3852476.csv'

## GDP PP vs life expectancy

## change wide to long
le0 <- read.csv(csvfile6, stringsAsFactor=T,
                sep = ',', dec = ".",  header=T, na.string="NA") %>%
  select (code=Country.Code, name=Country.Name,  X1960:X2020) %>%
  pivot_longer(cols=starts_with("X"), names_to = 'year', values_to = 'le') %>%
  mutate (year = substr (year,2,6), year=as.numeric(year))

## GDP PP vs life expectancy for OECD members
## get members of OECD from file
oecdCSV <- "/home/tomek/Projekty/COVID/oecd/OECD-list.csv"
oecd <- read.csv(oecdCSV, stringsAsFactor=T,
                 col.names = c('name', 'year', 'iso'),
                sep = ';', dec = ".",  header=F, na.string="NA")

library("countrycode")

## Transform ISO-2 -> ISO-3
oecd3 <- oecd %>% 
  mutate(iso3 = countrycode(iso, origin='iso2c', destination = 'iso3c'))

## list of oecd countries
oecd_coutries <- as.vector(oecd3$iso3)

## Join g0 (GDP PPP PC) and life-expectancy
gdple <- left_join (g0, le0, by=c("code", 'year')) %>%
  filter (code %in% oecd_coutries)

## there is no LE for 2020
gdple2020 <- gdple %>% filter (year == 2019)

## 17. another scatter plot
p20 <- ggplot (gdple2020, aes(x=value, y=le)) +
  geom_point(size=.8, color="black") +
  geom_text(aes(y = le, label = sprintf("%s", code) ), 
            color = "red", size=3, alpha=.45)+
  geom_smooth(method="lm")
p20

## Time series (World)
## <<TODO>>

## Spatial
library("sf")

# shapes of provinces
woj.PL <-read_sf("/home/tomek/Data/GiS", "Wojewodztwa")

# we use data (divp) from  p.prov
p.prov


p.prov.x <- left_join(woj.PL, p.prov, by=c("JPT_KOD_JE"='woj'))


r1 <- ggplot(data=p.prov.x, aes(fill=divp)) +
  geom_sf() +
  scale_fill_viridis_c()
r1

## Continous color scale (_c)
## or
## Discrete (better; _d)
r2 <- p.prov.x %>% mutate (divp_p = cut_interval(divp, 6)) %>%
  ggplot(aes(fill=divp_p)) +
  geom_sf() +
  scale_fill_viridis_d()
r2

## High-Medium-Low
r3 <- p.prov.x %>% mutate (divp_p = cut_interval(divp, 3)) %>%
  ggplot(aes(fill=divp_p)) +
  geom_sf() +
  scale_fill_viridis_d() +
  ggtitle("Divorces per 10000 in Poland (2020)") +
  labs(caption="Source: Bank Danych Lokalnych/Ludność/P1971/Rozwody i separacje")
r3

## Florence Nightingale

## ENDE :-)

################################################################################
#### ###########################################################################
################################################################################
