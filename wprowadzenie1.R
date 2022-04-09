## R
## Minimalistic introduction

## vectors
wektor <- c(2, 4, 5)

?seq 

tez.wektor <- seq(0, 10, by=1)

length(tez.wektor)

## More vectors
##  https://www.forbes.com/billionaires/
## Wektors napisów (character vector)

milioner <- c('Jeff Bezos', 'Bill Gates', 'Bernard Arnault', 'Warren Buffett', 'Larry Ellison',
"Amancio Ortega", "Mark Zuckerberg", "Jim Walton", "Alice Walton", "Rob Walton")

## numeric vectors
## majątek = wealth
majatek <- c(113, 98,76,67.5,59,55.1,54.7,54.6,54.4, 54.1, 52.7, 52.1, 50.9, 49.1, 48.9)

## wiek = age
wiek <- c(56, 64, 71, 89, 75, 84, 35, 71, 70, 75, 64, 80, 47, 46, 66)

## urodzony = born
## character vector (fake) dates
urodzony <- c( '1964-01-01', '1956-01-01', '1949-01-01', '1931-01-01', '1945-01-01',
'1936-01-01', '1985-01-01', '1949-01-01', '1950-01-01', '1945-01-01', '1956-01-01',
'1940-01-01', '1973-01-01', '1974-01-01', '1954-01-01')

## Dwuliterowe kody ISO krajów
## Niby napisów ale czynników (factors)
kraj <- c( 'US', 'US', 'FR', 'US', 'US', 'ES', 'US', 'US', 'US', 'US', 'US', 'MX', 'US', 'US', 'FR');

## F&I = finance and investment
branza <- c("Technology", "Technology", "Retail", "F&I", "Technology", "Retail",
   "Technology", "Retail", "Retail", "Retail", "Technology", "Telecom", "Technology", "Technology",  "Retail")

## frame = list of named vectors

forbes <- data.frame(milioner, majatek, urodzony, wiek, kraj, branza)

## str(ucture)
str(forbes)

## print frame
forbes

## print column using name 
forbes$majatek

## print column using numbers
forbes[, 3]

## useful functions
head(forbes)
##
tail(forbes)

## size-of 
?nrow

nrow(forbes)

## Basic statistics

?mean
?median

## mean of a vector majatek
mean(majatek)

## summary (forbes) ERROR
## but
summary(forbes.wiek)

min(forbes.wiek)

## 
plot(forbes$wiek)

## Useful functions
ls()
Inf 
NA
## Objects in R have attributes
## list of attributes
attributes(object)

## Set name in vector vv to
names(vv) <- c('integer', 'logical', 'complex')


###
### cf https://rpubs.com/amachno/statystyka_opisowa_projekt

### End ###########################################################################
### Start again
### Import data into frame (function csv.read)


forbes <- read.csv("FB2020.csv", dec=".", sep = ';',  header=T, na.string="NA");

str(forbes)

## indexing
w  <- forbes[,3]
p <- forbes[1,]

p
w <- forbes$worth

billionares <- forbes[,"name"]
## or: billionares <- forbes$name

## 
## Basic statistics
summary(forbes$worth)
forbes.summary <- summary(forbes$worth)
str(forbes.summary)
forbes.summary[1]

## Extract atribute 'Median'
forbes.summary["Median"]
forbes.median <- forbes.summary["Median"]
forbes.median

## Printing results
print (forbes.median)
## Formatted print
sprintf ("%.2", forbes.median)
## or (cat = concatenate)
cat ("Mediana:", forbes.median)

summary(forbes)

## 
forbes.table <- table(forbes$worth)
length(forbes.table)
?table
cut(forbes$worth, breaks=seq(0,120, by=10))
table(cut(forbes$worth, breaks=seq(0,120, by=10)))

##
##
## Fundamental data manipulation libraries dplyr/tidyverse

## Filtering rows
library("dplyr")
## install.packages("dplyr") if not found
## installation is automatic (upon confirmation) in RStudio

nonus.forbes <- filter(forbes, country != "United States")
nonus.forbes

## pipe operator %>%
## selecting columns
nonus.forbes.worth <- filter(forbes, country != "United States") %>% 
  select(worth)
## łącznie ile mają
sum(nonus.forbes.worth)

## Print countries  without repetitions
select(forbes, country) %>% unique
## How many countries
select(forbes, country) %>% unique %>% nrow
## alternative syntax:
forbes %>% select(country) %>% unique %>% nrow

##
##
## Graphics
## default chart for list of numbers
plot (forbes$worth)
## boxplot
boxplot(forbes$worth)
## color= breaks
hist(forbes$worth)

## 
boxplot(worth ~ branch, data=forbes)
select(forbes, branch) %>% unique %>% nrow

## mutate = create new variables
forbes.x <- mutate(forbes, 
  branch = case_when(branch == "Technology" ~ "IT", 
                     branch == "Fashion & Retail" ~ "FR",
                     TRUE ~ "Other"))
forbes.x
boxplot(worth ~ branch, data=forbes.x)

## basic plot
## for two lists (XY-plot)
plot(forbes.x$age, forbes.x$worth)

##
##
## better plots ggplot2

library ("ggplot2")

#  quick-plot (default charts are generated based on data types)
qplot(data=forbes.x, age, worth, color=branch)
qplot(data=forbes.x, age, worth, facets = . ~ branch)


### End ###########################################################################
## Start again
## Already appended
#library ("dplyr")
#library ("tidyr")
#library ("ggplot2")

# 
note <- ("Source: OWiD")
# subset from
#
c <- read.csv("owid-covid-data.csv", sep = ',',  header=T, na.string="NA" )
## One can read from URL
# c19d.url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
#
#c <- read.table(c19d.url)
#c <- read.csv(c19d.url)



# last date reported
dat <- last(c$date)

# transform
# extract some columns (select)
# group into groups by iso_code (group)
# compute summaries (sumarise)
c <- c %>% select (iso_code, continent, location, date, total_cases_per_million,
     total_deaths_per_million, 
     icu_patients_per_million, 
     people_fully_vaccinated_per_hundred) %>%
   group_by(iso_code) %>% 
   summarise (
     continent = last(continent),
     name = last(location),
     dat = last(date),
     c = last(total_cases_per_million),
     d = last (total_deaths_per_million),
     icu = last (icu_patients_per_million),
     v = last(people_fully_vaccinated_per_hundred)
 )

## Filter: only Europe
##c <- c %>% filter (continent == 'Europe')
## Remove rows with NA in d, c v
##c <- c %>% drop_na(c, d, v)
## How many countries?
countries <- nrow(c)

p1 <- ggplot(c, aes(x = v, y =d )) +
  geom_point(size=1) +
  geom_smooth(method="lm") +
  ggtitle(sprintf("Double vaccinated vs deaths/1mln (%s)", dat), 
	subtitle=sprintf("%i countries", countries) ) +
  ylab("Deaths/1mln") +
  xlab("%Vaccinated") +
  coord_cartesian(xlim = c(0, 105)) +
  theme_nikw() +
  labs(caption=note)

## save for later as PNG file
ggsave(plot=p1, file="Covid19_vacc_vs_deaths.png", width=10)

p2 <- ggplot(c, aes(x = v, y =c )) +
  ggtitle(sprintf("Double vaccinated vs cases/1mln (%s)", dat),
	subtitle=sprintf("%i countries", countries) ) +
  geom_point(size=1) +
  geom_smooth(method="lm") +
  ylab("Cases/1mln") +
  xlab("%Vaccinated") +
  coord_cartesian(xlim = c(0, 105)) +
  theme_nikw() +
  labs(caption=note)

## save for later as JPG file
ggsave(plot=p2, file="Covid19_vacc_vs_cases.jpg", width=10)

## GRAND END
