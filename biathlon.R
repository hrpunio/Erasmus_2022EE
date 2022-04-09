#library("ggplot2")
library("dplyr")
library("tidyr")

b0 <- read.csv("biathlonDATA.csv", sep = ';',  header=T, na.string="NA" )
nrow(b0)

## attractive if > 4
ba <- b0 %>% filter (attractiveness > 4) 
## non-attractive if < 4 (if == 4 rater undecided)
bn <- b0 %>% filter (attractiveness < 4) 

## % attractive
nrow(ba)/( nrow(bn) + nrow(ba) ) * 100

## Transform
## select some columns (select)
## omit 4 (filter)
## if attractiveness > 4 code as 1; if < 4 code as -1 (mutate)
## group by targetID (group_by)
## summarise
## if summed attractiveness (column att) < 0 unatractive
## if att > 0 attractive
## if att == 0 NA (default value)
## if  suma < 0  nieatrakcyjny; jeżeli > 0 atrakcyjny (jeżeli 0 NA)
## The same with performance
## if p < 0 Average; if p >= 0 Champion :-)
b <- b0 %>% select (attractiveness, targetID, sex, ethnicity, zAge, zHeight, zBMI, zWCptsPB) %>% 
  filter (attractiveness != 4) %>%
  mutate( attractiveness = case_when(
      attractiveness < 4 ~ -1,
      attractiveness > 4 ~ +1
    )) %>%
  group_by(targetID) %>%
  summarise (att = sum(attractiveness, na.rm=T), 
             id = first(targetID),
             sex = first(sex), 
             eth=first(ethnicity),
             age = first(zAge), 
             ht = first(zHeight), 
             bmi = first(zBMI),
             p = first(zWCptsPB)) %>% 
  mutate( att = case_when(
    att < 0 ~ "U",
    att > 0 ~ "A"
  ),
  p = case_when(
    p < 0 ~ "A",
    p >= 0 ~ "C"
  )
  
  ) %>% mutate_if(is.character, as.factor)

# ### ###
# Print summary 
summary(b)

## Two-way tables and Chi^2 test of independence
# attracctiveness by sex
bx <- b %>% select (att, sex)

bx.table <- table(bx)
bx.table

margin.table(bx.table,1)
margin.table(bx.table,2)
bx.table.m <- addmargins(bx.table)

bx.table.m

chi_bx <- chisq.test(bx.table)

chi_bx

## attractiveness by performance

by <- b %>% select (att, p)

by.table <- table(by)
by.table

chi_by <- chisq.test(by.table)
chi_by

## GRAND END
