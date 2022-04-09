library("ggplot2")
library("dplyr")
library("tidyr")
library("scales")
library("ggthemes")
library("ggpubr")

## curl "https://apps.who.int/gho/athena/api/GHO/WHS9_86?format=csv" > pop_who.csv
url <- "https://apps.who.int/gho/athena/api/GHO/WHS9_86?format=csv"
obesity <- read.csv(url, sep=',', header=T)

nrow(obesity)

write.csv(obesity, "obesityWHO.csv")

## Improve using RStudio
