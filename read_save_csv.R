## --R--
library ("dplyr")

data.url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
in.file <- "owid-covid-data.csv"
out.file <- "owid-covid-data_00.csv"


##c19 <- read.table(data.url)
# This is not working
#c19 <- read.csv(data.url, sep = ',',  header=T, na.string="NA")
c19 <- read.csv(in.file, sep = ',',  header=T, na.string="NA")

#library ("tidyverse")
#c19 <- read_csv(data.url, sep = ',',  header=T, na.string="NA")

print("Data download!")

## we do not need all the data
c19 <- c19 %>% select (iso_code, continent, location, date, total_cases_per_million,
     total_deaths_per_million,
     icu_patients_per_million,
     people_fully_vaccinated_per_hundred)
nrow(c19)

print("About to write!")

write.csv(c19, out.file, append=F)

