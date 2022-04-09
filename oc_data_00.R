### R ###
library ("dplyr")
library ("tidyr")
library ("ggplot2")

note <- ("Source: OWiD")
#csv_file <- "owid_covid_data.csv" # wrong
csv_file <- "owid-covid-data.csv" # wrong

c <- read.csv(csv_file, sep = ',',  header=T, na.string="NA" )
dat <- last(c$date)

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

##c <- c %>% filter (continent == 'Europe')
c <- c %>% drop_na(c, d, v)
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

ggsave(plot=p2, file="Covid19_vacc_vs_cases.png", width=10)
##
