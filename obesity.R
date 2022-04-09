library("ggplot2")
library("dplyr")
library("tidyr")
library("scales")
library("ggthemes")
library("ggpubr")

## curl "https://apps.who.int/gho/athena/api/GHO/WHS9_86?format=csv" > pop_who.csv
obesity <- read.csv(file='obesity_who_all.csv',sep=';',header=T)
obesity$yr <- as.numeric(as.character(obesity$rok))
## ecdc_countries_names_codes3166.csv
countries <- read.csv(file='ecdc_countries_names_codes3166.csv', 
                      sep=';',header=T)

obesityPL <- obesity %>% filter (kraj == 'POL' & plec != 'BTSX')

##qq <- as.factor(obesityPL$rok)

#popl <- ggplot(obesityPL, aes(x= as.factor(rok), 
popl <- ggplot(obesityPL, aes(x= as.Date(as.character(rok), "%Y"),
        group=as.factor(plec), color=as.factor(plec), y=nadwaga )) +
  geom_line(size=.5 ) +
  geom_point(size=2.5, alpha=.3) +
  ##xlab(label="") +
  ylab(label="tys") +
  ##theme_nikw()+
  ##labs(caption=source, color='Rok') +
  scale_color_discrete(name="Płeć", labels =c("M", 'F'),
                       breaks=c('MLE', 'FMLE')) +
  ggtitle("Nadwaga w PL", 
          subtitle=sprintf(""))
popl

### ###

#res <- df %>%
#  group_by(siteID, parameter) %>%
#  summarise(difference = value[which.max(YEAR)]-value[which.min(YEAR)])

obesity_diff <- obesity %>%
  filter (plec == 'BTSX' & yr > 1979 ) %>%
  group_by(kraj) %>%
  summarise(fst= nadwaga[which.min(yr)], 
            lst = nadwaga[which.max(yr)],
            diff = nadwaga[which.max(yr)] - nadwaga[which.min(yr)])
  
### ###

countries <- c('POL', 'GER', 'ESP', 'CZE', 'ITA', 'FRA', 'FIN',
               'AFG', 'CHN', 'IND', 'NZA',
               'FJI', 'TON', 'WSM',
               'ETH', 'NGA', 'USA', 'CAN')
o <- obesity %>% filter (plec != 'BTSX' & kraj %in% countries) %>%
  as.data.frame

rokn <- as.numeric(obesity$rok)

olast <- obesity %>% filter (plec == 'BTSX' ) %>%
  group_by(kraj) %>%  drop_na(nadwaga) %>% arrange(rok) %>%
  filter(row_number()==n()) %>%  as.data.frame

ofirst <- obesity %>% filter (plec == 'BTSX' & as.numeric(as.character(rok)) > 1979 ) %>%
  group_by(kraj) %>%  drop_na(nadwaga) %>% arrange(rok) %>%
  filter(row_number()==1) %>%  as.data.frame

## ###
## ###

po <- ggplot(o, aes(x= as.factor(rok), 
          group=as.factor(plec), color=as.factor(plec), y=nadwaga )) +
  geom_line(size=.5, alpha=.6 ) +
  geom_point(size=.5, alpha=.3) +
  ##xlab(label="") +
  ylab(label="tys") +
  ##theme_nikw()+
  ##labs(caption=source, color='Rok') +
  facet_wrap( ~kraj, scales = "fixed") +
  ggtitle("Nadwaga w PL", 
          subtitle=sprintf(""))
po
ggsave(plot=po, file='obesity_c.png', width=10, height=10)
##

olast$kraj

###
poworld <- ggplot(olast, aes(x = reorder(kraj, nadwaga), color=region )) +
  geom_point(aes(y = nadwaga), size=1) +
  xlab(label="kraj") +
  ylab(label="obesity") +
  ggtitle("Nadwaga") +
  theme(axis.text = element_text(size = 4)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()
poworld

ggsave(plot=poworld, file='obesity_w.png', height=15)

##
##
## reorder is a generic function. The "default" method treats its first argument as a categorical variable, and reorders its levels based on the values of a second variable, usually numeric.
oo <- left_join(olast, ofirst, by='kraj')

oo$diff.xy <- oo$nadwaga.x - oo$nadwaga.y

#oo <- bind_rows(olast, ofirst)
#zz2 <- reorder(oo$kraj, oo$nadwaga)
#print (zz2)

#poworld2 <- oo %>%
#  pivot_longer(cols(nadwaga.x, nadwaga.z, diff), names_to = "nadwaga", 
#               values_to = "value") %>%
poworld21 <- oo %>%
  ggplot(aes(x = reorder(kraj, nadwaga.x) )) +
  geom_point(aes(y = nadwaga.x, color="nadwaga.x"), size=1 ) +
  geom_point(aes(y = nadwaga.y, color="nadwaga.y"), size=1 ) +
  geom_bar(aes(y = diff.xy, fill='roznica'), stat="identity", alpha=.25 ) +
  xlab(label="kraj") +
  ylab(label="obesity") +
  ggtitle("Nadwaga") +
  ##
  #scale_color_manual(name = "ltitle") +
  #scale_color_manual(name="Rok: ", labels =c("2016", "1980"),
  scale_color_manual(name="", labels =c("2016", "1980"),
                     values = c(nadwaga.x="red", nadwaga.y="blue" ) ) +
  scale_fill_manual(name="", values = c(roznica="green" ) ) +
  #
  ##
  theme(axis.text = element_text(size = 4)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()
poworld21

ggsave(plot=poworld21, file='obesity_wx.png', height=15)


############
poworld22 <- obesity_diff %>%
  ggplot(aes(x = reorder(kraj, lst ))) +
  geom_point(aes(y = lst, color="lst"), size=1 ) +
  geom_point(aes(y = fst, color="fst"), size=1 ) +
  geom_bar(aes(y = diff, fill='diff'), stat="identity", alpha=.25 ) +
  xlab(label="kraj") +
  ylab(label="obesity") +
  ggtitle("Nadwaga") +
  ##
  scale_color_manual(name="", labels =c("2016", "1980"),
                     values = c(lst="red", fst="blue" ) ) +
  scale_fill_manual(name="", values = c( diff="green" ) ) +
  #
  theme(axis.text = element_text(size = 4)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()
poworld22
ggsave(plot=poworld22, file='obesity_wx2.png', height=15)
