if (!require(tidyverse)) install.packages("tidyverse") 
if (!require(data.table)) install.packages("data.table") 
library(tidyverse)
library(data.table)

if (!require(devtools)) install.packages("devtools") 
if (!require(DBI)) devtools::install_github("rstats-db/DBI")  # different from CRAN repository 
if (!require(RSQLite)) devtools::install_github("rstats-db/RSQLite")
if (!require(RMySQL)) devtools::install_github("rstats-db/RMySQL")
if (!require(bigrquery)) devtools::install_github("rstats-db/bigrquery")
if (!require(data.table)) install.packages("data.table")
if (!require(readr)) install.packages("readr")
library(tidyr)

library(ggplot2)
ggplot(data=mpg) +
  geom_point(aes(displ, hwy,
                 colour = class))

mpg

if(!require("gapminder")){install.packages("gapminder")}

library(gapminder)
str(gapminder)

p<-ggplot(gapminder, aes(x=gdpPercap, y = lifeExp))
p

summary(p)

p_point <- p + geom_point()
p_point

summary(p_point)

gap_af <- dplyr::select(gapminder, gapminder$country==Afghanistan)

gap_af <- filter(gapminder, gapminder$country=="Afganistan")
gap_af

gap_af <- gapminder[gapminder$country="Afganistan"]

head(gap_af)
gap_af

p<- ggplot(gap_af) + geom_line(aes(year, lifeExp))
p

ggplot(gapminder) + geom_point(aes(x = log10(gdpPercap), y=lifeExp))

(p_point_color <- p + geom_point(aes(color = continent)))

summary(p_point_color)

p + geom_point(alpha = (1/3), size = 3)

p_point + stat_smooth()

p_point + geom_smooth(lwd = 2, se = FALSE, method = "lm")

(lp <- ggplot(gapminder) + geom_jitter(aes(x=year, y = lifeExp)))

lp + facet_wrap(~ continent)

gapminder

pophis <- geom_histogram(gapminder$pop) + aes(color = pop)

pophis <- geom_histogram(gapminder, binwidth = 5, aes(pop, count))

pophis

ggplot(data=gapminder, aes(gapminder$pop)) + geom_histogram()


p <- ggplot(gapminder, aes(gapminder$continent, lifeExp))
p + geom_violin() + stat_summary(fun.y=mean, geom="point", shape=23, size=2, fill = "blue")

gapminder

## continent_freq <- table(gapminder$continent)

continent_freq <- count(gapminder, continent)
continent_freq

ggplot(continent_freq, aes(x=continent, y=n)) + geom_bar(stat="identity")

ggplot(gapminder, aes(x=continent)) + geom_bar()

jCountries <- c("Canada", "Rwanda", "Cambodia", "Mexico")
plot4 <- gapminder %>% filter(country %in% jCountries) %>%
  ggplot(aes(x=year, y=lifeExp, color = country)) +
  geom_line() + geom_point()
plot4

dir.create("../ggsave", showWarnings = F)
ggsave("../ggsave/last.png")


if(!require("ggmap"))
{devtools::install_github("dkahle/ggmap")}

library(ggmap)

remove.packages("ggmap", "ggmap")

loc<-"서울"
tar<-"서울시청"
loc

rm.packages("ggmap")
wifidata<-fread("./data/wifi.csv")
wifidata


wifi_seoul <- filter(wifidata, grepl("서울", `소재지도로명주소`))


if(!require(ggsci)) devtools::install_github("road2stat/ggsci")

install.packages("ggedit")
library(ggedit)
library(dplyr)
install.packages("dplyr")

remove.packages("dplyr")



if (!require(tidyverse)) install.packages("tidyverse") 
if (!require(data.table)) install.packages("data.table") 
if (!require(arules)) install.packages("arules") 
library(tidyverse)
library(data.table)
library(arules)

wifiseoul<-fread("./data/wifi.csv",  encoding = "UTF-8")
wifiseoul

head(wifiseoul)

