if (!require(tidyverse)) install.packages("tidyverse") 
if (!require(data.table)) install.packages("data.table") 
if (!require(ggfortify)) devtools::install_github("sinhrks/ggfortify")
if (!require(ggmap)) devtools::install_github("dkahle/ggmap")
library(tidyverse)
library(data.table)
library(ggfortify)
library(ggmap)

# 과제 1

## 데이터 불러오기
chen<-fread("./recomen/chennel.csv")
comp<-fread("./recomen/competitor.csv")

## 날짜 자료형 변경
comp$useDate <- comp$useDate %>% paste0(.,"01") %>% as.Date(format="%Y%m%d")

# 과제 1.1

chen[,sum(useCnt),by=(chennel)] %>% ggplot(aes(chennel, V1)) + geom_bar(stat = "identity")
chen %>% ggplot(aes(x=chennel,y=useCnt)) + geom_bar(stat="sum")


# 과제 1.2

ggplot(comp[,.N,by=(useDate)], aes(x=useDate, y=N)) + geom_line() + scale_x_date(date_breaks="6 months")
ggplot(comp, aes(x=useDate)) + geom_line(stat="count") + scale_x_date(date_breaks="6 months")

# 과제 1.3
ggplot(comp[,.N,by=.(useDate,partner)], aes(x=useDate, y=N, color=partner)) + geom_line() + scale_x_date(date_breaks="6 months") + theme(legend.position = "top")
ggplot(comp, aes(x=useDate,color=partner)) + geom_line(stat="count") + scale_x_date(date_breaks="6 months") + theme(legend.position = "top")

# 과제 2

m <- lm(Petal.Width ~ ., data = iris)
p4 <- autoplot(m, which = 1:6, ncol = 3, label.size = 3)

p4


## 과제 계속
wifi<-fread("./data/wifi.csv", encoding="UTF-8")

## 과제 계속 1.1

sw <- wifi[grep("^서울",`소재지도로명주소`), ]
tar <- sw[,`설치시군구명`] %>% unique %>% paste0("서울특별시 ",.)
latlon<-c()
for (i in 1:length(tar)){
  latlon<-rbind(latlon, geocode(tar[i]))
}
sw
sw<-sw[,.N,by=(`설치시군구명`)]
tar<-cbind(tar,latlon) %>%
  separate(tar, into=c("city","tar", sep=" ")) %>%
  select(tar, lon, lat)

names(sw)<-c("tar", "N")
sw <- sw %>% left_join(tar)
loc<-URLencode(enc2utf8("서울"))

sw

pg <- get_googlemap(loc, maptype = "roadmap", zoom = 11) %>% ggmap()
pg + geom_point(data=sw, aes(x=lon, y=lat, size=N), color="blue", alpha=0.3)

## 과제 계속 1.2

sw <- wifi
sw[`설치시도명`=="겅상남도",`설치시도명`:="경상남도"]
tar <- sw[,`설치시도명`] %>% unique
latlon<-c()
for (i in 1:length(tar)){
  latlon<-rbind(latlon, geocode(tar[i]))
}

sw<-sw %>% 
  group_by(`설치시도명`) %>%
  summarise(N=n())
tar<-cbind(tar,latlon)

names(sw)<-c("tar", "N")
sw <- sw %>% left_join(tar)
loc<-URLencode(enc2utf8("대한민국"))

pg <- get_googlemap(loc, maptype = "roadmap",zoom =  7) %>% ggmap()
pg + geom_point(data=sw, aes(x=lon, y=lat, size=N), color="blue", alpha=0.3)

## 과제 계속 1.3
library(lubridate)

sw<-wifi
dat<-sw$설치년월 %>% unique
da<-dat
da[nchar(da)==7]<-paste0(da[nchar(da)==7],"-01")
da[nchar(da)==10]<-gsub("\\.","-",da[nchar(da)==10])
da[da=="190001"]<-""
da[nchar(da)==4]<-paste0(da[nchar(da)==4],"-01-01")
da[nchar(da)==6 & grepl("^[[:digit:]]*$",da)]<-paste0(da[nchar(da)==6 & grepl("^[[:digit:]]*$",da)],"01")
da[nchar(da)==6& grepl("[a-z]$",da)]<-paste0(da[nchar(da)==6& grepl("[a-z]$",da)],"-01")
da[nchar(da)==6]<-paste0(substr(da[nchar(da)==6],5,6),"-",substr(da[nchar(da)==6],1,3),"-01")
da[da=="2013-Sep"]<-"2013-Sep-01"
da <- data.frame(ori=dat,tar=ymd(da))
da
swc <- sw %>% left_join(da,by=c(`설치년월`="ori"))


head(swc)


swc <- swc[!is.na(swc$tar),]

install.packages("extrafont")
library(extrafont)


loadfonts(device="postscript")

element_text(family="AppleGothic")


swc %>%
  select(`설치시도명`,tar) %>%
  mutate(ym=paste0(year(tar),sprintf("%02d",month(tar)))) %>%
  group_by(`설치시도명`,ym) %>%
  summarise(N=n()) %>%
  mutate(ym=as.Date(paste0(ym,"01"),format="%Y%m%d")) %>%
  as.data.frame() %>%
  ggplot(aes(x=ym, y=N, color=`설치시도명`)) +
  geom_line(aes(y = cumsum(N)))

## 과제 계속 1.4

sw<-wifi
dat<-sw$서비스제공사명 %>% unique
dac<-dat
dac<-data.frame(ori=dat,tar=dac,sk=NA,kt=NA,lg=NA,etc=NA)
dac$tar<-tolower(dac$tar)
dac$tar<-gsub("skt","skcom",dac$tar)
dac[grep("sk",dac$tar),"sk"]<-T
dac[grep("kt|케이티",dac$tar),"kt"]<-T
dac[grep("lg|u\\+|엘지",dac$tar),"lg"]<-T
dac[is.na(dac$sk)&is.na(dac$kt)&is.na(dac$lg),"etc"]<-T

# for chk code
dac[grep("kt",dac$tar),"tar"]
dac[!is.na(dac$kt),c("tar","sk","kt","lg")]

swc <- sw %>% left_join(dac[,-2],by=c(`서비스제공사명`="ori"))

sum(!is.na(swc[,c("sk","kt","lg","etc")]))


tem<-swc %>%
  select(`설치시도명`,
         `와이파이SSID`,
         `설치년월`,
         sk:etc) %>%
  gather(key="comp",
         value="TF",
         -c(`설치년월`,
            `설치시도명`,
            `와이파이SSID`))

sum(!is.na(tem$TF))

tem<-tem[!is.na(tem$TF),]
da$ori<-as.character(da$ori)
temc <- tem %>% 
  left_join(da,by=c(`설치년월`="ori"))





temc %>%
  select(`설치시도명`,tar,comp) %>%
  mutate(ym=paste0(year(tar),sprintf("%02d",month(tar)))) %>%
  group_by(`설치시도명`,comp,ym) %>%
  summarise(N=n()) %>%
  mutate(ym=as.Date(paste0(ym,"01"),format="%Y%m%d")) %>%
  as.data.frame() %>%
  ggplot(aes(x=ym, y=N, color=`설치시도명`)) +
  geom_line(aes(y = cumsum(N))) +
  facet_wrap(~ comp)

#시스템에 설치된 나눔고딕을 사용하기 위해 extrafont 패키지를 설치했다. 
#관련 내용은 http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html 참고

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggthemes)
  library(extrafont)
})


#개인화된 테마 작업
theme_gogamza<- function(base_size = 12, base_family = "NanumGothic"){
  (theme_foundation(base_size = base_size, base_family = base_family) +
     theme(line = element_line(colour = "black"), rect = element_rect(fill = ggthemes_data$fivethirtyeight["ltgray"],
                                                                      linetype = 0, colour = NA), text = element_text(colour = ggthemes_data$fivethirtyeight["dkgray"]),
           axis.title = element_text(), axis.text = element_text(),
           axis.ticks = element_blank(), axis.line = element_blank(),
           legend.background = element_rect(), legend.position = "bottom",
           legend.direction = "horizontal", legend.box = "vertical",
           panel.grid = element_line(colour = NULL), panel.grid.major = element_line(colour = ggthemes_data$fivethirtyeight["medgray"]),
           panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0,
                                                                         size = rel(1.5), face = "bold"), plot.margin = grid::unit(c(1,
                                                                                                                                     1, 0.5, 0.5), "lines"), strip.background = element_rect(), panel.margin.x=NULL, panel.margin.y=NULL))
}

#ggplot2 메인 테마로 설정
theme_set(theme_gogamza())

#기본 테마에서 한글 폰트 정의를 하고 싶다면 아래 코드만 실행한다. 
theme_set(theme_gray(base_family='NanumGothic'))

ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
  geom_point(aes(colour=Species)) + ggtitle("나눔고딕체 제목")


{R, results="asis",echo=F}    
if(!require(knitr)){
  install.packages("knitr")}
library(knitr)    
obj<-c("소스코드","텍스트",
       "플롯","메세지","경고")    
role<-c("코드청크에 들어있는 R 코드",    
        "summary(iris)와 같은 글자 출력 결과물",    
        "plot(iris)와 같은 그림 출력 결과물",
        "메세지","경고")    
dat<-data.frame("객체"=obj,"목적"=role)    
kable(dat,align="cl", "html")

