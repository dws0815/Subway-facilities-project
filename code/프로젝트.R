library(dplyr)
library(caret)
library(ggplot2)
#### 장애인 편의시설 현황 데이터 전처리(컬럼명 통일화 및 지하철역 패턴 동일화)
fs_with <- read.csv('project/Total_facility_status_with.csv',
               fileEncoding = 'cp949')
fs_with <- rename(fs_with, 지하철역=역명)
fs_with$지하철역 <- gsub("\\(.*?\\)","",fs_with$지하철역)
us$지하철역 <- gsub("\\(.*?\\)","",us$지하철역)
#### 지하철 사용현황 데이터 전처리(컬럼명 통일화 및 지하철역 패턴 동일화)
us <- read.csv('user_status.csv',
               fileEncoding = 'cp949')
exit
exit$지하철역 <- gsub("\\(.*?\\)","",exit$지하철역)
fs_with <- fs_with %>% group_by(지하철역) %>% 
  summarise(facilities=sum(Count_facility))

View(fs_with)
View(us)
us <- us[,-1]
###이너 조인으로 데이터 합치기
output_inner <- inner_join(us,fs_with,by='지하철역')
View(output_inner)


output_left <- left_join(fs_with,us,by='지하철역')
View(output_left)
output_left[is.na(t2)]=0
t2 <- t2[,-3]

lm1 <- lm(facilities~.평균무임이용자수,t1)
summary(lm1)
t1
plot(t1$facilities,t1$평균유임이용자수)
abline(lm(facilities~평균유임이용자수,t1),col="red")

area <- read.csv('area.csv', fileEncoding = 'cp949')
area <- rename(area,지하철역=역명)
area <- area[,-1]
View(area)
area<-area%>% group_by(지하철역)%>%summarise(면적=sum(면적.제곱미터.))
t3 <- left_join(output_inner,area,by='지하철역')
View(t3)
t3_without_station <- t3[,c(-1,-6)]

install.packages('heatmap')
library(heatmap)
heatmap(df,scale='none')

library(corrplot)
t.cor <- cor(t7)
t.cor

corrplot(t.cor, method='num')

infra <- read.csv('project/infra.csv', fileEncoding = 'UTF-8')
head(infra,5)
infra<-infra%>%select("전철역명","count")
View(infra)
infra<-unique(infra)

infra <- rename(infra, 지하철역=전철역명)
t4 <- left_join(output_inner,infra,by='지하철역')
View(t4)
unique(t4)
t4_cor<-t4[,c(-1)]
View(t4_cor)

t.cor <- cor(t4_cor)
t.cor

corrplot(t.cor, method='num')
boxplot(t5$환승거리합)
fit <- lm(facilities.y~출구수+count+평균무임이용자수+면적.제곱미터.,t8)
lm(facilities.y~출구수,t8)

abline(fit)
summary(fit)
plot(t8$출구수,t8$facilities.y)
abline(fit,col="red")
transfer<-read.csv('project/transfer.csv',fileEncoding='UTF-8')
transfer <- transfer[,-1]
View(transfer)
t5<-left_join(t4,transfer,by="지하철역")
View(t5)
unique(t5)
t5$환승거리합[is.na(t2)]=0
t6 <- t6[,c(-1,-3,-5,-6,-8)]
t5
exit<-read.csv('project/exit.csv',fileEncoding='UTF-8')
View(exit)
exit <- exit[,-1]
t6<-inner_join(t4,exit,by="지하철역")
View(t8)

pairs(t8, panel = panel.smooth)
t8<-inner_join(t6,fs_with,by="지하철역")
t8<-unique(t8)
View(t8)

ramp<-read.csv('project/ramp.csv',fileEncoding='cp949')
ramp <- ramp[,-1]
View(ramp)

t9<-inner_join(t8,ramp,by="지하철역")
t8<-unique(t8)
View(t8)

transfer<-read.csv('project/transfer.csv',fileEncoding='UTF-8')
transfer <- transfer[,-1]
View(transfer)

t10<-inner_join(t9,transfer,by="지하철역")
t10<-unique(t10)
View(t10)

t10_cor<- t10[,c(-1,-4,-6,-13)]

t.cor <- cor(t10_cor)
t.cor

corrplot(t.cor, method='num')

View(df)
t7 <- t7[,c(-5,-6,-8)]

에스컬레이터랑 출구수 
fit <- lm(편의시설~출구수+주변주요시설+면적.제곱미터.+평균유임이용자수,df_corr)

summary(fit)
t7<-unique(t7)
t7 <- rename(t7,층수=층수.y)
df<-t7
unique(df)
write.csv(df,"Dataframe.csv")
View(df)
df_cor<-df[,c(-1,-8)]
t.cor <- cor(df_cor)
t.cor

corrplot(t.cor, method='num')
scatterplotMatrix(df)
library(psych)
pairs.panels(df, stars = TRUE,  lm =TRUE)
pairs(df_cor)

install.packages("car")
library(car)
vif(fit)
View(df)
p <- ggplot(df, aes(x=출구수, y=편의시설)) + 
  geom_violin()
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(df_corr, histogram=TRUE, pch=19)

region<-read.csv('project/Region.csv',fileEncoding='UTF-8')
View(region)
region <- region[,-1]
View(region)
region<-region %>%select(역명,시군구명)
region <- rename(region, 지하철역=역명)
region<-unique(region)
df_reg <- left_join(df,region,by='지하철역')
View(df_reg)

a<-df_reg%>%filter(is.na(시군구명))
a<-unique(a)
View(a)
a<-a %>% select(지하철역,시군구명)

fit <- lm(편의시설~.,t7)
summary(fit)
ab<-step(fit,direction='forward')

sigun<-read.csv('project/sigun.csv',fileEncoding='CP949')
View(sigun)
sigun<-sigun %>%filter(Sido=="서울") %>% select(Sigun,id)
sigun <- rename(sigun, 구주소=Sigun)
region_<-inner_join(region,sigun,by="구주소")
View(region_)
region_<-region_%>%select(지하철역,구주소,id)
View(unique(region_))
write.csv(sigun,"sigun.csv")

map <- shapefile("project/TL_SCCO_SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')
View(new_map)

head(dfmap_seoul$id)
korea<-ggplot2::korea
