#지하철역 별 장애인 편의시설개수 전처리
elevator<-read.csv("project/elevator.csv",fileEncoding = "CP949")
elevator<-elevator["역명"]
elevator$facility<-"엘리베이터"
elevator
elevator<-elevator %>% group_by(역명) %>% summarise(count = n())


#각 데이터 합치기
moving_walk<-read.csv("project/무빙워크.csv",fileEncoding = "CP949")
moving_walk<-moving_walk %>% select(역명,count,facility)
escalator<-read.csv("project/에스컬레이터.csv",fileEncoding = "CP949")
escalator<-escalator %>% select(역명,count,facility)
wheel_chair<-read.csv("project/휠체어리프트.csv",fileEncoding = "CP949")
wheel_chair<-wheel_chair %>% select(역명,count,facility)
facility_status_with<-rbind(elevator,moving_walk,wheel_chair,escalator)
write.csv(facility_status_with,"facility_status_with.csv",fileEncoding="CP949")
Total_facility_status_with<-facility_status_with %>% group_by(역명) %>% summarise(Count_facility=sum(count))
Total_facility_status_with
write.csv(Total_facility_status_with,"project/Total_facility_status_with.csv",fileEncoding="CP949")



#지하철 역별 무임및 유임 지하철역 사용자 수 전처리
d<-read.csv("project/5월_이용현황.csv",fileEncoding = "CP949")
e<-read.csv("project/6월_이용현황.csv",fileEncoding = "CP949")
f<-read.csv("project/7월_이용현황.csv",fileEncoding = "CP949")
user_status<-rbind(d,e,f)
user_status
user_status$무임이용자<-user_status$무임승차인원+user_status$무임하차인원
user_status$유임이용자<-user_status$유임승차인원+user_status$유임하차인원
user_status<-user_status%>% select("사용월","지하철역","무임이용자","유임이용자")
user_status<-user_status %>% group_by(지하철역) %>%summarise(평균무임이용자수=sum(무임이용자),평균유임이용자수=sum(유임이용자))
user_status
write.csv(user_status,"user_status.csv",fileEncoding="CP949")

lm1<-lm(t2$facilities~T2$평균무임이용자수)

area<-read.csv("project/서울교통공사_역사건축정보_20210628.csv",fileEncoding = "CP949")
area<-area%>% select("역명","길이.미터.","층수","면적.제곱미터.")
area
write.csv(area,"area.csv",fileEncoding="CP949")
