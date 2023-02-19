data=read.csv("/Users/baeknarim/날씨빅콘/data/full.csv")

#삭제 변수 
#"sum_gsr","sum_cmb_ss_hr","hr1_max_icsr","hr1_max_icsr_hrmt",max_rfit, 
#dd_mes_hrmt, dd_mefs_hrmt, mi10_max_rn_hrmt, hr1_max_rn_hrmt
# min_vs min_vs_hour min_vs_min
#2016 후반부 데이터가 이상함.. 
#시계열성도 안보이고 NA도 8만개 이상 -> 삭제 

data=subset(data, select=-c(sum_gsr,sum_cmb_ss_hr,hr1_max_icsr,
                            hr1_max_icsr_hrmt,max_rfit,max_rfit_hrmt,
                            min_vs, min_vs_hrmt,dd_mes_hrmt, dd_mefs_hrmt,
                            mi10_max_rn_hrmt, hr1_max_rn_hrmt))




#hrmt 변수  -> min, hour로 나누기 

data$max_ws_min = str_sub(data$max_ws_hrmt,-2,-1) #분 
data$max_ws_hour = str_sub(data$max_ws_hrmt,-4,-3)
data$max_ws_hour[data$max_ws_hour==""]=0

data$max_ins_ws_min = str_sub(data$max_ins_ws_hrmt,-2,-1) #분 
data$max_ins_ws_hour = str_sub(data$max_ins_ws_hrmt,-4,-3)
data$max_ins_ws_hour[data$max_ins_ws_hour==""]=0

data2=subset(data, select=-c(max_ws_hrmt,max_ins_ws_hrmt))

#type 변환
# tma  -> datetime
# -hour, min -> int 형으로 

data2$max_ws_min = as.integer(data2$max_ws_min)
data2$max_ws_hour = as.integer(data2$max_ws_hour)
data2$max_ins_ws_min = as.integer(data2$max_ins_ws_min)
data2$max_ins_ws_hour = as.integer(data2$max_ins_ws_hour)

data2$tma = as.Date(data2$tma, format = "%Y-%m-%d")

# hour가 0~24인 경우 존재  ( hrmt보니까 00~ 2400까지) 
# 24 -> 0으로 변경 

data2$max_ws_hour[data2$max_ws_hour ==24] = 0
table(data2$max_ws_hour)

data2$max_ins_ws_hour[data2$max_ins_ws_hour ==24] = 0
table(data2$max_ins_ws_hour)

#일조 
#ssrate NA: 220 
plot(data2$tma,data2$ssrate) #일조율 : 100이상 -> 3개 outlier

data2 %>%filter(ssrate >100)
data2$ssrate[(data2$ssrate >100)]=NA

plot(data2$tma,data2$ssrate) 
#시계열성이 딱히 없다 

#ss_dur 가조시간 
plot(data2$tma,data2$ss_dur) #가조시간 : 24시간 이내 

#140 이상인 outlier- > NA로 
data2 %>%filter(ss_dur >140)
data2$ss_dur[(data2$ss_dur >140)]=NA

#시계열성이 매우 뚜렷 

#sum_ss_hr: 합계 일조시간 
plot(data2$tma,data2$sum_ss_hr) #: 24시간 이내 
data2 %>%filter(sum_ss_hr >24)
data2$sum_ss_hr[(data2$sum_ss_hr >24)]=NA
#시계열성 존재 

#기압 
#avg_pa
plot(data2$tma,data2$avg_pa)  #outlier는 크게 안보임 
#시계열성 존재 

#max_pa
plot(data2$tma,data2$max_pa)
#2000이상, 200이하 -> 2개 outlier 
data2 %>%filter(max_pa >2000 | max_pa < 200 )
data2$max_pa[(data2$max_pa >2000 | data2$max_pa < 200)]=NA
#시계열성 존재 

#min_pa
plot(data2$tma,data2$min_pa)
#600이하 -> 39개 outlier 
data2 %>%filter(min_pa <600 )
data2$min_pa[(data2$min_pa <600)]=NA
#시계열성 존재 

#avg_pv
plot(data2$tma,data2$avg_pv) 
#80이상 1개 outlier 
data2 %>%filter(avg_pv >80 )
data2$avg_pv[(data2$avg_pv >80)]=NA
#시계열성 존재 

#max_pv
plot(data2$tma,data2$max_pv) #no outlier 
#시계열성 존재 

#min_pv
plot(data2$tma,data2$min_pv) #no outlier 
#시계열성 존재 

#상대습도 :100이하 값만 가능 
#avg_rhm
plot(data2$tma,data2$avg_rhm) #no outlier 
data2 %>%filter(avg_rhm >100 )
#시계열성 존재 

#min_rhm
plot(data2$tma,data2$min_rhm)  #no outlier 
data2 %>%filter(avg_rhm >100 )
#시계열성 존재 - 애매 

#온도 
#min_tg  #일최저 지상온도 
plot(data2$tma,data2$min_tg)
data2 %>% filter(min_tg < -40 )
#시계열성 존재
# 일최저 초상온도가 -40도 이하 (강진군 2014년 1월 후반) 
# 눈에 띄게 낮은 초상온도...

#avg_ta 일평균 기온 
plot(data2$tma,data2$avg_ta)  
#시계열성 존재

#max_ta
plot(data2$tma,data2$max_ta) 
#시계열성 존재

#min_ta
plot(data2$tma,data2$min_ta) 
#시계열성 존재

#풍속 풍향 
#avg_ws #일 평균 풍속 
plot(data2$tma,data2$avg_ws) 
#약한 시계열성이 보임 

#mix_ws #일 합성 풍속
plot(data2$tma,data2$mix_ws) 
#약한 시계열성이 보임 

#mix_wd #일 합성 풍향 
plot(data2$tma,data2$mix_wd) 
#시계열성 안보임 

#max_ws 일 최대 풍속 
plot(data2$tma,data2$max_ws) 
data2 %>% filter(max_ws > 40 )
#약한 시계열성 

#max_ws_hour
plot(data2$tma,data2$max_ws_hour) 
data2 %>% filter(max_ws_hour > 24 )
table(data2$max_ws_hour)
#시계열성 안보임 

#max_ws_min
plot(data2$tma,data2$max_ws_min) 
data2 %>% filter(max_ws_min > 60 )
#시계열성 안보임 

#max_ws_wd 일 최대풍속시 풍향
plot(data2$tma,data2$max_ws_wd) 
#시계열성 안보임 

#max_ins_ws 일 최대순간 풍속
plot(data2$tma,data2$max_ins_ws) 
data2 %>% filter(max_ins_ws > 50 )
#시계열성 존재 

#max_ins_ws_hour
plot(data2$tma,data2$max_ins_ws_hour) 
data2 %>% filter(max_ins_ws_hour > 24 )

#시계열성 안보임 

#max_ins_ws_min
plot(data2$tma,data2$max_ins_ws_min) 
data2 %>% filter(max_ins_ws_min > 60 )

#시계열성 안보임 

#max_ins_ws_wd 일 최대순간 풍속시 풍향
plot(data2$tma,data2$max_ins_ws_wd) 
#시계열성 안보임 

#hr24_sum_rws 24시간 합계 풍정
plot(data2$tma,data2$hr24_sum_rws) 
#약한 시계열성 

#max_wd 일 최다 풍향
plot(data2$tma,data2$max_wd) 
#시계열성 안보임 

#####################적설##################
#dd_mes
plot(data2$tma,data2$dd_mes) 
data2 %>% filter(dd_mes ==0)
table(data2$month[which(data2$dd_mes==0)])

#겨울(11,12,1,2,3)인 경우 눈이 오지 않을때 0
#다른 계절인 경우 눈이 오지 않는 경우 NA 

#연, 월 , 일 변수 생성
data2$year = as.integer(str_sub(data2$tma,1,4))
data2$month = as.integer(str_sub(data2$tma,6,7))
data2$days = as.integer(str_sub(data2$tma,9,10))


#4~10월 NA -> 0 
table(data2$month[which(data2$month >=4 & data2$month<=10 & is.na(data2$dd_mes))])
table(data2$dd_mes[which(data2$month >=4 & data2$month<=10 & is.na(data2$dd_mes))])
data2$dd_mes[which(data2$month >4 & data2$month<=10 & is.na(data2$dd_mes))]<-0


#sum_dpth_fhsc 일 합계 깊이 신적설
plot(data2$tma,data2$sum_dpth_fhsc) 
table(data2$month[which(data2$sum_dpth_fhsc==0)])

#4~10월 NA -> 0 
table(data2$month[which(data2$month >=4 & data2$month<=10 & is.na(data2$sum_dpth_fhsc))])
data2$sum_dpth_fhsc[which(data2$month >=4 & data2$month<=10 & is.na(data2$sum_dpth_fhsc))]<-0
table(data2$month[ is.na(data2$sum_dpth_fhsc)])

#dd_mefs 일 최심신적설
plot(data2$tma,data2$dd_mefs) 
table(data2$month[which(data2$dd_mefs==0)])

#4~10월 NA -> 0 
table(data2$month[which(data2$month >=4 & data2$month<=10 & is.na(data2$dd_mefs))])
data2$dd_mefs[which(data2$month >=4 & data2$month<=10 & is.na(data2$dd_mefs))]<-0
table(data2$month[ is.na(data2$dd_mefs)])

#####################강수량#####################
#sum_rn
plot(data2$tma,data2$sum_rn) 
table(data2$month[which(data2$sum_rn==0)])

#sum_rn_dur 일 합계 강수량 계속시간
plot(data2$tma,data2$sum_rn_dur) 
#250이상 -> outlier 삭제 
data2 %>%filter(sum_rn_dur >250)
data2$sum_rn_dur[(data2$sum_rn_dur >250)]=NA

#mi10_max_rn 일 10분 최다 강수량
plot(data2$tma,data2$mi10_max_rn) 
#60 이상인 경우 outlier 1개 
data2 %>%filter(mi10_max_rn >60)
data2$mi10_max_rn[(data2$mi10_max_rn >60)]=NA

#hr1_max_rn 일 1시간 최다 강수량
plot(data2$tma,data2$hr1_max_rn) 

#hr6_max_rn 일 6시간 최다 강수량
plot(data2$tma,data2$hr6_max_rn) 

#hr6_max_rn_dt 일 6시간 최다 강수량 일자
plot(data2$tma,data2$hr6_max_rn_dt)
table(data2$hr6_max_rn_dt) #1~31일 

#dd_rn 일 강수량(X)
plot(data2$tma,data2$dd_rn)

#n9_9_rn 9_9 강수량
plot(data2$tma,data2$n9_9_rn)


#######################3### NA 처리 ##########################

#변수들 간의 corr 판단 
cor_a = cor(subset(data2, select=-c(tma,stn)), use="pairwise.complete.obs")

# mice model
library(mice)
library(scorecard)


##########################.지역을 합쳐서(강원/충남/충북 등) 시도별 mice 돌리기 #######
back=read.csv("/Users/baeknarim/날씨빅콘/data/back_hospital.csv")
table(back$back_hospital.area)

table(data2$stn)

##stn 합쳐서 분류하는 where column 생성 (강원/ 충남/ 충복 등등 )
gangwon=c("강릉","대관령","동해","북강릉","속초","영월","원주","인제","정선군",
          "철원","춘천","태백","홍천")
genggi=c("동두천","수원","양평","이천","파주")
gengnam=c("거제","거창","김해시","남해","밀양","북창원","산청","양산시",
          "의령군","진주","창원","통영","함양군","합천")
gengbook=c("경주시","구미","문경","봉화","상주","성산","안동","영덕","영주",
           "영천","울릉도","울진","의성","청송군","포항")
gwangju=c("광주")
daegu=c("대구")
daejeon=c("대전")
busan=c("부산")
seoul=c("서울")
ulsan=c("울산")
incheon=c("강화","백령도","인천")
jeonnam=c("강진군","고흥","광양시","목포","보성군","순천","여수","영광군",
          "완도","장흥","진도(첨찰산)","진도군","해남","흑산도")
jeonbook=c("고산","고창","고창군","군산","남원","부안","순창군","임실","장수",
           "전주","정읍")
jeju=c("서귀포","제주")
chungnam=c("금산","보령","부여","서산")
chungbook=c("보은","제천","청주","추풍령","충주")

data2$where= ifelse(data2$stn %in% gangwon, "강원", 
       ifelse(data2$stn %in% genggi,"경기",
              ifelse(data2$stn %in% gengnam, "경남",
                     ifelse(data2$stn %in% gengbook, "경북",
                            ifelse(data2$stn %in% gwangju, "광주",
                                   ifelse(data2$stn %in% daegu, "대구",
                                          ifelse(data2$stn %in% daejeon, "대전",
                                                 ifelse(data2$stn %in% busan, "부산",
                                                        ifelse(data2$stn %in% seoul,"서울",
                                                               ifelse(data2$stn %in% ulsan, "울산",
                                                                      ifelse(data2$stn %in% incheon,"인천",
                                                                             ifelse(data2$stn %in% jeonnam, "전남",
                                                                                    ifelse(data2$stn %in% jeonbook, "전북",
                                                                                           ifelse(data2$stn %in% jeju, "제주",
                                                                                                  ifelse(data2$stn %in% chungnam, "충남",
                                                                                                         "충북")))))))))))))))


                     
table(data2$where)
#날짜순으로 정렬
data2_order <- data2[order(data2$tma), ]

#where column에 따라 data 나누기 
data3_where = split(data2_order, data2_order$where) 

#train data만 이용해서 mice model training 
#2016년 test data 

result=data.frame()
imp <- mice(data3_where$강원, 
                   ignore = c(rep(FALSE, length(which(data3_where$강원$year != 2016))), rep(TRUE,length(which(data3_where$강원$year == 2016)))), 
                   maxit = 5, m = 2, seed = 1)


result <- rbind(result,complete(imp))

#sum(is.na(data3_where$강원)) #200269
#sum(is.na(result$강원)) #17894

#summary(data3_where$강원)
#summary(result$강원)

#경기 
imp<- mice(data3_where$경기, 
                   ignore = c(rep(FALSE, length(which(data3_where$경기$year != 2016))), rep(TRUE,length(which(data3_where$경기$year == 2016)))), 
                   maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))

#
#경남 
imp<- mice(data3_where$경남, 
           ignore = c(rep(FALSE, length(which(data3_where$경남$year != 2016))), rep(TRUE,length(which(data3_where$경남$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))

#경기 
imp<- mice(data3_where$경북, 
           ignore = c(rep(FALSE, length(which(data3_where$경북$year != 2016))), rep(TRUE,length(which(data3_where$경북$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))

#광주 
imp<- mice(data3_where$광주, 
           ignore = c(rep(FALSE, length(which(data3_where$광주$year != 2016))), rep(TRUE,length(which(data3_where$광주$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))

#대구 
imp<- mice(data3_where$대구, 
           ignore = c(rep(FALSE, length(which(data3_where$대구$year != 2016))), rep(TRUE,length(which(data3_where$대구$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))

#대전 
imp<- mice(data3_where$대전, 
           ignore = c(rep(FALSE, length(which(data3_where$대전$year != 2016))), rep(TRUE,length(which(data3_where$대전$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))

#부산 
imp<- mice(data3_where$부산, 
           ignore = c(rep(FALSE, length(which(data3_where$부산$year != 2016))), rep(TRUE,length(which(data3_where$부산$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))

#서울 
imp<- mice(data3_where$서울, 
           ignore = c(rep(FALSE, length(which(data3_where$서울$year != 2016))), rep(TRUE,length(which(data3_where$서울$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))

#울산 
imp<- mice(data3_where$울산, 
           ignore = c(rep(FALSE, length(which(data3_where$울산$year != 2016))), rep(TRUE,length(which(data3_where$울산$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))

#인천 
imp<- mice(data3_where$인천, 
           ignore = c(rep(FALSE, length(which(data3_where$인천$year != 2016))), rep(TRUE,length(which(data3_where$인천$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))

#전남 
imp<- mice(data3_where$전남, 
           ignore = c(rep(FALSE, length(which(data3_where$전남$year != 2016))), rep(TRUE,length(which(data3_where$전남$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))


#전북 
imp<- mice(data3_where$전북, 
           ignore = c(rep(FALSE, length(which(data3_where$전북$year != 2016))), rep(TRUE,length(which(data3_where$전북$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))


#제주 
imp<- mice(data3_where$제주, 
           ignore = c(rep(FALSE, length(which(data3_where$제주$year != 2016))), rep(TRUE,length(which(data3_where$제주$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))


#충남 
imp<- mice(data3_where$충남, 
           ignore = c(rep(FALSE, length(which(data3_where$충남$year != 2016))), rep(TRUE,length(which(data3_where$충남$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))


#충북 
imp<- mice(data3_where$충북, 
           ignore = c(rep(FALSE, length(which(data3_where$충북$year != 2016))), rep(TRUE,length(which(data3_where$충북$year == 2016)))), 
           maxit = 5, m = 2, seed = 1)

result <- rbind(result,complete(imp))

summary(result)


write.csv(result, "/Users/baeknarim/날씨빅콘/data/full_전처리완료.csv", row.names=FALSE)

#########################NA 처리 #############################
data=read_csv("/Users/baeknarim/날씨빅콘/data/full_전처리완료.csv")
summary(data)

#stn이 NA인 경우 처리 
na_stn=data %>% filter(is.na(data$stn))
table(na_stn$stn_id)

where=read.csv("/Users/baeknarim/날씨빅콘/data/ASOS지점.csv")
where2= where[c("지점번호","지점명.한글.")]
names(where2)=c("stn_id", "stn")

#dd_mes, dd_mefs, dd_rn, n9_9_rn, hr24_sum_rws NA 처리 

#dd_mes
data %>% filter(stn=="강릉") %>% ggplot(aes(tma, dd_mes)) + geom_point()
data %>% filter(where=="강원") %>% ggplot(aes(tma, dd_mes)) + geom_point()
data %>% ggplot(aes(tma, dd_mes)) + geom_point()

na_dd_mes=data %>% filter(is.na(dd_mes))
table(na_dd_mes$stn) #모두 부산인 경우 dd_mes의 경우 NA -> 0으로 변경 
# 부산지역이 눈이 오긴 했지만 딱 5건이라.. (그것도 모두 10 이하)
data$dd_mes[is.na(data$dd_mes)] <- 0
data %>% filter(is.na(dd_mes)) #완료 

#dd_mefs
data %>% filter(stn=="강릉") %>% ggplot(aes(tma, dd_mefs)) + geom_point()
data %>% filter(where=="강원") %>% ggplot(aes(tma, dd_mefs)) + geom_point()
data %>% ggplot(aes(tma, dd_mefs)) + geom_point()

na_dd_mefs=data %>% filter(is.na(dd_mefs))
table(na_dd_mefs$stn) #모두 부산인 경우 dd_mefs 경우 NA -> 0으로 변경 

data$dd_mefs[is.na(data$dd_mefs)] <- 0
data %>% filter(is.na(dd_mefs)) #완료 

#dd_rn 일 강수량 -> 변수 삭제 
data %>% filter(stn=="강릉") %>% ggplot(aes(tma, dd_rn)) + geom_point()
data %>% filter(where=="강원") %>% ggplot(aes(tma, dd_rn)) + geom_point()
data %>% ggplot(aes(tma, dd_rn)) + geom_point()

na_dd_rn=data %>% filter(is.na(dd_rn))
table(na_dd_rn$where) 
cor(data$sum_rn, data$dd_rn,use="pairwise.complete.obs") # 1 
data= data %>% select(-dd_rn) #완료 

#n9_9_rn  9_9 강수량 

data %>% filter(stn=="강릉") %>% ggplot(aes(tma, n9_9_rn)) + geom_point()
data %>% filter(where=="강원") %>% ggplot(aes(tma, n9_9_rn)) + geom_point()
data %>% ggplot(aes(tma, n9_9_rn)) + geom_point()

na_n9_9_rn=data %>% filter(is.na(n9_9_rn))
table(na_n9_9_rn$where) #광주 대구 울산은 전체 값이  NA 

#광주 : 전남 평균치로 대체 
jen = data %>% filter(where=="전남")  %>% group_by(tma) %>% summarise(mean_n9_9_rn = mean(n9_9_rn))
data[data$where=="광주",]$n9_9_rn = jen$mean_n9_9_rn


#대구 :  경북으로 대체 
gen = data %>% filter(where=="경북")  %>% group_by(tma) %>% summarise(mean_n9_9_rn = mean(n9_9_rn))
data[data$where=="대구",]$n9_9_rn = gen$mean_n9_9_rn

 
#울산 :  경남 경북 평균으로 대체 
gen_2 = data %>% filter(where=="경남" | where=="경북")  %>% group_by(tma) %>% summarise(mean_n9_9_rn = mean(n9_9_rn))
data[data$where=="울산",]$n9_9_rn = gen_2$mean_n9_9_rn

data %>% filter(is.na(n9_9_rn))

#hr24_sum_rws : 모두 처리가 안됨.. 데이터의 범위가 넓어서 생기는 문제인듯 

data %>% filter(stn=="강릉") %>% ggplot(aes(tma, hr24_sum_rws)) + geom_point()
data %>% filter(where=="강원") %>% ggplot(aes(tma, hr24_sum_rws)) + geom_point()
data %>% ggplot(aes(tma, hr24_sum_rws)) + geom_point()

na_hr24_sum_rws=data %>% filter(is.na(hr24_sum_rws) )
table(na_hr24_sum_rws$stn)  #전체 지역에서 문제 
table(na_hr24_sum_rws$tma)

#이전 값으로 대체 
#각 where 평균치로 대체 
where_hr24_mean= data %>% group_by(where,tma) %>% summarise(mean_hr24_sum_rws = mean(hr24_sum_rws, na.rm=TRUE))

a=as.data.frame(left_join(na_hr24_sum_rws,where_hr24_mean,by=c("where","tma")))
data[is.na(data$hr24_sum_rws),"hr24_sum_rws"] = a$mean_hr24_sum_rws

#NA가 5가 존재 -> 모두 train data이므로 이전 값으로 대체 
data %>% filter(is.na(hr24_sum_rws) )
data[is.na(data$hr24_sum_rws),"hr24_sum_rws"]

library(zoo)
data$hr24_sum_rws=na.locf(data$hr24_sum_rws)

summary(data)

#stn NA인거 삭제 
unique(data2$stn)
data2= data[!is.na(data$stn),]

write.csv(data2, "/Users/baeknarim/날씨빅콘/data/full_전처리완료2.csv", row.names=FALSE)


a=read_csv("/Users/baeknarim/날씨빅콘/data/full_전처리완료2.csv")


 