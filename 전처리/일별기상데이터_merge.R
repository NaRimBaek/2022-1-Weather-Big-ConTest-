data1<-read.csv("/Users/baeknarim/날씨빅콘/data/DB_SFC_DSNW_DD.csv")
data2<-read.csv("/Users/baeknarim/날씨빅콘/data/DB_SFC_ICSR_SS_DD.csv")
data3<-read.csv("/Users/baeknarim/날씨빅콘/data/DB_SFC_LWT_TG_DD.csv")
data4<-read.csv("/Users/baeknarim/날씨빅콘/data/DB_SFC_PRSR_DD.csv")
data5<-read.csv("/Users/baeknarim/날씨빅콘/data/DB_SFC_PV_DD.csv")
data6<-read.csv("/Users/baeknarim/날씨빅콘/data/DB_SFC_RHM_DD.csv")
data7<-read.csv("/Users/baeknarim/날씨빅콘/data/DB_SFC_RN_DD.csv")
data8<-read.csv("/Users/baeknarim/날씨빅콘/data/DB_SFC_TA_DD.csv")
data9<-read.csv("/Users/baeknarim/날씨빅콘/data/DB_SFC_VS_DD.csv")
data10<-read.csv("/Users/baeknarim/날씨빅콘/data/DB_SFC_WIND_DD.csv")

#1열 삭제 
data1=data1[-1]
data2=data2[-1]
data3=data3[-1]
data4=data4[-1]
data5=data5[-1]
data6=data6[-1]
data7=data7[-1]
data8=data8[-1]
data9=data9[-1]
data10=data10[-1]

#tma 2011-01-01형식으로 변환 
#data1 ~ data8
#2011-01-01 00:00:00.0  -> 2011-01-01 :  앞 10글자만 

data1$tma=substr(data1$tma, 1,10)
data2$tma=substr(data2$tma, 1,10)
data3$tma=substr(data3$tma, 1,10)
data4$tma=substr(data4$tma, 1,10)
data5$tma=substr(data5$tma, 1,10)
data6$tma=substr(data6$tma, 1,10)
data7$tma=substr(data7$tma, 1,10)
data8$tma=substr(data8$tma, 1,10)
data9$tma=substr(data9$tma, 1,10)
data10$tma=substr(data10$tma, 1,10)


#stn_id 
where=read.csv("/Users/baeknarim/날씨빅콘/data/ASOS지점.csv")
where2= where[c("지점번호","지점명.한글.")]
names(where2)=c("stn_id", "stn")

#tma, stn_id 기준으로 병합 
library(tidyverse)

full= merge(data1, data2, by=c("tma","stn_id"), all=T)
full= merge(full, data3, by=c("tma","stn_id"), all=T)
full= merge(full, data4, by=c("tma","stn_id"), all=T)
full= merge(full, data5, by=c("tma","stn_id"), all=T)
full= merge(full, data6, by=c("tma","stn_id"), all=T)
full= merge(full, data7, by=c("tma","stn_id"), all=T)
full= merge(full, data8, by=c("tma","stn_id"), all=T)
full= merge(full, data9, by=c("tma","stn_id"), all=T)
full= merge(full, data10, by=c("tma","stn_id"), all=T)

full= merge(full, where2, by="stn_id", all=T)

#stn_id = 864 삭제
full = full[!(full$stn_id == 864),]

#full stn_id, tma 순으로 정렬 
library(tidyverse)
full=arrange(full, stn_id,tma)

dim(full) #204241     52
summary(full)


write.csv(full, "/Users/baeknarim/날씨빅콘/data/full.csv", row.names=FALSE)
data=read.csv("/Users/baeknarim/날씨빅콘/data/full.csv")








