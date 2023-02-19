#
full= read.csv("/Users/baeknarim/날씨빅콘/data/full_전처리완료2.csv")

#천안(132)/ 청주(131) / 보은(226) / 대전(133) / 부여(236) / 금산(238)  평균치


list_w = c("천안","청주","보은","대전", "부여", "금산") 

a = full %>% filter(stn %in% list_w)
a = arrange( a, tma) #날짜별 정렬 

sejong= a %>% group_by(tma) %>% summarise(dd_mes= mean(dd_mes),
                                  sum_dpth_fhsc= mean(sum_dpth_fhsc),
                                  dd_mefs= mean(dd_mefs),
                                  ssrate= mean(ssrate),
                                  ss_dur= mean(ss_dur),
                                  sum_ss_hr = mean(sum_ss_hr),
                                  min_tg= mean(min_tg),
                                  avg_pa= mean(avg_pa),
                                  max_pa= mean(max_pa),
                                  min_pa= mean(min_pa),
                                  avg_pv= mean(avg_pv),
                                  max_pv= mean(max_pv),
                                  min_pv= mean(min_pv),
                                  avg_rhm= mean(avg_rhm),
                                  min_rhm= mean(min_rhm),
                                  sum_rn= mean(sum_rn),
                                  sum_rn_dur= mean(sum_rn_dur),
                                  mi10_max_rn= mean(mi10_max_rn),
                                  hr1_max_rn = mean(hr1_max_rn),
                                  hr6_max_rn= mean(hr6_max_rn),
                                  hr6_max_rn_dt= mean(hr6_max_rn_dt),
                                  n9_9_rn= mean(n9_9_rn),
                                  avg_ta= mean(avg_ta),
                                  max_ta= mean(max_ta),
                                  min_ta= mean(min_ta),
                                  avg_ws= mean(avg_ws),
                                  mix_ws= mean(mix_ws),
                                  mix_wd= mean(mix_wd),
                                  max_ws= mean(max_ws),
                                  max_ws_wd= mean(max_ws_wd),
                                  max_ins_ws= mean(max_ins_ws),
                                  max_ins_ws_wd= mean(max_ins_ws_wd),
                                  hr24_sum_rws= mean(hr24_sum_rws),
                                  max_wd = mean(max_wd),
                                  max_ws_min= mean(max_ws_min),
                                  max_ws_hour= mean(max_ws_hour),
                                  max_ins_ws_min= mean(max_ins_ws_min),
                                  max_ins_ws_hour= mean(max_ins_ws_hour),
                                   )

sejong$year = substr(sejong$tma, 1, 4)

sejong$month =substr(sejong$tma, 6, 7)

sejong$days =substr(sejong$tma, 9, 10)

sejong$where= "세종"

sejong[2:39]=round(sejong[2:39],1)
head(sejong)

#저장
write.csv(sejong, "/Users/baeknarim/날씨빅콘/data/sejong_full.csv", row.names=FALSE)
data=read.csv("/Users/baeknarim/날씨빅콘/data/sejong_full.csv")





