.libPaths(c("\\\\ds.leeds.ac.uk/lib/Apps/R/R-3.6.0/library",.libPaths()))

# This script merges the CSV files that has already classified in QGIS,
## a field 'Order' has been added to specify the section 
# 
# 
# The code require the libraries: tidyverse and data.table


library(tidyverse)
library(data.table)

### importing file with after being processed in QGIS with the sector classification    #####

new_df <- read_csv("~/2. Dissertation/3. Data Processing/results/QGIS_processed_concentrations_points.csv", 
                   col_types = cols(ABS_time = col_datetime(format = "X%Y.%m.%d.%H.%M.%S"),
                                    DateShort = col_date(format = "%d/%m/%Y"),
                                    Participan = col_factor(levels = c("A","B", "X", "Y")),
                                    desc = col_factor(levels = c("F","D")), 
                                    TypeRoute = col_factor(levels = c("Polluted", "Track")),
                                    TypeSpeed = col_factor(levels = c("Low","High")), 
                                    TypeLeg = col_factor(levels = c("Inbound","Outbound","Track"))))
new_df$Order<-as.factor(new_df$Order)

### importing file with data of sectors' properties #####

route_sections<- read_csv("~/2. Dissertation/3. Data Processing/results/QGIS_processed_route_sections.csv", 
                          col_types = cols(Order = col_character()))
route_sections$Order<-as.factor(route_sections$Order)

### importing file with breathign data and GPS after being processed in QGIS with the sector classification #####

data_breath <- read_csv("~/2. Dissertation/3. Data Processing/results/new_df_sz_complete3.csv", 
                        col_types = cols(ABS_time = col_datetime(format = "X%Y.%m.%d.%H.%M.%S"),
                                         DateShort = col_date(format = "%d/%m/%Y"),
                                         Participan = col_factor(levels = c("A","B", "X", "Y")),
                                         desc = col_factor(levels = c("F","D")), 
                                         TypeRoute = col_factor(levels = c("Polluted", "Track")), 
                                         TypeSpeed = col_factor(levels = c("Low","High")), 
                                         TypeLeg = col_factor(levels = c("Inbound","Outbound","Track"))))
data_breath$Order<-as.factor(data_breath$Order)


##### Calculating concentration aggreggated values per sector ######


sum_conc_d<-new_df%>%filter(!is.na(Order))%>%group_by(Order,TypeLeg,Day)%>%summarise(n_part=length(conc[!is.na(conc)]),
                                                                               mean_part=mean(conc,na.rm = TRUE),
                                                                               median_part=median(conc,na.rm = TRUE),
                                                                               sd_part=sd(conc,na.rm = TRUE),
                                                                               min_part=min(conc,na.rm = TRUE),
                                                                               max_part=max(conc,na.rm = TRUE),
                                                                               q1_part=quantile(conc,0.25,na.rm = TRUE),
                                                                               q2_part=quantile(conc,0.75,na.rm = TRUE))




##### Calculating breathing aggreggated values per sector ######

data_breath$t_volume<-data_breath$flow/data_breath$breathRate

sum_breath<-data_breath%>%filter(!is.na(Order))%>%group_by(Order,
                                                           TypeSpeed,
                                                           TypeLeg,
                                                           Participan)%>%summarise(n_vent=length(flow[!is.na(flow)]),
                                                                                   mean_vent=mean(flow,na.rm = TRUE),
                                                                                   median_vent=median(flow,na.rm = TRUE),
                                                                                   sd_vent=sd(flow,na.rm = TRUE),
                                                                                   min_vent=min(flow,na.rm = TRUE),
                                                                                   max_vent=max(flow,na.rm = TRUE),
                                                                                   q1_vent=quantile(flow,0.25,na.rm = TRUE),
                                                                                   q2_vent=quantile(flow,0.75,na.rm = TRUE),
                                                                                   n_HR=length(HR[!is.na(HR)]),
                                                                                   mean_HR=mean(HR,na.rm = TRUE),
                                                                                   median_HR=median(HR,na.rm = TRUE),
                                                                                   sd_HR=sd(HR,na.rm = TRUE),
                                                                                   min_HR=min(HR,na.rm = TRUE),
                                                                                   max_HR=max(HR,na.rm = TRUE),
                                                                                   q1_HR=quantile(HR,0.25,na.rm = TRUE),
                                                                                   q2_HR=quantile(HR,0.75,na.rm = TRUE),
                                                                                   n_BR=length(breathRate[!is.na(breathRate)]),
                                                                                   mean_BR=mean(breathRate,na.rm = TRUE),
                                                                                   median_BR=median(breathRate,na.rm = TRUE),
                                                                                   sd_BR=sd(breathRate,na.rm = TRUE),
                                                                                   min_BR=min(breathRate,na.rm = TRUE),
                                                                                   max_BR=max(breathRate,na.rm = TRUE),
                                                                                   q1_BR=quantile(breathRate,0.25,na.rm = TRUE),
                                                                                   q2_BR=quantile(breathRate,0.75,na.rm = TRUE),
                                                                                   mean_speed=mean(speed,na.rm = TRUE),
                                                                                   mean_TV=mean(t_volume,na.rm = TRUE),
                                                                                   median_TV=median(t_volume,na.rm = TRUE),
                                                                                   sd_TV=sd(t_volume,na.rm = TRUE),
                                                                                   min_TV=min(t_volume,na.rm = TRUE),
                                                                                   max_TV=max(t_volume,na.rm = TRUE),
                                                                                   q1_TV=quantile(t_volume,0.25,na.rm = TRUE),
                                                                                   q2_TV=quantile(t_volume,0.75,na.rm = TRUE),
                                                                                   mean_speed=mean(speed,na.rm = TRUE))


sum_speeds<-data_breath%>%filter(!is.na(Order))%>%group_by(TypeSpeed,
                                                           Order,
                                                           Day,
                                                           Participan,
                                                           TypeLeg)%>%summarise(n_speed=length(speed),
                                                                                n_lowspeed=length(speed[(speed<5)]),
                                                                                sec_time=difftime(max(ABS_time),min(ABS_time),units = "secs")+1,
                                                                                elev_change=Elev[ABS_time==max(ABS_time)]-Elev[ABS_time==min(ABS_time)])
sum_elev<-sum_speeds%>%group_by(Order,TypeLeg)%>%summarise(sector_elev_change=mean(elev_change,na.rm = TRUE))

sum_elev$slope<-sum_elev$sector_elev_change/abs(sum_elev$sector_elev_change)
sum_elev2<-sum_elev%>%group_by(Order)%>%summarise(Sec_elev_delta=max(abs(sector_elev_change)))

sum_elev<-data.table(sum_elev, key="Order")[
  data.table(sum_elev2, key="Order"),
  allow.cartesian=TRUE
  ]
rm(sum_elev2)

sum_elev$Sec_elev_delta<-sum_elev$Sec_elev_delta*sum_elev$slope

sum_elev<-sum_elev[,c(1,2,4,5)]



sum_speeds<-sum_speeds%>%group_by(Order,TypeSpeed,TypeLeg)%>%summarise(low_speed_port=sum(n_lowspeed)/sum(n_speed),
                                                                       sector_time=mean(sec_time))


##### Calculating total length per sector ######

sum_sections<-route_sections%>%group_by(Order)%>%summarise(length=sum(longitude),
                                                           Bus_stops=max(Bus_stops),
                                                           Traffic_lights=max(Traffic_lights),
                                                           canyon=max(Canyon))

sum_sections$Traffic_lights<-as.logical(sum_sections$Traffic_lights)
sum_sections$Bus_stops<-as.logical(sum_sections$Bus_stops)
sum_sections$canyon<-as.logical(sum_sections$canyon)
##### Merging summaries ######

Sum_overall<-data.table(sum_conc_d, key="Order,TypeLeg")[
  data.table(sum_speeds, key="Order,TypeLeg"),
  allow.cartesian=TRUE
  ]

Sum_overall<-data.table(Sum_overall, key="Order,TypeLeg")[
  data.table(sum_elev, key="Order,TypeLeg"),
  allow.cartesian=TRUE
  ]

Sum_overall<-data.table(Sum_overall, key="Order,TypeLeg,TypeSpeed")[
  data.table(sum_breath, key="Order,TypeLeg,TypeSpeed"),
  allow.cartesian=TRUE
  ]

Sum_overall<-data.table(Sum_overall, key="Order")[
  data.table(sum_sections, key="Order"),
  allow.cartesian=TRUE
  ]

##### ordering the section as they were named ######

Sum_overall$Order<-factor(Sum_overall$Order,levels = c("1","2","3","4","5","6","7","8","9",
                                                       "10","13","15","16","17","18","20",
                                                       "21","22","23","24","25","26","27",
                                                       "28","29","30","31","32","33","34",
                                                       "35","36","37","38","39","41","43",
                                                       "45","46","48","49","51","52","53",
                                                       "54","56","57","58","60","61","62",
                                                       "63","65","66","67","68","69","70",
                                                       "71","72","73","74","76","79.1","79.2"),
                          ordered = TRUE)


Sum_overall$avg_speed<-(Sum_overall$length/as.numeric(Sum_overall$sector_time))*3.6
Sum_overall$sec_dose<-as.numeric(Sum_overall$median_part*(Sum_overall$median_vent*(1000/60))*Sum_overall$sector_time)
Sum_overall$sec_inhvol<-as.numeric((Sum_overall$median_vent*(1000/60))*Sum_overall$sector_time)

Sum_overall$sec_inhvol_CC<-Sum_overall$sec_inhvol

Sum_overall$sec_dose_sd<-((Sum_overall$mean_part^2+Sum_overall$sd_part^2)*
                            ((Sum_overall$mean_vent*(1000/60)*as.numeric(Sum_overall$sector_time))^2+
                               (Sum_overall$sd_vent*(1000/60)*as.numeric(Sum_overall$sector_time))^2)-
                                       (Sum_overall$mean_vent*(1000/60)*as.numeric(Sum_overall$sector_time))^2*
                                       Sum_overall$mean_part^2)


Sum_overall$avg_grad<-Sum_overall$Sec_elev_delta/Sum_overall$length*100



Sum_overall<-Sum_overall[order(Sum_overall$Day,Sum_overall$Participan,Sum_overall$TypeSpeed,Sum_overall$TypeLeg,Sum_overall$Order)]

sum_Out<-Sum_overall[Sum_overall$TypeLeg=="Outbound",]
sum_Out<-sum_Out[order(sum_Out$Day,sum_Out$Participan,sum_Out$TypeSpeed,-sum_Out$Order)]

sum_In<-Sum_overall[Sum_overall$TypeLeg=="Inbound",]
sum_In<-sum_In[order(sum_In$Participan,sum_In$TypeSpeed,sum_In$Order)]

Sum_overall<-rbind(sum_Out,sum_In)
Sum_overall<-Sum_overall%>%filter(Order!="79.1"&Order!="79.2")

Sum_overall<-Sum_overall%>%group_by(Participan,Day,TypeLeg,TypeSpeed)%>%mutate(cum_dose=cumsum(sec_dose),
                                                                           cum_dist=cumsum(length),
                                                                           cum_time=cumsum(as.numeric(sector_time)),
                                                                           cum_elev=cumsum(Sec_elev_delta))

Sum_overall<-Sum_overall%>%group_by(Participan,Day,TypeLeg,TypeSpeed)%>%mutate(rel_freq=length/max(cum_dist),
                                                                           sec_dose_CC=sum(median_part))

Sum_overall$sec_dose_CC<-as.numeric((Sum_overall$rel_freq*Sum_overall$sec_dose_CC)*(Sum_overall$median_vent*(1000/60))*Sum_overall$sector_time)

Sum_overall$cum_elev[Sum_overall$TypeLeg=="Inbound"]<-Sum_overall$cum_elev[Sum_overall$TypeLeg=="Inbound"]+max(Sum_overall$cum_elev)

Sum_overall$cum_elev<-Sum_overall$cum_elev-min(Sum_overall$cum_elev)

Sum_overall<-Sum_overall%>%group_by(Participan,TypeLeg,TypeSpeed)%>%mutate(cum_dose_CC=cumsum(sec_dose_CC))

Sum_overall$sec_dose_CV<-as.numeric(Sum_overall$median_part*(55*(1000/60))*Sum_overall$sector_time)

Sum_overall$sec_inhvol_CV<-as.numeric((55*(1000/60))*Sum_overall$sector_time)

Sum_overall$sec_dose_CV[Sum_overall$TypeSpeed=="Low"]<-Sum_overall$sec_dose_CV[Sum_overall$TypeSpeed=="Low"]/(55/40)

Sum_overall$sec_inhvol_CV[Sum_overall$TypeSpeed=="Low"]<-Sum_overall$sec_inhvol_CV[Sum_overall$TypeSpeed=="Low"]/(55/40)

Sum_overall<-Sum_overall%>%group_by(Participan,TypeLeg,TypeSpeed)%>%mutate(cum_dose_CV=cumsum(sec_dose_CV))






######## Merging type of section to main dataframe #######

new_df<-data.table(new_df, key="Order")[
  data.table(sum_sections, key="Order"),
  allow.cartesian=TRUE
  ]

new_df$Order<-factor(new_df$Order,levels = c("1","2","3","4","5","6","7","8","9",
                                             "10","13","15","16","17","18","20",
                                             "21","22","23","24","25","26","27",
                                             "28","29","30","31","32","33","34",
                                             "35","36","37","38","39","41","43",
                                             "45","46","48","49","51","52","53",
                                             "54","56","57","58","60","61","62",
                                             "63","65","66","67","68","69","70",
                                             "71","72","73","74","76","79.1","79.2"),
                     ordered = TRUE)

data_breath<-data.table(data_breath, key="Order")[
  data.table(sum_sections, key="Order"),
  allow.cartesian=TRUE
  ]

data_breath$Order<-factor(data_breath$Order,levels = c("1","2","3","4","5","6","7","8","9",
                                             "10","13","15","16","17","18","20",
                                             "21","22","23","24","25","26","27",
                                             "28","29","30","31","32","33","34",
                                             "35","36","37","38","39","41","43",
                                             "45","46","48","49","51","52","53",
                                             "54","56","57","58","60","61","62",
                                             "63","65","66","67","68","69","70",
                                             "71","72","73","74","76","79.1","79.2"),
                     ordered = TRUE)

#####ratios calculation #####

ratios_overall<-cbind(Sum_overall%>%group_by(Participan,TypeLeg,Order)%>%filter(TypeSpeed=="Low")%>%select(Participan,TypeLeg,Order,cum_dist,sec_dose,avg_speed,avg_grad,median_vent),Sum_overall%>%group_by(Participan,TypeLeg,Order)%>%filter(TypeSpeed=="High")%>%select(Participan,TypeLeg,Order,sec_dose,avg_speed,median_vent))

ratios_overall<-ratios_overall%>%select(Participan,TypeLeg,Order,cum_dist,sec_dose,sec_dose1,avg_speed,avg_speed1,avg_grad,median_vent,median_vent1)

names(ratios_overall)<-c("Participan","TypeLeg","Order","cum_dist","sec_dose_LS","sec_dose_HS","speed_LS","speed_HS","avg_grad","vent_LS","vent_HS")

ratios_overall$ratio_dose<-((ratios_overall$sec_dose_HS-ratios_overall$sec_dose_LS)/ratios_overall$sec_dose_LS)
ratios_overall$ratio_vent<-((ratios_overall$vent_HS-ratios_overall$vent_LS)/ratios_overall$vent_LS)
ratios_overall$speed_diff<-ratios_overall$speed_HS-ratios_overall$speed_LS

######### summary of doses########

sum_doses<-Sum_overall%>%group_by(Participan,Day,TypeLeg,TypeSpeed)%>%summarise(mean_dose=sum(sec_dose,na.rm = TRUE),
                                                                            CV_dose=sum(sec_dose_CV,na.rm = TRUE),
                                                                            CC_dose=sum(sec_dose_CC,na.rm = TRUE))

sum_inhvol<-Sum_overall%>%group_by(Participan,Day,TypeLeg,TypeSpeed)%>%summarise(mean_inhvol=sum(sec_inhvol,na.rm = TRUE),
                                                                             CV_inhvol=sum(sec_inhvol_CV,na.rm = TRUE),
                                                                             CC_inhvol=sum(sec_inhvol_CC,na.rm = TRUE))


####### RATIO OF LOW-HIGH #########

ratio_doses<-cbind(sum_doses%>%group_by(Participan,TypeLeg)%>%filter(TypeSpeed=="Low")%>%select("Participan","TypeLeg","mean_dose","CV_dose","CC_dose"),sum_doses%>%group_by(Participan,TypeLeg)%>%filter(TypeSpeed=="High")%>%select("Participan","TypeLeg","mean_dose","CV_dose","CC_dose"))

ratio_doses<-ratio_doses%>%select("Participan","TypeLeg","mean_dose","CV_dose","CC_dose","mean_dose1","CV_dose1","CC_dose1")
names(ratio_doses)<-c("Participan","TypeLeg","dose_LS","CV_LS","CC_LS","dose_HS","CV_HS","CC_HS")
ratio_doses$ratio_real<-(ratio_doses$dose_HS-ratio_doses$dose_LS)/ratio_doses$dose_LS
ratio_doses$ratio_CV<-(ratio_doses$CV_HS-ratio_doses$CV_LS)/ratio_doses$CV_LS
ratio_doses$ratio_CC<-(ratio_doses$CC_HS-ratio_doses$CC_LS)/ratio_doses$CC_LS
ratio_doses<-ratio_doses%>%select("Participan","TypeLeg","ratio_real","ratio_CV","ratio_CC")

r1<-ratio_doses%>%select("Participan","TypeLeg","ratio_real")
names(r1)<-c("Participan","TypeLeg","Ratio")
r1$Type<-"Dynamic"

r2<-ratio_doses%>%select("Participan","TypeLeg","ratio_CV")
names(r2)<-c("Participan","TypeLeg","Ratio")
r2$Type<-"Fixed Par"

r3<-ratio_doses%>%select("Participan","TypeLeg","ratio_CC")
names(r3)<-c("Participan","TypeLeg","Ratio")
r3$Type<-"Avg. Conc."

ratio_doses<-rbind(r1,r2,r3)

ratio_doses$Type2<-"Inhaled_Dos"

#####ratio inhvol

ratio_inhvol<-cbind(sum_inhvol%>%group_by(Participan,TypeLeg)%>%filter(TypeSpeed=="Low")%>%select("Participan","TypeLeg","mean_inhvol","CV_inhvol","CC_inhvol"),sum_inhvol%>%group_by(Participan,TypeLeg)%>%filter(TypeSpeed=="High")%>%select("Participan","TypeLeg","mean_inhvol","CV_inhvol","CC_inhvol"))

ratio_inhvol<-ratio_inhvol%>%select("Participan","TypeLeg","mean_inhvol","CV_inhvol","CC_inhvol","mean_inhvol1","CV_inhvol1","CC_inhvol1")
names(ratio_inhvol)<-c("Participan","TypeLeg","dose_LS","CV_LS","CC_LS","dose_HS","CV_HS","CC_HS")
ratio_inhvol$ratio_real<-(ratio_inhvol$dose_HS-ratio_inhvol$dose_LS)/ratio_inhvol$dose_LS
ratio_inhvol$ratio_CV<-(ratio_inhvol$CV_HS-ratio_inhvol$CV_LS)/ratio_inhvol$CV_LS
ratio_inhvol$ratio_CC<-(ratio_inhvol$CC_HS-ratio_inhvol$CC_LS)/ratio_inhvol$CC_LS
ratio_inhvol<-ratio_inhvol%>%select("Participan","TypeLeg","ratio_real","ratio_CV","ratio_CC")

r1<-ratio_inhvol%>%select("Participan","TypeLeg","ratio_real")
names(r1)<-c("Participan","TypeLeg","Ratio")
r1$Type<-"Dynamic"

r2<-ratio_inhvol%>%select("Participan","TypeLeg","ratio_CV")
names(r2)<-c("Participan","TypeLeg","Ratio")
r2$Type<-"Fixed Par"

r3<-ratio_inhvol%>%select("Participan","TypeLeg","ratio_CC")
names(r3)<-c("Participan","TypeLeg","Ratio")
r3$Type<-"Avg. Conc."

ratio_inhvol<-rbind(r1,r2,r3)

ratio_inhvol$Type2<-"Inhaled_Vol"

ratio_final<-rbind(ratio_doses,ratio_inhvol)

rm(r1,r2,r3,ratio_doses,ratio_inhvol)

ratio_final$Type<-factor(ratio_final$Type,levels = c("Dynamic","Fixed Par","Avg. Conc."),ordered = T)

####### RATIO OF DYNAMIC-OTHERS #########

ratio_doses<-cbind(sum_doses%>%group_by(Participan,TypeLeg)%>%filter(TypeSpeed=="Low")%>%select("Participan","TypeLeg","mean_dose","CV_dose","CC_dose"),sum_doses%>%group_by(Participan,TypeLeg)%>%filter(TypeSpeed=="High")%>%select("Participan","TypeLeg","mean_dose","CV_dose","CC_dose"))

ratio_doses<-ratio_doses%>%select("Participan","TypeLeg","mean_dose","CV_dose","CC_dose","mean_dose1","CV_dose1","CC_dose1")
names(ratio_doses)<-c("Participan","TypeLeg","dose_LS","CV_LS","CC_LS","dose_HS","CV_HS","CC_HS")
ratio_doses$ratio_CV_LS<-(ratio_doses$CV_LS-ratio_doses$dose_LS)/ratio_doses$dose_LS
ratio_doses$ratio_CV_HS<-(ratio_doses$CV_HS-ratio_doses$dose_HS)/ratio_doses$dose_HS
ratio_doses$ratio_CC_LS<-(ratio_doses$CC_LS-ratio_doses$dose_LS)/ratio_doses$dose_LS
ratio_doses$ratio_CC_HS<-(ratio_doses$CC_HS-ratio_doses$dose_HS)/ratio_doses$dose_HS
ratio_doses<-ratio_doses%>%select("Participan","TypeLeg","ratio_CV_LS","ratio_CV_HS","ratio_CC_LS","ratio_CC_HS")

r1<-ratio_doses%>%select("Participan","TypeLeg","ratio_CV_LS")
names(r1)<-c("Participan","TypeLeg","Ratio")
r1$Type<-"Fixed Par"
r1$Type1<-"Low speed"

r2<-ratio_doses%>%select("Participan","TypeLeg","ratio_CV_HS")
names(r2)<-c("Participan","TypeLeg","Ratio")
r2$Type<-"Fixed Par"
r2$Type1<-"High speed"

r3<-ratio_doses%>%select("Participan","TypeLeg","ratio_CC_LS")
names(r3)<-c("Participan","TypeLeg","Ratio")
r3$Type<-"Avg. Conc."
r3$Type1<-"Low speed"

r4<-ratio_doses%>%select("Participan","TypeLeg","ratio_CC_HS")
names(r4)<-c("Participan","TypeLeg","Ratio")
r4$Type<-"Avg. Conc."
r4$Type1<-"High speed"

ratio_doses<-rbind(r1,r2,r3,r4)

ratio_doses$Type2<-"Inhaled_Dos"

#####ratio inhvol

ratio_inhvol<-cbind(sum_inhvol%>%group_by(Participan,TypeLeg)%>%filter(TypeSpeed=="Low")%>%select("Participan","TypeLeg","mean_inhvol","CV_inhvol","CC_inhvol"),sum_inhvol%>%group_by(Participan,TypeLeg)%>%filter(TypeSpeed=="High")%>%select("Participan","TypeLeg","mean_inhvol","CV_inhvol","CC_inhvol"))

ratio_inhvol<-ratio_inhvol%>%select("Participan","TypeLeg","mean_inhvol","CV_inhvol","CC_inhvol","mean_inhvol1","CV_inhvol1","CC_inhvol1")
names(ratio_inhvol)<-c("Participan","TypeLeg","dose_LS","CV_LS","CC_LS","dose_HS","CV_HS","CC_HS")
ratio_inhvol$ratio_CV_LS<-(ratio_inhvol$CV_LS-ratio_inhvol$dose_LS)/ratio_inhvol$dose_LS
ratio_inhvol$ratio_CV_HS<-(ratio_inhvol$CV_HS-ratio_inhvol$dose_HS)/ratio_inhvol$dose_HS
ratio_inhvol$ratio_CC_LS<-(ratio_inhvol$CC_LS-ratio_inhvol$dose_LS)/ratio_inhvol$dose_LS
ratio_inhvol$ratio_CC_HS<-(ratio_inhvol$CC_HS-ratio_inhvol$dose_HS)/ratio_inhvol$dose_HS
ratio_inhvol<-ratio_inhvol%>%select("Participan","TypeLeg","ratio_CV_LS","ratio_CV_HS","ratio_CC_LS","ratio_CC_HS")

r1<-ratio_inhvol%>%select("Participan","TypeLeg","ratio_CV_LS")
names(r1)<-c("Participan","TypeLeg","Ratio")
r1$Type<-"Fixed Par"
r1$Type1<-"Low speed"

r2<-ratio_inhvol%>%select("Participan","TypeLeg","ratio_CV_HS")
names(r2)<-c("Participan","TypeLeg","Ratio")
r2$Type<-"Fixed Par"
r2$Type1<-"High speed"

r3<-ratio_inhvol%>%select("Participan","TypeLeg","ratio_CC_LS")
names(r3)<-c("Participan","TypeLeg","Ratio")
r3$Type<-"Avg. Conc."
r3$Type1<-"Low speed"

r4<-ratio_inhvol%>%select("Participan","TypeLeg","ratio_CC_HS")
names(r4)<-c("Participan","TypeLeg","Ratio")
r4$Type<-"Avg. Conc."
r4$Type1<-"High speed"

ratio_inhvol<-rbind(r1,r2,r3,r4)

ratio_inhvol$Type2<-"Inhaled_Vol"

ratio_final2<-rbind(ratio_doses,ratio_inhvol)

rm(r1,r2,r3,ratio_doses,ratio_inhvol)

ratio_final2$Type<-factor(ratio_final$Type,levels = c("Dynamic","Fixed Par","Avg. Conc."),ordered = T)



######### summary of sectors ########

sum_ps<-Sum_overall%>%group_by(Participan,Day,TypeLeg,TypeSpeed)%>%summarise(mean_dose_ps=mean(sec_dose,na.rm = TRUE),
                                                                         range_dose_ps=range(sec_dose,na.rm = TRUE)[2]-range(sec_dose,na.rm = TRUE)[1],
                                                                         mean_conc_ps=mean(median_part,na.rm = TRUE),
                                                                         range_conc_ps=range(median_part,na.rm = TRUE)[2]-range(median_part,na.rm = TRUE)[1],
                                                                         mean_BR_ps=mean(median_BR,na.rm = TRUE),
                                                                         range_BR_ps=range(median_BR,na.rm = TRUE)[2]-range(median_BR,na.rm = TRUE)[1],
                                                                         mean_vent_ps=mean(median_vent,na.rm = TRUE),
                                                                         range_vent_ps=range(median_vent,na.rm = TRUE)[2]-range(median_vent,na.rm = TRUE)[1])
                                                                         
                                                                          

view(sum_ps)



########## summary of ventilation rates#########

sum_ventrates<-data_breath%>%group_by(Participan,TypeLeg,TypeSpeed)%>%summarise(n_vent=length(flow[!is.na(flow)]),
                                                                     mean_vent=mean(flow,na.rm = TRUE),
                                                                     sd_vent=sd(flow,na.rm = TRUE),
                                                                     min_vent=quantile(flow,0.05,na.rm = TRUE),
                                                                     q1_vent=quantile(flow,0.25,na.rm = TRUE),
                                                                     median_vent=median(flow,na.rm = TRUE),
                                                                     q2_vent=quantile(flow,0.75,na.rm = TRUE),
                                                                     max_vent=quantile(flow,0.95,na.rm = TRUE),
                                                                     mean_HR=mean(HR,na.rm = TRUE),
                                                                     sd_HR=sd(HR,na.rm = TRUE),
                                                                     min_HR=quantile(HR,0.05,na.rm = TRUE),
                                                                     q1_HR=quantile(HR,0.25,na.rm = TRUE),
                                                                     median_HR=median(HR,na.rm = TRUE),
                                                                     q2_HR=quantile(HR,0.75,na.rm = TRUE),
                                                                     max_HR=quantile(HR,0.95,na.rm = TRUE),
                                                                     mean_BR=mean(breathRate,na.rm = TRUE),
                                                                     sd_BR=sd(breathRate,na.rm = TRUE),
                                                                     min_BR=quantile(breathRate,0.05,na.rm = TRUE),
                                                                     q1_BR=quantile(breathRate,0.25,na.rm = TRUE),
                                                                     median_BR=median(breathRate,na.rm = TRUE),
                                                                     q2_BR=quantile(breathRate,0.75,na.rm = TRUE),
                                                                     max_BR=quantile(breathRate,0.95,na.rm = TRUE))


sum_ventrates

########## summary of concentrations ################

sum_concs<-new_df%>%group_by(TypeLeg)%>%summarise(n_part=length(conc[!is.na(conc)]),
                                                                mean_conc=mean(conc,na.rm = TRUE),
                                                                sd_part=sd(conc,na.rm = TRUE),
                                                                min_part=quantile(conc,0.05,na.rm = TRUE),
                                                                q1_part=quantile(conc,0.25,na.rm = TRUE),
                                                                median_conc=median(conc,na.rm = TRUE),
                                                                q2_part=quantile(conc,0.75,na.rm = TRUE),
                                                                max_part=quantile(conc,0.95,na.rm = TRUE))
                                                              


sum_concs


########## summary of concentrations ################

sum_concs<-new_df%>%group_by(Day)%>%summarise(n_part=length(conc[!is.na(conc)]),
                                                  mean_conc=mean(conc,na.rm = TRUE),
                                                  sd_part=sd(conc,na.rm = TRUE),
                                                  min_part=quantile(conc,0.05,na.rm = TRUE),
                                                  q1_part=quantile(conc,0.25,na.rm = TRUE),
                                                  median_conc=median(conc,na.rm = TRUE),
                                                  q2_part=quantile(conc,0.75,na.rm = TRUE),
                                                  max_part=quantile(conc,0.95,na.rm = TRUE))



sum_concs



########## summary of canyons ################

sum_cany<-new_df%>%filter(canyon)%>%group_by(Order,TypeLeg,Day)%>%summarise(n_part=length(conc[!is.na(conc)]),
                                                  mean_conc=mean(conc,na.rm = TRUE),
                                                  sd_part=sd(conc,na.rm = TRUE),
                                                  min_part=quantile(conc,0.05,na.rm = TRUE),
                                                  q1_part=quantile(conc,0.25,na.rm = TRUE),
                                                  median_conc=median(conc,na.rm = TRUE),
                                                  q2_part=quantile(conc,0.75,na.rm = TRUE),
                                                  max_part=quantile(conc,0.95,na.rm = TRUE))



sum_cany
write.csv(sum_cany,"canyons.csv")

########## summary of traffic lights ################

sum_traff<-new_df%>%filter(Traffic_lights)%>%group_by(Order,TypeLeg)%>%summarise(n_part=length(conc[!is.na(conc)]),
                                                                        mean_conc=mean(conc,na.rm = TRUE),
                                                                        sd_part=sd(conc,na.rm = TRUE),
                                                                        min_part=quantile(conc,0.05,na.rm = TRUE),
                                                                        q1_part=quantile(conc,0.25,na.rm = TRUE),
                                                                        median_conc=median(conc,na.rm = TRUE),
                                                                        q2_part=quantile(conc,0.75,na.rm = TRUE),
                                                                        max_part=quantile(conc,0.95,na.rm = TRUE))



view(sum_traff)


########## summary of standing time ################

sum_traff<-data_breath%>%group_by(Traffic_lights)%>%summarise(n_speeds=length(speed[!is.na(speed)]),
                                                                  n_lowspeeds=length(speed[!is.na(speed)][speed[!is.na(speed)]<5]))


view(sum_traff)

########## summary of GPS speeds ################

sum_GPSspeeds<-data_breath%>%group_by(TypeSpeed,TypeLeg)%>%summarise(in_start=min(ABS_time,na.rm = TRUE),
                                                                               in_end=max(ABS_time,na.rm = TRUE),
                                                                               mean_GPS=mean(speed,na.rm = TRUE))


view(sum_GPSspeeds)

sum_times<-Sum_overall%>%group_by(TypeSpeed,TypeLeg)%>%summarise(time_min=max(cum_dist,na.rm = TRUE),
                                                                 time_max=max(cum_time,na.rm = TRUE))


sum_times





###### summary of data collection #######

sum_datacPC<-new_df%>%group_by(Day,TypeSpeed,Participan,TypeLeg)%>%summarise(in_start=min(ABS_time,na.rm = TRUE),
                                                                  in_end=max(ABS_time,na.rm = TRUE),
                                                                  n_cpc=length(conc),
                                                                  n_cpc_na=length(conc[is.na(conc)]))

sum_datacPC$coll_rate<-1-sum_datacPC$n_cpc_na/sum_datacPC$n_cpc

sum_dataSZ<-data_breath%>%group_by(Day,TypeSpeed,Participan,TypeLeg)%>%summarise(in_start=min(ABS_time,na.rm = TRUE),
                                                                             in_end=max(ABS_time,na.rm = TRUE),
                                                                             n_flow=length(flow),
                                                                             n_flow_na=length(flow[is.na(flow)]),
                                                                             n_speed=length(speed),
                                                                             n_speed_na=length(speed[is.na(speed)]))

sum_dataSZ$rateflow<-1-sum_dataSZ$n_flow_na/sum_dataSZ$n_flow
sum_dataSZ$ratespeed<-1-sum_dataSZ$n_speed_na/sum_dataSZ$n_speed



write.csv(sum_datacPC,"sum_collCPC.csv")
write.csv(sum_dataSZ,"sum_collSZ.csv")
write.csv(sum_sections,"sum_sections.csv")





######## plot for cumulative dose#########
#plot_cumdose<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(col=TypeSpeed))+
plot_cumdose<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(col=factor(Day)))+
  geom_line(aes(x=cum_dist,y=cum_dose*10^(-6)),size=0.7)+
  #geom_line(aes(x=cum_dist,y=cum_dose_CV*10^(-6),linetype="Fixed Par."),size=0.7)+
  #geom_line(aes(x=cum_dist,y=cum_dose_CC*10^(-6),linetype="Avg Conc."),size=0.7)+
  labs(col="Sampling Day",y=expression(paste("x10"^6, " inhaled particles")),x="Travelled distance [m]")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  #scale_colour_identity(name = "Day")+
  #scale_linetype_manual("Dose estimation", values = c("Dynamic"="solid","Fixed Par."="longdash","Avg Conc."="dotted"))+
  theme_light()+
  facet_grid(TypeSpeed~TypeLeg,scales="free_y")

plot_cumdose
ggsave(filename = "trial1.png",device = "png",dpi = 300,plot = plot_cumdose)
######## plot for cumulative dose#########
#plot_cumdose<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(col=TypeSpeed))+



plot_cumdoser<-ggplot(Sum_overall,aes(col=TypeSpeed))+
  geom_line(aes(x=cum_dist,y=sec_dose_CV_ratio,linetype="Fixed Par."),size=0.7)+
  geom_line(aes(x=cum_dist,y=sec_dose_CC_ratio,linetype="Avg Conc."),size=0.7)+
  labs(y=expression(paste("x10"^6, " inhaled particles")),x="Travelled distance [m]")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  scale_colour_manual(name = "Speed Level", values = c("#e80000","#0095ff"))+
  scale_linetype_manual("Dose estimation", values = c("Real"="solid","Fixed Par."="longdash","Avg Conc."="dotted"))+
  theme_light()

plot_cumdoser+facet_grid(Participan~TypeLeg,scales="free_y")


######## plot for cumulative time#########

plot_time<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(x=cum_dist,y=cum_time/60,col=TypeSpeed))+
  geom_line(size=0.7)+
  labs(y=expression(paste("Elapsed time [min]")),x="Travelled distance [m]")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  scale_y_continuous(limits=c(0, 25))+
  scale_colour_manual(name = "Speed Level", values = c("#e80000","#0095ff"))+
  theme_light()#+
  #theme(legend.position="bottom")
  

plot_time+facet_grid(.~TypeLeg)

######## plot for speed #########


plot_speed<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(x=cum_dist,y=avg_speed,col=TypeSpeed))+
  geom_line(size=0.7)+
  geom_smooth(alpha=0.3,se = FALSE,linetype="dashed")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  scale_y_continuous(limits=c(0, 40))+
  labs(y=expression(paste("Mean Speed [km/h]")),x="Travelled distance [m]")+
  scale_colour_manual(name = "Speed Level", values = c("#e80000","#0095ff"))+
  theme_light()
plot_speed+facet_grid(.~TypeLeg)

rm(plot_speed)


######## plot for concentrations #########

plot_conc<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(x=cum_dist,y=median_part/10^3,col=factor(Day)))+
  geom_line()+
  #geom_point(aes(y=mean_part/10^3,shape=1),size=1.5)+
  #geom_ribbon(aes(ymin=(q1_part)/10^3,ymax=(q2_part)/10^3,fill='black'),alpha=0.15)+
  #geom_smooth(alpha=0.3,se = FALSE,linetype="dashed")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  scale_y_continuous(limits=c(0, 300))+
  labs(col="Day",y=expression(paste("x10"^3," PNC/cm"^3)),x="Travelled distance [m]")+
  #scale_shape_identity(name = NULL, guide = 'legend',labels = c('Mean'))+
  #scale_colour_manual(name = ' ',values =c("black"="black"),labels = c('Median'))+
  #scale_fill_identity(name = NULL, guide = 'legend',labels = c(expression(paste("IQR"))))+
  theme_light()+
  #theme(legend.position = "bottom")+
  facet_grid(TypeLeg~.)
plot_conc

ggsave(filename = "trial2.png",device = "png",dpi = 300,plot = plot_conc)

plot_conc2<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(x=cum_dist,y=n_part,col=factor(Day)))+
  geom_line()+
  #geom_point(aes(y=mean_part/10^3,shape=1),size=1.5)+
  #geom_ribbon(aes(ymin=(q1_part)/10^3,ymax=(q2_part)/10^3,fill='black'),alpha=0.15)+
  #geom_smooth(alpha=0.3,se = FALSE,linetype="dashed")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  #scale_y_continuous(limits=c(0, 300))+
  labs(col="Sampling Day",y=expression(paste("x10"^3," PNC/cm"^3)),x="Travelled distance [m]")+
  #scale_shape_identity(name = NULL, guide = 'legend',labels = c('Mean'))+
  #scale_colour_manual(name = ' ',values =c("black"="black"),labels = c('Median'))+
  #scale_fill_identity(name = NULL, guide = 'legend',labels = c(expression(paste("IQR"))))+
  theme_light()+
  #theme(legend.position = "bottom")+
  facet_grid(TypeLeg~.)
plot_conc2


######## plot for elevation #########

plot_elev<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(x=cum_dist,y=cum_elev))+geom_line()+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  scale_y_continuous(limits=c(0, 60))+
  labs(y=expression(paste("Relative elevation [m]")),x="Travelled distance [m]")+
  theme_classic()
plot_elev+facet_grid(.~TypeLeg)

######## plot for ventilation #########
#plot_vent<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(x=cum_dist,y=mean_vent,col=TypeSpeed))+
plot_vent<-ggplot(Sum_overall,aes(x=cum_dist,y=median_vent,col=TypeSpeed))+
  geom_line(size=0.7)+
  #geom_errorbar(aes(ymin=mean_vent-sd_vent,ymax=mean_vent+sd_vent),alpha=0.3)+
  #geom_pointrange(aes(ymin=mean_vent-sd_vent,ymax=mean_vent+sd_vent),alpha=0.3,size=0.2)+
  #geom_ribbon(aes(ymin=mean_vent-sd_vent,ymax=mean_vent+sd_vent),alpha=0.1,linetype=2)+
  geom_smooth(alpha=0.3,method="auto",se = FALSE,linetype="dashed")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  labs(y=expression(paste("Median ventilation [lpm]")),x="Travelled distance [m]")+
  scale_colour_manual(name = "Speed Level", values = c("#e80000","#0095ff"))+
  theme_light()
plot_vent+facet_grid(Participan~TypeLeg,scales="free_y")

######## plot for BR #########
#plot_vent<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(x=cum_dist,y=mean_vent,col=TypeSpeed))+
plot_BR<-ggplot(Sum_overall,aes(x=cum_dist,y=median_BR,col=TypeSpeed))+
  geom_line(size=0.7)+
  #geom_errorbar(aes(ymin=mean_BR-sd_BR,ymax=mean_BR+sd_BR),alpha=0.3)+
  geom_smooth(alpha=0.3,se = FALSE,linetype="dashed")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  scale_y_continuous(limits=c(0, 50))+
  labs(y=expression(paste("Median breathing rate [bpm]")),x="Travelled distance [m]")+
  scale_colour_manual(name = "Speed Level", values = c("#e80000","#0095ff"))+
  theme_light()
plot_BR+facet_grid(Participan~TypeLeg)


###### plot gradient#######




#### explorartory ####

pairs(Sum_overall[Sum_overall$Order!="79.1"&Sum_overall$Order!="79.2"&Sum_overall$Participan=="Y",c(12,18,26,36,37,38)],lower.panel=panel.smooth,upper.panel=NULL)


view(Sum_overall[,c(1,15,33,38)])
### Box plot of concentrations along the route by section########

test<-ggplot(new_df%>%filter(conc<1000000),aes(fill=TypeLeg))

test+geom_boxplot(aes(x=Order,y=conc))#,outlier.shape = NA)#+facet_grid(.~Order)#+scale_y_continuous(name = "Strava Speed km/h")+scale_x_continuous(name = NULL)
test+geom_density(aes(conc))+facet_grid(TypeLeg~.)

test<-ggplot(Sum_overall,aes(fill=TypeSpeed))

test+geom_boxplot(aes(x=Order,y=mean_part))+facet_grid(Participan~TypeLeg)#,outlier.shape = NA)#+facet_grid(.~Order)#+scale_y_continuous(name = "Strava Speed km/h")+scale_x_continuous(name = NULL)
test+geom_density(aes(conc))+facet_grid(TypeLeg~.)



### Box plot of concentrations along the route by section########

test2<-ggplot(data_breath%>%filter(Order!="79.1"&Order!="79.2"&!is.na(Order)),aes(fill=TypeLeg))

test2+geom_boxplot(aes(x=Order,y=speed),outlier.shape = NA)#+facet_grid(.~Order)#+scale_y_continuous(name = "Strava Speed km/h")+scale_x_continuous(name = NULL)



#db_speed <- read_csv("~/2. Dissertation/3. Data Processing/results/Databases/db_speed.csv",
#                     col_types = cols(Order = col_character(),
#                                      TypeLeg = col_factor(levels = c("Inbound","Outbound")), 
#                                      TypeSpeed = col_factor(levels = c("Low","High"),ordered=TRUE)))
#db_speed$avg_speed<-(db_speed$length/db_speed$time)*3.6
#
#
#
#db_dose <- read_csv("~/2. Dissertation/3. Data Processing/results/Databases/db_dose.csv",
#                    col_types = cols(Order = col_character(),
#                                           Participan = col_factor(levels = c("B", "Y")),
#                                           TypeLeg = col_factor(levels = c("Inbound","Outbound")),
#                                           TypeSpeed = col_factor(levels = c("Low","High"),ordered=TRUE)))
####Calculation of dose!!!!!!!!######
#db_dose$sector_dose<-db_dose$mean_part*(db_dose$mean_vent*(1000/60))*db_dose$sector_time

##### random plots #######
pdose<-ggplot(db_dose%>%filter(Participan=="Y"),aes(Order,sector_dose/10^6))
pdose+geom_bar(stat="identity",fill="darkblue",alpha=0.5)+facet_grid(TypeLeg~TypeSpeed)+theme_minimal()

sum_dose<-db_dose%>%group_by(Participan,TypeSpeed,TypeLeg)%>%summarise(total_dose=sum(sector_dose)/10^6)
sum_dose


write.csv(sum_dose, file = "sum_dose.csv")
write.csv(sum_breath,file = "breath.csv")
write.csv(sum_concs,file = "concs.csv")

write.csv(Sum_overall, file = "sum_overall.csv")

pspeed<-ggplot(new_df,aes(speed))+geom_histogram(alpha=0.5)+facet_grid(TypeLeg~TypeSpeed)+theme_minimal()
pspeed2<-ggplot(db_speed,aes(avg_speed))+geom_histogram(alpha=0.5)+facet_grid(TypeLeg~TypeSpeed)+theme_minimal()


pconcdaily2<-ggplot(new_df%>%filter(conc<10000000),aes(conc/1000,fill=TypeLeg))+
  geom_histogram(data=new_df%>%filter(TypeLeg=="Inbound"),binwidth = 5,col="white", alpha = 0.3)+
  geom_histogram(data=new_df%>%filter(TypeLeg=="Outbound"),binwidth = 5,col="white", alpha = 0.3)+
  labs(x=expression(paste("x1000 particles/cm"^3)),y="Count")+
  scale_x_continuous(limits=c(0, 100),breaks=seq(0, 100, by=20),labels=c(seq(0,80, by=20), "100+"))+
  #geom_vline(aes(xintercept = mean(conc,rm.na=TRUE)/1000,color="Mean"),linetype="dotted",size=1,show.legend=NA)+
  #geom_vline(aes(xintercept = median(conc,rm.na=TRUE)/1000,color="Median"),linetype="dotted",size=1,show.legend=NA)+
  scale_fill_manual(name = "Trip Type", values = c("#00AFBB","#f5e31b"))+
  scale_colour_manual(name = "Overall central tendency", values = c("red","blue"))+
  facet_wrap(Day~.,labeller = label_both)

pconcdaily2


  
speed_low<-data_breath%>%filter(TypeSpeed=="Low",Participan=="B",!is.na(speed))
speed_low<-speed_low$speed

speed_high<-data_breath%>%filter(TypeSpeed=="High",Participan=="B",!is.na(speed))
speed_high<-speed_high$speed

t.test(speed_low,speed_high)


#### t-test for speeds #####

speed_low<-data_breath%>%filter(TypeSpeed=="Low",TypeLeg=="Inbound",!is.na(speed))
speed_low<-speed_low$speed

speed_high<-data_breath%>%filter(TypeSpeed=="High",TypeLeg=="Inbound",!is.na(speed))
speed_high<-speed_high$speed

t.test(speed_low,speed_high)

speed_low<-data_breath%>%filter(TypeSpeed=="Low",TypeLeg=="Outbound",!is.na(speed))
speed_low<-speed_low$speed

speed_high<-data_breath%>%filter(TypeSpeed=="High",TypeLeg=="Outbound",!is.na(speed))
speed_high<-speed_high$speed

t.test(speed_low,speed_high)

######### t-test for traffic light speeds ######

speed_low<-data_breath%>%filter(TypeSpeed=="Low",TypeLeg=="Inbound",Traffic_lights,!is.na(speed))
speed_low<-speed_low$speed

speed_high<-data_breath%>%filter(TypeSpeed=="Low",TypeLeg=="Inbound",!Traffic_lights,!is.na(speed))
speed_high<-speed_high$speed

t.test(speed_low,speed_high)



speed_low<-data_breath%>%filter(TypeSpeed=="High",TypeLeg=="Inbound",Traffic_lights,!is.na(speed))
speed_low<-speed_low$speed

speed_high<-data_breath%>%filter(TypeSpeed=="High",TypeLeg=="Inbound",!Traffic_lights,!is.na(speed))
speed_high<-speed_high$speed

t.test(speed_low,speed_high)


#outbound
speed_low<-data_breath%>%filter(TypeSpeed=="Low",TypeLeg=="Outbound",Traffic_lights,!is.na(speed))
speed_low<-speed_low$speed

speed_high<-data_breath%>%filter(TypeSpeed=="Low",TypeLeg=="Outbound",!Traffic_lights,!is.na(speed))
speed_high<-speed_high$speed

t.test(speed_low,speed_high)



speed_low<-data_breath%>%filter(TypeSpeed=="High",TypeLeg=="Outbound",Traffic_lights,!is.na(speed))
speed_low<-speed_low$speed

speed_high<-data_breath%>%filter(TypeSpeed=="High",TypeLeg=="Outbound",!Traffic_lights,!is.na(speed))
speed_high<-speed_high$speed

t.test(speed_low,speed_high)


######### t-test for bus stops speeds ######

speed_low<-data_breath%>%filter(TypeSpeed=="Low",TypeLeg=="Inbound",Bus_stops,!is.na(speed))
speed_low<-speed_low$speed

speed_high<-data_breath%>%filter(TypeSpeed=="Low",TypeLeg=="Inbound",!Bus_stops,!is.na(speed))
speed_high<-speed_high$speed

t.test(speed_low,speed_high)



speed_low<-data_breath%>%filter(TypeSpeed=="High",TypeLeg=="Inbound",Bus_stops,!is.na(speed))
speed_low<-speed_low$speed

speed_high<-data_breath%>%filter(TypeSpeed=="High",TypeLeg=="Inbound",!Bus_stops,!is.na(speed))
speed_high<-speed_high$speed

t.test(speed_low,speed_high)


#outbound
speed_low<-data_breath%>%filter(TypeSpeed=="Low",TypeLeg=="Outbound",Bus_stops,!is.na(speed))
speed_low<-speed_low$speed

speed_high<-data_breath%>%filter(TypeSpeed=="Low",TypeLeg=="Outbound",!Bus_stops,!is.na(speed))
speed_high<-speed_high$speed

t.test(speed_low,speed_high)



speed_low<-data_breath%>%filter(TypeSpeed=="High",TypeLeg=="Outbound",Bus_stops,!is.na(speed))
speed_low<-speed_low$speed

speed_high<-data_breath%>%filter(TypeSpeed=="High",TypeLeg=="Outbound",!Bus_stops,!is.na(speed))
speed_high<-speed_high$speed

t.test(speed_low,speed_high)

######### t-test for gradients speeds ######

speed_low<-Sum_overall%>%filter(avg_grad<0,!is.na(avg_speed))
speed_low<-speed_low$avg_speed

speed_high<-Sum_overall%>%filter(avg_grad>0,!is.na(avg_speed))
speed_high<-speed_high$avg_speed

t.test(speed_low,speed_high)


##### t-test for ventilation rate ######
flow_low<-data_breath%>%filter(Participan=="Y",TypeLeg=="Outbound",!is.na(flow))
flow_low<-flow_low$flow
median(flow_low)

flow_high<-data_breath%>%filter(Participan=="Y",TypeLeg=="Inbound",!is.na(flow))
flow_high<-flow_high$flow
median(flow_high)

t.test(flow_low,flow_high)


##### t-test for breathing rate ######
BR_low<-data_breath%>%filter(Participan=="B",!is.na(breathRate))
BR_low<-BR_low$breathRate

BR_high<-data_breath%>%filter(Participan=="Y",!is.na(breathRate))
BR_high<-BR_high$breathRate

t.test(BR_low,BR_high)


####### t-test concentrations by leg #####

prueba<-rbind(c(1,2,3,4,5,6,7),c(1,2,3,4,5,6,7))
prueba

conc_in<-new_df%>%filter(TypeLeg=="Inbound",!is.na(conc))
conc_in<-conc_in$conc

conc_out<-new_df%>%filter(TypeLeg=="Outbound",!is.na(conc))
conc_out<-conc_out$conc

t.test(conc_in,conc_out)

for (i in (1:7)) {

conc_in<-new_df%>%filter(TypeLeg=="Inbound",Day==i,!is.na(conc))
conc_in<-conc_in$conc
prueba[1,i]<-median(conc_in)
conc_out<-new_df%>%filter(TypeLeg=="Outbound",Day==i,!is.na(conc))
conc_out<-conc_out$conc
prueba[2,i]<-median(conc_out)


}
#####t test canyons####

conc_in<-new_df%>%filter(canyon,!is.na(conc))
conc_in<-conc_in$conc
median(conc_in)

conc_out<-new_df%>%filter(!canyon,!is.na(conc))
conc_out<-conc_out$conc
median(conc_out)
t.test(conc_in,conc_out)

####### t test traffic lights #########


conc_in<-new_df%>%filter(TypeLeg=="Inbound",Traffic_lights,!is.na(conc))
conc_in<-conc_in$conc
median(conc_in)

conc_out<-new_df%>%filter(TypeLeg=="Inbound",!Traffic_lights,!is.na(conc))
conc_out<-conc_out$conc
median(conc_out)
t.test(conc_in,conc_out)

#####outbound

conc_in<-new_df%>%filter(TypeLeg=="Outbound",Traffic_lights,!is.na(conc))
conc_in<-conc_in$conc
median(conc_in)

conc_out<-new_df%>%filter(TypeLeg=="Outbound",!Traffic_lights,!is.na(conc))
conc_out<-conc_out$conc
median(conc_out)
t.test(conc_in,conc_out)


### y ####

BR_low<-data_breath%>%filter(TypeSpeed=="Low",Participan=="Y",!is.na(breathRate))
BR_low<-BR_low$breathRate

BR_high<-data_breath%>%filter(TypeSpeed=="High",Participan=="Y",!is.na(breathRate))
BR_high<-BR_high$breathRate

t.test(BR_low,BR_high)

############ boxplot ##########

### Box plot of concentrations along the route by section########

plot_tlights<-ggplot(new_df%>%filter(Order!="79.1"&Order!="79.2"&!is.na(Order)),
                     aes(x=Traffic_lights,y=conc,fill=Traffic_lights))

####this one ######

png(filename = "trafficlights.png",res = 300,width = 2000,height = 1400)  

plot_tlights<-ggplot(new_df%>%filter(Order!="79.1",Order!="79.2",!is.na(Order),(Traffic_lights)),
                     aes(x=Order,y=conc,fill=Traffic_lights))


plot_tlights+geom_boxplot(outlier.shape = NA,alpha=0.6)+
  stat_summary(fun.y=mean,aes(shape="mean"), geom="point", size=2)+
  scale_fill_manual(name = "Section type", values = c("#16cc3e","#fa3e05"),labels=c("No traffic lights","With traffic lights"))+
  scale_shape_manual("", values=c("mean"=2))+
  scale_y_continuous(limits=c(0, 150000),breaks=seq(0, 150000, by=25000),labels=c(seq(0,150, by=25)))+
  labs(y=expression(paste("x1000 particles/cm"^3)),x="Section")+
  theme_light()+
  theme(legend.position = "bottom")+
  facet_grid(TypeLeg~.)

dev.off()



plot_tlights+
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun.y=mean, geom="point", shape=2, size=2)+
  scale_fill_manual(name = "Section type", values = c("#00AFBB","#f5e31b"),labels=c("No traffic lights","With traffic lights"))+
  scale_y_continuous(limits=c(0, 150000),breaks=seq(0, 150000, by=25000),labels=c(seq(0,150, by=25)))+
  labs(y=expression(paste("x1000 particles/cm"^3)),x="")+
  theme_light()+
  facet_grid(TypeLeg~.)



###### box plots comparing concentrations ########

boxtraffic<-ggplot(new_df,aes(x=Traffic_lights,y=conc))+geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits=c(0, 100000),breaks=seq(0, 100000, by=25000),labels=c(seq(0,100, by=25)))+
  labs(y=expression(paste("x10"^3," PNC/cm"^3)),x="Travelled distance [m]")+facet_grid(.~TypeLeg)
#+facet_grid(.~Order)#+scale_y_continuous(name = "Strava Speed km/h")+scale_x_continuous(name = NULL))
boxtraffic


boxcanyon<-ggplot(new_df,aes(x=canyon,y=conc))+geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits=c(0, 100000),breaks=seq(0, 100000, by=25000),labels=c(seq(0,100, by=25)))+
  labs(y=expression(paste("x10"^3," PNC/cm"^3)),x="Travelled distance [m]")+facet_grid(.~TypeLeg)
#+facet_grid(.~Order)#+scale_y_continuous(name = "Strava Speed km/h")+scale_x_continuous(name = NULL))
boxcanyon



##### speed plots avg and estimated ####

plot_spcom<-ggplot(Sum_overall,aes(x=mean_speed,y=avg_speed))+
  geom_point(aes(col=Traffic_lights,shape=Bus_stops),size=2,alpha=0.6)+
  geom_smooth(method = "lm")+
  scale_y_continuous(limits=c(0, 45))+
  scale_x_continuous(limits=c(0, 45))+
  labs(y=expression(paste("Time based avg. speed [km/h]")),x="GPS based avg. speed [km/h]")+
  #scale_shape_identity(name = NULL, guide = 'legend',labels = c('Without','With'))+
  theme_light()
plot_spcom

summary(lm(Sum_overall$mean_speed~Sum_overall$avg_speed))


##### speed vs ventilation ####

plot_ventspe<-ggplot(data_breath%>%filter(Participan=="Y"),aes(x=longitude,y=latitute,size=flow,col=speed))+
  geom_point(alpha=0.3)+
  facet_grid(TypeLeg+TypeSpeed~Day)





###### the greatest plot ever #######

png(filename = "RATIOSRoute.png",res = 300,width = 4600,height = 4500*6/19)

comp_dose_plot<-ggplot(ratios_overall%>%filter(Participan=="B"),aes())+
  annotate("text", label = "Weetwood", x = 14.5, y = -0.6, size = 2.5, colour = "black")+
  annotate("rect", xmin = 11, xmax = 18, ymin = -1, ymax = 3,alpha = .15)+
  annotate("text", label = "Bodington", x = 4.5, y = -0.6, size = 2.5, colour = "black")+
  annotate("rect", xmin = 1, xmax = 8, ymin = -1, ymax = 3,alpha = .15)+
  annotate("text", label = "Headingley\nCentral", x = 35.5, y = -0.6, size = 2.5, colour = "black")+
  annotate("rect", xmin = 33, xmax = 38, ymin = -1, ymax = 3,alpha = .15)+
  annotate("text", label = "Hyde\nPark", x = 50, y = -0.6, size = 2.5, colour = "black")+
  annotate("rect", xmin = 49, xmax = 51, ymin = -1, ymax = 3,alpha = .15)+
  annotate("text", label = "Woodhouse\nMoor", x = 55, y = 2, size = 2.5, colour = "black")+
  annotate("rect", xmin = 52, xmax = 58, ymin = -1, ymax = 3,alpha = .15)+
  annotate("text", label = "Campus", x = 61, y = -0.6, size = 2.5, colour = "black")+
  annotate("rect", xmin = 59, xmax = 63, ymin = -1, ymax = 3,alpha = .15)+
  geom_bar(aes(x=Order,y=ratio_dose,fill="Dose"),stat = "identity",alpha=0.5)+
  geom_bar(aes(x=Order,y=ratio_vent,fill="Ventilation"),stat = "identity",alpha=0.5)+
  scale_fill_manual(name = NULL,values=c("Dose"="#ff7083","Ventilation"="#25ccfa"),labels = c('Dose','Ventilation'))+
  labs(y=expression(paste("Ratio (High speed to Low Speed)")),x="")+
  geom_hline(yintercept=0, linetype="dashed",col="darkgray")+
  scale_y_continuous(limits=c(-1, 3),breaks=seq(-1, 3, by=1),labels=c(seq(0,4, by=1)))+
  scale_x_discrete(breaks=c(0,6000),labels=c(0,6000))+
  theme_test()+
  theme(legend.position = "top")
  
comp_dose_plot+facet_grid(Participan~TypeLeg)

dev.off() 




##comp_dose_plot<-ggplot(ratios_single,aes(x=Order,y=ratio,fill=TypeRatio))+
##  geom_bar(alpha=0.5,stat = "identity",col="white")+
##  theme_minimal()
##comp_dose_plot+facet_grid(Participan~TypeLeg)

comp_vent_plot<-ggplot(ratios_overall,aes())+
  geom_bar(aes(x=Order,y=ratio_vent,fill=avg_grad),stat = "identity",alpha=0.75)

comp_vent_plot+facet_grid(Participan~TypeLeg)


####### scatter for ventilation ####

png(filename = "Vent_grad2.png",res = 300,width = 1200,height = 2400)

scat_vent<-ggplot(ratios_overall,aes(x=avg_grad,y=1+ratio_vent,col=speed_diff))+
  geom_point()+
  geom_vline(xintercept=0, linetype="dotted")+
  geom_hline(yintercept=1, linetype="dotted")+
  scale_y_continuous(limits=c(0, 4))+
  scale_x_continuous(limits=c(-5,5))+
  labs(y=expression(paste("Ventilation Ratio (High speed to Low Speed)")),x="Average gradient [%]")+
  annotate("text", label = "I", x = 4, y = 3.5, size = 5, colour = "black")+
  annotate("text", label = "II", x = -4, y = 3.5, size = 5, colour = "black")+
  annotate("text", label = "III", x = -4, y = 0.2, size = 5, colour = "black")+
  annotate("text", label = "IV", x = 4, y = 0.2, size = 5, colour = "black")+
  scale_color_gradientn(colours=c("red","orange","green"),limits=c(-18,18),guide = guide_colourbar(title = "Speed difference [ḳm/h]",direction="horizontal"))+
  theme_test()+
  theme(legend.position="bottom")
scat_vent+facet_grid(Participan~.)
  


dev.off() 

####### scatter for ventilation vs speed ####

png(filename = "Vent_speed3.png",res = 300,width = 1200,height = 2400)

scat_vent<-ggplot(ratios_overall,aes(x=speed_diff,y=1+ratio_vent,col=avg_grad))+
  geom_point()+
  geom_vline(xintercept=0, linetype="dotted")+
  geom_hline(yintercept=1, linetype="dotted")+
  scale_y_continuous(limits=c(0, 4))+
  scale_x_continuous(limits=c(-15,15))+
  labs(y=expression(paste("Ventilation Ratio (High speed to Low Speed)")),x="Speed difference [ḳm/h]")+
  annotate("text", label = "I", x = 13, y = 3.5, size = 5, colour = "black")+
  annotate("text", label = "II", x = -13, y = 3.5, size = 5, colour = "black")+
  annotate("text", label = "III", x = -13, y = 0.2, size = 5, colour = "black")+
  annotate("text", label = "IV", x = 13, y = 0.2, size = 5, colour = "black")+
  scale_color_gradientn(colours=c("green","orange","red"),limits=c(-5,5),guide = guide_colourbar(title = "Average gradient [%]",direction="horizontal"))+
  theme_test()+
  theme(legend.position="bottom")
scat_vent+facet_grid(Participan~.)



dev.off() 


####### scatter tidal vs br#########

png(filename = "tidal_volumeB.png",res = 300,width = 1500,height = 1500)

scat_tidal<-ggplot(Sum_overall%>%filter(Participan=="B"),aes(x=median_TV,y=median_BR,col=median_vent))+
  geom_point()+
  labs(y=expression(paste("Measured breath rate [#breaths/min]")),x="Estimated tidal volume [liters]")+
  geom_vline(xintercept=4.26, linetype="dashed", col="red",size=1)+
  scale_color_gradientn(colours=c("orange","darkred"),limits=c(0,350),guide = guide_colourbar(title = "Ventilation [liters/min]",direction="horizontal"))+
  theme_test()+
  theme(legend.position="bottom")

scat_tidal

dev.off()

png(filename = "tidal_volumeY.png",res = 300,width = 1500,height = 1500)

scat_tidal<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(x=median_TV,y=median_BR,col=median_vent))+
  geom_jitter()+
  labs(y=expression(paste("Measured breath rate [#breaths/min]")),x="Estimated tidal volume [liters]")+
  geom_vline(xintercept=4.43, linetype="dashed", col="red",size=1)+
  scale_color_gradientn(colours=c("orange","darkred"),limits=c(0,350),guide = guide_colourbar(title = "Ventilation [liters/min]",direction="horizontal"))+
  theme_test()+
  theme(legend.position="bottom")

scat_tidal

dev.off()







###### comparison ratios low-high ########

png(filename = "RATIOS.png",res = 300,width = 1800*2/3,height = 1800)

bars_ratios<-ggplot(ratio_final,aes(x=TypeLeg,y=Ratio,fill=Type2))+
  geom_bar(position="dodge",alpha=0.6,stat="identity",col="gray",width = 0.4)+
  scale_y_continuous(limits=c(-1, 1),breaks=seq(-1, 1, by=0.5),labels=c(seq(0,2, by=0.5)))+
  labs(y=expression(paste("Ratio (High speed to Low Speed)")),x="")+
  geom_hline(yintercept=0, linetype="dashed",col="darkgray")+
  scale_fill_manual(name = NULL,values=c("#ff7083","#25ccfa"),labels = c('Dose','Volume'))+
  theme_test()+
  theme(legend.position="bottom")
  
bars_ratios+facet_grid(Type~Participan)

dev.off() 

###### comparison ratios methods ########

png(filename = "RATIOSmeth.png",res = 300,width = 1500*3/2,height = 1500) 

bars_ratios<-ggplot(ratio_final2,aes(x=Type,y=Ratio,fill=Type2))+
  geom_bar(position="dodge",alpha=0.6,stat="identity",col="gray",width = 0.4)+
  scale_y_continuous(limits=c(-1, 1),breaks=seq(-1, 1, by=0.5),labels=c(seq(0,2, by=0.5)))+
  labs(y=expression(paste("Ratio (Other methods to dynamic ventilation)")),x="")+
  geom_hline(yintercept=0, linetype="dashed",col="darkgray")+
  scale_fill_manual(name = NULL,values=c("#ff7083","#25ccfa"),labels = c('Dose','Volume'))+
  theme_test()+
  theme(legend.position="bottom")

bars_ratios+facet_grid(Type1~TypeLeg+Participan)

dev.off() 


write.csv(Sum_overall,"overall.csv")

write.csv(ratios_overall,"ratios_overall.csv")
