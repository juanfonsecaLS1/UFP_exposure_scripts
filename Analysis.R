.libPaths(c("\\\\ds.leeds.ac.uk/lib/Apps/R/R-3.6.0/library",.libPaths()))

# This script merges the CSV files that has already classified in QGIS,
## a field 'Order' has been added to specify the section 
# 
# 
# The code require the libraries: tidyverse and data.table


library(readr)
library(tidyverse)
library(lubridate)
library(data.table)

### importing file with after being processed in QGIS with the sector classification    #####

new_df <- read_csv("QGIS_processed_concentrations_points.csv", 
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

data_breath <- read_csv("new_df_sz_complete2.csv", 
                        col_types = cols(ABS_time = col_datetime(format = "X%Y.%m.%d.%H.%M.%S"),
                                         DateShort = col_date(format = "%d/%m/%Y"),
                                         Participan = col_factor(levels = c("A","B", "X", "Y")),
                                         desc = col_factor(levels = c("F","D")), 
                                         TypeRoute = col_factor(levels = c("Polluted", "Track")), 
                                         TypeSpeed = col_factor(levels = c("Low","High")), 
                                         TypeLeg = col_factor(levels = c("Inbound","Outbound","Track"))))
data_breath$Order<-as.factor(data_breath$Order)


##### Calculating concentration aggreggated values per sector ######


sum_conc<-new_df%>%filter(!is.na(Order))%>%group_by(Order,TypeLeg)%>%summarise(n_part=length(conc[!is.na(conc)]),
                                                                               mean_part=mean(conc,na.rm = TRUE),
                                                                               median_part=median(conc,na.rm = TRUE),
                                                                               sd_part=sd(conc,na.rm = TRUE),
                                                                               min_part=min(conc,na.rm = TRUE),
                                                                               max_part=max(conc,na.rm = TRUE),
                                                                               q1_part=quantile(conc,0.25,na.rm = TRUE),
                                                                               q2_part=quantile(conc,0.75,na.rm = TRUE))




##### Calculating breathing aggreggated values per sector ######

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

Sum_overall<-data.table(sum_conc, key="Order,TypeLeg")[
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
Sum_overall$sec_dose<-as.numeric(Sum_overall$mean_part*(Sum_overall$mean_vent*(1000/60))*Sum_overall$sector_time)
Sum_overall$sec_dose_sd<-((Sum_overall$mean_part^2+Sum_overall$sd_part^2)*
                            ((Sum_overall$mean_vent*(1000/60)*as.numeric(Sum_overall$sector_time))^2+
                               (Sum_overall$sd_vent*(1000/60)*as.numeric(Sum_overall$sector_time))^2)-
                                       (Sum_overall$mean_vent*(1000/60)*as.numeric(Sum_overall$sector_time))^2*
                                       Sum_overall$mean_part^2)


Sum_overall$avg_grad<-Sum_overall$Sec_elev_delta/Sum_overall$length*100



Sum_overall<-Sum_overall[order(Sum_overall$Participan,Sum_overall$TypeSpeed,Sum_overall$TypeLeg,Sum_overall$Order)]

sum_Out<-Sum_overall[Sum_overall$TypeLeg=="Outbound",]
sum_Out<-sum_Out[order(sum_Out$Participan,sum_Out$TypeSpeed,-sum_Out$Order)]

sum_In<-Sum_overall[Sum_overall$TypeLeg=="Inbound",]
sum_In<-sum_In[order(sum_In$Participan,sum_In$TypeSpeed,sum_In$Order)]

Sum_overall<-rbind(sum_Out,sum_In)
Sum_overall<-Sum_overall%>%filter(Order!="79.1"&Order!="79.2")

Sum_overall<-Sum_overall%>%group_by(Participan,TypeLeg,TypeSpeed)%>%mutate(cum_dose=cumsum(sec_dose),
                                                                           cum_dist=cumsum(length),
                                                                           cum_time=cumsum(as.numeric(sector_time)),
                                                                           cum_elev=cumsum(Sec_elev_delta))

Sum_overall<-Sum_overall%>%group_by(Participan,TypeLeg,TypeSpeed)%>%mutate(rel_freq=length/max(cum_dist),
                                                                           sec_dose_CC=sum(mean_part))

Sum_overall$sec_dose_CC<-as.numeric((Sum_overall$rel_freq*Sum_overall$sec_dose_CC)*(Sum_overall$mean_vent*(1000/60))*Sum_overall$sector_time)

Sum_overall$cum_elev[Sum_overall$TypeLeg=="Inbound"]<-Sum_overall$cum_elev[Sum_overall$TypeLeg=="Inbound"]+max(Sum_overall$cum_elev)

Sum_overall$cum_elev<-Sum_overall$cum_elev-min(Sum_overall$cum_elev)

Sum_overall<-Sum_overall%>%group_by(Participan,TypeLeg,TypeSpeed)%>%mutate(cum_dose_CC=cumsum(sec_dose_CC))

Sum_overall$sec_dose_CV<-as.numeric(Sum_overall$mean_part*(55*(1000/60))*Sum_overall$sector_time)

Sum_overall$sec_dose_CV[Sum_overall$TypeSpeed=="Low"]<-Sum_overall$sec_dose_CV[Sum_overall$TypeSpeed=="Low"]/(55/40)

Sum_overall<-Sum_overall%>%group_by(Participan,TypeLeg,TypeSpeed)%>%mutate(cum_dose_CV=cumsum(sec_dose_CV))

Sum_overall$sec_dose_CC_ratio<-Sum_overall$sec_dose_CC/Sum_overall$sec_dose

Sum_overall$sec_dose_CV_ratio<-Sum_overall$sec_dose_CV/Sum_overall$sec_dose




######## merging type of section to main dataframe #######

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
######### summary of doses########

sum_doses<-Sum_overall%>%group_by(Participan,TypeLeg,TypeSpeed)%>%summarise(mean_dose=sum(sec_dose,na.rm = TRUE),
                                                                            sd_dose=sum(sec_dose_sd,na.rm = TRUE),
                                                                            CV_dose=sum(sec_dose_CV,na.rm = TRUE),
                                                                            CC_dose=sum(sec_dose_CC,na.rm = TRUE))


view(sum_doses)



########## summary of ventilation rates#########

sum_ventrates<-data_breath%>%group_by(Participan,TypeLeg,TypeSpeed)%>%summarise(n_vent=length(flow[!is.na(flow)]),
                                                                     mean_vent=mean(flow,na.rm = TRUE),
                                                                     sd_vent=sd(flow,na.rm = TRUE),
                                                                     min_vent=min(flow,na.rm = TRUE),
                                                                     q1_vent=quantile(flow,0.25,na.rm = TRUE),
                                                                     median_vent=median(flow,na.rm = TRUE),
                                                                     q2_vent=quantile(flow,0.75,na.rm = TRUE),
                                                                     max_vent=max(flow,na.rm = TRUE),
                                                                     mean_HR=mean(HR,na.rm = TRUE),
                                                                     sd_HR=sd(HR,na.rm = TRUE),
                                                                     min_HR=min(HR,na.rm = TRUE),
                                                                     q1_HR=quantile(HR,0.25,na.rm = TRUE),
                                                                     median_HR=median(HR,na.rm = TRUE),
                                                                     q2_HR=quantile(HR,0.75,na.rm = TRUE),
                                                                     max_HR=max(HR,na.rm = TRUE),
                                                                     mean_BR=mean(breathRate,na.rm = TRUE),
                                                                     sd_BR=sd(breathRate,na.rm = TRUE),
                                                                     min_BR=min(breathRate,na.rm = TRUE),
                                                                     q1_BR=quantile(breathRate,0.25,na.rm = TRUE),
                                                                     median_BR=median(breathRate,na.rm = TRUE),
                                                                     q2_BR=quantile(breathRate,0.75,na.rm = TRUE),
                                                                     max_BR=max(breathRate,na.rm = TRUE))


sum_ventrates

########## summary of concentrations ################

sum_concs<-new_df%>%group_by(TypeLeg)%>%summarise(n_part=length(conc[!is.na(conc)]),
                                                                mean_conc=mean(conc,na.rm = TRUE),
                                                                sd_part=sd(conc,na.rm = TRUE),
                                                                min_part=min(conc,na.rm = TRUE),
                                                                q1_part=quantile(conc,0.25,na.rm = TRUE),
                                                                median_conc=median(conc,na.rm = TRUE),
                                                                q2_part=quantile(conc,0.75,na.rm = TRUE),
                                                                max_part=max(conc,na.rm = TRUE))
                                                              


sum_concs


########## summary of GPS speeds ################

sum_GPSspeeds<-data_breath%>%group_by(TypeSpeed,TypeLeg)%>%summarise(in_start=min(ABS_time,na.rm = TRUE),
                                                                               in_end=max(ABS_time,na.rm = TRUE),
                                                                               mean_GPS=mean(speed,na.rm = TRUE))


sum_GPSspeeds

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
plot_cumdose<-ggplot(Sum_overall,aes(col=TypeSpeed))+
  geom_line(aes(x=cum_dist,y=cum_dose*10^(-6),linetype="Real"),size=0.7)+
  geom_line(aes(x=cum_dist,y=cum_dose_CV*10^(-6),linetype="Constant Vent."),size=0.7)+
  geom_line(aes(x=cum_dist,y=cum_dose_CC*10^(-6),linetype="Constant Conc."),size=0.7)+
  labs(y=expression(paste("x10"^6, " inhaled particles")),x="Travelled distance [m]")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  scale_colour_manual(name = "Speed Level", values = c("#e80000","#0095ff"))+
  scale_linetype_manual("Dose estimation", values = c("Real"="solid","Constant Vent."="longdash","Constant Conc."="dotted"))+
  theme_light()

plot_cumdose+facet_grid(Participan~TypeLeg,scales="free_y")

######## plot for cumulative dose#########
#plot_cumdose<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(col=TypeSpeed))+
plot_cumdoser<-ggplot(Sum_overall,aes(col=TypeSpeed))+
  geom_line(aes(x=cum_dist,y=sec_dose_CV_ratio,linetype="Constant Vent."),size=0.7)+
  geom_line(aes(x=cum_dist,y=sec_dose_CC_ratio,linetype="Constant Conc."),size=0.7)+
  labs(y=expression(paste("x10"^6, " inhaled particles")),x="Travelled distance [m]")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  scale_colour_manual(name = "Speed Level", values = c("#e80000","#0095ff"))+
  scale_linetype_manual("Dose estimation", values = c("Real"="solid","Constant Vent."="longdash","Constant Conc."="dotted"))+
  theme_light()

plot_cumdoser+facet_grid(Participan~TypeLeg,scales="free_y")


######## plot for cumulative time#########

#plot_time<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(x=cum_dist,y=cum_time/60,col=TypeSpeed))+
  geom_line(size=0.7)+
  labs(y=expression(paste("Elapsed time [min]")),x="Travelled distance [m]")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  scale_y_continuous(limits=c(0, 25))+
  scale_colour_manual(name = "Speed Level", values = c("#e80000","#0095ff"))+
  theme_light()

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


######## plot for concentrations #########

plot_conc<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(x=cum_dist,y=median_part/10^3))+
  geom_line(aes(color="black"))+
  geom_point(aes(y=mean_part/10^3,shape=1),size=1.5)+
  geom_ribbon(aes(ymin=(q1_part)/10^3,ymax=(q2_part)/10^3,fill='black'),alpha=0.15)+
  #geom_smooth(alpha=0.3,se = FALSE,linetype="dashed")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  scale_y_continuous(limits=c(0, 80))+
  labs(y=expression(paste("x10"^3," PNC/cm"^3)),x="Travelled distance [m]")+
  scale_shape_identity(name = NULL, guide = 'legend',labels = c('Mean'))+
  scale_colour_manual(name = ' ',values =c("black"="black"),labels = c('Median'))+
  scale_fill_identity(name = NULL, guide = 'legend',labels = c(expression(paste("IQR"))))+
  theme_light()+
  theme(legend.position = "bottom")
plot_conc+facet_grid(TypeLeg~.)

######## plot for elevation #########

plot_elev<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(x=cum_dist,y=cum_elev))+geom_line()+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  scale_y_continuous(limits=c(0, 60))+
  labs(y=expression(paste("Relative elevation [m]")),x="Travelled distance [m]")+
  theme_light()
plot_elev+facet_grid(.~TypeLeg)

######## plot for ventilation #########
#plot_vent<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(x=cum_dist,y=mean_vent,col=TypeSpeed))+
plot_vent<-ggplot(Sum_overall,aes(x=cum_dist,y=mean_vent,col=TypeSpeed))+
  geom_line(size=0.7)+
  #geom_errorbar(aes(ymin=mean_vent-sd_vent,ymax=mean_vent+sd_vent),alpha=0.3)+
  #geom_pointrange(aes(ymin=mean_vent-sd_vent,ymax=mean_vent+sd_vent),alpha=0.3,size=0.2)+
  #geom_ribbon(aes(ymin=mean_vent-sd_vent,ymax=mean_vent+sd_vent),alpha=0.1,linetype=2)+
  geom_smooth(alpha=0.3,method="auto",se = FALSE,linetype="dashed")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  labs(y=expression(paste("Mean ventilation [lpm]")),x="Travelled distance [m]")+
  scale_colour_manual(name = "Speed Level", values = c("#e80000","#0095ff"))+
  theme_light()
plot_vent+facet_grid(Participan~TypeLeg,scales="free_y")

######## plot for BR #########
#plot_vent<-ggplot(Sum_overall%>%filter(Participan=="Y"),aes(x=cum_dist,y=mean_vent,col=TypeSpeed))+
plot_BR<-ggplot(Sum_overall,aes(x=cum_dist,y=mean_BR,col=TypeSpeed))+
  geom_line(size=0.7)+
  geom_errorbar(aes(ymin=mean_BR-sd_BR,ymax=mean_BR+sd_BR),alpha=0.3)+
  geom_smooth(alpha=0.3,se = FALSE,linetype="dashed")+
  scale_x_continuous(limits=c(0, 6000),breaks=seq(0, 6000, by=1000),labels=c(seq(0,6000, by=1000)))+
  scale_y_continuous(limits=c(0, 50))+
  labs(y=expression(paste("Mean breathing rate [bpm]")),x="Travelled distance [m]")+
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

##### t-test for ventilation rate ######
flow_low<-data_breath%>%filter(TypeSpeed=="Low",Participan=="B",!is.na(flow))
flow_low<-flow_low$flow

flow_high<-data_breath%>%filter(TypeSpeed=="High",Participan=="B",!is.na(flow))
flow_high<-flow_high$flow

t.test(flow_low,flow_high)


##### t-test for breathing rate ######
BR_low<-data_breath%>%filter(TypeSpeed=="Low",Participan=="B",!is.na(breathRate))
BR_low<-BR_low$breathRate

BR_high<-data_breath%>%filter(TypeSpeed=="High",Participan=="B",!is.na(breathRate))
BR_high<-BR_high$breathRate

t.test(BR_low,BR_high)


####### t-test concentrations by leg #####

##### t-test for breathing rate ######
prueba<-rbind(c(1,2,3,4,5,6,7),c(1,2,3,4,5,6,7))
prueba

for (i in (1:7)) {

conc_in<-new_df%>%filter(TypeLeg=="Inbound",Day==i,!is.na(conc))
conc_in<-conc_in$conc
prueba[1,i]<-median(conc_in)
conc_out<-new_df%>%filter(TypeLeg=="Outbound",Day==i,!is.na(conc))
conc_out<-conc_out$conc
prueba[2,i]<-median(conc_out)


}

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

plot_tlights+geom_boxplot(aes(x=Order,y=conc),outlier.shape = NA)+facet_grid(TypeLeg~.)

plot_tlights+
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun.y=mean, geom="point", shape=2, size=2)+
  scale_fill_manual(name = "Section type", values = c("#00AFBB","#f5e31b"),labels=c("No traffic lights","With traffic lights"))+
  scale_y_continuous(limits=c(0, 50000),breaks=seq(0, 50000, by=10000),labels=c(seq(0,50, by=10)))+
  labs(y=expression(paste("x1000 particles/cm"^3)),x="")+
  theme_light()+
  facet_grid(.~TypeLeg)



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

