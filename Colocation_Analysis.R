library(readr)
library(ggplot2)
library(tidyverse)
library(mcr)

data_colocation <- read_csv("M:/2. Dissertation/3. Data Processing/CPC/Colocation/Compilation.csv", col_types = cols(ID = col_factor(levels = c("AIR-POLL-6","AIR-POLL-8", "AIR-POLL-9", "AIR-POLL-10")),TYPE = col_factor(levels = c("RS1", "Roadside", "Background")), Time = col_time(format = "%H:%M:%S")))

back_colo <- read_csv("~/2. Dissertation/3. Data Processing/CPC/Colocation/background_colocation.csv", 
                      col_types = cols(AP10 = col_double(), 
                                       AP6 = col_double(), AP8 = col_double(), 
                                       AP9 = col_double()))

back_colo2 <- read_csv("~/2. Dissertation/3. Data Processing/CPC/Colocation/background_colocation_rm.csv", 
                      col_types = cols(AP10 = col_double(), 
                                       AP6 = col_double(), AP8 = col_double(), 
                                       AP9 = col_double()))

road_colo2 <- read_csv("~/2. Dissertation/3. Data Processing/CPC/Colocation/roadside_colocation_rm2.csv", 
                      col_types = cols(AP10 = col_double(), 
                                       AP6 = col_double(), AP8 = col_double(), 
                                       AP9 = col_double()))
road_colo5 <- read_csv("~/2. Dissertation/3. Data Processing/CPC/Colocation/roadside_colocation_rm1.csv", 
                       col_types = cols(AP10 = col_double(), 
                                        AP6 = col_double(), AP8 = col_double(), 
                                        AP9 = col_double()))



back_colo$avg<-(back_colo$AP6+back_colo$AP8+back_colo$AP9+back_colo$AP10)/4
road_colo$avg<-(road_colo$AP6+road_colo$AP8+road_colo$AP9+road_colo$AP10)/4

li_back<-ggplot(back_colo,aes())+
  geom_point(aes(x=avg,y=AP6),col="blue",alpha=0.7,size=0.8)+
  geom_smooth(aes(x=avg,y=AP6),method='lm',formula=y~x)+
  geom_point(aes(x=avg,y=AP8),col="red",alpha=0.7,size=0.8)+
  geom_smooth(aes(x=avg,y=AP8),method='lm',formula=y~x)+
  geom_point(aes(x=avg,y=AP9),col="pink",alpha=0.7,size=0.8)+
  geom_smooth(aes(x=avg,y=AP9),method='lm',formula=y~x)+
  geom_point(aes(x=avg,y=AP10),col="green",alpha=0.7,size=0.8)+
  geom_smooth(aes(x=avg,y=AP10),method='lm',formula=y~x)

###### comparison 6 nd 10 ########

rm_back$AP6<-frollmean(back_colo$AP6,3)
view(rm_back)
li_back<-ggplot(back_colo,aes())+
  geom_point(aes(x=AP6,y=AP10),col="blue",alpha=0.7,size=0.8)+
  geom_smooth(aes(x=AP6,y=AP10),method='lm',formula=y~x)scale_x_continuous(limits=c(10000, 15000),breaks=seq(10000, 15000, by=1000),labels=c(seq(10,15, by=1)))+
  scale_y_continuous(limits=c(10000, 15000),breaks=seq(10000, 15000, by=1000),labels=c(seq(10,15, by=1)))+
  labs(y=expression(paste("x10"^3," particles/cm"^3)),x=expression(paste("x10"^3," particles/cm"^3)))+
  theme_light()

li_back

li_road<-ggplot(road_colo2,aes())+
  geom_point(aes(x=AP6,y=AP10),col="black",alpha=0.8,size=0.7)+
  geom_smooth(aes(x=AP6,y=AP10),method='lm',formula=y~x)+
  labs(y=expression(paste("AP10 [x10"^3," particles/cm"^3,"]")),x=expression(paste("AP6 [x10"^3," particles/cm"^3,"]")))+
  scale_x_continuous(limits=c(9000, 60000),breaks=seq(10000, 60000, by=10000),labels=c(seq(10,60, by=10)))+
  scale_y_continuous(limits=c(9000, 60000),breaks=seq(10000, 60000, by=10000),labels=c(seq(10,60, by=10)))+
  theme_light()
li_road

####### linear regresion #######


li_road<-ggplot(road_colo,aes())+
  geom_point(aes(x=avg,y=AP6),col="blue",alpha=0.7,size=0.8)+
  geom_smooth(aes(x=avg,y=AP6),method='lm',formula=y~x)+
  geom_point(aes(x=avg,y=AP8),col="red",alpha=0.7,size=0.8)+
  geom_smooth(aes(x=avg,y=AP8),method='lm',formula=y~x)+
  geom_point(aes(x=avg,y=AP9),col="pink",alpha=0.7,size=0.8)+
  geom_smooth(aes(x=avg,y=AP9),method='lm',formula=y~x)+
  geom_point(aes(x=avg,y=AP10),col="green",alpha=0.7,size=0.8)+
  geom_smooth(aes(x=avg,y=AP10),method='lm',formula=y~x)

summary_test<-data_colocation%>%group_by(TYPE,ID)%>%summarise(Mean=mean(Reading),Med=median(Reading),MaxR=max(Reading),MinR=min(Reading),Range=(max(Reading)-min(Reading)),IQRR=IQR(Reading, na.rm = TRUE),SdR=sd(Reading, na.rm = TRUE))

boxcol<-ggplot(data_colocation,aes(y=Reading))+geom_boxplot(aes(fill=ID),alpha=0.6,outlier.shape = NA)
boxcol+facet_grid(~TYPE)

hist<-ggplot(data_colocation[data_colocation$TYPE=="Roadside",],aes(Reading))+
  geom_histogram(aes(fill=ID),binwidth = 1000,col="white", alpha = 0.5) +
  labs(x=expression(paste("fine particle count per cm"^3)),y="Count")+
  scale_x_continuous(breaks=seq(0, max(data_colocation$Reading), by=10000))#+
  #geom_vline(aes(xintercept = 200,color="Hourly mean"),linetype="dotted",size=1,show.legend=NA)+
  #geom_vline(aes(xintercept = 40,color="Annual mean"),linetype="dotted",size=1,show.legend=NA)+
  #scale_fill_manual(name = "Site type", values = c("#00AFBB","#E7B800"))+
  #scale_colour_manual(name = "Limits", values = c("blue", "red"))
hist+facet_grid(ID~TYPE)

plot1<-ggplot(data_colocation[data_colocation$TYPE=="Roadside",],aes(x=Time,y=Reading))+
  geom_line(aes(col=ID), alpha = 0.5) +
  labs(x=expression(paste("fine particle count per cm"^3)),y="Count")+
  scale_x_continuous(breaks=seq(0, max(data_colocation$Reading), by=10000))+
  geom_smooth(aes(col=ID))
plot1


hist2<-ggplot(data_colocation[data_colocation$TYPE=="Background",],aes(Reading))+
  geom_histogram(aes(fill=ID),binwidth = 250,col="white", alpha = 0.5) +
  labs(x=expression(paste("fine particle count per cm"^3)),y="Count")+
  scale_x_continuous(breaks=seq(0, max(data_colocation$Reading), by=1000))#+
#geom_vline(aes(xintercept = 200,color="Hourly mean"),linetype="dotted",size=1,show.legend=NA)+
#geom_vline(aes(xintercept = 40,color="Annual mean"),linetype="dotted",size=1,show.legend=NA)+
#scale_fill_manual(name = "Site type", values = c("#00AFBB","#E7B800"))+
#scale_colour_manual(name = "Limits", values = c("blue", "red"))
hist2+facet_grid(ID~TYPE)

plot2<-ggplot(data_colocation[data_colocation$TYPE=="Background",],aes(x=Time,y=Reading))+
  geom_line(aes(col=ID), alpha = 1) +
  labs(x=expression(paste("fine particle count per cm"^3)),y="Count")+
  scale_x_continuous(breaks=seq(0, max(data_colocation$Reading), by=10000))+
  geom_smooth(aes(col=ID))
plot2


######## MCR REGRESSION Passing Bablok Roadside ########

PB.reg <- mcreg(road_colo2$AP6,road_colo2$AP10, method.reg = "PaBaLarge", alpha=0.2)
PB.reg@mnames<-c("AP6","AP10")
PB.reg@para

PB.reg <- mcreg(road_colo5$AP6,road_colo5$AP8, method.reg = "PaBaLarge", alpha=0.2)
PB.reg@mnames<-c("AP6","AP8")
PB.reg@para

MCResult.plot(PB.reg, equal.axis = TRUE, 
              x.lab = "AP6 - [N.part/cm^3]",
              y.lab = "AP10 - [N.part/cm^3]",
              points.col = "#FF7F5060", points.pch = 19,
              ci.area = TRUE, ci.area.col = "#0000FF50",
              main = "", sub = "", add.grid = FALSE,
              points.cex = 1)


######## MCR REGRESSION Passing Bablok Background ########

PB.reg <- mcreg(back_colo2$AP6,back_colo2$AP9, method.reg = "PaBaLarge", alpha=0.2)
PB.reg@mnames<-c("AP6","AP9")
PB.reg@para

PB.reg <- mcreg(back_colo2$AP6,back_colo2$AP8, method.reg = "PaBaLarge", alpha=0.2)
PB.reg@mnames<-c("AP6","AP8")
PB.reg@para

MCResult.plot(PB.reg, equal.axis = TRUE, 
              x.lab = "AP6 - [N.part/cm^3]",
              y.lab = "AP10 - [N.part/cm^3]",
              points.col = "#FF7F5060", points.pch = 19,
              ci.area = TRUE, ci.area.col = "#0000FF50",
              main = "", sub = "", add.grid = FALSE,
              points.cex = 1)
