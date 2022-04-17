#Libraries Needed to Run Code in R
library(tidyverse)
library(scales)
library(lubridate)
library(title)
library(moderndive)

#Code to Acess the Data
All_Data_pfas <- read.csv("~/Westfieldfinalproject/pfas/All_Data_pfas.csv")

########################################################################################

#This code is used to produce average reportable concentrations for PFAS compounds in the influents, effluent, and filtercake at two
#wastewater treatment facilities owned by the Narragansett Bay Commission. Any input into
#the ALL_Data_pfas excel sheet that contains reportable concentrations will be reflected in the output of this code. 

#1. Avg. Reportable Conc.(ng/L) of PFAS compounds at Fields Point from Oct. 2020 to May 2021

#taking fp influent and effluent reportable avg concentrations. Ignoring "u" and "j" qualifiers because the data are non-detect and non-reportable
FP_INFLUENT<-All_Data_pfas %>% filter(Description=="FP Influent") %>% filter(Qualifier==" ") %>%    
  group_by(Description,Compound,Plant)%>% 
  summarize(textResult= mean(textResult))
FP_EFFLUENT<-All_Data_pfas %>% filter(Description=="FP Final Effluent") %>% filter(Qualifier==" ") %>% 
  group_by(Description,Compound,Plant) %>% 
  summarize(textResult=mean(textResult))
#joining PFAS compounds in FP influent and Effluent that had average reportable concentrations
FP_INF_EFF<-rbind(FP_EFFLUENT,FP_INFLUENT)
#graph of FP INF AND EFF reportable PFAS compound concentrations 
ggplot(FP_INF_EFF,mapping=aes(Description,textResult,fill=Description))+geom_col()+
  facet_wrap(~Compound)+scale_x_discrete(limits=c("FP Influent","FP Final Effluent"))+
 labs(title="Avg. Reportable Conc.(ng/L) of PFAS compounds at Fields Point from Oct. 2020 to May 2021")+
    ylab("Avg. Concenration (ng/L)")+xlab(" ")+theme(legend.position = "none")+theme(axis.text.x = element_text(angle = -90))


#check to see if avg conc. is a function of INFULENT and EFFLUENT
model_FP<-lm(textResult~Description, data=FP_INF_EFF)
#get P_value
summaryFP<-get_regression_summaries(model_FP)%>% pull(p_value)

#######################################################################
#BUCKLIN POINT
#most code is the same except filtering for Bucklin Point instead of Fields Point
#taking fp influent and effluent reportable avg concentrations.
BP_INFLUENT<-All_Data_pfas %>% filter(Description=="BP Influent") %>% filter(Qualifier==" ") %>%    
  group_by(Description,Compound,Plant) %>% 
  summarize(textResult= mean(textResult))
BP_EFFLUENT<-All_Data_pfas %>% filter(Description=="BP Final Effluent") %>% filter(Qualifier==" ") %>% 
  group_by(Description,Compound,Plant) %>% 
  summarize(textResult=mean(textResult))
#joining BP influent and Effluent that had reportable concentration
BP_INF_EFF<-rbind(BP_EFFLUENT,BP_INFLUENT)
#Graph of BP INF and EFF avg reportable pfas compound concentrations. 
ggplot(BP_INF_EFF,mapping=aes(Description,textResult,fill=Description))+geom_col()+
  facet_wrap(~Compound)+scale_x_discrete(limits=c("BP Influent","BP Final Effluent"))+
  labs(title="Avg. Reportable Conc.(ng/L) of PFAS compounds at Bucklin Point from Oct. 2020 to May 2021")+
  ylab("Avg. Concenration (ng/L)")+xlab(" ")+theme(legend.position = "none")+theme(axis.text.x = element_text(angle = -90))

#check to see if avg conc. is a function of INFULENT and EFFLUENT
model_BP<-lm(textResult~Description,data=BP_INF_EFF)
#get P_value
summaryBP<-get_regression_summaries(model_BP)%>% pull(p_value)
################################################################################
#FP and BP Filter Cake
#Not a lot of PFAS compounds found in filter cake so they are joined together to make one plot
FP_FCAK<-All_Data_pfas %>% filter(Description=="FP Filter Cake") %>% filter(Qualifier==" ") %>%    
  group_by(Description,Compound,Plant) %>% 
  summarize(textResult= mean(textResult))
BP_FCAK<-All_Data_pfas %>% filter(Description=="BP Filter Cake") %>% filter(Qualifier==" ") %>% 
  group_by(Description,Compound,Plant) %>% 
  summarize(textResult=mean(textResult))
FP_BP_FCAK<-rbind(FP_FCAK,BP_FCAK)
#graph Avg. Reportable Conc.(ug/Kg) of PFAS compounds in Fields Point and Bucklin Point Filter Cake from October 2020 to May 2021     
ggplot(FP_BP_FCAK,mapping=aes(Description,textResult,fill=Description))+geom_col()+
  facet_wrap(~Compound)+scale_x_discrete(limits=c("FP Filter Cake","BP Filter Cake"))+
  labs(title="Avg. Reportable Conc.(ug/Kg) of PFAS compounds in Fields Point and Bucklin Point Filter Cake from October 2020 to May 2021")+
  ylab("Avg. Concenration (ug/Kg)")+xlab(" ")+theme(legend.position = "none")+theme(axis.text.x = element_text(angle = -90))
#is result dependent on Fields Point or Bucklin Point
model_fcak<-lm(textResult~Description,data=FP_BP_FCAK)
summaryFCAK<-get_regression_summaries(model_fcak)%>% pull(p_value)


############################################################################################
# FP SIU DATA

FP_SIU<-All_Data_pfas %>% filter(Sampling.point.type=="SIU") %>% filter(Qualifier==" ") %>% filter(Plant=="FP") %>%    
  group_by(Point.location,Compound,Plant) %>%  
  summarize(textResult= mean(textResult))

ggplot(FP_SIU,mapping=aes(Compound,textResult,color=Compound))+geom_point()+facet_wrap(~Point.location)+theme(axis.text.x = element_text(angle = -90))+theme(legend.position = "none")+
  labs(title="Avg. Reportable Conc.(ng/L) of PFAS compounds found in Fields Point Industrial Users")+
  ylab("Avg. Conc. (ng/L)")+xlab("Compound")+ theme(text = element_text(size = 8)) 

#################################################################################################
#BP SIU DATA
BP_SIU<-All_Data_pfas %>% filter(Sampling.point.type=="SIU") %>% filter(Qualifier==" ") %>% filter(Plant=="BP") %>%     
  group_by(Point.location,Compound,Plant) %>% 
  summarize(textResult= mean(textResult))

ggplot(BP_SIU,mapping=aes(Compound,textResult,color=Compound))+geom_point()+facet_wrap(~Point.location)+theme(axis.text.x = element_text(angle = -90))+theme(legend.position = "none")+
  labs(title="Avg. Reportable Conc.(ng/L) of PFAS compounds found in Bucklin Point Industrial Users")+
  ylab("Avg. Conc. (ng/L)")+xlab("Compound")+theme(text = element_text(size = 8)) 


