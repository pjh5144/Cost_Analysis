#EDA 
#1.20.21

#EDA for the cost data. As data was previously cleaned and processed through PreProcess_PH_Proj.R (from alt account), this will examine/verify that data
#Full Cost - Pharm Cost (need to incorp pharm cost while finalizing)

#Analysis Plan
#1. EDA - Verify Data/Graphics
#2. Estimation of parameters (stan)
#3. HC patients 
#   a. Estimation - EDA
#   b. Factors
#   c. Prediction
#4. HC BH Patients
#5. Total Cost Prediction

#Libraries
library(tidyverse)
library(ggplot2)

#Global Scripts
'%ni%' <- Negate('%in%')

#Dependencies
comp_mh<-read.csv("/Users/peterhoover/Documents/Analysis_Projects/Global_data/mapping_files/symp/comparre_MH_final.csv")
edc<-read.csv("/Users/peterhoover/Documents/Analysis_Projects/Global_data/mapping_files/symp/edc_merged.csv")

psych<-edc%>%
  filter(grepl("^PSY",EDC))

#Data
pts<-data.table::fread("/Users/peterhoover/Documents/Analysis_Projects/Global_data/cost/clean/perPatientTotals_proj.csv")
encs<-data.table::fread("/Users/peterhoover/Documents/Analysis_Projects/Global_data/cost/clean/tbi_enc_proj.csv")

#EDA

##Demographics
length(unique(pts$pseudo_personid))

#Age
summary(pts$age)

#Input age for missing with mean
pts<-pts%>%
  mutate(age = case_when(is.na(age) ~ mean(age,na.rm=T),
                         TRUE ~ age))%>%
  mutate(age_grp = cut(round(as.numeric(age),1),breaks=c(0,24,34,44,54,64),
                       labels=c("18-24","25-34","35-44","45-54","55-64"),include.lowest = T))

pts%>%group_by(age_grp)%>%summarise(min=min(age),max=max(age))
#gender
table(pts$gender)

#ethnicity
table(pts$ethnicity)

#bos
table(pts$sponservice)

#Input sponservice for missing (this needs to be updated at a later date)
pts<-pts%>%
  mutate(sponservice = case_when(sponservice==""~"X",
                                 TRUE ~ sponservice))

#Index Date

pts$tbi_index<-as.Date(pts$tbi_index)
ggplot(pts,aes(x=lubridate::floor_date(tbi_index,"month")))+geom_bar()+labs(x="Date",y="Freq of mTBI")+ggtitle("Freq of mTBI per Date")+
  theme(plot.title = element_text(hjust = 0.5))


#Cost
summary(pts$Cost_Post)

ggplot(pts,aes(x=Cost_Post,y=Cost_Pre))+geom_point()

ggplot(pts,aes(x=Cost_Post))+geom_histogram()
ggplot(pts%>%filter(Cost_Post<1000000),aes(x=Cost_Post))+geom_histogram()

ggplot(pts,aes(x=Cost_Post-Cost_Pre))+geom_histogram()


#Create Flag for Pre-MH 

pre_mh<-encs%>%
  dplyr::filter(enc_flag=="Pre")%>%
  select(pseudo_personid,diag1:diag5)%>%
  reshape2::melt(id.vars=c("pseudo_personid"))%>%
  filter(value %in% psych$DX)

pts<-pts%>%
  mutate(mh_pre_flag = case_when(pseudo_personid %in% pre_mh$pseudo_personid ~ "1",
                                 TRUE ~ "0"))


#Create zero flag

pts<-pts%>%
  mutate(Post_Cost_Zero = case_when(Cost_Post ==0 ~ 1,
                                    TRUE ~ 0))
