#1.20.21
#Preprocessing

#This preprocessign script is used to create features which will be used to explore HC patients

#Libraries
library(tidyverse)
library(ggplot2)

#Global Scripts
'%ni%' <- Negate('%in%')

#Dependencies
comp_mh<-read.csv("/Users/peterhoover/Documents/Analysis_Projects/Global_data/mapping_files/symp/comparre_MH_final.csv")
edc<-read.csv("/Users/peterhoover/Documents/Analysis_Projects/Global_data/mapping_files/symp/edc_merged.csv")

#Data
pts<-data.table::fread("/Users/peterhoover/Documents/Analysis_Projects/Global_data/cost/clean/perPatientTotals_proj.csv")
encs<-data.table::fread("/Users/peterhoover/Documents/Analysis_Projects/Global_data/cost/clean/tbi_enc_proj.csv")

#Correct/Reformat Variables

#Age Breaks
pts<-pts%>%
  mutate(age = case_when(is.na(age) ~ mean(age,na.rm=T),
                         TRUE ~ age))%>%
  mutate(age_grp = cut(round(as.numeric(age),1),breaks=c(0,24,34,44,54,64),
                       labels=c("18-24","25-34","35-44","45-54","55-64"),include.lowest = T))
#Missing BOS
pts<-pts%>%
  mutate(sponservice = case_when(sponservice==""~"X",
                                 TRUE ~ sponservice))


#Create Percentile Flags 
#90th and 95th

pts<-pts%>%
  mutate(perc_90_total = case_when(Cost_Post >= quantile(Cost_Post,.90) ~ 1,
                             TRUE ~ 0),
         perc_95_total = case_when(Cost_Post >= quantile(Cost_Post,.95) ~ 1,
                             TRUE ~ 0),
         perc_90_mh = case_when(Cost_Post_MH >= quantile(Cost_Post_MH,.90) ~ 1,
                                TRUE ~ 0),
         perc_95_mh = case_when(Cost_Post_MH >+ quantile(Cost_Post_MH,.95) ~1,
                                TRUE ~ 0))

table(pts$perc_90)

#Already those of high cost pre 
pts%>%
  #group_by(perc_90_total)%>%
  group_by(perc_95_total)%>%
  summarise(min=min(Cost_Pre),med=median(Cost_Pre),max=max(Cost_Pre))

pts%>%
  #group_by(perc_90_mh)%>%
  group_by(perc_95_mh)%>%
  summarise(min=min(Cost_Post_MH),med=median(Cost_Post_MH),max=max(Cost_Pre_MH))

#Comorbitiy (Pre)
#Creates pre comorbidity flag (charlson - other can be applied) 
library(comorbidity)

dx_long<-encs%>%
  filter(enc_flag=="Pre")%>%
  select(pseudo_personid,diag1:diag5)%>%
  reshape2::melt(id.vars='pseudo_personid')%>%
  filter(value != "")

comor<-comorbidity(x=dx_long,id="pseudo_personid",code="value",score="charlson",assign0=F)

#summary(comor$windex)


#Append features to final df

#Add and fill in co-morbidity 
pts<-pts%>%
  left_join(comor,by="pseudo_personid")%>%
  mutate_at(vars(score,wscore,),~replace(.,is.na(.),0))%>%
  mutate_at(vars(index,windex,),~replace(.,is.na(.),"0"))






df<-pts

############################
###Create Train Test Sets###
############################

set.seed(1111)

###################################
#Create train/test sets 

sample<-sample.int(n=nrow(df),size=floor(0.75*nrow(df)),replace=F)
train<-df[sample,]
test<-df[-sample,]






