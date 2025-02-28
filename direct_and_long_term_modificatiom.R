library(dplyr)
library(tidyverse)
library(data.table)
library(Hmisc)
library(lubridate)
library(stringr)
library(sf)
library(purrr)
library(splines)
library(dplyr)
library(mgcv)
library(dlnm)
library(ggplot2)
library(interactionR)
library(metafor)
library(ggh4x)
library(ggsci)
library(dlnm)
library(splines)
library(xtable)
library(mvmeta)
library(ggplot2)
library(patchwork)
library(compareGroups)
library(raster)
library(ncdf4)
library(doParallel)
library(terra)
#install.packages("magrittr")
#install.packages("exactextractr")
#library(exactextractr)
#install.packages("furrr")
library(furrr)
#install.packages("humidity")
library(sp)
library("humidity")
library(magrittr)
library(haven)
library(doParallel)
library(parallel)
library(interactionR)
library(geepack)
library(zoo)
library(gnm)
library(MASS)
library(qcc)
library(future.apply)
library(haven)
library(tableone)
library(cowplot)
library(colorspace)
library(boot)

##############################################################读取下面的数据##############
########################################################################################
#######################################################################################
#####################################################################################
#load data####
diseases2_total<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_city.rds")
diseases2_cvd2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_city.rds")
diseases2_suicides2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_city.rds")
diseases2_resp2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_city.rds")

#the count of climatic area
result<-diseases2_total %>% 
  group_by(Climate_cat_code) %>% 
  summarise(citycount=n_distinct(city_code)) %>% 
  arrange(desc(citycount))
write.csv(result,"E:/PKU_fanyi/Pro1/result_long/climate_code_count.csv")
#table S1#####
vars<-colnames(diseases2_total[,c(54,56,9:12,30,39,63:70,90:92,75,78,81)])
tableS2<-CreateTableOne(vars=vars,strata="dead_year",data=diseases2_total,addOverall = T)
tableS2_1<-as.data.frame(print(tableS2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE,showAllLevels = T,digits = 4))
summary_tableS2<-diseases2_total %>% 
  group_by(dead_year) %>% 
  summarise(
    mean_avg_forest_shrub_grassland_fraction_1year=mean(avg_forest_shrub_grassland_fraction_1year),
    sd_avg_forest_shrub_grassland_fraction_1year=sd(avg_forest_shrub_grassland_fraction_1year),
    mean_avg_water_wetland_fraction_1year=mean(avg_water_wetland_fraction_1year),
    sd_avg_water_wetland_fraction_1year=sd(avg_water_wetland_fraction_1year),
    mean_heat_temperature_proportion=mean(heat_temperature_proportion),
    sd_heat_temperature_proportion=sd(heat_temperature_proportion),
    
    mean_cold_temperature_proportion=mean(cold_temperature_proportion),
    sd_cold_temperature_proportion=sd(cold_temperature_proportion),
    
    mean_extreme_temperature_proportion=mean(extreme_temperature_proportion),
    sd_extreme_temperature_proportion=sd(extreme_temperature_proportion),
  )
write.csv(tableS2_1,"E:/PKU_fanyi/Pro1/result_long/tableS2_11.csv")
write.csv(summary_tableS2,"E:/PKU_fanyi/Pro1/result_long/summary_tableS2.csv")
#the direct association between blue and green space and mortality#####
datasets <- list(
  total= diseases2_total, 
  cvd= diseases2_cvd2, 
  resp= diseases2_resp2,
  suicide=diseases2_suicides2
)
#
count<-0
# 遍历数据集
result_continous=data.frame()
result_cat=data.frame()
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 获取当前数据集
  data<-data %>% 
    mutate(avg_forest_fraction_1year_scaled=scale(avg_forest_fraction_1year),
           avg_forest_fraction_3year_scaled=scale(avg_forest_fraction_3year),
           avg_forest_fraction_5year_scaled=scale(avg_forest_fraction_5year),
           avg_shrub_fraction_1year_scaled=scale(avg_shrub_fraction_1year),
           avg_shrub_fraction_3year_scaled=scale(avg_shrub_fraction_3year),
           avg_shrub_fraction_5year_scaled=scale(avg_shrub_fraction_5year),
           avg_grassland_fraction_1year_scaled=scale(avg_grassland_fraction_1year),
           avg_grassland_fraction_3year_scaled=scale(avg_grassland_fraction_3year),
           avg_grassland_fraction_5year_scaled=scale(avg_grassland_fraction_5year),
           avg_forest_shrub_fraction_1year_scaled=scale(avg_forest_shrub_fraction_1year),
           avg_forest_shrub_fraction_3year_scaled=scale(avg_forest_shrub_fraction_3year),
           avg_forest_shrub_fraction_5year_scaled=scale(avg_forest_shrub_fraction_5year),
           avg_forest_grassland_fraction_1year_scaled=scale(avg_forest_grassland_fraction_1year),
           avg_forest_grassland_fraction_3year_scaled=scale(avg_forest_grassland_fraction_3year),
           avg_forest_grassland_fraction_5year_scaled=scale(avg_forest_grassland_fraction_5year),
           avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
           avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
           avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
           avg_water_fraction_1year_scaled=scale(avg_water_fraction_1year),
           avg_water_fraction_3year_scaled=scale(avg_water_fraction_3year),
           avg_water_fraction_5year_scaled=scale(avg_water_fraction_5year),
           avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
           avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
           avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  #
  data$avg_forest_fraction_1year_scaled_2group<- cut(data$avg_forest_fraction_1year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_1year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_fraction_3year_scaled_2group<- cut(data$avg_forest_fraction_3year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_3year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_fraction_5year_scaled_2group<- cut(data$avg_forest_fraction_5year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_5year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_shrub_fraction_1year_scaled_2group<- cut(data$avg_shrub_fraction_1year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_1year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_shrub_fraction_3year_scaled_2group<- cut(data$avg_shrub_fraction_3year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_3year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_shrub_fraction_5year_scaled_2group<- cut(data$avg_shrub_fraction_5year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_5year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_grassland_fraction_1year_scaled_2group<- cut(data$avg_grassland_fraction_1year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_1year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  
  data$avg_grassland_fraction_3year_scaled_2group<- cut(data$avg_grassland_fraction_3year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_3year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  
  data$avg_grassland_fraction_5year_scaled_2group<- cut(data$avg_grassland_fraction_5year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_5year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_forest_shrub_fraction_1year_scaled_2group<- cut(data$avg_forest_shrub_fraction_1year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_1year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_fraction_3year_scaled_2group<- cut(data$avg_forest_shrub_fraction_3year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_3year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_fraction_5year_scaled_2group<- cut(data$avg_forest_shrub_fraction_5year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_5year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_forest_grassland_fraction_1year_scaled_2group<- cut(data$avg_forest_grassland_fraction_1year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_1year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_grassland_fraction_3year_scaled_2group<- cut(data$avg_forest_grassland_fraction_3year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_3year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_grassland_fraction_5year_scaled_2group<- cut(data$avg_forest_grassland_fraction_5year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_5year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  #
  
  data$avg_forest_shrub_grassland_fraction_1year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_1year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_1year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_grassland_fraction_3year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_3year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_3year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_grassland_fraction_5year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_5year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_5year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  #
  data$avg_water_fraction_1year_scaled_2group<- cut(data$avg_water_fraction_1year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_1year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_fraction_3year_scaled_2group<- cut(data$avg_water_fraction_3year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_3year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_fraction_5year_scaled_2group<- cut(data$avg_water_fraction_5year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_5year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  #
  data$avg_water_wetland_fraction_1year_scaled_2group<- cut(data$avg_water_wetland_fraction_1year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_1year_scaled, 
                                                                              probs = c(0, 0.50, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_wetland_fraction_3year_scaled_2group<- cut(data$avg_water_wetland_fraction_3year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_3year_scaled, 
                                                                              probs = c(0, 0.50, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_wetland_fraction_5year_scaled_2group<- cut(data$avg_water_wetland_fraction_5year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_5year_scaled, 
                                                                              probs = c(0, 0.50, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  data$lpopulation<-log(data$total.population)
  #
  #continuous####
  
  ns_values<-1:6
  
  for(space in c("avg_forest_shrub_grassland_fraction_1year_scaled","avg_forest_shrub_grassland_fraction_3year_scaled","avg_forest_shrub_grassland_fraction_5year_scaled",
                 "avg_water_wetland_fraction_1year_scaled","avg_water_wetland_fraction_3year_scaled","avg_water_wetland_fraction_5year_scaled")){
    for (ns_value in ns_values){
      
      formula <- as.formula(paste("n ~",space,"+ns(dead_year,",ns_value,
                                  ")+as.factor(GDP_avg_quintile)",
                                  "+offset(lpopulation)",
                                  "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                  "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                  "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                  "+s(Climate_cat_code,bs='re')",
                                  sep = ""))
      model<-gam(formula, family = quasipoisson, data = data,na.action = na.omit,method = "REML")
      result<-as.data.frame(summary(model)$p.table)[space, ]
      beta=as.numeric(result$Estimate)
      se=as.numeric(result$`Std. Error`)
      result$diseases<-dataset_name
      
      result$family<-model[["family"]][["family"]]
      result$space<-space
      result$ns<-ns_value
      result$gcv<-summary(model)[["sp.criterion"]][["REML"]]
      result$r2<-summary(model)[["r.sq"]]
      result$dev.expl<-summary(model)[["dev.expl"]]
      result$PC=round((exp(beta)-1)*100,digits = 2)
      result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
      result_continous<- rbind(result_continous, result)
      
    }
  }
  ###cat####
  for(space in c("avg_forest_shrub_grassland_fraction_1year_scaled_2group","avg_forest_shrub_grassland_fraction_3year_scaled_2group","avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                 "avg_water_wetland_fraction_1year_scaled_2group","avg_water_wetland_fraction_3year_scaled_2group","avg_water_wetland_fraction_5year_scaled_2group")){
    
    formula <- as.formula(paste("n ~as.factor(",space,")+ns(dead_year,1)",
                                "+as.factor(GDP_avg_quintile)",
                                "+offset(lpopulation)",
                                "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                "+s(Climate_cat_code,bs='re')",
                                sep = ""))
    
    model<-gam(formula, family = quasipoisson, data = data,na.action = na.omit,method = "REML")
    result<-as.data.frame(summary(model)$p.table)[2, ]
    beta=as.numeric(result$Estimate)
    se=as.numeric(result$`Std. Error`)
    result$diseases<-dataset_name
    
    result$family<-model[["family"]][["family"]]
    result$space<-space
    result$gcv<-summary(model)[["sp.criterion"]][["REML"]]
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    result_cat<- rbind(result_cat, result)
  }
  count=count+1
  print(count)
}
write.csv(result_continous,"E:/PKU_fanyi/Pro1/result_long/result_blue_green_direct_continous.csv")
write.csv(result_cat,"E:/PKU_fanyi/Pro1/result_long/result_blue_green_direct_cat.csv")
######the long-term modification####
count<-0
result_long_pollution_2=data.frame()
result_long_continous_modefication=data.frame()
result_long_cat_modefication=data.frame()
result_long_cat_interaction=data.frame()
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  
  data<-data %>% 
    mutate(avg_forest_fraction_1year_scaled=scale(avg_forest_fraction_1year),
           avg_forest_fraction_3year_scaled=scale(avg_forest_fraction_3year),
           avg_forest_fraction_5year_scaled=scale(avg_forest_fraction_5year),
           avg_shrub_fraction_1year_scaled=scale(avg_shrub_fraction_1year),
           avg_shrub_fraction_3year_scaled=scale(avg_shrub_fraction_3year),
           avg_shrub_fraction_5year_scaled=scale(avg_shrub_fraction_5year),
           avg_grassland_fraction_1year_scaled=scale(avg_grassland_fraction_1year),
           avg_grassland_fraction_3year_scaled=scale(avg_grassland_fraction_3year),
           avg_grassland_fraction_5year_scaled=scale(avg_grassland_fraction_5year),
           avg_forest_shrub_fraction_1year_scaled=scale(avg_forest_shrub_fraction_1year),
           avg_forest_shrub_fraction_3year_scaled=scale(avg_forest_shrub_fraction_3year),
           avg_forest_shrub_fraction_5year_scaled=scale(avg_forest_shrub_fraction_5year),
           avg_forest_grassland_fraction_1year_scaled=scale(avg_forest_grassland_fraction_1year),
           avg_forest_grassland_fraction_3year_scaled=scale(avg_forest_grassland_fraction_3year),
           avg_forest_grassland_fraction_5year_scaled=scale(avg_forest_grassland_fraction_5year),
           avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
           avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
           avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
           avg_water_fraction_1year_scaled=scale(avg_water_fraction_1year),
           avg_water_fraction_3year_scaled=scale(avg_water_fraction_3year),
           avg_water_fraction_5year_scaled=scale(avg_water_fraction_5year),
           avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
           avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
           avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  #
  data$avg_forest_fraction_1year_scaled_2group<- cut(data$avg_forest_fraction_1year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_1year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_fraction_3year_scaled_2group<- cut(data$avg_forest_fraction_3year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_3year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_fraction_5year_scaled_2group<- cut(data$avg_forest_fraction_5year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_5year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_shrub_fraction_1year_scaled_2group<- cut(data$avg_shrub_fraction_1year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_1year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_shrub_fraction_3year_scaled_2group<- cut(data$avg_shrub_fraction_3year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_3year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_shrub_fraction_5year_scaled_2group<- cut(data$avg_shrub_fraction_5year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_5year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_grassland_fraction_1year_scaled_2group<- cut(data$avg_grassland_fraction_1year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_1year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  
  data$avg_grassland_fraction_3year_scaled_2group<- cut(data$avg_grassland_fraction_3year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_3year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  
  data$avg_grassland_fraction_5year_scaled_2group<- cut(data$avg_grassland_fraction_5year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_5year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_forest_shrub_fraction_1year_scaled_2group<- cut(data$avg_forest_shrub_fraction_1year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_1year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_fraction_3year_scaled_2group<- cut(data$avg_forest_shrub_fraction_3year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_3year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_fraction_5year_scaled_2group<- cut(data$avg_forest_shrub_fraction_5year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_5year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_forest_grassland_fraction_1year_scaled_2group<- cut(data$avg_forest_grassland_fraction_1year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_1year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_grassland_fraction_3year_scaled_2group<- cut(data$avg_forest_grassland_fraction_3year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_3year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_grassland_fraction_5year_scaled_2group<- cut(data$avg_forest_grassland_fraction_5year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_5year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  #
  
  data$avg_forest_shrub_grassland_fraction_1year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_1year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_1year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_grassland_fraction_3year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_3year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_3year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_grassland_fraction_5year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_5year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_5year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  #
  data$avg_water_fraction_1year_scaled_2group<- cut(data$avg_water_fraction_1year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_1year_scaled, 
                                                                      probs = c(0, 0.60, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_fraction_3year_scaled_2group<- cut(data$avg_water_fraction_3year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_3year_scaled, 
                                                                      probs = c(0, 0.60, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_fraction_5year_scaled_2group<- cut(data$avg_water_fraction_5year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_5year_scaled, 
                                                                      probs = c(0, 0.60, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  #
  data$avg_water_wetland_fraction_1year_scaled_2group<- cut(data$avg_water_wetland_fraction_1year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_1year_scaled, 
                                                                              probs = c(0, 0.60, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_wetland_fraction_3year_scaled_2group<- cut(data$avg_water_wetland_fraction_3year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_3year_scaled, 
                                                                              probs = c(0, 0.60, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_wetland_fraction_5year_scaled_2group<- cut(data$avg_water_wetland_fraction_5year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_5year_scaled, 
                                                                              probs = c(0, 0.60, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  data$lpopulation<-log(data$total.population)
  #
  #####the direct assocaiton between polltion and mortality####
  for(pollution_ in c("PM1_1year","PM2.5_1year","PM10_1year")){
    formula <- as.formula(paste("n ~",pollution_,"+ns(dead_year,1)",
                                "+as.factor(GDP_avg_quintile)",
                                "+offset(lpopulation)",
                                "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                "+s(Climate_cat_code,bs='re')",
                                sep = ""))
    model<-gam(formula, family = quasipoisson, data = data,na.action = na.omit,method = "REML")
    result<-as.data.frame(summary(model)$p.table)[pollution_, ]
    beta=as.numeric(result$Estimate)
    se=as.numeric(result$`Std. Error`)
    result$data<-dataset_name
    result$blue_green<-blue_green_ 
    result$pollution<-pollution_
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    result_long_pollution<- rbind(result_long_pollution, result)
    
    
  }
  #####continuous_forest####
  for(pollution_ in c("PM1_1year","PM2.5_1year","PM10_1year")){
    for (blue_green_ in c("avg_forest_shrub_grassland_fraction_1year_scaled","avg_forest_shrub_grassland_fraction_3year_scaled","avg_forest_shrub_grassland_fraction_5year_scaled"
    )){
      change_values<-seq(0,0.90,by=0.05) 
      model<-gam(as.formula(paste("n ~",pollution_,"*",blue_green_,"+ns(dead_year,1)",
                                  "+as.factor(GDP_avg_quintile)",
                                  "+offset(lpopulation)",
                                  "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                  "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                  "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                  "+s(Climate_cat_code,bs='re')",
                                  sep = "")),family = quasipoisson, data = data,na.action = na.omit,method = "REML")
      beta1_boot<-summary(model)$p.table[pollution_,"Estimate"]
      beta2_boot<-summary(model)$p.table[paste0(pollution_,":",blue_green_),"Estimate"]
      se1_boot<-summary(model)$p.table[pollution_,"Std. Error"]
      se2_boot<-summary(model)$p.table[paste0(pollution_,":",blue_green_),"Std. Error"]
      se<-sqrt(se1_boot^2+se2_boot^2)
      result_df<-data.frame(
        data=dataset_name,
        blue_green=blue_green_,
        pollution=pollution_,
        change_value=change_values,
        beta=numeric(length(change_values)),
        se=numeric(length(change_values))
      )
      for (i in seq_along(change_values)){
        change_value<-change_values[i]
        result_df$beta[i]<-beta1_boot+beta2_boot*change_value
        result_df$se[i]<-se
      }
      result_long_continous_modefication<-rbind(result_long_continous_modefication,result_df)
      
    }
  }
  #####continuous_water####
  
  for(pollution_ in c("PM1_1year","PM2.5_1year","PM10_1year")){
    for (blue_green_ in c(
      "avg_water_wetland_fraction_1year_scaled","avg_water_wetland_fraction_3year_scaled","avg_water_wetland_fraction_5year_scaled")){
      change_values<-seq(0,0.25,by=0.01) 
      model<-gam(as.formula(paste("n ~",pollution_,"*",blue_green_,"+ns(dead_year,1)",
                                  "+as.factor(GDP_avg_quintile)",
                                  "+offset(lpopulation)",
                                  "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                  "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                  "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                  "+s(Climate_cat_code,bs='re')",
                                  sep = "")),family = quasipoisson, data = data,na.action = na.omit,method = "REML")
      beta1_boot<-summary(model)$p.table[pollution_,"Estimate"]
      beta2_boot<-summary(model)$p.table[paste0(pollution_,":",blue_green_),"Estimate"]
      se1_boot<-summary(model)$p.table[pollution_,"Std. Error"]
      se2_boot<-summary(model)$p.table[paste0(pollution_,":",blue_green_),"Std. Error"]
      se<-sqrt(se1_boot^2+se2_boot^2)
      result_df<-data.frame(
        data=dataset_name,
        blue_green=blue_green_,
        pollution=pollution_,
        change_value=change_values,
        beta=numeric(length(change_values)),
        se=numeric(length(change_values))
      )
      for (i in seq_along(change_values)){
        change_value<-change_values[i]
        result_df$beta[i]<-beta1_boot+beta2_boot*change_value
        result_df$se[i]<-se
      }
      result_long_continous_modefication<-rbind(result_long_continous_modefication,result_df)
      
    }
  }
  ####cat####
  for (strate_factor in c("avg_forest_shrub_grassland_fraction_1year_scaled_2group","avg_forest_shrub_grassland_fraction_3year_scaled_2group","avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                          "avg_water_wetland_fraction_1year_scaled_2group","avg_water_wetland_fraction_3year_scaled_2group","avg_water_wetland_fraction_5year_scaled_2group")){
    for (fenzu in 1:2) {
      data1 <- subset(data, data[[strate_factor]] == fenzu)
      for (pollution_ in c("PM1_1year","PM2.5_1year","PM10_1year")) {
        formula <- as.formula(paste("n ~",pollution_,"+ns(dead_year,1)",
                                    "+as.factor(GDP_avg_quintile)",
                                    "+offset(lpopulation)",
                                    "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                    "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                    "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                    "+s(Climate_cat_code,bs='re')",
                                    sep = ""))
        
        model<-gam(formula, family = quasipoisson, data = data1, na.action = na.omit,method="REML")
        result<-as.data.frame(summary(model)$p.table)[pollution_, ]
        beta=as.numeric(result$Estimate)
        se=as.numeric(result$`Std. Error`)
        result$data<-dataset_name
        result$strate_factor <- strate_factor
        result$fenzu<-fenzu
        result$pollution<-pollution_
        result$PC=round((exp(beta)-1)*100,digits = 2)
        result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
        result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
        result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
        result_long_cat_modefication<- rbind(result_long_cat_modefication, result)
        
      }
    }
  }
  
  ####interaction####
  for (strate_factor in c("avg_forest_shrub_grassland_fraction_1year_scaled_2group","avg_forest_shrub_grassland_fraction_3year_scaled_2group","avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                          "avg_water_wetland_fraction_1year_scaled_2group","avg_water_wetland_fraction_3year_scaled_2group","avg_water_wetland_fraction_5year_scaled_2group")) {
    for (pollution_ in c("PM1_1year","PM2.5_1year","PM10_1year")){
      formula <- as.formula(paste("n ~",pollution_,"*",strate_factor,"+ns(dead_year,1)",
                                  "+as.factor(GDP_avg_quintile)",
                                  "+offset(lpopulation)",
                                  "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                  "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                  "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                  "+s(Climate_cat_code,bs='re')",
                                  sep = ""))
      model<-gam(formula, family = quasipoisson, data = data1, na.action = na.omit,method="REML")
      result<-as.data.frame(summary(model)$p.table)[paste0(pollution_,":",strate_factor), ]
      beta=as.numeric(result$Estimate)
      se=as.numeric(result$`Std. Error`)
      result$data<-dataset_name
      result$strate_factor <- strate_factor
      result$fenzu<-fenzu
      result$pollution<-pollution_
      result$PC=round((exp(beta)-1)*100,digits = 2)
      result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
      result_long_cat_interaction<- rbind(result_long_cat_interaction, result)
      
    }
  }
  count=count+1
  print(count)
}
result_long_cat_modefication <- result_long_cat_modefication%>%
  arrange(data,strate_factor, desc(pollution),fenzu)
result_long_cat_interaction <- result_long_cat_interaction%>%
  arrange(data,strate_factor, desc(pollution))
write.csv(result_long_pollution_2,"E:/PKU_fanyi/Pro1/result_long/result_long_pollution_2.csv")
write.csv(result_long_cat_modefication,"E:/PKU_fanyi/Pro1/result_long/result_long_continous_modefication.csv")
write.csv(result_long_cat_modefication,"E:/PKU_fanyi/Pro1/result_long/result_long_cat_modefication.csv")
write.csv(result_long_cat_interaction,"E:/PKU_fanyi/Pro1/result_long/result_long_cat_interaction.csv")
#####nonlinear modifaction effect#####
#####long-term non_linear_interaction#####
diseases2_total<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_city.rds")
diseases2_cvd2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_city.rds")
diseases2_suicides2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_city.rds")
diseases2_resp2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_city.rds")
###*
datasets <- list(
  total = diseases2_total, 
  cvd = diseases2_cvd2, 
  resp = diseases2_resp2,
  suicide=diseases2_suicides2
)

# 
count<-0
colnames(data)
# 
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  #
  
  for (pollution_ in c("PM1_1year","PM2.5_1year","PM10_1year")){
    print(pollution_)
    for (blue_green_ in c('avg_forest_shrub_grassland_fraction_5year_scaled',"avg_water_wetland_fraction_5year_scaled")){
      print(blue_green_)
      data$interaction<-data[[pollution_]]*data[[blue_green_]]
      model <- gam(as.formula(paste("n ~","s(interaction,k=3)+",pollution_,"+",blue_green_,"+ns(dead_year,1)",
                                    "+as.factor(GDP_avg_quintile)",
                                    "+offset(lpopulation)",
                                    "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                    "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                    "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                    "+s(Climate_cat_code,bs='re')",
                                    sep = "")),family = quasipoisson, data = data,method="REML")
      prediction<-predict.gam(model,data=data,type="terms",se.fit=T)
      beta1<-prediction$fit[,pollution_]
      beta2<-prediction$fit[,'s(interaction)']
      se1<-prediction$se.fit[,pollution_]
      se2<-prediction$se.fit[,'s(interaction)']
      beta<-beta1+beta2
      se<-sqrt(se1^2+se2^2)
      result<-data.frame(
        blue.green.space.value=data[[blue_green_]],
        beta=beta,
        se=se
      )
      count=count+1
      print(count)
      out_filename<-paste0("C:/Users/user/Desktop/PKU_fanyi/Pro1/result2/",
                           dataset_name,"_",pollution_,"_",blue_green_,"non_linear!.csv")
      
      write.csv(result, out_filename,row.names=F) 
    }
  }
}
#模型判别
result_long_nonlinear_P=data.frame()
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 获取当前数据集
  
  data<-data %>% 
    mutate(avg_forest_fraction_1year_scaled=scale(avg_forest_fraction_1year),
           avg_forest_fraction_3year_scaled=scale(avg_forest_fraction_3year),
           avg_forest_fraction_5year_scaled=scale(avg_forest_fraction_5year),
           avg_shrub_fraction_1year_scaled=scale(avg_shrub_fraction_1year),
           avg_shrub_fraction_3year_scaled=scale(avg_shrub_fraction_3year),
           avg_shrub_fraction_5year_scaled=scale(avg_shrub_fraction_5year),
           avg_grassland_fraction_1year_scaled=scale(avg_grassland_fraction_1year),
           avg_grassland_fraction_3year_scaled=scale(avg_grassland_fraction_3year),
           avg_grassland_fraction_5year_scaled=scale(avg_grassland_fraction_5year),
           avg_forest_shrub_fraction_1year_scaled=scale(avg_forest_shrub_fraction_1year),
           avg_forest_shrub_fraction_3year_scaled=scale(avg_forest_shrub_fraction_3year),
           avg_forest_shrub_fraction_5year_scaled=scale(avg_forest_shrub_fraction_5year),
           avg_forest_grassland_fraction_1year_scaled=scale(avg_forest_grassland_fraction_1year),
           avg_forest_grassland_fraction_3year_scaled=scale(avg_forest_grassland_fraction_3year),
           avg_forest_grassland_fraction_5year_scaled=scale(avg_forest_grassland_fraction_5year),
           avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
           avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
           avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
           avg_water_fraction_1year_scaled=scale(avg_water_fraction_1year),
           avg_water_fraction_3year_scaled=scale(avg_water_fraction_3year),
           avg_water_fraction_5year_scaled=scale(avg_water_fraction_5year),
           avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
           avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
           avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  #
  data$avg_forest_fraction_1year_scaled_2group<- cut(data$avg_forest_fraction_1year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_1year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_fraction_3year_scaled_2group<- cut(data$avg_forest_fraction_3year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_3year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_fraction_5year_scaled_2group<- cut(data$avg_forest_fraction_5year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_5year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_shrub_fraction_1year_scaled_2group<- cut(data$avg_shrub_fraction_1year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_1year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_shrub_fraction_3year_scaled_2group<- cut(data$avg_shrub_fraction_3year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_3year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_shrub_fraction_5year_scaled_2group<- cut(data$avg_shrub_fraction_5year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_5year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_grassland_fraction_1year_scaled_2group<- cut(data$avg_grassland_fraction_1year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_1year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  
  data$avg_grassland_fraction_3year_scaled_2group<- cut(data$avg_grassland_fraction_3year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_3year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  
  data$avg_grassland_fraction_5year_scaled_2group<- cut(data$avg_grassland_fraction_5year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_5year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_forest_shrub_fraction_1year_scaled_2group<- cut(data$avg_forest_shrub_fraction_1year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_1year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_fraction_3year_scaled_2group<- cut(data$avg_forest_shrub_fraction_3year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_3year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_fraction_5year_scaled_2group<- cut(data$avg_forest_shrub_fraction_5year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_5year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_forest_grassland_fraction_1year_scaled_2group<- cut(data$avg_forest_grassland_fraction_1year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_1year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_grassland_fraction_3year_scaled_2group<- cut(data$avg_forest_grassland_fraction_3year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_3year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_grassland_fraction_5year_scaled_2group<- cut(data$avg_forest_grassland_fraction_5year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_5year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  #
  
  data$avg_forest_shrub_grassland_fraction_1year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_1year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_1year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_grassland_fraction_3year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_3year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_3year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_grassland_fraction_5year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_5year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_5year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  #
  data$avg_water_fraction_1year_scaled_2group<- cut(data$avg_water_fraction_1year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_1year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_fraction_3year_scaled_2group<- cut(data$avg_water_fraction_3year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_3year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_fraction_5year_scaled_2group<- cut(data$avg_water_fraction_5year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_5year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  #
  data$avg_water_wetland_fraction_1year_scaled_2group<- cut(data$avg_water_wetland_fraction_1year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_1year_scaled, 
                                                                              probs = c(0, 0.50, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_wetland_fraction_3year_scaled_2group<- cut(data$avg_water_wetland_fraction_3year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_3year_scaled, 
                                                                              probs = c(0, 0.50, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_wetland_fraction_5year_scaled_2group<- cut(data$avg_water_wetland_fraction_5year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_5year_scaled, 
                                                                              probs = c(0, 0.50, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  data$lpopulation<-log(data$total.population)
  for (pollution_ in c("PM1_1year","PM2.5_1year","PM10_1year")){
    print(pollution_)
    for (blue_green_ in c('avg_forest_shrub_grassland_fraction_5year_scaled',"avg_water_wetland_fraction_5year_scaled")){
      print(blue_green_)
      data$interaction<-data[[pollution_]]*data[[blue_green_]]
      model <- gam(as.formula(paste("n ~","s(interaction,k=3)+",pollution_,"+",blue_green_,"+ns(dead_year,1)",
                                    "+as.factor(GDP_avg_quintile)",
                                    "+offset(lpopulation)",
                                    "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                    "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                    "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                    "+s(Climate_cat_code,bs='re')",
                                    sep = "")),family = quasipoisson, data = data,method="REML")
      model2 <- gam(as.formula(paste("n ~",pollution_,"*",blue_green_,"+ns(dead_year,1)",
                                     "+as.factor(GDP_avg_quintile)",
                                     "+offset(lpopulation)",
                                     "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                     "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                     "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                     "+s(Climate_cat_code,bs='re')",
                                     sep = "")),family = quasipoisson, data = data,method="REML")
      result<-as.data.frame(anova(model2,model,test="F"))
      result$diseases<-dataset_name
      result$space<-blue_green_
      result$pollution<-pollution_
      result_long_nonlinear_P<- rbind(result_long_nonlinear_P, result)
    }
  }
}
write.csv(result_long_nonlinear_P,"E:/PKU_fanyi/Pro1/result_long/result_long_nonlinear_P.csv")
###
datasets <- list(
  total = diseases2_total, 
  cvd = diseases2_cvd2, 
  resp = diseases2_resp2,
  suicide=diseases2_suicides2
)

# 
count<-0

#
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  
  for (pollution_ in c("PM1_1year","PM2.5_1year","PM10_1year")){
    print(pollution_)
    for (blue_green_ in c('avg_forest_shrub_grassland_fraction_5year_scaled',"avg_water_wetland_fraction_5year_scaled")){
      print(blue_green_)
      data$interaction<-data[[pollution_]]*data[[blue_green_]]
      model <- gam(as.formula(paste("n ~","s(interaction,k=3)+",pollution_,"+ns(dead_year,1)",
                                    "+as.factor(GDP_avg_quintile)",
                                    "+offset(lpopulation)",
                                    "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                    "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                    "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                    "+s(Climate_cat_code,bs='re')",
                                    sep = "")),family = quasipoisson, data = data,method="REML")
      prediction<-predict.gam(model,data=data,type="terms",se.fit=T)
      beta1<-prediction$fit[,pollution_]
      beta2<-prediction$fit[,'s(interaction)']
      se1<-prediction$se.fit[,pollution_]
      se2<-prediction$se.fit[,'s(interaction)']
      beta<-beta1+beta2
      se<-sqrt(se1^2+se2^2)
      result<-data.frame(
        blue.green.space.value=data[[blue_green_]],
        beta=beta,
        se=se
      )
      count=count+1
      print(count)
      out_filename<-paste0("C:/Users/user/Desktop/PKU_fanyi/Pro1/result2/",
                           dataset_name,"_",pollution_,"_",blue_green_,"non_linear.csv")
      
      write.csv(result, out_filename,row.names=F) 
    }
  }
} 

#
result_long_nonlinear_P2=data.frame()
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  #
  data<-data %>% 
    mutate(avg_forest_fraction_1year_scaled=scale(avg_forest_fraction_1year),
           avg_forest_fraction_3year_scaled=scale(avg_forest_fraction_3year),
           avg_forest_fraction_5year_scaled=scale(avg_forest_fraction_5year),
           avg_shrub_fraction_1year_scaled=scale(avg_shrub_fraction_1year),
           avg_shrub_fraction_3year_scaled=scale(avg_shrub_fraction_3year),
           avg_shrub_fraction_5year_scaled=scale(avg_shrub_fraction_5year),
           avg_grassland_fraction_1year_scaled=scale(avg_grassland_fraction_1year),
           avg_grassland_fraction_3year_scaled=scale(avg_grassland_fraction_3year),
           avg_grassland_fraction_5year_scaled=scale(avg_grassland_fraction_5year),
           avg_forest_shrub_fraction_1year_scaled=scale(avg_forest_shrub_fraction_1year),
           avg_forest_shrub_fraction_3year_scaled=scale(avg_forest_shrub_fraction_3year),
           avg_forest_shrub_fraction_5year_scaled=scale(avg_forest_shrub_fraction_5year),
           avg_forest_grassland_fraction_1year_scaled=scale(avg_forest_grassland_fraction_1year),
           avg_forest_grassland_fraction_3year_scaled=scale(avg_forest_grassland_fraction_3year),
           avg_forest_grassland_fraction_5year_scaled=scale(avg_forest_grassland_fraction_5year),
           avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
           avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
           avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
           avg_water_fraction_1year_scaled=scale(avg_water_fraction_1year),
           avg_water_fraction_3year_scaled=scale(avg_water_fraction_3year),
           avg_water_fraction_5year_scaled=scale(avg_water_fraction_5year),
           avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
           avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
           avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  #
  data$avg_forest_fraction_1year_scaled_2group<- cut(data$avg_forest_fraction_1year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_1year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_fraction_3year_scaled_2group<- cut(data$avg_forest_fraction_3year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_3year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_fraction_5year_scaled_2group<- cut(data$avg_forest_fraction_5year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_5year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_shrub_fraction_1year_scaled_2group<- cut(data$avg_shrub_fraction_1year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_1year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_shrub_fraction_3year_scaled_2group<- cut(data$avg_shrub_fraction_3year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_3year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_shrub_fraction_5year_scaled_2group<- cut(data$avg_shrub_fraction_5year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_5year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_grassland_fraction_1year_scaled_2group<- cut(data$avg_grassland_fraction_1year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_1year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  
  data$avg_grassland_fraction_3year_scaled_2group<- cut(data$avg_grassland_fraction_3year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_3year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  
  data$avg_grassland_fraction_5year_scaled_2group<- cut(data$avg_grassland_fraction_5year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_5year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_forest_shrub_fraction_1year_scaled_2group<- cut(data$avg_forest_shrub_fraction_1year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_1year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_fraction_3year_scaled_2group<- cut(data$avg_forest_shrub_fraction_3year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_3year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_fraction_5year_scaled_2group<- cut(data$avg_forest_shrub_fraction_5year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_5year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_forest_grassland_fraction_1year_scaled_2group<- cut(data$avg_forest_grassland_fraction_1year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_1year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_grassland_fraction_3year_scaled_2group<- cut(data$avg_forest_grassland_fraction_3year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_3year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_grassland_fraction_5year_scaled_2group<- cut(data$avg_forest_grassland_fraction_5year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_5year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  #
  
  data$avg_forest_shrub_grassland_fraction_1year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_1year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_1year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_grassland_fraction_3year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_3year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_3year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_grassland_fraction_5year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_5year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_5year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  #
  data$avg_water_fraction_1year_scaled_2group<- cut(data$avg_water_fraction_1year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_1year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_fraction_3year_scaled_2group<- cut(data$avg_water_fraction_3year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_3year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_fraction_5year_scaled_2group<- cut(data$avg_water_fraction_5year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_5year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  #
  data$avg_water_wetland_fraction_1year_scaled_2group<- cut(data$avg_water_wetland_fraction_1year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_1year_scaled, 
                                                                              probs = c(0, 0.50, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_wetland_fraction_3year_scaled_2group<- cut(data$avg_water_wetland_fraction_3year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_3year_scaled, 
                                                                              probs = c(0, 0.50, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_wetland_fraction_5year_scaled_2group<- cut(data$avg_water_wetland_fraction_5year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_5year_scaled, 
                                                                              probs = c(0, 0.50, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  data$lpopulation<-log(data$total.population)
  for (pollution_ in c("PM1_1year","PM2.5_1year","PM10_1year")){
    print(pollution_)
    for (blue_green_ in c('avg_forest_shrub_grassland_fraction_5year_scaled',"avg_water_wetland_fraction_5year_scaled")){
      print(blue_green_)
      data$interaction<-data[[pollution_]]*data[[blue_green_]]
      model <- gam(as.formula(paste("n ~","s(interaction,k=3)+",pollution_,"+ns(dead_year,1)",
                                    "+as.factor(GDP_avg_quintile)",
                                    "+offset(lpopulation)",
                                    "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                    "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                    "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                    "+s(Climate_cat_code,bs='re')",
                                    sep = "")),family = quasipoisson, data = data,method="REML")
      model2 <- gam(as.formula(paste("n ~",pollution_,"+",blue_green_,"+ns(dead_year,1)",
                                     "+as.factor(GDP_avg_quintile)",
                                     "+offset(lpopulation)",
                                     "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                     "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                     "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                     "+s(Climate_cat_code,bs='re')",
                                     sep = "")),family = quasipoisson, data = data,method="REML")
      result<-as.data.frame(anova(model2,model,test="F"))
      result$diseases<-dataset_name
      result$space<-blue_green_
      result$pollution<-pollution_
      result_long_nonlinear_P2<- rbind(result_long_nonlinear_P2, result)
      
    }
  }
} 
write.csv(result_long_nonlinear_P2,"E:/PKU_fanyi/Pro1/result_long/result_long_nonlinear_P2.csv")
#####heterogeneity test####
heterogeneity <- function(beta, se) {
  
  if (length(beta) != length(se)) {
    stop("Lengths of beta and se must match.")
  }
  if (any(se <= 0)) {
    stop("Standard errors must be positive.")
  }
  df <- length(beta) - 1
  expected_beta <- sum(beta / se^2) / sum(1 / se^2)
  heterogeneity_test_statistic <- sum(((beta - expected_beta) / se)^2)
  
  p <- pchisq(heterogeneity_test_statistic, df = df, lower.tail = FALSE)
  return(list(
    "test statistic" = heterogeneity_test_statistic, 
    "p value" = p
  ))
}
#读取数据###
result_strate_cat<-read.csv("E:/deskbook/Result_China/new/result_long/result_long_cat_modefication.csv")
colnames(result_strate_cat)
# 筛选 space 列中包含 "5year" 的行
result_strate_cat_5year <- result_strate_cat %>%
  filter(str_detect(strate_factor, "5year"))
table(result_strate_cat_5year$strate_factor)
table(result_strate_cat_5year$data)
# 创建新的数据集
result_strate_cat_5year2 <- result_strate_cat_5year %>%
  mutate(
    dataset = ifelse(str_detect(data, "^total"), "total", 
                     ifelse(str_detect(data, "^cvd"), "cvd",
                            ifelse(str_detect(data, "^resp"), "resp","suicide"))), 
    space = ifelse(str_detect(strate_factor, "forest_shrub_grassland"), "green_space", 
                   "blue_space"), 
    fenzu = ifelse(fenzu==1, 1,2)                           # 提取 data 列最后一个数字
  ) %>%
  select(dataset, space,pollution, fenzu, Estimate, Std..Error)
#
for (i in seq(1, nrow(result_strate_cat_5year2), by = 2)) {
  # 提取两行 beta 和 se
  beta_pair <-result_strate_cat_5year2$Estimate[i:(i+1)]
  se_pair <- result_strate_cat_5year2$Std..Error[i:(i+1)]
  result <- heterogeneity(beta_pair, se_pair)
  result_strate_cat_5year2$p_value[i:(i+1)] <- result$p
}
write.csv(result_strate_cat_5year2,"E:/deskbook/Result_China/figure_raw2/heterogeneity_test_result_strate_cat_5year2.csv",row.names = F)
#####分层作用#####
diseases2_total_sex1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_total_sex1.rds")
diseases2_total_sex2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_total_sex2.rds")
diseases2_total_age1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_total_age1.rds")
diseases2_total_age2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_total_age2.rds")
diseases2_total_education1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_total_education1.rds")
diseases2_total_education2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_total_education2.rds")
diseases2_total_marital1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_total_marital1.rds")
diseases2_total_marital2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_total_marital2.rds")
diseases2_cvd2_sex1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_sex1.rds")
diseases2_cvd2_sex2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_sex2.rds")
diseases2_cvd2_age1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_age1.rds")
diseases2_cvd2_age2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_age2.rds")
diseases2_cvd2_education1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_education1.rds")
diseases2_cvd2_education2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_education2.rds")
diseases2_cvd2_marital1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_marital1.rds")
diseases2_cvd2_marital2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_marital2.rds")
diseases2_resp2_sex1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_sex1.rds")
diseases2_resp2_sex2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_sex2.rds")
diseases2_resp2_age1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_age1.rds")
diseases2_resp2_age2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_age2.rds")
diseases2_resp2_education1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_education1.rds")
diseases2_resp2_education2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_education2.rds")
diseases2_resp2_marital1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_marital1.rds")
diseases2_resp2_marital2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_marital2.rds")
diseases2_suicides2_sex1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_sex1.rds")
diseases2_suicides2_sex2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_sex2.rds")
diseases2_suicides2_age1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_age1.rds")
diseases2_suicides2_age2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_age2.rds")
diseases2_suicides2_education1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_education1.rds")
diseases2_suicides2_education2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_education2.rds")
diseases2_suicides2_marital1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_marital1.rds")
diseases2_suicides2_marital2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_marital2.rds")
diseases2_total_GDP1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_total_GDP1.rds")
diseases2_total_GDP2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_total_GDP2.rds")
diseases2_total_urban_rate1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_total_urban_rate1.rds")
diseases2_total_urban_rate2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_total_urban_rate2.rds")
diseases2_cvd2_GDP1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_GDP1.rds")
diseases2_cvd2_GDP2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_GDP2.rds")
diseases2_cvd2_urban_rate1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_urban_rate1.rds")
diseases2_cvd2_urban_rate2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_urban_rate2.rds")

diseases2_resp2_GDP1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_GDP1.rds")
diseases2_resp2_GDP2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_GDP2.rds")
diseases2_resp2_urban_rate1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_urban_rate1.rds")
diseases2_resp2_urban_rate2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_urban_rate2.rds")
diseases2_suicides2_GDP1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_GDP1.rds")
diseases2_suicides2_GDP2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_GDP2.rds")
diseases2_suicides2_urban_rate1<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_urban_rate1.rds")
diseases2_suicides2_urban_rate2<-read_rds("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_urban_rate2.rds")
###
datasets <- list(
  total_sex1 = diseases2_total_sex1, 
  total_sex2= diseases2_total_sex2, 
  total_age1 = diseases2_total_age1,
  total_age2=diseases2_total_age2,
  total_education1 = diseases2_total_education1,
  total_education2=diseases2_total_education2,
  total_marital1 = diseases2_total_marital1,
  total_marital2=diseases2_total_marital2,
  cvd2_sex1 = diseases2_cvd2_sex1, 
  cvd2_sex2= diseases2_cvd2_sex2, 
  cvd2_age1 = diseases2_cvd2_age1,
  cvd2_age2=diseases2_cvd2_age2,
  cvd2_education1 = diseases2_cvd2_education1,
  cvd2_education2=diseases2_cvd2_education2,
  cvd2_marital1 = diseases2_cvd2_marital1,
  cvd2_marital2=diseases2_cvd2_marital2,
  resp2_sex1 = diseases2_resp2_sex1, 
  resp2_sex2= diseases2_resp2_sex2, 
  resp2_age1 = diseases2_resp2_age1,
  resp2_age2=diseases2_resp2_age2,
  resp2_education1 = diseases2_resp2_education1,
  resp2_education2=diseases2_resp2_education2,
  resp2_marital1 = diseases2_resp2_marital1,
  resp2_marital2=diseases2_resp2_marital2,
  suicides2_sex1 = diseases2_suicides2_sex1, 
  suicides2_sex2= diseases2_suicides2_sex2, 
  suicides2_age1 = diseases2_suicides2_age1,
  suicides2_age2=diseases2_suicides2_age2,
  suicides2_education1 = diseases2_suicides2_education1,
  suicides2_education2=diseases2_suicides2_education2,
  suicides2_marital1 = diseases2_suicides2_marital1,
  suicides2_marital2=diseases2_suicides2_marital2,
  total_GDP1 = diseases2_total_GDP1, 
  total_GDP2= diseases2_total_GDP2, 
  total_urban_rate1 = diseases2_total_urban_rate1,
  total_urban_rate2=diseases2_total_urban_rate2,
  cvd2_GDP1 = diseases2_cvd2_GDP1, 
  cvd2_GDP2= diseases2_cvd2_GDP2, 
  cvd2_urban_rate1 = diseases2_cvd2_urban_rate1,
  cvd2_urban_rate2=diseases2_cvd2_urban_rate2,
  resp2_GDP1 = diseases2_resp2_GDP1, 
  resp2_GDP2= diseases2_resp2_GDP2, 
  resp2_urban_rate1 = diseases2_resp2_urban_rate1,
  resp2_urban_rate2=diseases2_resp2_urban_rate2,
  suicide2_GDP1 = diseases2_suicides2_GDP1, 
  suicide2_GDP2=diseases2_suicides2_GDP1, 
  suicide2_urban_rate1 = diseases2_suicides2_urban_rate1,
  suicide2_urban_rate2=diseases2_suicides2_urban_rate2
  
)

# 
count<-0
# 
result_strate_continous=data.frame()
result_strate_long_continous_modefication=data.frame()
result_strate_cat2=data.frame()
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 
  data<-data %>% 
    mutate(avg_forest_fraction_1year_scaled=scale(avg_forest_fraction_1year),
           avg_forest_fraction_3year_scaled=scale(avg_forest_fraction_3year),
           avg_forest_fraction_5year_scaled=scale(avg_forest_fraction_5year),
           avg_shrub_fraction_1year_scaled=scale(avg_shrub_fraction_1year),
           avg_shrub_fraction_3year_scaled=scale(avg_shrub_fraction_3year),
           avg_shrub_fraction_5year_scaled=scale(avg_shrub_fraction_5year),
           avg_grassland_fraction_1year_scaled=scale(avg_grassland_fraction_1year),
           avg_grassland_fraction_3year_scaled=scale(avg_grassland_fraction_3year),
           avg_grassland_fraction_5year_scaled=scale(avg_grassland_fraction_5year),
           avg_forest_shrub_fraction_1year_scaled=scale(avg_forest_shrub_fraction_1year),
           avg_forest_shrub_fraction_3year_scaled=scale(avg_forest_shrub_fraction_3year),
           avg_forest_shrub_fraction_5year_scaled=scale(avg_forest_shrub_fraction_5year),
           avg_forest_grassland_fraction_1year_scaled=scale(avg_forest_grassland_fraction_1year),
           avg_forest_grassland_fraction_3year_scaled=scale(avg_forest_grassland_fraction_3year),
           avg_forest_grassland_fraction_5year_scaled=scale(avg_forest_grassland_fraction_5year),
           avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
           avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
           avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
           avg_water_fraction_1year_scaled=scale(avg_water_fraction_1year),
           avg_water_fraction_3year_scaled=scale(avg_water_fraction_3year),
           avg_water_fraction_5year_scaled=scale(avg_water_fraction_5year),
           avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
           avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
           avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  #
  data$avg_forest_fraction_1year_scaled_2group<- cut(data$avg_forest_fraction_1year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_1year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_fraction_3year_scaled_2group<- cut(data$avg_forest_fraction_3year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_3year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_fraction_5year_scaled_2group<- cut(data$avg_forest_fraction_5year_scaled, 
                                                     breaks = quantile(data$avg_forest_fraction_5year_scaled, 
                                                                       probs = c(0, 0.50, 1)), 
                                                     include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_shrub_fraction_1year_scaled_2group<- cut(data$avg_shrub_fraction_1year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_1year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_shrub_fraction_3year_scaled_2group<- cut(data$avg_shrub_fraction_3year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_3year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_shrub_fraction_5year_scaled_2group<- cut(data$avg_shrub_fraction_5year_scaled, 
                                                    breaks = quantile(data$avg_shrub_fraction_5year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_grassland_fraction_1year_scaled_2group<- cut(data$avg_grassland_fraction_1year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_1year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  
  data$avg_grassland_fraction_3year_scaled_2group<- cut(data$avg_grassland_fraction_3year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_3year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  
  data$avg_grassland_fraction_5year_scaled_2group<- cut(data$avg_grassland_fraction_5year_scaled, 
                                                        breaks = quantile(data$avg_grassland_fraction_5year_scaled, 
                                                                          probs = c(0, 0.50, 1)), 
                                                        include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_forest_shrub_fraction_1year_scaled_2group<- cut(data$avg_forest_shrub_fraction_1year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_1year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_fraction_3year_scaled_2group<- cut(data$avg_forest_shrub_fraction_3year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_3year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_fraction_5year_scaled_2group<- cut(data$avg_forest_shrub_fraction_5year_scaled, 
                                                           breaks = quantile(data$avg_forest_shrub_fraction_5year_scaled, 
                                                                             probs = c(0, 0.50, 1)), 
                                                           include.lowest = TRUE, labels = FALSE)
  #
  
  data$avg_forest_grassland_fraction_1year_scaled_2group<- cut(data$avg_forest_grassland_fraction_1year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_1year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_grassland_fraction_3year_scaled_2group<- cut(data$avg_forest_grassland_fraction_3year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_3year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_grassland_fraction_5year_scaled_2group<- cut(data$avg_forest_grassland_fraction_5year_scaled, 
                                                               breaks = quantile(data$avg_forest_grassland_fraction_5year_scaled, 
                                                                                 probs = c(0, 0.50, 1)), 
                                                               include.lowest = TRUE, labels = FALSE)
  
  #
  
  data$avg_forest_shrub_grassland_fraction_1year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_1year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_1year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_grassland_fraction_3year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_3year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_3year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  
  data$avg_forest_shrub_grassland_fraction_5year_scaled_2group<- cut(data$avg_forest_shrub_grassland_fraction_5year_scaled, 
                                                                     breaks = quantile(data$avg_forest_shrub_grassland_fraction_5year_scaled, 
                                                                                       probs = c(0, 0.50, 1)), 
                                                                     include.lowest = TRUE, labels = FALSE)
  #
  data$avg_water_fraction_1year_scaled_2group<- cut(data$avg_water_fraction_1year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_1year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_fraction_3year_scaled_2group<- cut(data$avg_water_fraction_3year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_3year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_fraction_5year_scaled_2group<- cut(data$avg_water_fraction_5year_scaled, 
                                                    breaks = quantile(data$avg_water_fraction_5year_scaled, 
                                                                      probs = c(0, 0.50, 1)), 
                                                    include.lowest = TRUE, labels = FALSE)
  
  #
  data$avg_water_wetland_fraction_1year_scaled_2group<- cut(data$avg_water_wetland_fraction_1year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_1year_scaled, 
                                                                              probs = c(0, 0.50, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_wetland_fraction_3year_scaled_2group<- cut(data$avg_water_wetland_fraction_3year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_3year_scaled, 
                                                                              probs = c(0, 0.50, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  
  data$avg_water_wetland_fraction_5year_scaled_2group<- cut(data$avg_water_wetland_fraction_5year_scaled, 
                                                            breaks = quantile(data$avg_water_wetland_fraction_5year_scaled, 
                                                                              probs = c(0, 0.50, 1)), 
                                                            include.lowest = TRUE, labels = FALSE)
  data$lpopulation<-log(data$total.population)
  #
  #continuous####
  for(space in c("avg_forest_shrub_grassland_fraction_1year_scaled","avg_forest_shrub_grassland_fraction_3year_scaled","avg_forest_shrub_grassland_fraction_5year_scaled",
                 "avg_water_wetland_fraction_1year_scaled","avg_water_wetland_fraction_3year_scaled","avg_water_wetland_fraction_5year_scaled")){
    formula <- as.formula(paste("n ~",space,"+ns(dead_year,1)",
                                "+as.factor(GDP_avg_quintile)",
                                "+offset(lpopulation)",
                                "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                "+s(Climate_cat_code,bs='re')",
                                sep = ""))
    model<-gam(formula, family = quasipoisson, data = data,na.action = na.omit,method = "REML")
    result<-as.data.frame(summary(model)$p.table)[space, ]
    beta=as.numeric(result$Estimate)
    se=as.numeric(result$`Std. Error`)
    result$data<-dataset_name
    result$family<-model[["family"]][["family"]]
    result$space<-space
    
    result$gcv<-summary(model)[["sp.criterion"]][["REML"]]
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    result_strate_continous<- rbind(result_strate_continous, result)
    
  }
  #cat####
  for(space in c("avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                 "avg_water_wetland_fraction_5year_scaled_2group")){
    formula <- as.formula(paste("n ~as.factor(",space,")+ns(dead_year,1)",
                                "+as.factor(GDP_avg_quintile)",
                                "+offset(lpopulation)",
                                "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                "+s(Climate_cat_code,bs='re')",
                                sep = ""))
    model<-gam(formula, family = quasipoisson, data = data,na.action = na.omit,method = "REML")
    result<-as.data.frame(summary(model)$p.table)[2, ]
    beta=as.numeric(result$Estimate)
    se=as.numeric(result$`Std. Error`)
    result$data<-dataset_name
    result$family<-model[["family"]][["family"]]
    result$space<-space
    
    result$gcv<-summary(model)[["sp.criterion"]][["REML"]]
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    result_strate_cat<- rbind(result_strate_cat, result)
    
    
  }
  #####continuous_forest####
  
  for(pollution_ in c("PM1_1year","PM2.5_1year","PM10_1year")){
    for (blue_green_ in c("avg_forest_shrub_grassland_fraction_1year_scaled","avg_forest_shrub_grassland_fraction_3year_scaled","avg_forest_shrub_grassland_fraction_5year_scaled"
    )){
      change_values<-seq(0,0.90,by=0.05) 
      model<-gam(as.formula(paste("n ~",pollution_,"*",blue_green_,"+ns(dead_year,1)",
                                  "+as.factor(GDP_avg_quintile)",
                                  "+offset(lpopulation)",
                                  "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                  "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                  "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                  "+s(Climate_cat_code,bs='re')",
                                  sep = "")),family = quasipoisson, data = data,na.action = na.omit,method = "REML")
      beta1_boot<-summary(model)$p.table[pollution_,"Estimate"]
      beta2_boot<-summary(model)$p.table[paste0(pollution_,":",blue_green_),"Estimate"]
      se1_boot<-summary(model)$p.table[pollution_,"Std. Error"]
      se2_boot<-summary(model)$p.table[paste0(pollution_,":",blue_green_),"Std. Error"]
      se<-sqrt(se1_boot^2+se2_boot^2)
      result_df<-data.frame(
        data=dataset_name,
        blue_green=blue_green_,
        pollution=pollution_,
        change_value=change_values,
        beta=numeric(length(change_values)),
        se=numeric(length(change_values))
      )
      for (i in seq_along(change_values)){
        change_value<-change_values[i]
        result_df$beta[i]<-beta1_boot+beta2_boot*change_value
        result_df$se[i]<-se
      }
      result_strate_long_continous_modefication<-rbind(result_strate_long_continous_modefication,result_df)
      
    }
  }
  #####continuous_water####
  
  for(pollution_ in c("PM1_1year","PM2.5_1year","PM10_1year")){
    for (blue_green_ in c(
      "avg_water_wetland_fraction_1year_scaled","avg_water_wetland_fraction_3year_scaled","avg_water_wetland_fraction_5year_scaled")){
      change_values<-seq(0,0.25,by=0.01) 
      model<-gam(as.formula(paste("n ~",pollution_,"*",blue_green_,"+ns(dead_year,1)",
                                  "+as.factor(GDP_avg_quintile)",
                                  "+offset(lpopulation)",
                                  "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                  "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                  "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                  "+s(Climate_cat_code,bs='re')",
                                  sep = "")),family = quasipoisson, data = data,na.action = na.omit,method = "REML")
      beta1_boot<-summary(model)$p.table[pollution_,"Estimate"]
      beta2_boot<-summary(model)$p.table[paste0(pollution_,":",blue_green_),"Estimate"]
      se1_boot<-summary(model)$p.table[pollution_,"Std. Error"]
      se2_boot<-summary(model)$p.table[paste0(pollution_,":",blue_green_),"Std. Error"]
      se<-sqrt(se1_boot^2+se2_boot^2)
      result_df<-data.frame(
        data=dataset_name,
        blue_green=blue_green_,
        pollution=pollution_,
        change_value=change_values,
        beta=numeric(length(change_values)),
        se=numeric(length(change_values))
      )
      for (i in seq_along(change_values)){
        change_value<-change_values[i]
        result_df$beta[i]<-beta1_boot+beta2_boot*change_value
        result_df$se[i]<-se
      }
      result_strate_long_continous_modefication<-rbind(result_strate_long_continous_modefication,result_df)
      
    }
  }
  count=count+1
  print(count)
}
write.csv(result_strate_continous,"E:/PKU_fanyi/Pro1/result_long/result_strate_continous.csv")
write.csv(result_strate_long_continous_modefication,"E:/PKU_fanyi/Pro1/result_long/result_strate_long_continous_modefication.csv")
write.csv(result_strate_cat2,"E:/PKU_fanyi/Pro1/result_long/result_strate_cat2.csv")