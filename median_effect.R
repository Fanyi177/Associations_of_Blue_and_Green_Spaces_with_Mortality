######中介作用#####
rm(list=ls())
diseases2_total<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_city.rds")
diseases2_cvd2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_city.rds")
diseases2_suicides2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_city.rds")
diseases2_resp2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_city.rds")
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

###
datasets <- list(
  total=diseases2_total,
  
  cvd2=diseases2_cvd2,
  
  resp=diseases2_resp2,
  
  suicide=diseases2_suicides2
  
  
)
#function
set.seed(9999)
n_boot<-100
bootstrap_indirect_effect<-function(data,blue_green,pollution_tem){
  boot_data<-data[sample(1:nrow(data),nrow(data),replace = T),]
  mediation_model<-gam(as.formula(paste(pollution_tem,"~",blue_green,"+ns(dead_year,1)",
                                        "+as.factor(GDP_avg_quintile)",
                                        "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                        "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                        "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                        "+s(Climate_cat_code,bs='re')",
                                        sep = "")),family = quasipoisson, data = boot_data,na.action = na.omit,method = "REML")
  beta_a<-mediation_model[["coefficients"]][[blue_green]]
  outcome_model<-gam(as.formula(paste("n~",blue_green,"+",pollution_tem,"+ns(dead_year,1)",
                                      "+as.factor(GDP_avg_quintile)",
                                      "+offset(lpopulation)",
                                      "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                      "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                      "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                      "+s(Climate_cat_code,bs='re')",
                                      sep = "")),family = quasipoisson, data = boot_data,na.action = na.omit,method = "REML")
  beta_b<-outcome_model[["coefficients"]][[pollution_tem]]
  beta_c2<-outcome_model[["coefficients"]][[blue_green]]
  indirect_effect<-beta_a*beta_b
  direct_effect<-beta_c2
  total_effect<-indirect_effect+direct_effect
  return(c(indirect_effect=indirect_effect,direct_effect=direct_effect,total_effect=total_effect))
}
# 存储所有结果的列表
count<-0
# 遍历数据集
result_mediation=data.frame()
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 获取当前数据集
  data<-data %>% 
    mutate(
      avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
      avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
      avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
      
      avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
      avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
      avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  data$lpopulation<-log(data$total.population)
  #
  for(blue_green_ in c("avg_forest_shrub_grassland_fraction_5year_scaled",
                       "avg_water_wetland_fraction_5year_scaled")){
    for (pollution_tem_ in c(
      "heat_temperature","cold_temperature","extreme_temperature")){
      boot_results<-replicate(n_boot,bootstrap_indirect_effect(data,blue_green_,pollution_tem_))
      boot_results<-t(boot_results)
      effect_means<-colMeans(boot_results)
      conf_interval<-apply(boot_results,2,quantile,probs=c(0.025,0.975))
      
      indirect_effect_data<-data.frame(
        data=dataset_name,
        blue_green=blue_green_,
        pollution_tem=pollution_tem_,
        mean=effect_means,
        lower=conf_interval[1,],
        upper=conf_interval[2,]
      )
      result_mediation<-rbind(result_mediation,indirect_effect_data)
      count<-count+1
      print(count)
    }
  }
  
}
write.csv(result_mediation,"E:/PKU_fanyi/Pro1/result_long/result_mediation2.csv")
#分层中介####
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
  suicides2_marital2=diseases2_suicides2_marital2
  
)
#function

#####并行运算
n_cores<-parallel::detectCores()-17
cl<-makeCluster(n_cores)
registerDoParallel(cl)
result_strate_mediation=data.frame()
task_count<-0
total_tasks<-length(datasets)*2*6
results<-foreach(dataset_name=names(datasets),.combine = rbind,.packages = c("stats","tidyverse","mgcv","splines","doParallel")) %dopar%{
  data <- datasets[[dataset_name]]  # 获取当前数据集
  data<-data %>% left_join(diseases2_total_short2,by=c("dead_year"="year","city"="city"))
  data<-data %>% 
    mutate(
      avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
      avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
      avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
      
      avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
      avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
      avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  data$lpopulation<-log(data$total.population)
  foreach(blue_green_=c("avg_forest_shrub_grassland_fraction_5year_scaled",
                        "avg_water_wetland_fraction_5year_scaled"),.combine=rbind) %:%
    foreach(pollution_tem_=c("PM1_1year","PM2.5_1year","PM10_1year",
                             "heat_temperature","cold_temperature","extreme_temperature"),.combine=rbind) %dopar%{
                               task_count<-task_count+1
                               message(paste0(
                                 "[",Sys.time(),"]",
                                 "Processing dataset:",dataset_name,",",
                                 "Blue_green:",blue_green_,",",
                                 "pollution:",pollution_tem_,",",
                                 "Task:",task_count,"/",total_tasks
                               ))
                               
                               boot_results<-replicate(n_boot,bootstrap_indirect_effect(data,blue_green_,pollution_tem_))
                               boot_results<-t(boot_results)
                               effect_means<-colMeans(boot_results)
                               conf_interval<-apply(boot_results,2,quantile,probs=c(0.025,0.975))
                               
                               data.frame(
                                 data=dataset_name,
                                 blue_green=blue_green_,
                                 pollution_tem=pollution_tem_,
                                 mean=effect_means,
                                 lower=conf_interval[1,],
                                 upper=conf_interval[2,]
                               )                             
                             }
}





set.seed(9999)
n_boot<-100
bootstrap_indirect_effect<-function(data,blue_green,pollution_tem){
  boot_data<-data[sample(1:nrow(data),nrow(data),replace = T),]
  mediation_model<-gam(as.formula(paste(pollution_tem,"~",blue_green,"+ns(dead_year,1)",
                                        "+as.factor(GDP_avg_quintile)",
                                        
                                        "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                        "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                        "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                        "+s(Climate_cat_code,bs='re')",
                                        sep = "")),family = quasipoisson, data = boot_data,na.action = na.omit,method = "REML")
  beta_a<-mediation_model[["coefficients"]][[blue_green]]
  outcome_model<-gam(as.formula(paste("n~",blue_green,"+",pollution_tem,"+ns(dead_year,1)",
                                      "+as.factor(GDP_avg_quintile)",
                                      "+offset(lpopulation)",
                                      "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                                      "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                                      "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                                      "+s(Climate_cat_code,bs='re')",
                                      sep = "")),family = quasipoisson, data = boot_data,na.action = na.omit,method = "REML")
  beta_b<-outcome_model[["coefficients"]][[pollution_tem]]
  beta_c2<-outcome_model[["coefficients"]][[blue_green]]
  indirect_effect<-beta_a*beta_b
  direct_effect<-beta_c2
  total_effect<-indirect_effect+direct_effect
  return(c(indirect_effect=indirect_effect,direct_effect=direct_effect,total_effect=total_effect))
}
