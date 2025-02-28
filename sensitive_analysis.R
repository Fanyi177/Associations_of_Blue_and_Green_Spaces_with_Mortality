


#sensitive analysis####
rm(list=ls())
#####读取数据####
diseases2_total<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_city.rds")
diseases2_cvd2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_city.rds")
diseases2_suicides2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_city.rds")
diseases2_resp2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_city.rds")
#####1.额外调整污染物看结果####
#做蓝绿空间直接关联
datasets <- list(
  total= diseases2_total, 
  cvd= diseases2_cvd2, 
  resp= diseases2_resp2,
  suicide=diseases2_suicides2
)

# 存储所有结果的列表
count<-0
# 遍历数据集
sen1_result_continous=data.frame()
sen1_result_cat=data.frame()
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
  #continuous
  
  for(space in c("avg_forest_shrub_grassland_fraction_1year_scaled","avg_forest_shrub_grassland_fraction_3year_scaled","avg_forest_shrub_grassland_fraction_5year_scaled",
                 "avg_water_wetland_fraction_1year_scaled","avg_water_wetland_fraction_3year_scaled","avg_water_wetland_fraction_5year_scaled")){
    
    
    formula <- as.formula(paste("n ~",space,"+ns(dead_year,1)",
                                "+as.factor(GDP_avg_quintile)",
                                "+offset(lpopulation)","+PM10_1year",
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
    
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    sen1_result_continous<- rbind(sen1_result_continous, result)
    
    
  }
  ###cat
  for(space in c("avg_forest_shrub_grassland_fraction_1year_scaled_2group","avg_forest_shrub_grassland_fraction_3year_scaled_2group","avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                 "avg_water_wetland_fraction_1year_scaled_2group","avg_water_wetland_fraction_3year_scaled_2group","avg_water_wetland_fraction_5year_scaled_2group")){
    
    formula <- as.formula(paste("n ~as.factor(",space,")+ns(dead_year,1)",
                                "+as.factor(GDP_avg_quintile)",
                                "+offset(lpopulation)","+PM10_1year",
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
    
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    sen1_result_cat<- rbind(sen1_result_cat, result)
  }
  count=count+1
  print(count)
}
write.csv(sen1_result_continous,"E:/PKU_fanyi/Pro1/result_long/sen1_result_blue_green_direct_continous.csv")
write.csv(sen1_result_cat,"E:/PKU_fanyi/Pro1/result_long/sen1_result_blue_green_direct_cat.csv")
#####2.剔除死亡率在前5%和后5%的数据####

# 存储所有结果的列表
count<-0
# 遍历数据集
sen2_result_continous=data.frame()
sen2_result_cat=data.frame()
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
  print(nrow(data))
  #
  p5th_percentile <- quantile(data$mortality, probs = c(0.05, 0.95))[1]
  p95th_percentile <- quantile(data$mortality, probs = c(0.05, 0.95))[2]
  data<-data %>% setDT()
  data<-data[data$mortality >= p5th_percentile & data$mortality<= p95th_percentile, ]
  print(nrow(data))
  #
  #continuous
  
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
    result$diseases<-dataset_name
    
    result$family<-model[["family"]][["family"]]
    result$space<-space
    
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    sen2_result_continous<- rbind(sen2_result_continous, result)
    
    
  }
  ###cat
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
    
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    sen2_result_cat<- rbind(sen2_result_cat, result)
  }
  count=count+1
  print(count)
}
write.csv(sen2_result_continous,"E:/PKU_fanyi/Pro1/result_long/sen2_result_blue_green_direct_continous.csv")
write.csv(sen2_result_cat,"E:/PKU_fanyi/Pro1/result_long/sen2_result_blue_green_direct_cat.csv")

###4.使用年平均替代夏季冬季####
# 存储所有结果的列表
count<-0
# 遍历数据集
sen4_result_continous=data.frame()
sen4_result_cat=data.frame()
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
  print(nrow(data))
  
  #continuous
  
  for(space in c("avg_forest_shrub_grassland_fraction_1year_scaled","avg_forest_shrub_grassland_fraction_3year_scaled","avg_forest_shrub_grassland_fraction_5year_scaled",
                 "avg_water_wetland_fraction_1year_scaled","avg_water_wetland_fraction_3year_scaled","avg_water_wetland_fraction_5year_scaled")){
    
    
    formula <- as.formula(paste("n ~",space,"+ns(dead_year,1)",
                                "+as.factor(GDP_avg_quintile)",
                                "+offset(lpopulation)",
                                "+t2m_mean_year","+t2m_sd_year",
                                "+rh_mean_year","+rh_sd_year",
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
    
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    sen4_result_continous<- rbind(sen4_result_continous, result)
    
    
  }
  ###cat
  for(space in c("avg_forest_shrub_grassland_fraction_1year_scaled_2group","avg_forest_shrub_grassland_fraction_3year_scaled_2group","avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                 "avg_water_wetland_fraction_1year_scaled_2group","avg_water_wetland_fraction_3year_scaled_2group","avg_water_wetland_fraction_5year_scaled_2group")){
    
    formula <- as.formula(paste("n ~as.factor(",space,")+ns(dead_year,1)",
                                "+as.factor(GDP_avg_quintile)",
                                "+offset(lpopulation)",
                                "+t2m_mean_year","+t2m_sd_year",
                                "+rh_mean_year","+rh_sd_year",
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
    
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    sen4_result_cat<- rbind(sen4_result_cat, result)
  }
  count=count+1
  print(count)
}
write.csv(sen4_result_continous,"E:/PKU_fanyi/Pro1/result_long/sen4_result_blue_green_direct_continous.csv")
write.csv(sen4_result_cat,"E:/PKU_fanyi/Pro1/result_long/sen4_result_blue_green_direct_cat.csv")
#### 5.排除死亡率波动大的城市######
# 存储所有结果的列表
count<-0
# 遍历数据集
sen5_result_continous=data.frame()
sen5_result_cat=data.frame()
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
  print(nrow(data))
  #
  data<-data %>% 
    arrange(city_code,dead_year) %>% 
    mutate(
      rate_change=(mortality-lag(mortality))/lag(mortality)
    ) %>% ungroup()
  
  
  cities_to_exclude<-data %>% 
    filter(abs(rate_change)>2) %>% 
    pull(city_code) %>% 
    unique()
  data<-data %>% 
    filter(!city_code %in% cities_to_exclude)
  print(nrow(data))
  #continuous
  
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
    result$diseases<-dataset_name
    
    result$family<-model[["family"]][["family"]]
    result$space<-space
    
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    sen5_result_continous<- rbind(sen5_result_continous, result)
    
    
  }
  ###cat
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
    
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    sen5_result_cat<- rbind(sen5_result_cat, result)
  }
  count=count+1
  print(count)
}
write.csv(sen5_result_continous,"E:/PKU_fanyi/Pro1/result_long/sen5_result_blue_green_direct_continous.csv")
write.csv(sen5_result_cat,"E:/PKU_fanyi/Pro1/result_long/sen5_result_blue_green_direct_cat.csv")

