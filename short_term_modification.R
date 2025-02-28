##############################################################读取下面的数据##############
########################################################################################
#######################################################################################
#####################################################################################
#读取数据####
diseases2_total_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_short.rds")
diseases2_cvd2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_short.rds")
diseases2_suicides2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_short.rds")
diseases2_resp2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_short.rds")
diseases2_total_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_short_heat.rds")
diseases2_cvd2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_short_heat.rds")
diseases2_suicides2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_short_heat.rds")
diseases2_resp2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_short_heat.rds")
diseases2_total_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_short_cold.rds")
diseases2_cvd2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_short_cold.rds")
diseases2_suicides2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_short_cold.rds")
diseases2_resp2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_short_cold.rds")
#####短期修饰作用：PM####
datasets <- list(
  total_short= diseases2_total_short, 
  cvd_short= diseases2_cvd2_short, 
  resp_short= diseases2_resp2_short,
  suicide_short=diseases2_suicides2_short
)
colnames(diseases2_total_short)
# 存储所有结果的列表
count<-0
result_short_pollution=data.frame()
result_short_pollution_2=data.frame()
result_short_continous_modefication_pollution=data.frame()
result_short_cat_modefication_pollution=data.frame()
result_short_cat_interaction_pollution=data.frame()
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 获取当前数据集
  data<-data %>% 
    mutate(avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
           avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
           avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
           
           avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
           avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
           avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  #
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
  #####污染物与结局的直接关系####
  for(pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")){
    formula <-as.formula(paste("n ~",pollution_,"+as.factor(dow)",
                               "+as.factor(season)",
                               "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                               "+as.factor(Climate_cat_code)",
                               sep = ""))
    model<-gam(formula, family = quasipoisson, data = data,na.action = na.omit)
    print(summary(model))
    result<-as.data.frame(summary(model)$p.table)[pollution_, ]
    beta=as.numeric(result$Estimate)
    se=as.numeric(result$`Std. Error`)
    result$data<-dataset_name
    result$pollution<-pollution_
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    result_short_pollution<- rbind(result_short_pollution, result)
    
    
  }
  for(pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")){
    for (blue_green_ in c("avg_forest_shrub_grassland_fraction_5year_scaled",
                          "avg_water_wetland_fraction_5year_scaled")){
      formula <-as.formula(paste("n ~",pollution_,"+",blue_green_,"+as.factor(dow)",
                                 "+as.factor(season)",
                                 "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                 "+as.factor(Climate_cat_code)",
                                 sep = ""))
      model<-gam(formula, family = quasipoisson, data = data,na.action = na.omit)
      print(summary(model))
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
      result_short_pollution_2<- rbind(result_short_pollution_2, result)
      
    }
  }
  #####continuous_forest####
  
  for(pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")){
    for (blue_green_ in c("avg_forest_shrub_grassland_fraction_1year_scaled","avg_forest_shrub_grassland_fraction_3year_scaled","avg_forest_shrub_grassland_fraction_5year_scaled"
    )){
      change_values<-seq(0,0.90,by=0.05) 
      model<-gam(as.formula(paste("n ~",pollution_,"*",blue_green_,"+as.factor(dow)",
                                  "+as.factor(season)",
                                  "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                  "+as.factor(Climate_cat_code)",
                                  sep = "")),family = quasipoisson, data = data,na.action = na.omit)
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
      result_short_continous_modefication_pollution<-rbind(result_short_continous_modefication_pollution,result_df)
      
    }
  }
  #####continuous_water####
  
  for(pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")){
    for (blue_green_ in c(
      "avg_water_wetland_fraction_1year_scaled","avg_water_wetland_fraction_3year_scaled","avg_water_wetland_fraction_5year_scaled")){
      change_values<-seq(0,0.25,by=0.01) 
      model<-gam(as.formula(paste("n ~",pollution_,"*",blue_green_,"+as.factor(dow)",
                                  "+as.factor(season)",
                                  "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                  "+as.factor(Climate_cat_code)",
                                  sep = "")),family = quasipoisson, data = data,na.action = na.omit)
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
      result_short_continous_modefication_pollution<-rbind(result_short_continous_modefication_pollution,result_df)
      
    }
  }
  ####cat####
  for (blue_green_ in c("avg_forest_shrub_grassland_fraction_1year_scaled_2group","avg_forest_shrub_grassland_fraction_3year_scaled_2group","avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                        "avg_water_wetland_fraction_1year_scaled_2group","avg_water_wetland_fraction_3year_scaled_2group","avg_water_wetland_fraction_5year_scaled_2group")){
    for (fenzu in 1:2) {
      data1 <- subset(data, data[[blue_green_]] == fenzu)
      for (pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")) {
        formula <- as.formula(paste("n ~",pollution_,"+as.factor(dow)",
                                    "+as.factor(season)",
                                    "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                    "+as.factor(Climate_cat_code)",
                                    sep = ""))
        
        model<-gam(formula, family = quasipoisson, data = data1, na.action = na.omit)
        result<-as.data.frame(summary(model)$p.table)[pollution_, ]
        beta=as.numeric(result$Estimate)
        se=as.numeric(result$`Std. Error`)
        result$data<-dataset_name
        result$blue_green <- blue_green_
        result$fenzu<-fenzu
        result$pollution<-pollution_
        result$PC=round((exp(beta)-1)*100,digits = 2)
        result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
        result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
        result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
        result_short_cat_modefication_pollution<- rbind(result_short_cat_modefication_pollution, result)
        
      }
    }
  }
  
  ####交互作用####
  for (blue_green_ in c("avg_forest_shrub_grassland_fraction_1year_scaled_2group","avg_forest_shrub_grassland_fraction_3year_scaled_2group","avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                        "avg_water_wetland_fraction_1year_scaled_2group","avg_water_wetland_fraction_3year_scaled_2group","avg_water_wetland_fraction_5year_scaled_2group")) {
    for (pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")){
      formula <- as.formula(paste("n ~",pollution_,"*",blue_green_,"+as.factor(dow)",
                                  "+as.factor(season)",
                                  "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                  "+as.factor(Climate_cat_code)",
                                  sep = ""))
      model<-gam(formula, family = quasipoisson, data = data1, na.action = na.omit)
      result<-as.data.frame(summary(model)$p.table)[paste0(pollution_,":",blue_green_), ]
      beta=as.numeric(result$Estimate)
      se=as.numeric(result$`Std. Error`)
      result$data<-dataset_name
      result$blue_green <- blue_green_
      result$fenzu<-fenzu
      result$pollution<-pollution_
      result$PC=round((exp(beta)-1)*100,digits = 2)
      result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
      result_short_cat_interaction_pollution<- rbind(result_short_cat_interaction_pollution, result)
      
    }
  }
  count=count+1
  print(count)
}
result_short_cat_modefication_pollution <- result_short_cat_modefication_pollution%>%
  arrange(data,blue_green, desc(pollution),fenzu)
result_short_cat_interaction_pollution <- result_short_cat_interaction_pollution%>%
  arrange(data,blue_green, desc(pollution))
write.csv(result_short_pollution,"E:/PKU_fanyi/Pro1/result_short/result_short_pollution.csv")
write.csv(result_short_pollution_2,"E:/PKU_fanyi/Pro1/result_short/result_short_pollution_2.csv")
write.csv(result_short_continous_modefication_pollution,"E:/PKU_fanyi/Pro1/result_short/result_short_continous_modefication_pollution.csv")
write.csv(result_short_cat_modefication_pollution,"E:/PKU_fanyi/Pro1/result_short/result_short_cat_modefication_pollution.csv")
write.csv(result_short_cat_interaction_pollution,"E:/PKU_fanyi/Pro1/result_short/result_short_cat_interaction_pollution.csv")

#####短期修饰作用：tem####
datasets <- list(
  total_short_heat= diseases2_total_short_heat,
  total_short_cold= diseases2_total_short_cold,
  cvd2_short_heat= diseases2_cvd2_short_heat,
  cvd2_short_cold= diseases2_cvd2_short_cold,
  resp2_short_heat= diseases2_resp2_short_heat,
  resp2_short_cold= diseases2_resp2_short_cold,
  suicides2_short_heat= diseases2_suicides2_short_heat,
  suicides2_short_cold= diseases2_suicides2_short_cold
)
colnames(diseases2_total_short_cold)
# 存储所有结果的列表
count<-0
result_short_tem=data.frame()
result_short_tem_2=data.frame()
result_short_continous_modefication_tem=data.frame()
result_short_cat_modefication_tem=data.frame()
result_short_cat_interaction_tem=data.frame()
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 获取当前数据集
  data<-data %>% 
    mutate(avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
           avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
           avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
           
           avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
           avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
           avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  #
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
  #####污染物与结局的直接关系####
  for(tem_ in colnames(data)[c(101)]){
    formula <-as.formula(paste("n ~",tem_,"+as.factor(dow)",
                               "+as.factor(season)",
                               "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                               "+as.factor(Climate_cat_code)",
                               sep = ""))
    model<-gam(formula, family = quasipoisson, data = data,na.action = na.omit)
    print(summary(model))
    result<-as.data.frame(summary(model)$p.table)[tem_, ]
    beta=as.numeric(result$Estimate)
    se=as.numeric(result$`Std. Error`)
    result$data<-dataset_name
    result$tem<-tem_
    result$r2<-summary(model)[["r.sq"]]
    result$dev.expl<-summary(model)[["dev.expl"]]
    result$PC=round((exp(beta)-1)*100,digits = 2)
    result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
    result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
    result_short_tem<- rbind(result_short_tem, result)
    
    
  }
  for(tem_ in colnames(data)[c(101)]){
    for (blue_green_ in c("avg_forest_shrub_grassland_fraction_5year_scaled",
                          "avg_water_wetland_fraction_5year_scaled")){
      formula <-as.formula(paste("n ~",tem_,"+",blue_green_,"+as.factor(dow)",
                                 "+as.factor(season)",
                                 "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                                 "+as.factor(Climate_cat_code)",
                                 sep = ""))
      model<-gam(formula, family = quasipoisson, data = data,na.action = na.omit)
      print(summary(model))
      result<-as.data.frame(summary(model)$p.table)[tem_, ]
      beta=as.numeric(result$Estimate)
      se=as.numeric(result$`Std. Error`)
      result$data<-dataset_name
      result$blue_green<-blue_green_ 
      result$tem<-tem_
      result$r2<-summary(model)[["r.sq"]]
      result$dev.expl<-summary(model)[["dev.expl"]]
      result$PC=round((exp(beta)-1)*100,digits = 2)
      result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
      result_short_tem_2<- rbind(result_short_tem_2, result)
      
    }
  }
  #####continuous_forest####
  
  for(tem_ in colnames(data)[c(101)]){
    for (blue_green_ in c("avg_forest_shrub_grassland_fraction_1year_scaled","avg_forest_shrub_grassland_fraction_3year_scaled","avg_forest_shrub_grassland_fraction_5year_scaled"
    )){
      change_values<-seq(0,0.90,by=0.05) 
      model<-gam(as.formula(paste("n ~",tem_,"*",blue_green_,"+as.factor(dow)",
                                  "+as.factor(season)",
                                  "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                                  "+as.factor(Climate_cat_code)",
                                  sep = "")),family = quasipoisson, data = data,na.action = na.omit)
      beta1_boot<-summary(model)$p.table[tem_,"Estimate"]
      beta2_boot<-summary(model)$p.table[paste0(tem_,":",blue_green_),"Estimate"]
      se1_boot<-summary(model)$p.table[tem_,"Std. Error"]
      se2_boot<-summary(model)$p.table[paste0(tem_,":",blue_green_),"Std. Error"]
      se<-sqrt(se1_boot^2+se2_boot^2)
      result_df<-data.frame(
        data=dataset_name,
        blue_green=blue_green_,
        tem=tem_,
        change_value=change_values,
        beta=numeric(length(change_values)),
        se=numeric(length(change_values))
      )
      for (i in seq_along(change_values)){
        change_value<-change_values[i]
        result_df$beta[i]<-beta1_boot+beta2_boot*change_value
        result_df$se[i]<-se
      }
      result_short_continous_modefication_tem<-rbind(result_short_continous_modefication_tem,result_df)
      
    }
  }
  #####continuous_water####
  
  for(tem_ in colnames(data)[c(101)]){
    for (blue_green_ in c(
      "avg_water_wetland_fraction_1year_scaled","avg_water_wetland_fraction_3year_scaled","avg_water_wetland_fraction_5year_scaled")){
      change_values<-seq(0,0.25,by=0.01) 
      model<-gam(as.formula(paste("n ~",tem_,"*",blue_green_,"+as.factor(dow)",
                                  "+as.factor(season)",
                                  "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                                  "+as.factor(Climate_cat_code)",
                                  sep = "")),family = quasipoisson, data = data,na.action = na.omit)
      beta1_boot<-summary(model)$p.table[tem_,"Estimate"]
      beta2_boot<-summary(model)$p.table[paste0(tem_,":",blue_green_),"Estimate"]
      se1_boot<-summary(model)$p.table[tem_,"Std. Error"]
      se2_boot<-summary(model)$p.table[paste0(tem_,":",blue_green_),"Std. Error"]
      se<-sqrt(se1_boot^2+se2_boot^2)
      result_df<-data.frame(
        data=dataset_name,
        blue_green=blue_green_,
        tem=tem_,
        change_value=change_values,
        beta=numeric(length(change_values)),
        se=numeric(length(change_values))
      )
      for (i in seq_along(change_values)){
        change_value<-change_values[i]
        result_df$beta[i]<-beta1_boot+beta2_boot*change_value
        result_df$se[i]<-se
      }
      result_short_continous_modefication_tem<-rbind(result_short_continous_modefication_tem,result_df)
      
    }
  }
  ####cat####
  for (blue_green_ in c("avg_forest_shrub_grassland_fraction_1year_scaled_2group","avg_forest_shrub_grassland_fraction_3year_scaled_2group","avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                        "avg_water_wetland_fraction_1year_scaled_2group","avg_water_wetland_fraction_3year_scaled_2group","avg_water_wetland_fraction_5year_scaled_2group")){
    for (fenzu in 1:2) {
      data1 <- subset(data, data[[blue_green_]] == fenzu)
      for (tem_ in colnames(data)[c(101)]) {
        formula <- as.formula(paste("n ~",tem_,"+as.factor(dow)",
                                    "+as.factor(season)",
                                    "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                                    "+as.factor(Climate_cat_code)",
                                    sep = ""))
        
        model<-gam(formula, family = quasipoisson, data = data1, na.action = na.omit)
        result<-as.data.frame(summary(model)$p.table)[tem_, ]
        beta=as.numeric(result$Estimate)
        se=as.numeric(result$`Std. Error`)
        result$data<-dataset_name
        result$blue_green <- blue_green_
        result$fenzu<-fenzu
        result$tem<-tem_
        result$PC=round((exp(beta)-1)*100,digits = 2)
        result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
        result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
        result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
        result_short_cat_modefication_tem<- rbind(result_short_cat_modefication_tem, result)
        
      }
    }
  }
  
  ####交互作用####
  for (blue_green_ in c("avg_forest_shrub_grassland_fraction_1year_scaled_2group","avg_forest_shrub_grassland_fraction_3year_scaled_2group","avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                        "avg_water_wetland_fraction_1year_scaled_2group","avg_water_wetland_fraction_3year_scaled_2group","avg_water_wetland_fraction_5year_scaled_2group")) {
    for (tem_ in colnames(data)[c(101)]){
      fformula <- as.formula(paste("n ~",tem_,"*","blue_green_","+as.factor(dow)",
                                   "+as.factor(season)",
                                   "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                                   "+as.factor(Climate_cat_code)",
                                   sep = ""))
      model<-gam(formula, family = quasipoisson, data = data1, na.action = na.omit)
      result<-as.data.frame(summary(model)$p.table)[paste0(tem_,":",blue_green_), ]
      beta=as.numeric(result$Estimate)
      se=as.numeric(result$`Std. Error`)
      result$data<-dataset_name
      result$blue_green <- blue_green_
      result$fenzu<-fenzu
      result$tem<-tem_
      result$PC=round((exp(beta)-1)*100,digits = 2)
      result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
      result_short_cat_interaction_tem<- rbind(result_short_cat_interaction_tem, result)
      
    }
  }
  count=count+1
  print(count)
}
result_short_cat_modefication_tem <- result_short_cat_modefication_tem%>%
  arrange(data,blue_green, desc(tem),fenzu)
result_short_cat_interaction_tem <- result_short_cat_interaction_tem%>%
  arrange(data,blue_green, desc(tem))
write.csv(result_short_tem,"E:/PKU_fanyi/Pro1/result_short/result_short_tem.csv")
write.csv(result_short_tem_2,"E:/PKU_fanyi/Pro1/result_short/result_short_tem_2.csv")
write.csv(result_short_continous_modefication,"E:/PKU_fanyi/Pro1/result_short/result_short_continous_modefication_tem.csv")
write.csv(result_short_cat_modefication,"E:/PKU_fanyi/Pro1/result_short/result_short_cat_modefication_tem.csv")
write.csv(result_short_cat_interaction,"E:/PKU_fanyi/Pro1/result_short/result_short_cat_interaction_tem.csv")
#####短期non_linear_interaction#####
diseases2_total_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_short.rds")
diseases2_cvd2_short<-readRDS("E:/PKU_fanyi/Pro1/fanyi/data_city/diseases2_cvd2_short.rds")
diseases2_resp2_short<-readRDS("E:/PKU_fanyi/Pro1/fanyi/data_city/diseases2_resp2_short.rds")
diseases2_total_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_short_heat.rds")
diseases2_cvd2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_short_heat.rds")
diseases2_resp2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_short_heat.rds")
diseases2_total_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_short_cold.rds")
diseases2_cvd2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_short_cold.rds")
diseases2_resp2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_short_cold.rds")
diseases2_suicides2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_short.rds")
diseases2_suicides2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_short_heat.rds")
diseases2_suicides2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_short_cold.rds")
###polution*
datasets <- list(
  total_short = diseases2_total_short, 
  cvd_short = diseases2_cvd2_short, 
  resp_short = diseases2_resp2_short,
  suicide_short=diseases2_suicides2_short
)

# 存储所有结果的列表
count<-0
colnames(data)
# 遍历数据集
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 获取当前数据集
  for (pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")){
    print(pollution_)
    for (blue_green_ in c('avg_forest_shrub_grassland_fraction_5year_scaled',"avg_water_wetland_fraction_5year_scaled")){
      print(blue_green_)
      data$interaction<-data[[pollution_]]*data[[blue_green_]]
      model <- gam(as.formula(paste("n ~","s(interaction,k=3)+",pollution_,"+",blue_green_,"+as.factor(dow)",
                                    "+as.factor(season)",
                                    "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                    "+as.factor(Climate_cat_code)",
                                    sep = "")),family = quasipoisson, data = data)
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
      out_filename<-paste0("/fs2/home/huangsd/fanyi/result2/short_",
                           dataset_name,"_",pollution_,"_",blue_green_,"non_linear!.csv")
      
      write.csv(result, out_filename,row.names=F) 
    }
  }
}
#模型判别
# 遍历数据集
n_cores<-parallel::detectCores()-15
cl<-makeCluster(n_cores)
registerDoParallel(cl)
task_count<-0
total_tasks<-length(datasets)*2*3
results<-foreach(dataset_name=names(datasets),.combine = rbind,.packages = c("stats","tidyverse","mgcv","splines","doParallel")) %dopar%{
  local_results<-data.frame()
  data <- datasets[[dataset_name]]  # 获取当前数据集
  data<-data %>% 
    mutate(avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
           avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
           avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
           
           avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
           avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
           avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  #
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
  for (pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")){
    
    for (blue_green_ in c('avg_forest_shrub_grassland_fraction_5year_scaled',"avg_water_wetland_fraction_5year_scaled")){
      task_count<-task_count+1
      message(paste0(
        "[",Sys.time(),"]",
        "Processing dataset:",dataset_name,",",
        "Blue_green:",blue_green_,",",
        "pollution:",pollution_,",",
        "Task:",task_count,"/",total_tasks
      ))
      data$interaction<-data[[pollution_]]*data[[blue_green_]]
      model <- gam(as.formula(paste("n ~","s(interaction,k=3)+",pollution_,"+",blue_green_,"+as.factor(dow)",
                                    "+as.factor(season)",
                                    "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                    "+as.factor(Climate_cat_code)",
                                    sep = "")),family = quasipoisson, data = data)
      model2 <- gam(as.formula(paste("n ~",pollution_,"*",blue_green_,"+as.factor(dow)",
                                     "+as.factor(season)",
                                     "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                     "+as.factor(Climate_cat_code)",
                                     sep = "")),family = quasipoisson, data = data)
      result<-as.data.frame(anova(model2,model,test="F"))
      result$diseases<-dataset_name
      result$space<-blue_green_
      result$pollution<-pollution_
      local_results<-rbind(local_results,result)
    }
  }
  return(local_results)
}









for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 获取当前数据集
  data<-data %>% 
    mutate(avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
           avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
           avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
           
           avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
           avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
           avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  #
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
  for (pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")){
    print(pollution_)
    for (blue_green_ in c('avg_forest_shrub_grassland_fraction_5year_scaled',"avg_water_wetland_fraction_5year_scaled")){
      print(blue_green_)
      data$interaction<-data[[pollution_]]*data[[blue_green_]]
      model <- gam(as.formula(paste("n ~","s(interaction,k=3)+",pollution_,"+",blue_green_,"+as.factor(dow)",
                                    "+as.factor(season)",
                                    "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                    "+as.factor(Climate_cat_code)",
                                    sep = "")),family = quasipoisson, data = data)
      model2 <- gam(as.formula(paste("n ~",pollution_,"*",blue_green_,"+as.factor(dow)",
                                     "+as.factor(season)",
                                     "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                     "+as.factor(Climate_cat_code)",
                                     sep = "")),family = quasipoisson, data = data)
      result<-as.data.frame(anova(model2,model,test="F"))
      result$diseases<-dataset_name
      result$space<-blue_green_
      result$pollution<-pollution_
      result_short_nonlinear_P<- rbind(result_short_nonlinear_P, result)
      
    }
  }
}
###tem*
datasets <- list(
  total_short_heat = diseases2_total_short_heat, 
  total_short_cold = diseases2_total_short_cold,
  cvd_short_heat = diseases2_cvd2_short_heat, 
  cvd_short_cold = diseases2_cvd2_short_cold, 
  resp_short_heat = diseases2_resp2_short_heat,
  resp_short_cold = diseases2_resp2_short_cold,
  suicides_short_heat=diseases2_suicides2_short_heat,
  suicides_short_cold=diseases2_suicides2_short_cold
)

# 存储所有结果的列表
count<-0
# 遍历数据集
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 获取当前数据集
  data<-data %>% 
    mutate(avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
           avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
           avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
           
           avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
           avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
           avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  #
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
  for (tem_ in colnames(data)[c(130)]){
    print(tem_)
    for (blue_green_ in c('avg_forest_shrub_grassland_fraction_5year_scaled',"avg_water_wetland_fraction_5year_scaled")){
      print(blue_green_)
      data$interaction<-data[[tem_]]*data[[blue_green_]]
      model <- gam(as.formula(paste("n ~","s(interaction,k=3)+",tem_,"+",blue_green_,"+as.factor(dow)",
                                    "+as.factor(season)",
                                    "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                                    "+as.factor(Climate_cat_code)",
                                    sep = "")),family = quasipoisson, data = data)
      model2 <- gam(as.formula(paste("n ~",tem_,"*",blue_green_,"+as.factor(dow)",
                                     "+as.factor(season)",
                                     "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                                     "+as.factor(Climate_cat_code)",
                                     sep = "")),family = quasipoisson, data = data)
      result<-as.data.frame(anova(model2,model,test="F"))
      result$diseases<-dataset_name
      result$space<-blue_green_
      result$pollution<-pollution_
      result_short_nonlinear_P<- rbind(result_short_nonlinear_P, result)
    }
  }
}

#####COV分层修饰作用#####
diseases2_total_sex1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_sex1_short.rds")
diseases2_total_sex2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_sex2_short.rds")
diseases2_total_age1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_age1_short.rds")
diseases2_total_age2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_age2_short.rds")
diseases2_total_education1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_education1_short.rds")
diseases2_total_education2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_education2_short.rds")
diseases2_total_marital1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_marital1_short.rds")
diseases2_total_marital2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_marital2_short.rds")

diseases2_cvd2_sex1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_sex1_short.rds")
diseases2_cvd2_sex2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_sex2_short.rds")
diseases2_cvd2_age1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_age1_short.rds")
diseases2_cvd2_age2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_age2_short.rds")
diseases2_cvd2_education1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_education1_short.rds")
diseases2_cvd2_education2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_education2_short.rds")
diseases2_cvd2_marital1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_marital1_short.rds")
diseases2_cvd2_marital2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_marital2_short.rds")

diseases2_resp2_sex1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_sex1_short.rds")
diseases2_resp2_sex2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_sex2_short.rds")
diseases2_resp2_age1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_age1_short.rds")
diseases2_resp2_age2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_age2_short.rds")
diseases2_resp2_education1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_education1_short.rds")
diseases2_resp2_education2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_education2_short.rds")
diseases2_resp2_marital1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_marital1_short.rds")
diseases2_resp2_marital2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_marital2_short.rds")

diseases2_suicides2_sex1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_sex1_short.rds")
diseases2_suicides2_sex2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_sex2_short.rds")
diseases2_suicides2_age1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_age1_short.rds")
diseases2_suicides2_age2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_age2_short.rds")
diseases2_suicides2_education1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_education1_short.rds")
diseases2_suicides2_education2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_education2_short.rds")
diseases2_suicides2_marital1_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_marital1_short.rds")
diseases2_suicides2_marital2_short<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_marital2_short.rds")

#####短期修饰作用：PM####
datasets <- list(
  diseases2_total_sex1_short= diseases2_total_sex1_short, 
  diseases2_total_sex2_short= diseases2_total_sex1_short, 
  diseases2_total_age1_short= diseases2_total_age1_short,
  diseases2_total_age1_short=diseases2_total_age1_short,
  diseases2_total_age2_short=diseases2_total_age2_short,
  diseases2_total_education1_short=diseases2_total_education1_short,
  diseases2_total_education2_short=diseases2_total_education2_short,
  diseases2_total_marital1_short=diseases2_total_marital1_short,
  diseases2_total_marital2_short=diseases2_total_marital2_short,
  diseases2_cvd2_sex1_short= diseases2_cvd2_sex1_short, 
  diseases2_cvd2_sex2_short= diseases2_cvd2_sex1_short, 
  diseases2_cvd2_age1_short= diseases2_cvd2_age1_short,
  diseases2_cvd2_age1_short=diseases2_cvd2_age1_short,
  diseases2_cvd2_age2_short=diseases2_cvd2_age2_short,
  diseases2_cvd2_education1_short=diseases2_cvd2_education1_short,
  diseases2_cvd2_education2_short=diseases2_cvd2_education2_short,
  diseases2_cvd2_marital1_short=diseases2_cvd2_marital1_short,
  diseases2_cvd2_marital2_short=diseases2_cvd2_marital2_short,
  diseases2_resp2_sex1_short= diseases2_resp2_sex1_short, 
  diseases2_resp2_sex2_short= diseases2_resp2_sex1_short, 
  diseases2_resp2_age1_short= diseases2_resp2_age1_short,
  diseases2_resp2_age1_short=diseases2_resp2_age1_short,
  diseases2_resp2_age2_short=diseases2_resp2_age2_short,
  diseases2_resp2_education1_short=diseases2_resp2_education1_short,
  diseases2_resp2_education2_short=diseases2_resp2_education2_short,
  diseases2_resp2_marital1_short=diseases2_resp2_marital1_short,
  diseases2_resp2_marital2_short=diseases2_resp2_marital2_short,
  diseases2_suicides2_sex1_short= diseases2_suicides2_sex1_short, 
  diseases2_suicides2_sex2_short= diseases2_suicides2_sex1_short, 
  diseases2_suicides2_age1_short= diseases2_suicides2_age1_short,
  diseases2_suicides2_age1_short=diseases2_suicides2_age1_short,
  diseases2_suicides2_age2_short=diseases2_suicides2_age2_short,
  diseases2_suicides2_education1_short=diseases2_suicides2_education1_short,
  diseases2_suicides2_education2_short=diseases2_suicides2_education2_short,
  diseases2_suicides2_marital1_short=diseases2_suicides2_marital1_short,
  diseases2_suicides2_marital2_short=diseases2_suicides2_marital2_short
)
# 存储所有结果的列表
count<-0
result_short_strate_continous_modefication_pollution=data.frame()
result_short_strate_cat_modefication_pollution=data.frame()
result_short_strate_cat_interaction_pollution=data.frame()
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 获取当前数据集
  data<-data %>% 
    mutate(avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
           avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
           avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
           
           avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
           avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
           avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  #
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
  #####continuous_forest####
  
  for(pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")){
    for (blue_green_ in c("avg_forest_shrub_grassland_fraction_1year_scaled","avg_forest_shrub_grassland_fraction_3year_scaled","avg_forest_shrub_grassland_fraction_5year_scaled"
    )){
      change_values<-seq(0,0.90,by=0.05) 
      model<-gam(as.formula(paste("n ~",pollution_,"*",blue_green_,"+as.factor(dow)",
                                  "+as.factor(season)",
                                  "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                  "+as.factor(Climate_cat_code)",
                                  sep = "")),family = quasipoisson, data = data,na.action = na.omit)
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
      result_shor_stratet_continous_modefication_pollution<-rbind(result_short_strate_continous_modefication_pollution,result_df)
      
    }
  }
  #####continuous_water####
  
  for(pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")){
    for (blue_green_ in c(
      "avg_water_wetland_fraction_1year_scaled","avg_water_wetland_fraction_3year_scaled","avg_water_wetland_fraction_5year_scaled")){
      change_values<-seq(0,0.25,by=0.01) 
      model<-gam(as.formula(paste("n ~",pollution_,"*",blue_green_,"+as.factor(dow)",
                                  "+as.factor(season)",
                                  "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                  "+as.factor(Climate_cat_code)",
                                  sep = "")),family = quasipoisson, data = data,na.action = na.omit)
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
      result_short_strate_continous_modefication_pollution<-rbind(result_short_strate_continous_modefication_pollution,result_df)
      
    }
  }
  ####cat####
  for (blue_green_ in c("avg_forest_shrub_grassland_fraction_1year_scaled_2group","avg_forest_shrub_grassland_fraction_3year_scaled_2group","avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                        "avg_water_wetland_fraction_1year_scaled_2group","avg_water_wetland_fraction_3year_scaled_2group","avg_water_wetland_fraction_5year_scaled_2group")){
    for (fenzu in 1:2) {
      data1 <- subset(data, data[[blue_green_]] == fenzu)
      for (pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")) {
        formula <- as.formula(paste("n ~",pollution_,"+as.factor(dow)",
                                    "+as.factor(season)",
                                    "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                    "+as.factor(Climate_cat_code)",
                                    sep = ""))
        
        model<-gam(formula, family = quasipoisson, data = data1, na.action = na.omit)
        result<-as.data.frame(summary(model)$p.table)[pollution_, ]
        beta=as.numeric(result$Estimate)
        se=as.numeric(result$`Std. Error`)
        result$data<-dataset_name
        result$blue_green <- blue_green_
        result$fenzu<-fenzu
        result$pollution<-pollution_
        result$PC=round((exp(beta)-1)*100,digits = 2)
        result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
        result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
        result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
        result_short_strate_cat_modefication_pollution<- rbind(result_short_strate_cat_modefication_pollution, result)
        
      }
    }
  }
  
  ####交互作用####
  for (blue_green_ in c("avg_forest_shrub_grassland_fraction_1year_scaled_2group","avg_forest_shrub_grassland_fraction_3year_scaled_2group","avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                        "avg_water_wetland_fraction_1year_scaled_2group","avg_water_wetland_fraction_3year_scaled_2group","avg_water_wetland_fraction_5year_scaled_2group")) {
    for (pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")){
      formula <- as.formula(paste("n ~",pollution_,"*",blue_green_,"+as.factor(dow)",
                                  "+as.factor(season)",
                                  "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                                  "+as.factor(Climate_cat_code)",
                                  sep = ""))
      model<-gam(formula, family = quasipoisson, data = data1, na.action = na.omit)
      result<-as.data.frame(summary(model)$p.table)[paste0(pollution_,":",blue_green_), ]
      beta=as.numeric(result$Estimate)
      se=as.numeric(result$`Std. Error`)
      result$data<-dataset_name
      result$blue_green <- blue_green_
      result$fenzu<-fenzu
      result$pollution<-pollution_
      result$PC=round((exp(beta)-1)*100,digits = 2)
      result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
      result_short_strate_cat_interaction_pollution<- rbind(result_short_strate_cat_interaction_pollution, result)
      
    }
  }
  count=count+1
  print(count)
}
result_short_strate_cat_modefication_pollution <- result_short_strate_cat_modefication_pollution%>%
  arrange(data,blue_green, desc(pollution),fenzu)
result_short_strate_cat_interaction_pollution <- result_short_strate_cat_interaction_pollution%>%
  arrange(data,blue_green, desc(pollution))
write.csv(result_short_strate_continous_modefication_pollution,"E:/PKU_fanyi/Pro1/result_short/result_short_strate_continous_modefication_pollution.csv")
write.csv(result_short_strate_cat_modefication_pollution,"E:/PKU_fanyi/Pro1/result_short/result_short_strate_cat_modefication_pollution.csv")
write.csv(result_short_strate_cat_interaction_pollution,"E:/PKU_fanyi/Pro1/result_short/result_short_strate_cat_interaction_pollution.csv")




#####读取数据####
rm(list=ls())
diseases2_total_sex1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_sex1_short_heat.rds")
diseases2_total_sex2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_sex2_short_heat.rds")
diseases2_total_age1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_age1_short_heat.rds")
diseases2_total_age2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_age2_short_heat.rds")
diseases2_total_education1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_education1_short_heat.rds")
diseases2_total_education2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_education2_short_heat.rds")
diseases2_total_marital1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_marital1_short_heat.rds")
diseases2_total_marital2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_marital2_short_heat.rds")

diseases2_cvd2_sex1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_sex1_short_heat.rds")
diseases2_cvd2_sex2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_sex2_short_heat.rds")
diseases2_cvd2_age1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_age1_short_heat.rds")
diseases2_cvd2_age2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_age2_short_heat.rds")
diseases2_cvd2_education1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_education1_short_heat.rds")
diseases2_cvd2_education2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_education2_short_heat.rds")
diseases2_cvd2_marital1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_marital1_short_heat.rds")
diseases2_cvd2_marital2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_marital2_short_heat.rds")

diseases2_resp2_sex1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_sex1_short_heat.rds")
diseases2_resp2_sex2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_sex2_short_heat.rds")
diseases2_resp2_age1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_age1_short_heat.rds")
diseases2_resp2_age2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_age2_short_heat.rds")
diseases2_resp2_education1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_education1_short_heat.rds")
diseases2_resp2_education2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_education2_short_heat.rds")
diseases2_resp2_marital1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_marital1_short_heat.rds")
diseases2_resp2_marital2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_marital2_short_heat.rds")

diseases2_suicides2_sex1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_sex1_short_heat.rds")
diseases2_suicides2_sex2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_sex2_short_heat.rds")
diseases2_suicides2_age1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_age1_short_heat.rds")
diseases2_suicides2_age2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_age2_short_heat.rds")
diseases2_suicides2_education1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_education1_short_heat.rds")
diseases2_suicides2_education2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_education2_short_heat.rds")
diseases2_suicides2_marital1_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_marital1_short_heat.rds")
diseases2_suicides2_marital2_short_heat<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_marital2_short_heat.rds")

print("start:cold")
diseases2_total_sex1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_sex1_short_cold.rds")
diseases2_total_sex2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_sex2_short_cold.rds")
diseases2_total_age1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_age1_short_cold.rds")
diseases2_total_age2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_age2_short_cold.rds")
diseases2_total_education1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_education1_short_cold.rds")
diseases2_total_education2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_education2_short_cold.rds")
diseases2_total_marital1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_marital1_short_cold.rds")
diseases2_total_marital2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_marital2_short_cold.rds")

diseases2_cvd2_sex1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_sex1_short_cold.rds")
diseases2_cvd2_sex2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_sex2_short_cold.rds")
diseases2_cvd2_age1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_age1_short_cold.rds")
diseases2_cvd2_age2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_age2_short_cold.rds")
diseases2_cvd2_education1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_education1_short_cold.rds")
diseases2_cvd2_education2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_education2_short_cold.rds")
diseases2_cvd2_marital1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_marital1_short_cold.rds")
diseases2_cvd2_marital2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_marital2_short_cold.rds")

diseases2_resp2_sex1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_sex1_short_cold.rds")
diseases2_resp2_sex2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_sex2_short_cold.rds")
diseases2_resp2_age1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_age1_short_cold.rds")
diseases2_resp2_age2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_age2_short_cold.rds")
diseases2_resp2_education1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_education1_short_cold.rds")
diseases2_resp2_education2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_education2_short_cold.rds")
diseases2_resp2_marital1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_marital1_short_cold.rds")
diseases2_resp2_marital2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_marital2_short_cold.rds")

diseases2_suicides2_sex1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_sex1_short_cold.rds")
diseases2_suicides2_sex2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_sex2_short_cold.rds")
diseases2_suicides2_age1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_age1_short_cold.rds")
diseases2_suicides2_age2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_age2_short_cold.rds")
diseases2_suicides2_education1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_education1_short_cold.rds")
diseases2_suicides2_education2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_education2_short_cold.rds")
diseases2_suicides2_marital1_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_marital1_short_cold.rds")
diseases2_suicides2_marital2_short_cold<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_marital2_short_cold.rds")


#####短期修饰作用：tem####
datasets <- list(
  diseases2_total_sex1_short_heat= diseases2_total_sex1_short_heat, 
  diseases2_total_sex2_short_heat= diseases2_total_sex1_short_heat, 
  diseases2_total_age1_short_heat= diseases2_total_age1_short_heat,
  diseases2_total_age1_short_heat=diseases2_total_age1_short_heat,
  diseases2_total_age2_short_heat=diseases2_total_age2_short_heat,
  diseases2_total_education1_short_heat=diseases2_total_education1_short_heat,
  diseases2_total_education2_short_heat=diseases2_total_education2_short_heat,
  diseases2_total_marital1_short_heat=diseases2_total_marital1_short_heat,
  diseases2_total_marital2_short_heat=diseases2_total_marital2_short_heat,
  diseases2_cvd2_sex1_short_heat= diseases2_cvd2_sex1_short_heat, 
  diseases2_cvd2_sex2_short_heat= diseases2_cvd2_sex1_short_heat, 
  diseases2_cvd2_age1_short_heat= diseases2_cvd2_age1_short_heat,
  diseases2_cvd2_age1_short_heat=diseases2_cvd2_age1_short_heat,
  diseases2_cvd2_age2_short_heat=diseases2_cvd2_age2_short_heat,
  diseases2_cvd2_education1_short_heat=diseases2_cvd2_education1_short_heat,
  diseases2_cvd2_education2_short_heat=diseases2_cvd2_education2_short_heat,
  diseases2_cvd2_marital1_short_heat=diseases2_cvd2_marital1_short_heat,
  diseases2_cvd2_marital2_short_heat=diseases2_cvd2_marital2_short_heat,
  diseases2_resp2_sex1_short_heat= diseases2_resp2_sex1_short_heat, 
  diseases2_resp2_sex2_short_heat= diseases2_resp2_sex1_short_heat, 
  diseases2_resp2_age1_short_heat= diseases2_resp2_age1_short_heat,
  diseases2_resp2_age1_short_heat=diseases2_resp2_age1_short_heat,
  diseases2_resp2_age2_short_heat=diseases2_resp2_age2_short_heat,
  diseases2_resp2_education1_short_heat=diseases2_resp2_education1_short_heat,
  diseases2_resp2_education2_short_heat=diseases2_resp2_education2_short_heat,
  diseases2_resp2_marital1_short_heat=diseases2_resp2_marital1_short_heat,
  diseases2_resp2_marital2_short_heat=diseases2_resp2_marital2_short_heat,
  diseases2_suicides2_sex1_short_heat= diseases2_suicides2_sex1_short_heat, 
  diseases2_suicides2_sex2_short_heat= diseases2_suicides2_sex1_short_heat, 
  diseases2_suicides2_age1_short_heat= diseases2_suicides2_age1_short_heat,
  diseases2_suicides2_age1_short_heat=diseases2_suicides2_age1_short_heat,
  diseases2_suicides2_age2_short_heat=diseases2_suicides2_age2_short_heat,
  diseases2_suicides2_education1_short_heat=diseases2_suicides2_education1_short_heat,
  diseases2_suicides2_education2_short_heat=diseases2_suicides2_education2_short_heat,
  diseases2_suicides2_marital1_short_heat=diseases2_suicides2_marital1_short_heat,
  diseases2_suicides2_marital2_short_heat=diseases2_suicides2_marital2_short_heat,
  diseases2_total_sex1_short_cold= diseases2_total_sex1_short_cold, 
  diseases2_total_sex2_short_cold= diseases2_total_sex1_short_cold, 
  diseases2_total_age1_short_cold= diseases2_total_age1_short_cold,
  diseases2_total_age1_short_cold=diseases2_total_age1_short_cold,
  diseases2_total_age2_short_cold=diseases2_total_age2_short_cold,
  diseases2_total_education1_short_cold=diseases2_total_education1_short_cold,
  diseases2_total_education2_short_cold=diseases2_total_education2_short_cold,
  diseases2_total_marital1_short_cold=diseases2_total_marital1_short_cold,
  diseases2_total_marital2_short_cold=diseases2_total_marital2_short_cold,
  diseases2_cvd2_sex1_short_cold= diseases2_cvd2_sex1_short_cold, 
  diseases2_cvd2_sex2_short_cold= diseases2_cvd2_sex1_short_cold, 
  diseases2_cvd2_age1_short_cold= diseases2_cvd2_age1_short_cold,
  diseases2_cvd2_age1_short_cold=diseases2_cvd2_age1_short_cold,
  diseases2_cvd2_age2_short_cold=diseases2_cvd2_age2_short_cold,
  diseases2_cvd2_education1_short_cold=diseases2_cvd2_education1_short_cold,
  diseases2_cvd2_education2_short_cold=diseases2_cvd2_education2_short_cold,
  diseases2_cvd2_marital1_short_cold=diseases2_cvd2_marital1_short_cold,
  diseases2_cvd2_marital2_short_cold=diseases2_cvd2_marital2_short_cold,
  diseases2_resp2_sex1_short_cold= diseases2_resp2_sex1_short_cold, 
  diseases2_resp2_sex2_short_cold= diseases2_resp2_sex1_short_cold, 
  diseases2_resp2_age1_short_cold= diseases2_resp2_age1_short_cold,
  diseases2_resp2_age1_short_cold=diseases2_resp2_age1_short_cold,
  diseases2_resp2_age2_short_cold=diseases2_resp2_age2_short_cold,
  diseases2_resp2_education1_short_cold=diseases2_resp2_education1_short_cold,
  diseases2_resp2_education2_short_cold=diseases2_resp2_education2_short_cold,
  diseases2_resp2_marital1_short_cold=diseases2_resp2_marital1_short_cold,
  diseases2_resp2_marital2_short_cold=diseases2_resp2_marital2_short_cold,
  diseases2_suicides2_sex1_short_cold= diseases2_suicides2_sex1_short_cold, 
  diseases2_suicides2_sex2_short_cold= diseases2_suicides2_sex1_short_cold, 
  diseases2_suicides2_age1_short_cold= diseases2_suicides2_age1_short_cold,
  diseases2_suicides2_age1_short_cold=diseases2_suicides2_age1_short_cold,
  diseases2_suicides2_age2_short_cold=diseases2_suicides2_age2_short_cold,
  diseases2_suicides2_education1_short_cold=diseases2_suicides2_education1_short_cold,
  diseases2_suicides2_education2_short_cold=diseases2_suicides2_education2_short_cold,
  diseases2_suicides2_marital1_short_cold=diseases2_suicides2_marital1_short_cold,
  diseases2_suicides2_marital2_short_cold=diseases2_suicides2_marital2_short_cold
)
# 存储所有结果的列表
count<-0
result_short_strate_continous_modefication_tem=data.frame()
result_short_strate_cat_modefication_tem=data.frame()
result_short_strate_cat_interaction_tem=data.frame()
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 获取当前数据集
  data<-data %>% 
    mutate(avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
           avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
           avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
           
           avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
           avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
           avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
  #
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
  #####continuous_forest####
  print(colnames(data))
  for(tem_ in colnames(data)[c(101)]){
    for (blue_green_ in c("avg_forest_shrub_grassland_fraction_5year_scaled"
    )){
      change_values<-seq(0,0.90,by=0.05) 
      model<-gam(as.formula(paste("n ~",tem_,"*",blue_green_,"+as.factor(dow)",
                                  "+as.factor(season)",
                                  "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                                  "+as.factor(Climate_cat_code)",
                                  sep = "")),family = quasipoisson, data = data,na.action = na.omit)
      beta1_boot<-summary(model)$p.table[tem_,"Estimate"]
      beta2_boot<-summary(model)$p.table[paste0(tem_,":",blue_green_),"Estimate"]
      se1_boot<-summary(model)$p.table[tem_,"Std. Error"]
      se2_boot<-summary(model)$p.table[paste0(tem_,":",blue_green_),"Std. Error"]
      se<-sqrt(se1_boot^2+se2_boot^2)
      result_df<-data.frame(
        data=dataset_name,
        blue_green=blue_green_,
        tem=tem_,
        change_value=change_values,
        beta=numeric(length(change_values)),
        se=numeric(length(change_values))
      )
      for (i in seq_along(change_values)){
        change_value<-change_values[i]
        result_df$beta[i]<-beta1_boot+beta2_boot*change_value
        result_df$se[i]<-se
      }
      result_short_strate_continous_modefication_tem<-rbind(result_short_strate_continous_modefication_tem,result_df)
      
    }
  }
  #####continuous_water####
  
  for(tem_ in colnames(data)[c(101)]){
    for (blue_green_ in c(
      "avg_water_wetland_fraction_5year_scaled")){
      change_values<-seq(0,0.25,by=0.01) 
      model<-gam(as.formula(paste("n ~",tem_,"*",blue_green_,"+as.factor(dow)",
                                  "+as.factor(season)",
                                  "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                                  "+as.factor(Climate_cat_code)",
                                  sep = "")),family = quasipoisson, data = data,na.action = na.omit)
      beta1_boot<-summary(model)$p.table[tem_,"Estimate"]
      beta2_boot<-summary(model)$p.table[paste0(tem_,":",blue_green_),"Estimate"]
      se1_boot<-summary(model)$p.table[tem_,"Std. Error"]
      se2_boot<-summary(model)$p.table[paste0(tem_,":",blue_green_),"Std. Error"]
      se<-sqrt(se1_boot^2+se2_boot^2)
      result_df<-data.frame(
        data=dataset_name,
        blue_green=blue_green_,
        tem=tem_,
        change_value=change_values,
        beta=numeric(length(change_values)),
        se=numeric(length(change_values))
      )
      for (i in seq_along(change_values)){
        change_value<-change_values[i]
        result_df$beta[i]<-beta1_boot+beta2_boot*change_value
        result_df$se[i]<-se
      }
      result_short_strate_continous_modefication_tem<-rbind(result_short_strate_continous_modefication_tem,result_df)
      
    }
  }
  ####cat####
  for (blue_green_ in c("avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                        "avg_water_wetland_fraction_5year_scaled_2group")){
    for (fenzu in 1:2) {
      data1 <- subset(data, data[[blue_green_]] == fenzu)
      for (tem_ in colnames(data)[c(101)]) {
        formula <- as.formula(paste("n ~",tem_,"+as.factor(dow)",
                                    "+as.factor(season)",
                                    "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                                    "+as.factor(Climate_cat_code)",
                                    sep = ""))
        
        model<-gam(formula, family = quasipoisson, data = data1, na.action = na.omit)
        result<-as.data.frame(summary(model)$p.table)[tem_, ]
        beta=as.numeric(result$Estimate)
        se=as.numeric(result$`Std. Error`)
        result$data<-dataset_name
        result$blue_green <- blue_green_
        result$fenzu<-fenzu
        result$tem<-tem_
        result$PC=round((exp(beta)-1)*100,digits = 2)
        result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
        result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
        result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
        result_short_strate_cat_modefication_tem<- rbind(result_short_strate_cat_modefication_tem, result)
        
      }
    }
  }
  
  ####交互作用####
  for (blue_green_ in c("avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                        "avg_water_wetland_fraction_5year_scaled_2group")) {
    for (tem_ in colnames(data)[c(101)]){
      fformula <- as.formula(paste("n ~",tem_,"*","blue_green_","+as.factor(dow)",
                                   "+as.factor(season)",
                                   "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                                   "+as.factor(Climate_cat_code)",
                                   sep = ""))
      model<-gam(formula, family = quasipoisson, data = data1, na.action = na.omit)
      result<-as.data.frame(summary(model)$p.table)[paste0(tem_,":",blue_green_), ]
      beta=as.numeric(result$Estimate)
      se=as.numeric(result$`Std. Error`)
      result$data<-dataset_name
      result$blue_green <- blue_green_
      result$fenzu<-fenzu
      result$tem<-tem_
      result$PC=round((exp(beta)-1)*100,digits = 2)
      result$lower=round((exp(beta-qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$upper=round((exp(beta+qnorm(0.975,mean=0,sd=1)*se)-1)*100,digits = 2)
      result$PCCI=paste0(result$PC," (",result$lower,", ",result$upper,")")
      result_short_strate_cat_interaction_tem<- rbind(result_short_strate_cat_interaction_tem, result)
      
    }
  }
  count=count+1
  print(count)
}
result_short_strate_cat_modefication_tem <- result_short_strate_cat_modefication_tem%>%
  arrange(data,blue_green, desc(tem),fenzu)
result_short_strate_cat_interaction_tem <- result_short_strate_cat_interaction_tem%>%
  arrange(data,blue_green, desc(tem))
write.csv(result_short_strate_cat_modefication_tem,"E:/PKU_fanyi/Pro1/result_short/result_short_strate_continous_modefication_tem.csv")
write.csv(result_short_strate_cat_modefication_tem,"E:/PKU_fanyi/Pro1/result_short/result_short_strate_cat_modefication_tem.csv")
write.csv(result_short_strate_cat_interaction_tem,"E:/PKU_fanyi/Pro1/result_short/result_short_strate_cat_interaction_tem.csv")
######
#####异质性检验####
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
result_strate_cat_short<-read.csv("E:/deskbook/Result_China/new/result_short/result_short_cat_modefication_pollution.csv")
colnames(result_strate_cat_short)
# 筛选 space 列中包含 "5year" 的行
result_strate_cat_5year_short <- result_strate_cat_short %>%
  filter(str_detect(blue_green, "5year"))
table(result_strate_cat_5year_short$data)
table(result_strate_cat_5year_short$blue_green)
table(result_strate_cat_5year_short$pollution)

# 创建新的数据集
result_strate_cat_5year_short2 <- result_strate_cat_5year_short %>%
  mutate(
    dataset = ifelse(str_detect(data, "^total"), "total", 
                     ifelse(str_detect(data, "^cvd"), "cvd",
                            ifelse(str_detect(data, "^resp"), "resp","suicide"))), 
    space = ifelse(str_detect(blue_green, "forest_shrub_grassland"), "green_space", 
                   "blue_space"), 
    fenzu = ifelse(fenzu==1, 1,2)                           # 提取 data 列最后一个数字
  ) %>%
  select(dataset, space,pollution, fenzu, Estimate, Std..Error)
result_strate_cat_5year_short2$dataset<-factor(result_strate_cat_5year_short2$dataset,levels=c("total","cvd","resp","suicide"))
result_strate_cat_5year_short2$pollution<-factor(result_strate_cat_5year_short2$pollution,levels=c("lag01_PM1","lag01_PM2.5","lag01_PM10"))
result_strate_cat_5year_short2<-result_strate_cat_5year_short2 %>% 
  arrange(dataset,space,pollution, fenzu)
#
for (i in seq(1, nrow(result_strate_cat_5year_short2), by = 2)) {
  # 提取两行 beta 和 se
  beta_pair <-result_strate_cat_5year_short2$Estimate[i:(i+1)]
  se_pair <- result_strate_cat_5year_short2$Std..Error[i:(i+1)]
  result <- heterogeneity(beta_pair, se_pair)
  result_strate_cat_5year_short2$p_value[i:(i+1)] <- result$p
}
write.csv(result_strate_cat_5year_short2,"E:/deskbook/Result_China/figure_raw2/heterogeneity_test_result_result_strate_cat_5year_short_pollution.csv",row.names = F)

#读取数据###
cvd_short_cold<-read.csv("E:/deskbook/Result_China/result_short/diseases2_cvd2_short_cold_tem_strate.csv")
cvd_short_heat<-read.csv("E:/deskbook/Result_China/result_short/diseases2_cvd2_short_heat_tem_strate.csv")
total_short_cold<-read.csv("E:/deskbook/Result_China/result_short/diseases2_total_short_cold_tem_strate.csv")
total_short_heat<-read.csv("E:/deskbook/Result_China/result_short/diseases2_total_short_heat_tem_strate.csv")
resp_short_cold<-read.csv("E:/deskbook/Result_China/result_short/diseases2_resp2_short_cold_tem_strate.csv")
resp_short_heat<-read.csv("E:/deskbook/Result_China/result_short/diseases2_resp2_short_heat_tem_strate.csv")
suicide_short_cold<-read.csv("E:/deskbook/Result_China/result_short/diseases2_suicides2_short_cold_tem_strate.csv")
suicide_short_heat<-read.csv("E:/deskbook/Result_China/result_short/diseases2_suicides2_short_heat_tem_strate.csv")
#
df_list <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]
# 按行合并所有数据框
result_strate_cat_tem_short <- do.call(rbind, lapply(df_list, function(x) get(x)))
result_strate_cat_tem_short$dataset <- rep(c("cvd", "total","resp","suicide"), each = 24)
colnames(result_strate_cat_tem_short)
# 筛选 space 列中包含 "5year" 的行
result_strate_cat_tem_short_5year <- result_strate_cat_tem_short %>%
  filter(str_detect(strate_factor, "5year"))
table(result_strate_cat_tem_short_5year$strate_factor)
table(result_strate_cat_tem_short_5year$tem)

# 创建新的数据集
result_strate_cat_tem_short_5year2 <- result_strate_cat_tem_short_5year %>%
  mutate(
    dataset = ifelse(str_detect(dataset, "^total"), "total", 
                     ifelse(str_detect(dataset, "^cvd"), "cvd",
                            ifelse(str_detect(dataset, "^resp"), "resp","suicide"))), 
    space = ifelse(str_detect(strate_factor, "forest_shrub_grassland"), "green_space", 
                   "blue_space"), 
    fenzu = ifelse(fenzu==1, 1,2)                           # 提取 data 列最后一个数字
  ) %>%
  select(dataset, space,tem, fenzu, Estimate, Std..Error)
result_strate_cat_tem_short_5year2$dataset<-factor(result_strate_cat_tem_short_5year2$dataset,levels=c("total","cvd","resp","suicide"))
result_strate_cat_tem_short_5year2$tem<-factor(result_strate_cat_tem_short_5year2$tem,levels=c("heat_events","cold_events"))
result_strate_cat_tem_short_5year2<-result_strate_cat_tem_short_5year2 %>% 
  arrange(dataset,space,tem, fenzu)
#
for (i in seq(1, nrow(result_strate_cat_tem_short_5year2), by = 2)) {
  # 提取两行 beta 和 se
  beta_pair <-result_strate_cat_tem_short_5year2$Estimate[i:(i+1)]
  se_pair <- result_strate_cat_tem_short_5year2$Std..Error[i:(i+1)]
  result <- heterogeneity(beta_pair, se_pair)
  result_strate_cat_tem_short_5year2$p_value[i:(i+1)] <- result$p
}
write.csv(result_strate_cat_tem_short_5year2,"E:/deskbook/Result_China/figure_raw2/heterogeneity_test_result_result_strate_cat_5year_short_tem.csv",row.names = F)

