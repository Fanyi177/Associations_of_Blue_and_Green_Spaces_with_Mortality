#PAF####
#读取数据####
rm(list=ls())
gc()
diseases2_total<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_total_city.rds")
diseases2_cvd2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_cvd2_city.rds")
diseases2_suicides2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_suicides2_city.rds")
diseases2_resp2<-readRDS("E:/PKU_fanyi/Pro1/data_city/diseases2_resp2_city.rds")
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
#####cat####
#####1.long:pollution#####
set.seed(123)
calc_PAF_cat_direct <- function(data, pollution) {
  formula<-as.formula(paste("n ~",pollution,"+ns(dead_year,1)",
                            "+as.factor(GDP_avg_quintile)",
                            "+offset(lpopulation)",
                            "+t2m_mean_summer","+t2m_mean_winter","+t2m_sd_summer","+t2m_sd_winter",
                            "+rh_mean_summer","+rh_mean_winter","+rh_sd_summer","+rh_sd_winter",
                            "+sex_male_proportion","+age_less_75_proportion","+marital_married_proportion","+education0_proportion",
                            "+s(Climate_cat_code,bs='re')",
                            sep = ""))
  fit <- try(gam(formula, data = data, family = quasipoisson,na.action = na.omit,method = "REML"), silent = TRUE)
  if (inherits(fit, "try-error")) return(NA)
  
  # 提取模型系数
  coef_fit <- coef(fit)
  se_fit<-sqrt(diag(vcov(fit)))
  beta_pollution <- coef_fit[pollution]
  se_pollution <- se_fit[pollution]
  # 计算参考分类的中位数暴露水平
  reference_exp <- median(data[[pollution]][data[[paste0(pollution, "_quantile")]] == 1], na.rm = TRUE)
  
  RRs_result <- lapply(1:10, function(cat) {
    if (cat==1) {
      return(list(RR=1,CI_Lower=1,CI_upper=1))
    }
    median_exp <- median(data[[pollution]][data[[paste0(pollution, "_quantile")]] == cat], na.rm = TRUE)
    if (is.na(median_exp) || is.na(reference_exp)) return(NA)
    log_rr=beta_pollution * (median_exp - reference_exp)
    list(RR=exp(log_rr),CI_Lower=exp(log_rr-1.96*se_pollution),CI_upper=exp(log_rr+1.96*se_pollution))
  })
  
  category_proportions <- prop.table(table(data[[paste0(pollution, "_quantile")]]))
  
  # 计算PAF
  PAFs <- (category_proportions * (sapply(RRs_result,`[[`,"RR") - 1)) / 
    (category_proportions * (sapply(RRs_result,`[[`,"RR") - 1) + 1)
  PAFs_lower <- (category_proportions * (sapply(RRs_result,`[[`,"CI_Lower") - 1)) / 
    (category_proportions * (sapply(RRs_result,`[[`,"CI_Lower") - 1) + 1)
  PAFs_upper <- (category_proportions * (sapply(RRs_result,`[[`,"CI_upper") - 1)) / 
    (category_proportions * (sapply(RRs_result,`[[`,"CI_upper") - 1) + 1)
  # 返回平均PAF
  list(
    PAF=mean(PAFs,na.rm=T),
    PAF_lower=mean(PAFs_lower ,na.rm=T),
    PAF_upper=mean(PAFs_upper ,na.rm=T)
  )
}
#
PAF_long_cat_pollution<- list()
count=0
datasets <- list(
  total=diseases2_total,
  cvd2=diseases2_cvd2,
  resp=diseases2_resp2,
  suicide=diseases2_suicides2
)
# 遍历每个数据集
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 获取当前数据集
  results <- list() 
  data<-data %>% 
    mutate(
      avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
      avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
      avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
      
      avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
      avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
      avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
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
  for(blue_green_ in c("avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                       "avg_water_wetland_fraction_5year_scaled_2group")){
    for (fenzu in 1:2) {
      # 筛选当前组的数据
      data1 <- subset(data, data[[blue_green_]] == fenzu)
      for (pollution_ in c("PM1_1year","PM2.5_1year","PM10_1year")){
        data1[[paste0(pollution_ , "_quantile")]] <- ntile(data1[[pollution_ ]], 10)
        #
        result_no_interaction<-calc_PAF_cat_direct(data1,pollution_)
        
        
        # 将结果存入列表，确保唯一标识
        results[[paste0(pollution_, "_", blue_green_, "_group", fenzu)]] <- data.frame(
          Dataset = dataset_name,
          Blue_Green_Space = blue_green_,
          Group = fenzu,
          Pollution = pollution_,
          Mean_PAF = result_no_interaction$PAF,
          PAF_Lower = result_no_interaction$PAF_lower,
          PAF_Upper = result_no_interaction$PAF_upper
        )
        count=count+1
        print(paste0("short_term_PAF_cat:",count))
      }
    }
  }
  
  # 将当前数据集的结果存入 all_results
  PAF_long_cat_pollution[[dataset_name]] <- do.call(rbind, results)
}

# 合并所有数据集的结果
PAF_long_cat_pollution <- do.call(rbind, PAF_long_cat_pollution)
write.csv(PAF_long_cat_pollution,"E:/PKU_fanyi/Pro1/result_long/PAF_long_cat_pollution.csv")

#####2.short:pollution#####
set.seed(123)
calc_PAF_cat_direct <- function(data, pollution) {
  formula <-as.formula(paste("n ~",pollution,"+as.factor(dow)",
                             "+as.factor(season)",
                             "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                             "+as.factor(Climate_cat_code)",
                             sep = ""))
  fit <- try(gam(formula, data = data, family = quasipoisson,na.action = na.omit), silent = TRUE)
  if (inherits(fit, "try-error")) return(NA)
  
  # 提取模型系数
  coef_fit <- coef(fit)
  se_fit<-sqrt(diag(vcov(fit)))
  beta_pollution <- coef_fit[pollution]
  se_pollution <- se_fit[pollution]
  # 计算参考分类的中位数暴露水平
  reference_exp <- median(data[[pollution]][data[[paste0(pollution, "_quantile")]] == 1], na.rm = TRUE)
  
  RRs_result <- lapply(1:10, function(cat) {
    if (cat==1) {
      return(list(RR=1,CI_Lower=1,CI_upper=1))
    }
    median_exp <- median(data[[pollution]][data[[paste0(pollution, "_quantile")]] == cat], na.rm = TRUE)
    if (is.na(median_exp) || is.na(reference_exp)) return(NA)
    log_rr=beta_pollution * (median_exp - reference_exp)
    list(RR=exp(log_rr),CI_Lower=exp(log_rr-1.96*se_pollution),CI_upper=exp(log_rr+1.96*se_pollution))
  })
  
  category_proportions <- prop.table(table(data[[paste0(pollution, "_quantile")]]))
  
  # 计算PAF
  PAFs <- (category_proportions * (sapply(RRs_result,`[[`,"RR") - 1)) / 
    (category_proportions * (sapply(RRs_result,`[[`,"RR") - 1) + 1)
  PAFs_lower <- (category_proportions * (sapply(RRs_result,`[[`,"CI_Lower") - 1)) / 
    (category_proportions * (sapply(RRs_result,`[[`,"CI_Lower") - 1) + 1)
  PAFs_upper <- (category_proportions * (sapply(RRs_result,`[[`,"CI_upper") - 1)) / 
    (category_proportions * (sapply(RRs_result,`[[`,"CI_upper") - 1) + 1)
  # 返回平均PAF
  list(
    PAF=mean(PAFs,na.rm=T),
    PAF_lower=mean(PAFs_lower ,na.rm=T),
    PAF_upper=mean(PAFs_upper ,na.rm=T)
  )
}
#
PAF_short_cat_pollution<- list()
count=0
datasets <- list(
  total_short=diseases2_total_short,
  cvd2_short=diseases2_cvd2_short,
  resp_short=diseases2_resp2_short,
  suicide_short=diseases2_suicides2_short
)
# 遍历每个数据集
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 获取当前数据集
  results <- list() 
  data<-data %>% 
    mutate(
      avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
      avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
      avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
      
      avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
      avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
      avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
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
  for(blue_green_ in c("avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                       "avg_water_wetland_fraction_5year_scaled_2group")){
    for (fenzu in 1:2) {
      # 筛选当前组的数据
      data1 <- subset(data, data[[blue_green_]] == fenzu)
      for (pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")){
        data1[[paste0(pollution_, "_quantile")]] <- ntile(data1[[pollution_]], 10)
        #
        result_no_interaction<-calc_PAF_cat_direct(data1,pollution_)
        
        
        # 将结果存入列表，确保唯一标识
        results[[paste0(pollution_, "_", blue_green_, "_group", fenzu)]] <- data.frame(
          Dataset = dataset_name,
          Blue_Green_Space = blue_green_,
          Group = fenzu,
          Pollution = pollution_,
          Mean_PAF = result_no_interaction$PAF,
          PAF_Lower = result_no_interaction$PAF_lower,
          PAF_Upper = result_no_interaction$PAF_upper
        )
        count=count+1
        print(paste0("short_term_PAF_cat:",count))
      }
    }
  }
  
  # 将当前数据集的结果存入 all_results
  PAF_short_cat_pollution[[dataset_name]] <- do.call(rbind, results)
}

# 合并所有数据集的结果
PAF_short_cat_pollution <- do.call(rbind, PAF_short_cat_pollution)
write.csv(PAF_short_cat_pollution,"E:/PKU_fanyi/Pro1/result_long/PAF_short_cat_pollution.csv")

#####3.short:tem#####
set.seed(123)
calc_PAF_cat_direct <- function(data,tem) {
  
  formula <-as.formula(paste("n ~",tem,"+as.factor(dow)",
                             "+as.factor(season)",
                             "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                             "+as.factor(Climate_cat_code)",
                             sep = ""))
  
  # 拟合 GAM 模型
  fit <- try(gam(formula, data = data, family = quasipoisson), silent = TRUE)
  
  # 检查是否成功拟合模型
  if (inherits(fit, "try-error")) return(NA)
  
  # 提取模型系数
  coef_fit <- coef(fit)
  se_fit<-sqrt(diag(vcov(fit)))
  beta_tem <- coef_fit[tem]
  se_tem <- se_fit[tem]
  
  RR<-exp(beta_tem)
  RR_lower<-exp(beta_tem-1.96*se_tem)
  RR_upper<-exp(beta_tem+1.96*se_tem)
  
  tem_proportion <- mean(data[[tem]]==1,na.rm=T)
  
  # 计算PAF
  list(
    PAF=(tem_proportion * (RR - 1)) / 
      (tem_proportion * (RR - 1) + 1),
    PAF_lower=(tem_proportion * (RR_lower - 1)) / 
      (tem_proportion * (RR_lower - 1) + 1),
    PAF_upper=(tem_proportion * (RR_upper - 1)) / 
      (tem_proportion * (RR_upper - 1) + 1)
  )
}
#
PAF_short_cat_tem<- list()
count=0
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
# 遍历每个数据集
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]  # 获取当前数据集
  results <- list() 
  data<-data %>% 
    mutate(
      avg_forest_shrub_grassland_fraction_1year_scaled=scale(avg_forest_shrub_grassland_fraction_1year),
      avg_forest_shrub_grassland_fraction_3year_scaled=scale(avg_forest_shrub_grassland_fraction_3year),
      avg_forest_shrub_grassland_fraction_5year_scaled=scale(avg_forest_shrub_grassland_fraction_5year),
      
      avg_water_wetland_fraction_1year_scaled=scale(avg_water_wetland_fraction_1year),
      avg_water_wetland_fraction_3year_scaled=scale(avg_water_wetland_fraction_3year),
      avg_water_wetland_fraction_5year_scaled=scale(avg_water_wetland_fraction_5year)
    )
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
  for(blue_green_ in c("avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                       "avg_water_wetland_fraction_5year_scaled_2group")){
    for (fenzu in 1:2) {
      # 筛选当前组的数据
      data1 <- subset(data, data[[blue_green_]] == fenzu)
      for (tem_ in colnames(data)[c(101)]) {
        if (all(data1[[tem_]]==0,na.rm=T)) {
          next
        }
        result_no_interaction<-calc_PAF_cat_direct(data1,tem_)
        
        
        # 将结果存入列表，确保唯一标识
        results[[paste0(tem_, "_", blue_green_, "_group", fenzu)]] <- data.frame(
          Dataset = dataset_name,
          Blue_Green_Space = blue_green_,
          Group = fenzu,
          tem = tem_,
          Mean_PAF = result_no_interaction$PAF,
          PAF_Lower = result_no_interaction$PAF_lower,
          PAF_Upper = result_no_interaction$PAF_upper
        )
        count=count+1
        print(paste0("short_term_PAF_cat:",count))
      }
    }
  }
  
  # 将当前数据集的结果存入 all_results
  PAF_short_cat_tem[[dataset_name]] <- do.call(rbind, results)
}

# 合并所有数据集的结果
PAF_short_cat_tem <- do.call(rbind, PAF_short_cat_tem)
write.csv(PAF_short_cat_tem,"E:/PKU_fanyi/Pro1/result_long/PAF_short_cat_tem.csv")
#####4.short:pollution_province####
set.seed(123)
calc_PAF_cat_direct <- function(data, pollution) {
  include_dow<-ifelse(length(unique(data$dow))>1,"+as.factor(dow)","")
  include_season<-ifelse(length(unique(data$season))>1,"+as.factor(season)","")
  formula <-as.formula(paste("n ~",pollution,include_dow,
                             include_season,
                             "+ns(lag03_rh,3)","+ns(lag03_t2m,df=6)","+ns(time,df=7*8)",
                             #"+as.factor(Climate_cat_code)",#
                             sep = ""))
  fit <- try(gam(formula, data = data, family = quasipoisson,na.action = na.omit), silent = TRUE)
  if (inherits(fit, "try-error")) {
    cat ("Error in gam fitting for pollution=",pollution,"\n")
    
    return(NA)
  }
  
  # 提取模型系数
  coef_fit <- coef(fit)
  se_fit<-sqrt(diag(vcov(fit)))
  beta_pollution <- coef_fit[pollution]
  se_pollution <- se_fit[pollution]
  cat ("beta_pollution=",beta_pollution,"\n")
  cat ("se_pollution=",se_pollution,"\n")
  # 计算参考分类的中位数暴露水平
  reference_exp <- median(data[[pollution]][data[[paste0(pollution, "_quantile")]] == 1], na.rm = TRUE)
  
  RRs_result <- lapply(1:10, function(cat) {
    if (cat==1) {
      return(list(RR=1,CI_Lower=1,CI_upper=1))
    }
    median_exp <- median(data[[pollution]][data[[paste0(pollution, "_quantile")]] == cat], na.rm = TRUE)
    if (is.na(median_exp) || is.na(reference_exp)) return(NA)
    log_rr=beta_pollution * (median_exp - reference_exp)
    list(RR=exp(log_rr),CI_Lower=exp(log_rr-1.96*se_pollution),CI_upper=exp(log_rr+1.96*se_pollution))
  })
  
  category_proportions <- prop.table(table(data[[paste0(pollution, "_quantile")]]))
  
  # 计算PAF
  PAFs <- (category_proportions * (sapply(RRs_result,`[[`,"RR") - 1)) / 
    (category_proportions * (sapply(RRs_result,`[[`,"RR") - 1) + 1)
  PAFs_lower <- (category_proportions * (sapply(RRs_result,`[[`,"CI_Lower") - 1)) / 
    (category_proportions * (sapply(RRs_result,`[[`,"CI_Lower") - 1) + 1)
  PAFs_upper <- (category_proportions * (sapply(RRs_result,`[[`,"CI_upper") - 1)) / 
    (category_proportions * (sapply(RRs_result,`[[`,"CI_upper") - 1) + 1)
  # 返回平均PAF
  PAF=mean(PAFs,na.rm=T)
  PAF_lower=mean(PAFs_lower ,na.rm=T)
  PAF_upper=mean(PAFs_upper ,na.rm=T)
  list(
    PAF=PAF,
    PAF_lower=PAF_lower,
    PAF_upper=PAF_upper
  )
}
#
PAF_short_cat_pollution_province<- list()
count=0
datasets <- list(
  total_short=diseases2_total_short,
  cvd2_short=diseases2_cvd2_short,
  resp_short=diseases2_resp2_short,
  suicide_short=diseases2_suicides2_short
)
# 遍历每个数据集
PAF_short_cat_pollution_province <-data.frame()
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
  provinces<-unique(data$province_code)
  for (province_ in provinces){
    print(province_)
    data_province<-subset(data,province_code==province_)
    print(nrow(data_province))
    if(nrow(data_province)==0) next
    
    for(blue_green_ in c("avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                         "avg_water_wetland_fraction_5year_scaled_2group")){
      for (fenzu in 1:2) {
        # 筛选当前组的数据
        data1 <- subset(data_province, data_province[[blue_green_]] == fenzu)
        if (nrow(data1)==0) next
        for (pollution_ in c("lag01_PM1","lag01_PM2.5","lag01_PM10")){
          data1[[paste0(pollution_, "_quantile")]] <- ntile(data1[[pollution_]], 10)
          #
          result_no_interaction<-calc_PAF_cat_direct(data1,pollution_)
          
          result <- data.frame(
            Dataset = dataset_name,
            province=province_,
            Pollution = pollution_,
            Green_Space = blue_green_,
            fenzu_=fenzu,
            Mean_PAF = result_no_interaction$PAF,
            PAF_Lower = result_no_interaction$PAF_lower,
            PAF_Upper = result_no_interaction$PAF_upper
          )
          PAF_short_cat_pollution_province <-rbind(PAF_short_cat_pollution_province,result)
          
          
        }
      }
    }
    count=count+1
    print(count)
  }
  
}

# 合并所有数据集的结果
write.csv(PAF_short_cat_pollution_province,"E:/PKU_fanyi/Pro1/result_long/PAF_short_cat_pollution_province.csv")

#####5.short:tem_province####
set.seed(123)
calc_PAF_cat_direct <- function(data,tem) {
  include_dow<-ifelse(length(unique(data$dow))>1,"+as.factor(dow)","")
  include_season<-ifelse(length(unique(data$season))>1,"+as.factor(season)","")
  formula <-as.formula(paste("n ~",tem,include_dow,
                             include_season,
                             "+ns(lag03_rh,3)","+lag01_PM2.5","+ns(time,df=7*8)",
                             #"+as.factor(Climate_cat_code)",#
                             sep = ""))
  
  # 拟合 GAM 模型
  fit <- try(gam(formula, data = data, family = quasipoisson), silent = TRUE)
  
  # 检查是否成功拟合模型
  if (inherits(fit, "try-error")) return(NA)
  
  # 提取模型系数
  coef_fit <- coef(fit)
  se_fit<-sqrt(diag(vcov(fit)))
  beta_tem <- coef_fit[tem]
  se_tem <- se_fit[tem]
  cat ("beta_tem=",beta_tem,"\n")
  cat ("se_tem=",se_tem,"\n")
  RR<-exp(beta_tem)
  RR_lower<-exp(beta_tem-1.96*se_tem)
  RR_upper<-exp(beta_tem+1.96*se_tem)
  
  tem_proportion <- mean(data[[tem]]==1,na.rm=T)
  
  # 计算PAF
  list(
    PAF=(tem_proportion * (RR - 1)) / 
      (tem_proportion * (RR - 1) + 1),
    PAF_lower=(tem_proportion * (RR_lower - 1)) / 
      (tem_proportion * (RR_lower - 1) + 1),
    PAF_upper=(tem_proportion * (RR_upper - 1)) / 
      (tem_proportion * (RR_upper - 1) + 1)
  )
}
#
PAF_short_cat_tem_province<- list()
count=0
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
# 遍历每个数据集
# 遍历每个数据集
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
  provinces<-unique(data$province_code)
  for (province_ in provinces){
    print(province_)
    data_province<-subset(data,province_code==province_)
    print(nrow(data_province))
    if(nrow(data_province)==0) next
    
    for(blue_green_ in c("avg_forest_shrub_grassland_fraction_5year_scaled_2group",
                         "avg_water_wetland_fraction_5year_scaled_2group")){
      for (fenzu in 1:2) {
        # 筛选当前组的数据
        data1 <- subset(data_province, data_province[[blue_green_]] == fenzu)
        if (nrow(data1)==0) next
        for (tem_ in colnames(data)[c(101)]){
          if (all(data1[[tem_]]==0,na.rm=T)) {
            next
          }
          #
          result_no_interaction<-calc_PAF_cat_direct(data1,tem_)
          
          result <- data.frame(
            Dataset = dataset_name,
            province=province_,
            tem = tem_,
            Green_Space = blue_green_,
            fenzu_=fenzu,
            Mean_PAF = result_no_interaction$PAF,
            PAF_Lower = result_no_interaction$PAF_lower,
            PAF_Upper = result_no_interaction$PAF_upper
          )
          PAF_short_cat_tem_province<-rbind(PAF_short_cat_tem_province,result)
        }
      }
    }
    count=count+1
    print(count)
    
  }
}
#
write.csv(PAF_short_cat_tem_province,"E:/PKU_fanyi/Pro1/result_long/PAF_short_cat_tem_province.csv")
#####整理PAF结果####
#生成PAF的difference#####
####long_continous####
PAF_long_cat_pollution<-read.csv("E:/Deskbook/Result_China/new/result_long/PAF_long_cat_pollution.csv")
PAF_long_cat_pollution<-PAF_long_cat_pollution %>% 
  mutate(data="Long_term")
PAF_short_cat_pollution<-read.csv("E:/Deskbook/Result_China/new/result_long/PAF_short_cat_pollution.csv")
PAF_short_cat_pollution<-PAF_short_cat_pollution %>% 
  mutate(data="short_term")
PAF_short_cat_tem<-read.csv("E:/Deskbook/Result_China/new/result_long/PAF_short_cat_tem.csv")
PAF_short_cat_tem<-PAF_short_cat_tem %>% 
  mutate(data="short_term")
colnames(PAF_short_cat_tem)<-names(PAF_short_cat_pollution)
table(PAF_short_cat_pollution$Dataset)
table(PAF_long_cat_pollution$Dataset)
table(PAF_short_cat_tem$Dataset)
table(PAF_long_cat_pollution$Pollution)
#
PAF_long_cat_pollution$Pollution<-gsub("PM1_1year","PM1",PAF_long_cat_pollution$Pollution)
PAF_long_cat_pollution$Pollution<-gsub("PM2.5_1year","PM2.5",PAF_long_cat_pollution$Pollution)
PAF_long_cat_pollution$Pollution<-gsub("PM10_1year","PM10",PAF_long_cat_pollution$Pollution)

#
PAF_short_cat_pollution$Dataset<-gsub("total_short","total",PAF_short_cat_pollution$Dataset)
PAF_short_cat_pollution$Dataset<-gsub("cvd2_short","cvd2",PAF_short_cat_pollution$Dataset)
PAF_short_cat_pollution$Dataset<-gsub("resp_short","resp",PAF_short_cat_pollution$Dataset)
PAF_short_cat_pollution$Dataset<-gsub("suicide_short","suicide",PAF_short_cat_pollution$Dataset)
#
PAF_short_cat_pollution$Pollution<-gsub("lag01_PM1","PM1",PAF_short_cat_pollution$Pollution)
PAF_short_cat_pollution$Pollution<-gsub("lag01_PM2.5","PM2.5",PAF_short_cat_pollution$Pollution)
PAF_short_cat_pollution$Pollution<-gsub("lag01_PM10","PM10",PAF_short_cat_pollution$Pollution)
#
PAF_short_cat_tem$Dataset<-gsub("total_short_heat","total",PAF_short_cat_tem$Dataset)
PAF_short_cat_tem$Dataset<-gsub("total_short_cold","total",PAF_short_cat_tem$Dataset)
PAF_short_cat_tem$Dataset<-gsub("cvd2_short_heat","cvd2",PAF_short_cat_tem$Dataset)
PAF_short_cat_tem$Dataset<-gsub("cvd2_short_cold","cvd2",PAF_short_cat_tem$Dataset)
PAF_short_cat_tem$Dataset<-gsub("resp2_short_heat","resp",PAF_short_cat_tem$Dataset)
PAF_short_cat_tem$Dataset<-gsub("resp2_short_cold","resp",PAF_short_cat_tem$Dataset)
PAF_short_cat_tem$Dataset<-gsub("suicides2_short_heat","suicide",PAF_short_cat_tem$Dataset)
PAF_short_cat_tem$Dataset<-gsub("suicides2_short_cold","suicide",PAF_short_cat_tem$Dataset)
merged_PAF_data<-rbind(PAF_long_cat_pollution,PAF_short_cat_pollution,PAF_short_cat_tem)
colnames(merged_PAF_data)
#
table(merged_PAF_data$Pollution)
table(merged_PAF_data$Blue_Green_Space)
merged_PAF_data$Dataset<-factor(merged_PAF_data$Dataset,levels = c("total","cvd2","resp","suicide"))
merged_PAF_data$Pollution<-factor(merged_PAF_data$Pollution,levels = c("PM1","PM2.5","PM10","heat_events","cold_events"))
merged_PAF_data$Blue_Green_Space<-factor(merged_PAF_data$Blue_Green_Space,levels = c("avg_water_wetland_fraction_5year_scaled_2group","avg_forest_shrub_grassland_fraction_5year_scaled_2group"))

merged_PAF_data<- merged_PAF_data %>% 
  mutate(PAF_95CI= paste0(round0(Mean_PAF*100, 2), " (",round(PAF_Lower*100, 2), ", ", round(PAF_Upper*100, 2), ")")
  )

merged_PAF_data <- merged_PAF_data%>%
  dplyr::group_by(Dataset,data,Pollution, Blue_Green_Space) %>%
  dplyr::mutate(
    PAF_1 = Mean_PAF[Group == 1],
    PAF_2 = Mean_PAF[Group == 2]
  ) %>%
  dplyr::mutate(
    PAF_difference = PAF_1 - PAF_2,
    PAF_difference_change_rate = as.numeric(round0((PAF_difference / abs(PAF_1)) * 100, 2)) # 变化率基于绝对值
  ) %>%
  dplyr::ungroup()

write.csv(merged_PAF_data,"E:/Deskbook/Result_China/merged_PAF_data.csv",row.names = F)