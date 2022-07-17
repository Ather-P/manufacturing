
setwd("D:\\Course\\R\\Manufacturing")

ld_train = read.csv("product_train.csv", stringsAsFactors = F)
ld_test = read.csv("product_test.csv", stringsAsFactors = F)

library(dplyr)
library(tidyr)
library(visdat)
library(randomForest)
library(randomForestSRC)

glimpse(ld_train)


lapply(ld_train, function(x) sum(is.na(ld_train)))

lapply(ld_test, function(x) sum(is.na(ld_test)))

ld_test$went_on_backorder = NA
ld_train$data = "train"
ld_test$data = "test"

ld_all = rbind(ld_train, ld_test)

glimpse(ld_all)


convert_numeric = c("potential_issue","oe_constraint","ppap_risk","stop_auto_buy","rev_stop","went_on_backorder","deck_risk")

for(i in convert_numeric){
  ld_all[,i] = as.numeric(ld_all[,i] == "Yes")
}

glimpse(ld_all)

ld_all$sku = NULL



library(ggplot2)

# p = ggplot(ld_all, aes(x=went_on_backorder, y=national_inv))
# 
# p+geom_boxplot()


ld_all = ld_all %>% 
  mutate(inv_less_than_0 = as.numeric(national_inv <= 0),
         inv_0_to_1000 = as.numeric(national_inv < 1000 & national_inv >0),
         inv_greater_than_1000 = as.numeric(national_inv > 1000)) %>% 
  select(-national_inv)
         

# q = ggplot(ld_all, aes(x=went_on_backorder, y = lead_time)) + geom_point()


high <- mean(ld_all$lead_time) + sd(ld_all$lead_time) * 1.5
low <- mean(ld_all$lead_time) - sd(ld_all$lead_time) * 1.5
ld_all$Outlier <- (ld_all$lead_time < low | ld_all$lead_time > high)
table(ld_all$Outlier)
outlier_range = round(mean(ld_all$lead_time[!ld_all$lead_time>high]))
ld_all$lead_time[ld_all$Outlier] = outlier_range
ld_all$Outlier = NULL

# r = ggplot(ld_all, aes(x=went_on_backorder, y = in_transit_qty)) + geom_point()

high1 <- mean(ld_all$in_transit_qty) + sd(ld_all$in_transit_qty) * 1.5
low1 <- mean(ld_all$in_transit_qty) - sd(ld_all$in_transit_qty) * 1.5
ld_all$Outlier <- (ld_all$in_transit_qty < low1 | ld_all$in_transit_qty > high1)
table(ld_all$Outlier)
outlier_range1 = round(mean(ld_all$in_transit_qty[!ld_all$in_transit_qty>high1]))
ld_all$in_transit_qty[ld_all$Outlier] = outlier_range1
ld_all$Outlier = NULL


outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}


ld_all$abc=outliers(ld_all$forecast_3_month)
table(abc)
ld_all$forecast_3_month[ld_all$abc]=Modes(ld_all$forecast_3_month)
ld_all = ld_all %>% 
  select(-abc)

ld_all$abc=outliers(ld_all$forecast_6_month)
table(abc)
ld_all$forecast_6_month[ld_all$abc]=Modes(ld_all$forecast_6_month)
ld_all = ld_all %>% 
  select(-abc)

ld_all$abc=outliers(ld_all$forecast_9_month)
table(abc)
ld_all$forecast_9_month[ld_all$abc]=Modes(ld_all$forecast_9_month)
ld_all = ld_all %>% 
  select(-abc)

ld_all$abc=outliers(ld_all$sales_1_month)
table(abc)
ld_all$sales_1_month[ld_all$abc]=round(mean(ld_all$sales_1_month))
ld_all = ld_all %>% 
  select(-abc)

ld_all$abc=outliers(ld_all$sales_3_month)
table(abc)
ld_all$sales_3_month[ld_all$abc]=round(mean(ld_all$sales_3_month))
ld_all = ld_all %>% 
  select(-abc)

ld_all$abc=outliers(ld_all$sales_6_month)
table(abc)
ld_all$sales_6_month[ld_all$abc]=round(mean(ld_all$sales_6_month))
ld_all = ld_all %>% 
  select(-abc)

ld_all$abc=outliers(ld_all$sales_9_month)
table(abc)
ld_all$sales_9_month[ld_all$abc]=round(mean(ld_all$sales_9_month))
ld_all = ld_all %>% 
  select(-abc)

table(ld_all$pieces_past_due)

ld_all = ld_all %>% 
  mutate(pieces_past_due_0 = as.numeric(pieces_past_due != 0)) %>% 
  select(-pieces_past_due)
  
table(outliers(ld_all$min_bank))

mean(ld_all$min_bank)

ld_all$abc=outliers(ld_all$min_bank)
table(abc)
ld_all$min_bank[ld_all$abc]=round(mean(ld_all$min_bank))
ld_all = ld_all %>% 
  select(-abc)

table(ld_all$perf_12_month_avg)

# vis_dat(ld_all, warn_large_data = F)


lgr_train = ld_all[ld_all$data=="train",]
lgr_test = ld_all[ld_all$data=="test",]

lgr_train$data = NULL
lgr_test$data = NULL
lgr_test$went_on_backorder = NULL

# o <- tune(went_on_backorder~., rf_train, doBest = TRUE)
# 
# o$optimal
# 
# 
# o
# best_params=data.frame(mtry=4,
#                        ntree=500,
#                        maxnodes=272,
#                        nodesize=3)
# 
# 
# rf_train$went_on_backorder = as.factor(rf_train$went_on_backorder)
# 
# 
# ld.rf.final=randomForest(went_on_backorder~.,
#                          mtry=best_params$mtry,
#                          ntree=best_params$ntree,
#                          maxnodes=best_params$maxnodes,
#                          nodesize=best_params$nodesize,
#                          data=rf_train)
# 
# test.predict = predict(ld.rf.final, newdata = rf_test)
# 
# 
# x=data.frame(a=1:62520)
# x$backorder =  test.predict
# 
# x$a = NULL
# table(x)
# library(dgof)
# xy = as.numeric(x)
# shapiro.test(x)

library(pROC)
library(car)

set.seed(2)
s=sample(1:nrow(lgr_train),0.8*nrow(lgr_train))
lgr_train1=lgr_train[s,] ## 80% Train data
lgr_train2=lgr_train[-s,] ## 20% Train Test Data

for_vif=lm(went_on_backorder~.,data = lgr_train1)

sort(vif(for_vif), decreasing = T)[1:3]

log_fit = glm(went_on_backorder~., data= lgr_train1)


log_fit = step(log_fit)
formula(log_fit)
summary(log_fit)

log_fit = glm(went_on_backorder ~ lead_time + in_transit_qty + forecast_3_month + 
                forecast_6_month + forecast_9_month + sales_1_month + sales_3_month + 
                sales_9_month + min_bank + perf_6_month_avg + local_bo_qty + 
                deck_risk + ppap_risk + inv_less_than_0 + inv_0_to_1000 + 
                pieces_past_due_0, data= lgr_train1, family = 'binomial')

summary(log_fit)


val.score=predict(log_fit,newdata = lgr_train2, type='response')

auc(roc(lgr_train2$went_on_backorder,val.score))


log_fit_final= glm(went_on_backorder ~ lead_time + in_transit_qty + forecast_3_month + 
                     forecast_6_month + forecast_9_month + sales_1_month + sales_3_month + 
                     sales_9_month + min_bank + perf_6_month_avg + local_bo_qty + 
                     deck_risk + ppap_risk + inv_less_than_0 + inv_0_to_1000 + 
                     pieces_past_due_0, data= lgr_train, family = 'binomial')

summary(log_fit_final)

test_prob_score = predict(log_fit_final, newdata = lgr_test, type='response',row.names= F )


train.score=predict(log_fit_final,newdata = lgr_train,type='response')

real=lgr_train$went_on_backorder

cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99999,Sn=99999,Sp=99999,KS=9999,F5=9999,F.1=9999,M=9999)

for(cutoff in cutoffs){
  
  ## Conversion into hard calsses
  predicted=as.numeric(train.score>cutoff)
  
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(100*FP+TP)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]

final_cutoff = 0.009

test_prob_score[test_prob_score>=0.009]="Yes"
test_prob_score[test_prob_score<0.009]="No"

table(test_prob_score)

write.csv(test_prob_score,"Atahar_Budihal_P3_part2.csv", row.names = F)


