# June, july, august are summer months   6,7,8
# Boston is normally free of snow every year from May to October. 5,6,7,8,9,10

# Using cohen kappa metric for evaluation to address target imbalance

df = read.csv(file.choose(), header = T)

library(tidyr)
library(dplyr)
library(purrr)
library(rlang)


#checking missing values
sapply(df,function(x) sum(is.na(x))) 

# lag_func <- function(data, variable, n=3){
#   variable <- enquo(variable)
#   
#   indices <- seq_len(n)
#   quosures <- map( indices, ~quo(lag(!!variable, !!.x)) ) %>% 
#     set_names(sprintf("%s_lag_%02d", quo_text(variable),indices))
#   
#   mutate( data, !!!quosures )
#   
# }
# lag_func(df[,4:6], Avg.Temp..F., 3) """
# """
# 
# library(DataCombine)
# 
# DataSlid1 <- slide(df[,4:6], Var = colnames(df)[4], slideBy = -1)"""


lags <- function(var, n=10){
  var <- enquo(var)
  
  indices <- seq_len(n)
  map( indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("lag_%s_%02d", quo_text(var), indices))
  
}
colnames(df)
# // 
#   """
#   "Year"                      "Month"                     "Day"                      
#  "High.Temp..F."             "Avg.Temp..F."              "Low.Temp..F."             
#  "High.Dew.Point..F."        "Avg.Dew.Point..F."         "Low.Dew.Point..F."        
#  "High.Humidity...."         "Avg.Humidity...."          "Low.Humidity...."         
#  "High.Sea.Level.Press..in." "Avg.Sea.Level.Press..in."  "Low.Sea.Level.Press..in." 
#  "High.Visibility..mi."      "Avg.Visibility..mi."       "Low.Visibility..mi."      
#  "High.Wind..mph."           "Avg.Wind..mph."            "High.Wind.Gust..mph."     
#  "Target"  
# """
# //


new_df = df %>% mutate( !!!lags(High.Temp..F., 3) , !!!lags(Avg.Temp..F., 3), !!!lags(Low.Temp..F., 3),
                        !!!lags(High.Dew.Point..F., 3), !!!lags(Avg.Dew.Point..F., 3), !!!lags(Low.Dew.Point..F., 3),
                        !!!lags(High.Humidity...., 3), !!!lags(Avg.Humidity...., 3), !!!lags(Low.Humidity...., 3),
                        !!!lags(High.Sea.Level.Press..in., 3), !!!lags(Avg.Sea.Level.Press..in., 3), !!!lags(Low.Sea.Level.Press..in., 3),
                        !!!lags(High.Visibility..mi., 3), !!!lags(Avg.Visibility..mi., 3), !!!lags(Low.Visibility..mi., 3),
                        !!!lags(High.Wind..mph., 3), !!!lags(Avg.Wind..mph., 3), !!!lags(High.Wind.Gust..mph., 3),
                        !!!lags(Target, 3)
                        
)

colnames(new_df)

# x=df["Month"][df["Month"] = 1]


# df[ df["Month"] >= 5 & df["Month"] <=10,"Month"]

new_df$Non_month = 1

new_df[new_df["Month"] >= 5 & new_df["Month"] <=10,"Non_month"] = 0
# Flagging the months in which snowfall is expected

new_df = new_df[,4:80]
colnames(new_df)

library(caret)

# Normalizing the features in the range of 0 and 1
preprocessParams = preProcess(new_df[,-19], method=c("range"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed = predict(preprocessParams, new_df[,-19])
# summarize the transformed dataset
summary(transformed)

transformed$Target = new_df$Target
# Removing feature values of the same day's
transformed = transformed[,19:77]
# Implementing logistic regression for feature importance and selection
transformed$Target = as.factor(transformed$Target)

logistic_model = glm(Target ~., data = transformed, family = "binomial")
imp = as.data.frame(varImp(logistic_model))
imp = data.frame(overall = imp$Overall,
                 names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]
ordered_imp = imp[order(imp$overall,decreasing = T),]
top20_ordered_imp = ordered_imp[1:20,]



library(ggplot2)
options(repr.plot.width=8, repr.plot.height=3)
ggplot(top20_ordered_imp, aes(x = reorder(top20_ordered_imp$names, top20_ordered_imp$overall), y = top20_ordered_imp$overall, main="Var Importance")) +
  geom_bar(stat = "identity") +
  coord_flip() + scale_y_continuous(name="Value") +
  scale_x_discrete(name="varible name") +
  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))



# overall                            names
# 55 4.49264247                    lag_Target_01
# 58 3.93709493                        Non_month
# 28 3.79759717 lag_High.Sea.Level.Press..in._01
# 49 3.65800820            lag_Avg.Wind..mph._01
# 31 3.49084334  lag_Avg.Sea.Level.Press..in._01
# 34 3.20622037  lag_Low.Sea.Level.Press..in._01
# 40 3.14771664       lag_Avg.Visibility..mi._01
# 46 2.88907433           lag_High.Wind..mph._01
# 43 2.61066998       lag_Low.Visibility..mi._01
# 1  1.85789224             lag_High.Temp..F._01
# 54 1.80451634      lag_High.Wind.Gust..mph._03
# 7  1.74498884              lag_Low.Temp..F._01
# 26 1.48743363          lag_Low.Humidity...._02
# 20 1.39302870         lag_High.Humidity...._02
# 11 1.32317079        lag_High.Dew.Point..F._02
# 23 1.29545964          lag_Avg.Humidity...._02
# 14 1.20304906         lag_Avg.Dew.Point..F._02
# 4  1.15458351              lag_Avg.Temp..F._01
# 17 1.09518224         lag_Low.Dew.Point..F._02
# 12 1.06926226        lag_High.Dew.Point..F._03
# 9  1.02102382              lag_Low.Temp..F._03
# 30 0.99234962 lag_High.Sea.Level.Press..in._03
# 51 0.96747127            lag_Avg.Wind..mph._03
# 13 0.96206262         lag_Avg.Dew.Point..F._01
# 47 0.93009403           lag_High.Wind..mph._02
# 52 0.86912714      lag_High.Wind.Gust..mph._01
# 16 0.81113680         lag_Low.Dew.Point..F._01
# 39 0.78109448      lag_High.Visibility..mi._03
# 2  0.74366802             lag_High.Temp..F._02
# 8  0.72768956              lag_Low.Temp..F._02
# 15 0.71322357         lag_Avg.Dew.Point..F._03
# 3  0.70459484             lag_High.Temp..F._03
# 45 0.70059948       lag_Low.Visibility..mi._03
# 6  0.68485273              lag_Avg.Temp..F._03
# 5  0.68356108              lag_Avg.Temp..F._02
# 27 0.68219249          lag_Low.Humidity...._03
# 21 0.65454762         lag_High.Humidity...._03
# 48 0.62635233           lag_High.Wind..mph._03
# 44 0.62130115       lag_Low.Visibility..mi._02
# 24 0.60827825          lag_Avg.Humidity...._03
# 53 0.58692708      lag_High.Wind.Gust..mph._02
# 33 0.56424163  lag_Avg.Sea.Level.Press..in._03
# 56 0.51949782                    lag_Target_02
# 57 0.47869723                    lag_Target_03
# 35 0.47795402  lag_Low.Sea.Level.Press..in._02
# 25 0.41822571          lag_Low.Humidity...._01
# 42 0.40907310       lag_Avg.Visibility..mi._03
# 36 0.35914151  lag_Low.Sea.Level.Press..in._03
# 29 0.35835335 lag_High.Sea.Level.Press..in._02
# 19 0.34490338         lag_High.Humidity...._01
# 10 0.33482785        lag_High.Dew.Point..F._01
# 50 0.32515544            lag_Avg.Wind..mph._02
# 22 0.20908372          lag_Avg.Humidity...._01
# 32 0.20371501  lag_Avg.Sea.Level.Press..in._02
# 18 0.14778318         lag_Low.Dew.Point..F._03
# 37 0.08313864      lag_High.Visibility..mi._01
# 38 0.07677366      lag_High.Visibility..mi._02
# 41 0.05308270       lag_Avg.Visibility..mi._02


# Selecting top 20 features from the variable importance from logistic regression.

# lag_High.Dew.Point..F._03,lag_Low.Dew.Point..F._02,lag_Avg.Temp..F._01,
# lag_Avg.Dew.Point..F._02,lag_Avg.Humidity...._02,lag_High.Dew.Point..F._02,
# lag_High.Humidity...._02,lag_Low.Humidity...._02,lag_Low.Temp..F._01,
# lag_High.Wind.Gust..mph._03,lag_High.Temp..F._01,lag_Low.Visibility..mi._01,
# lag_High.Wind..mph._01,lag_Avg.Visibility..mi._01,lag_Low.Sea.Level.Press..in._01,
# lag_Avg.Sea.Level.Press..in._01,lag_Avg.Wind..mph._01,lag_High.Sea.Level.Press..in._01,
# Non_month,lag_Target_01


# Making a decsion tree model using only these features



# 3000 - number of rows for train data - 80% train , 20% test

train_data = transformed[1:3000,]
test_data = transformed[3001:3749,]

library(caret)
library(rpart)
library(e1071)

cpgrid = expand.grid(.cp = seq(0.01,0.5,0.01))

numfolds = trainControl(method = "cv", number =10)

train_decision_tree = train(Target~ lag_High.Dew.Point..F._03 + lag_Low.Dew.Point..F._02 + lag_Avg.Temp..F._01 + lag_Avg.Dew.Point..F._02 + lag_Avg.Humidity...._02 + lag_High.Dew.Point..F._02 + lag_High.Humidity...._02 + lag_Low.Humidity...._02 + lag_Low.Temp..F._01 + lag_High.Wind.Gust..mph._03 + lag_High.Temp..F._01 + lag_Low.Visibility..mi._01 + lag_High.Wind..mph._01 + lag_Avg.Visibility..mi._01 + lag_Low.Sea.Level.Press..in._01 + lag_Avg.Sea.Level.Press..in._01 + lag_Avg.Wind..mph._01 + lag_High.Sea.Level.Press..in._01 + Non_month + lag_Target_01,
                            
                            
                            data= na.omit(train_data),method="rpart", trControl= numfolds, tuneGrid = cpgrid)
train_decision_tree
# choosing cp =0.01

decision_tree_final = rpart(Target~ lag_High.Dew.Point..F._03 + lag_Low.Dew.Point..F._02 + lag_Avg.Temp..F._01 + lag_Avg.Dew.Point..F._02 + lag_Avg.Humidity...._02 + lag_High.Dew.Point..F._02 + lag_High.Humidity...._02 + lag_Low.Humidity...._02 + lag_Low.Temp..F._01 + lag_High.Wind.Gust..mph._03 + lag_High.Temp..F._01 + lag_Low.Visibility..mi._01 + lag_High.Wind..mph._01 + lag_Avg.Visibility..mi._01 + lag_Low.Sea.Level.Press..in._01 + lag_Avg.Sea.Level.Press..in._01 + lag_Avg.Wind..mph._01 + lag_High.Sea.Level.Press..in._01 + Non_month + lag_Target_01,
                            
                            
                            data= train_data,method="class", cp=0.01)
summary(decision_tree_final)


train_dt_pred_train = predict(decision_tree_final, newdata = na.omit(train_data), type = "prob")

library(ROCR)
ROCRpred = prediction(train_dt_pred_train[1:2997,2], train_data$Target[4:3000])
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE, print.cutoffs.at = seq(0,1,0.05), text.adj= c(-0.2,1.7))

auc = as.numeric(performance(ROCRpred,"auc")@y.values)
auc

library(PRROC)

fg <- train_dt_pred_train[train_data$Target[4:3000] == 1,2]
bg <- train_dt_pred_train[train_data$Target[4:3000] == 0,2]

# ROC Curve    
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc)

# # PR Curve
# pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
# plot(pr)

# Predicting on test data

train_dt_pred_test = predict(decision_tree_final, newdata = test_data, type= "prob")
train_dt_pred_test

confusionMatrix(as.factor(as.numeric(train_dt_pred_test[1:749,2]>=0.15)), test_data$Target, positive = "1", mode="everything")


# Random forest with cross validation

train_rf = train(Target~ lag_High.Dew.Point..F._03 
                 + lag_Low.Dew.Point..F._02 + 
                   lag_Avg.Temp..F._01 + lag_Avg.Dew.Point..F._02
                 + lag_Avg.Humidity...._02 + lag_High.Dew.Point..F._02 +
                   lag_High.Humidity...._02 + lag_Low.Humidity...._02 + lag_Low.Temp..F._01 
                 + lag_High.Wind.Gust..mph._03 + lag_High.Temp..F._01 + lag_Low.Visibility..mi._01
                 + lag_High.Wind..mph._01 + lag_Avg.Visibility..mi._01 + lag_Low.Sea.Level.Press..in._01 
                 + lag_Avg.Sea.Level.Press..in._01 + lag_Avg.Wind..mph._01 + lag_High.Sea.Level.Press..in._01
                 + Non_month + lag_Target_01,
                 data = na.omit(train_data), 
                 method = 'rf',
                 trControl = trainControl(method = 'cv',number = 10))

train_rf

train_rf_pred_train = predict(train_rf, newdata = na.omit(train_data), type= "prob")

ROCRpred = prediction(train_rf_pred_train[1:2997,2], train_data$Target[4:3000])
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE, print.cutoffs.at = seq(0,1,0.05), text.adj= c(-0.2,1.7))

auc = as.numeric(performance(ROCRpred,"auc")@y.values)
auc


fg <- train_rf_pred_train[train_data$Target[4:3000] == 1,2]
bg <- train_rf_pred_train[train_data$Target[4:3000] == 0,2]

# ROC Curve    
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc)

train_rf_pred_test = predict(train_rf, newdata = test_data, type= "prob")
train_rf_pred_test

confusionMatrix(as.factor(as.numeric(train_rf_pred_test[1:749,2]>=0.15)),
                test_data$Target, positive = "1", mode="everything")

# XGBoost

library(xgboost)

train_x = data.matrix(train_data[,c('lag_High.Sea.Level.Press..in._01',
                                    'lag_Low.Sea.Level.Press..in._01',
                                    'lag_Low.Visibility..mi._01','lag_Low.Temp..F._01',
                                    'lag_High.Dew.Point..F._02','lag_Avg.Temp..F._01','lag_Target_01'
                                    ,'lag_Avg.Wind..mph._01','lag_Avg.Visibility..mi._01',
                                    'lag_High.Temp..F._01','lag_Low.Humidity...._02',
                                    'lag_Avg.Humidity...._02','lag_Low.Dew.Point..F._02',
                                    'Non_month','lag_Avg.Sea.Level.Press..in._01',
                                    'lag_High.Wind..mph._01','lag_High.Wind.Gust..mph._03',
                                    'lag_High.Humidity...._02','lag_Avg.Dew.Point..F._02',
                                    'lag_High.Dew.Point..F._03'
)])
train_y = data.matrix(train_data[,59])

test_x = data.matrix(test_data[,c('lag_High.Sea.Level.Press..in._01','lag_Low.Sea.Level.Press..in._01','lag_Low.Visibility..mi._01','lag_Low.Temp..F._01','lag_High.Dew.Point..F._02','lag_Avg.Temp..F._01','lag_Target_01','lag_Avg.Wind..mph._01','lag_Avg.Visibility..mi._01','lag_High.Temp..F._01','lag_Low.Humidity...._02','lag_Avg.Humidity...._02','lag_Low.Dew.Point..F._02','Non_month','lag_Avg.Sea.Level.Press..in._01','lag_High.Wind..mph._01','lag_High.Wind.Gust..mph._03','lag_High.Humidity...._02','lag_Avg.Dew.Point..F._02','lag_High.Dew.Point..F._03'
)])
test_y = data.matrix(test_data[,59])


dtrain = xgb.DMatrix(data = train_x,label = train_y) 
dtest = xgb.DMatrix(data = test_x,label=test_y)


params = list(booster = "gbtree", objective = "binary:logistic", 
              eta=0.3, gamma=0, max_depth=6, min_child_weight=1, 
              subsample=1, colsample_bytree=1)


# Cross_validation of XG

xgbcv = xgb.cv( params = params, data = dtrain, nrounds = 100,
                nfold = 5, showsd = T, stratified = T,
                print.every.n = 10,
                early.stop.round = 20,
                maximize = F)

# Best iteration number 6

xgb1 = xgb.train (params = params, data = dtrain, nrounds = 6, watchlist = list(val=dtest,train=dtrain), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")

xgb1_pred_train = predict(xgb1,dtrain)


ROCRpred = prediction(xgb1_pred_train, train_data$Target)
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE, print.cutoffs.at = seq(0,1,0.05), text.adj= c(-0.2,1.7))

auc = as.numeric(performance(ROCRpred,"auc")@y.values)
auc

fg <- xgb1_pred_train[train_data$Target[1:3000] == 1]
bg <- xgb1_pred_train[train_data$Target[1:3000] == 0]

# ROC Curve    
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc)


xgb1_pred_train_class =  ifelse (xgb1_pred_train >= 0.2,1,0)

confusionMatrix(as.factor(xgb1_pred_train_class), as.factor(train_y), positive = "1", mode = "everything")




xgb1_pred_test = predict(xgb1,dtest)

xgb1_pred_test_class =  ifelse (xgb1_pred_test >= 0.2,1,0)

confusionMatrix(as.factor(xgb1_pred_test_class), as.factor(test_y), positive = "1", mode = "everything")


mat = xgb.importance (feature_names = colnames(train_data[,c('lag_High.Sea.Level.Press..in._01',
                                                             'lag_Low.Sea.Level.Press..in._01',
                                                             'lag_Low.Visibility..mi._01','lag_Low.Temp..F._01',
                                                             'lag_High.Dew.Point..F._02','lag_Avg.Temp..F._01',
                                                             'lag_Target_01','lag_Avg.Wind..mph._01',
                                                             'lag_Avg.Visibility..mi._01','lag_High.Temp..F._01',
                                                             'lag_Low.Humidity...._02','lag_Avg.Humidity...._02',
                                                             'lag_Low.Dew.Point..F._02','Non_month','lag_Avg.Sea.Level.Press..in._01','lag_High.Wind..mph._01','lag_High.Wind.Gust..mph._03','lag_High.Humidity...._02','lag_Avg.Dew.Point..F._02','lag_High.Dew.Point..F._03')]),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])


xgb.plot.multi.trees(feature_names = names(train_x), 
                     model = model)