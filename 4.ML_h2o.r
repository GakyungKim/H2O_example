######### H2O_AutoML ######### 
###### ###### ###### with 1z ###### ###### ###### 
########## [1] load data ##########
### load data
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/connectome21/2021-1/project/adol_DepAnx/data/analysis_data")
train_1z <- read.csv("train_VOL_clf.visit_1.csv")
test_1z <- read.csv("test_VOL_clf.visit_1.csv")

### set data type
h2o_train$y013_pat_HC <- as.factor(h2o_train$y013_pat_HC)
h2o_train$y013_pat123_HC <- as.factor(h2o_train$y013_pat123_HC)

h2o_test$y013_pat_HC <- as.factor(h2o_test$y013_pat_HC)
h2o_test$y013_pat123_HC <- as.factor(h2o_test$y013_pat123_HC)

########## [2] preparation ##########
library(h2o)
h2o.init()

# to run the H2O package, you need to change dataset to h2o data frame
h2o_train <- as.h2o(train_1z)
h2o_test <- as.h2o(test_1z)


########## [3] set the x and y (target) variables ##########
names(h2o_train)
y_patHC = 'y013_pat_HC'
y_pat123HC = 'y013_pat123_HC'
x = names(h2o_train)[!names(h2o_train) %in% c(y_patHC, y_pat123HC)]
x = x[-1] #exclude ID

########## [4] predict model ##########
model_patHC <- h2o.automl(y = y_patHC, x = x,
                          training_frame = h2o_train,
                          leaderboard_frame = h2o_test,
                          nfolds = 5,
                          seed = 1234,
                          stopping_tolerance = 0,
                          max_runtime_secs = 30)
pred_patHC <- h2o.predict(model_patHC, h2o_test)
pred_patHC_df <- as.data.frame(pred_patHC)

perf_patHC <- h2o.performance(model_patHC@leader, h2o_test)
perf_patHC

pred_patHC_roc <- rand.perf@metrics$thresholds_and_metric_scores[c('tpr', 'fpr')]
