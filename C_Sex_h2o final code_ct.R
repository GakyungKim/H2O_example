############### Sex_h2o ###############
# Written by: Kakyeong Kim (bettybetty3k@gmail.com)
#######################################

### load data
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Connectome Lab/C_Sex Classification/C_Sex_final data")
train.ct <- read.csv("train.ct.csv")
test.ct <- read.csv("test.ct.csv")
summary(is.na(test.ct))


fact = c("sex", "race.ethnicity", "abcd_site")
for(i in 1:3){
  train.ct[,fact[i]] <- as.factor(train.ct[,fact[i]])
  test.ct[,fact[i]] <- as.factor(test.ct[,fact[i]])
}

### execute this code if you want to load the DAI package
#library(dai)
#dai.connect(uri = 'http://147.46.167.205:12345', username = 'connectome@snu.ac.kr', password = 'a816115!')

### execute this code if you want to load the h2o package
library(h2o)
h2o.init()

train <- as.h2o(train.ct) #as.DAIframe() or as.h2o()
test <- as.h2o(test.ct) #as.DAIframe() or as.h2o()

head(names(train))
y = "sex"
valids = c("race.ethnicity", "abcd_site")
x = names(train)[!names(train) %in% c(y, valids)]
x = x[-1]

### predict the model
?h2o.automl

rand.model <- h2o.automl(y = y, x = x,
                            training_frame = train,
                            leaderboard_frame = test,
                            nfolds = 5, seed = 1234,
                            stopping_tolerance = 0,
                            max_runtime_secs = 600)

race.model <- h2o.automl(y = y, x = x,
                         training_frame = train,
                         leaderboard_frame = test,
                         nfolds = h2o.nlevels(valids[1]),
                         fold_column = valids[1],
                         seed = 1234,
                         stopping_tolerance = 0,
                         max_runtime_secs = 600)

site.model <- h2o.automl(y = y, x = x,
                         training_frame = train,
                         leaderboard_frame = test,
                         nfolds = h2o.nlevels(valids[2]),
                         fold_column = valids[2],
                         seed = 1234,
                         stopping_tolearance = 0,
                         max_runtime_secs = 300)

rand.pred <- h2o.predict(rand.model, test)
rand.pred.df <- as.data.frame(rand.pred)

rand.perf <- h2o.performance(rand.model@leader, test)
rand.perf

rand.roc <- rand.perf@metrics$thresholds_and_metric_scores[c('tpr', 'fpr')]
library(ggplot2)
ggplot(data = rand.roc, aes(x = fpr, y = tpr)) + 
  geom_line(colour = 'red', size = .7) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1)), linetype = 3, col = 'grey50', size = .3) +
  xlab('False Positive Rate') +
  ylab('True Positive Rate') +
  theme_bw() + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = 'black', size = .5))

### stacked ensemble model
#didn't work
#best_glm <- h2o.glm(
#  x = x, y = y, training_frame = train, family = "binomial",
#  remove_collinear_columns = TRUE, nfolds = 5, fold_assignment = NULL, 
#  keep_cross_validation_predictions = TRUE, seed = 1234, max_runtime_secs = 30,
#  stopping_metric = "AUC")

best_gbm <- h2o.gbm(
  x = x, y = y, training_frame = train, ntrees = 5000,
  learn_rate = 0.01, max_depth = 7, min_rows = 5, sample_rate = 0.8,
  nfolds = 5, fold_assignment = "Random", keep_cross_validation_predictions = T,
  seed = 1234, stopping_rounds = 50, stopping_metric = "AUC", max_runtime_secs = 300)

best_xgb <- h2o.xgboost(
  x = x, y = y, training_frame = train, ntrees = 5000,
  learn_rate = 0.05, max_depth = 3, min_rows = 3, sample_rate = 0.8,
  categorical_encoding = "AUTO", nfolds = 5, fold_assignment = "Random",
  keep_cross_validation_predictions = TRUE, seed = 1234, stopping_rounds = 50,
  stopping_metric = "AUC", stopping_tolerance = 0, max_runtime_secs = 300)

best_deep <- h2o.deeplearning(
  x = x, y = y, training_frame = train, nfolds = 5, fold_assignment = "Random", fold_column = NULL, 
  keep_cross_validation_predictions = TRUE, seed = 1234, epochs = 10000, 
  stopping_metric = "AUC", stopping_tolerance = 0,
  hidden = c(64, 32, 16), max_runtime_secs = 300, variable_importances = T)

best_rf <- h2o.randomForest(
  x = x, y = y, training_frame = train, ntrees = 1000, mtries = 20,
  max_depth = 30, min_rows = 1, sample_rate = 0.8, nfolds = 5,
  fold_assignment = "Random", keep_cross_validation_predictions = TRUE,
  seed = 1234, stopping_rounds = 50, stopping_metric = "AUC",
  stopping_tolerance = 0, max_runtime_secs = 300)

ensemble_tree <- h2o.stackedEnsemble(y = y, x = x,
                                     training_frame = train,
                                     blending_frame = test,
                                     base_models = list(best_gbm, best_xgb, best_deep, best_rf),
                                     metalearner_algorithm = "AUTO",
                                     max_runtime_secs = 300)

### feature importance
pred <- function(object, newdata)  {
  results <- as.vector(h2o.predict(object, as.h2o(newdata)))
  return(results)
}

# implementation
vip::vip(
  ensemble_tree,
  train = as.data.frame(train),
  method = "permute",
  target = "sex",
  metric = "AUC",
  reference_class = "auc",
  nsim = 5,
  sample_frac = 0.5,
  pred_wrapper = pred
)

library(dplyr)
components_lime <- lime::lime(
  x = as.data.frame(train) %>% select(x),
  model = ensemble_tree, 
  n_bins = 20
)

### lime to explain previously defined instances; high-ob and low-ob
# contains the fitted explainer model (model_r2) and the weighted importance (feature_weight)
lime_explanation <- lime::explain(
  x = rbind(high_ob, low_ob),
  explainer = components_lime,
  n_permutations = 5000,
  dist_fun = "gower",
  kernel_width = 0.25,
  feature_selection = "highest_weights"
)

glimpse(lime_explanation)
plot_features(lime_explanation, ncol = 1)

### tune the LIME algorithm a bit
lime_explanation2 <- explain(
  x = rbind(high_ob, low_ob),
  explainer = components_lime,
  n_permutations = 5000,
  dist_fun = "euclidean",
  kernerl_width = 0.75,
  n_features = 10,
  feature_select = "lasso_path"
)

plot_features(lime_explanation2, ncol = 1)




rand.model@modeling_steps

rand.klim <- h2o.klime(train, x = x, y = y)




