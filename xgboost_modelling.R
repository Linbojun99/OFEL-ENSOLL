#SDM model for tuna species 
#xgoost

# packages
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(paradox)
library(ggplot2)
library(mlr3viz)
library(cowplot)
library(xgboost)
library(ggpubr)
library(gridExtra)
library(eoffice)

###northern_alb 
#modelling
northern_alb_model_data<-subset(pacific_tuna_model_data,pacific_tuna_model_data$Lat>0)

northern_data_task_ALB=northern_alb_model_data[,c("nCPUE_ALB",
                                                  "Month","Lon","Lat",
                                                  'Temp_0', 'Sali_300', 'MLD', 'Chl_200', 'O2_47', 'O2_97', 'O2_300',"Nppv_0")]
boxplot(northern_data_task_ALB$nCPUE_ALB)
# Create the task_alb_northern
task_alb_northern <- TaskRegr$new(id = "alb_task_northern", backend = northern_data_task_ALB, target = "nCPUE_ALB")
# Create the learner
learner <- lrn("regr.xgboost", predict_type = "response")
# Define the search space
search_space <- ParamSet$new(list(
  ParamInt$new("nrounds", lower = 50, upper = 150),
  ParamInt$new("max_depth", lower = 3, upper = 9),
  ParamDbl$new("eta", lower = 0.01, upper = 0.1),
  ParamDbl$new("gamma", lower = 0, upper = 0.2),
  ParamDbl$new("colsample_bytree", lower = 0.5, upper = 0.9),
  ParamInt$new("min_child_weight", lower = 1, upper = 5),
  ParamDbl$new("subsample", lower = 0.5, upper = 0.9)
))
# Set the resampling strategy
resampling <- rsmp("cv", folds = 5)
#
instance_alb_northern <- TuningInstanceSingleCrit$new(
  task = task_alb_northern,
  learner = learner,
  resampling = resampling,
  measure = msr("regr.mse"),
  search_space = search_space,
  terminator = trm("evals", n_evals = 100)
)
# Initialize the tuner
tuner <- tnr("grid_search")
#
#northern
results_alb_northern <- tuner$optimize(instance_alb_northern)
print(results_alb_northern)
autoplot_p1<-autoplot(instance_alb_northern,theme = theme_bw())
save(instance_alb_northern,file="/instance_alb_northern.RData")

# Assuming pacific_tuna_model_data is already loaded and is a data.frame
# Splitting data into training and validation sets for demonstration purposes
set.seed(123)  # for reproducibility
# for alb_northern
training_rows_alb_northern <- sample(nrow(northern_data_task_ALB), 0.8 * nrow(northern_data_task_ALB))
training_data_alb_northern <- northern_data_task_ALB[training_rows_alb_northern, ]
validation_data_alb_northern <- northern_data_task_ALB[-training_rows_alb_northern, ]
# Create DMatrix objects
dtrain_alb_northern <- xgb.DMatrix(data = as.matrix(training_data_alb_northern[-which(names(training_data_alb_northern) == "nCPUE_ALB")]), label = training_data_alb_northern$nCPUE_ALB)
dvalid_alb_northern <- xgb.DMatrix(data = as.matrix(validation_data_alb_northern[-which(names(validation_data_alb_northern) == "nCPUE_ALB")]), label = validation_data_alb_northern$nCPUE_ALB)
# Get the best performing model's parameters and corresponding scores
best_result_alb_northern <- instance_alb_northern$result
print(best_result_alb_northern$learner_param_vals)  # Check if these values make sense given your search space.

# Set parameters
params_alb_northern <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.06,
  max_depth = 8,
  gamma = 0.8111111,
  subsample = 0.7666667,
  colsample_bytree = 0.5888889,
  min_child_weight = 1,
  eval_metric = "rmse"
)

# Train model with early stopping
northern_ALB_xgb_model <- xgb.train(
  params = params_alb_northern, 
  data = dtrain_alb_northern, 
  nrounds = 106,
  watchlist = list(train = dtrain_alb_northern, eval = dvalid_alb_northern),
  early_stopping_rounds = 10,
  verbose = 0
)
save(northern_ALB_xgb_model,file="/northern_ALB_xgb_model.RData")

best_nrounds_alb_northern <- northern_ALB_xgb_model$best_iteration
d_northern_data_task_ALB <- xgb.DMatrix(data = as.matrix(northern_data_task_ALB[-which(names(northern_data_task_ALB) == "nCPUE_ALB")]), label = northern_data_task_ALB$nCPUE_ALB)

###predict
predictions_alb_northern <- predict(northern_ALB_xgb_model, d_northern_data_task_ALB)
predictions_alb_northern[predictions_alb_northern < 0] <- 0
boxplot(predictions_alb_northern)
northern_alb_model_data$pre_ALB<-predictions_alb_northern
save(northern_alb_model_data,file="/northern_alb_model_data.RData")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###southern pacific tuna
southern_alb_model_data<-subset(pacific_tuna_model_data,pacific_tuna_model_data$Lat<=0)
boxplot(southern_data_task_ALB$nCPUE_ALB)
southern_data_task_ALB=southern_alb_model_data[,c("nCPUE_ALB",
                                                  "Month","Lon","Lat",
                                                  'Temp_0', 'Temp_245', 'Temp_300', 'Sali_47', 'Sali_97', 'Sali_147',
                                                  'Sali_200', 'Sali_300', 'SSH', 'MLD', 'Chl_0', 'Chl_147', 'Chl_300',
                                                  'O2_0', 'O2_47', 'O2_97', 'O2_147', 'O2_245', 'O2_300', 'Nppv_0')]
boxplot(southern_data_task_ALB$nCPUE_ALB)
task_alb_southern <- TaskRegr$new(id = "alb_task_southern", backend = southern_data_task_ALB, target = "nCPUE_ALB")
# Create the learner
learner <- lrn("regr.xgboost", predict_type = "response")
# Define the search space
search_space <- ParamSet$new(list(
  ParamInt$new("nrounds", lower = 50, upper = 150),
  ParamInt$new("max_depth", lower = 3, upper = 9),
  ParamDbl$new("eta", lower = 0.01, upper = 0.1),
  ParamDbl$new("gamma", lower = 0, upper = 0.2),
  ParamDbl$new("colsample_bytree", lower = 0.5, upper = 0.9),
  ParamInt$new("min_child_weight", lower = 1, upper = 5),
  ParamDbl$new("subsample", lower = 0.5, upper = 0.9)
))

# Set the resampling strategy
resampling <- rsmp("cv", folds = 5)
##alb_south
instance_alb_northern <- TuningInstanceSingleCrit$new(
  task = task_alb_northern,
  learner = learner,
  resampling = resampling,
  measure = msr("regr.mse"),
  search_space = search_space,
  terminator = trm("evals", n_evals = 100)
)
instance_alb_southern <- TuningInstanceSingleCrit$new(
  task = task_alb_southern,
  learner = learner,
  resampling = resampling,
  measure = msr("regr.mse"),
  search_space = search_space,
  terminator = trm("evals", n_evals = 100)
)

# Initialize the tuner
tuner <- tnr("grid_search")
#southern
results_alb_southern <- tuner$optimize(instance_alb_southern)
print(results_alb_southern)
autoplot(instance_alb_southern,theme = theme_bw())
best_result_alb_southern <- instance_alb_southern$result
print(best_result_alb_southern$learner_param_vals)  # Check if these values make sense given your search space.

# Assuming pacific_tuna_model_data is already loaded and is a data.frame
# Splitting data into training and validation sets for demonstration purposes
set.seed(123)  # for reproducibility
# for alb_southern
training_rows_alb_southern <- sample(nrow(southern_data_task_ALB), 0.8 * nrow(southern_data_task_ALB))
training_data_alb_southern <- southern_data_task_ALB[training_rows_alb_southern, ]
validation_data_alb_southern <- southern_data_task_ALB[-training_rows_alb_southern, ]
dtrain_alb_southern <- xgb.DMatrix(data = as.matrix(training_data_alb_southern[-which(names(training_data_alb_southern) == "nCPUE_ALB")]), label = training_data_alb_southern$nCPUE_ALB)
dvalid_alb_southern <- xgb.DMatrix(data = as.matrix(validation_data_alb_southern[-which(names(validation_data_alb_southern) == "nCPUE_ALB")]), label = validation_data_alb_southern$nCPUE_ALB)

params_alb_southern <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.05,
  max_depth = 9,
  gamma = 0.1555556,
  subsample = 0.9,
  colsample_bytree = 0.8555556,
  min_child_weight = 4,
  eval_metric = "rmse"
)
southern_ALB_xgb_model <- xgb.train(
  params = params_alb_southern, 
  data = dtrain_alb_southern, 
  nrounds = 139,
  watchlist = list(train = dtrain_alb_southern, eval = dvalid_alb_southern),
  early_stopping_rounds = 10,
  verbose = 0
)
save(southern_ALB_xgb_model,file="/southern_ALB_xgb_model.RData")
best_nrounds_alb_southern <- southern_ALB_xgb_model$best_iteration
d_southern_data_task_ALB <- xgb.DMatrix(data = as.matrix(southern_data_task_ALB[-which(names(southern_data_task_ALB) == "nCPUE_ALB")]), label = southern_data_task_ALB$nCPUE_ALB)
save(instance_alb_southern,file="/instance_alb_southern.RData")

predictions_alb_southern <- predict(southern_ALB_xgb_model, d_southern_data_task_ALB)
predictions_alb_southern[predictions_alb_southern < 0] <- 0
boxplot(predictions_alb_southern)
southern_alb_model_data$pre_ALB<-predictions_alb_southern
save(southern_alb_model_data,file="/southern_alb_model_data.RData")


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###bigeye tuna

###bet
pacific_data_task_BET=pacific_tuna_model_data[,c("nCPUE_BET",
                                                 "Month","Lon","Lat",
                                                 'Temp_47', 'Sali_47', 'Sali_97', 'MLD', 'Chl_0', 'Chl_300', 'O2_47',
                                                 'O2_97', 'O2_200', 'O2_300')]
boxplot(pacific_data_task_BET$nCPUE_BET)
# Create the task_bet
task_bet <- TaskRegr$new(id = "bet_task", backend = pacific_data_task_BET, target = "nCPUE_BET")
# Create the learner
learner <- lrn("regr.xgboost", predict_type = "response")
# Define the search space
search_space <- ParamSet$new(list(
  ParamInt$new("nrounds", lower = 50, upper = 150),
  ParamInt$new("max_depth", lower = 3, upper = 9),
  ParamDbl$new("eta", lower = 0.01, upper = 0.1),
  ParamDbl$new("gamma", lower = 0, upper = 0.2),
  ParamDbl$new("colsample_bytree", lower = 0.5, upper = 0.9),
  ParamInt$new("min_child_weight", lower = 1, upper = 5),
  ParamDbl$new("subsample", lower = 0.5, upper = 0.9)
))
# Set the resampling strategy
resampling <- rsmp("cv", folds = 5)
##
instance_alb_northern <- TuningInstanceSingleCrit$new(
  task = task_alb_northern,
  learner = learner,
  resampling = resampling,
  measure = msr("regr.mse"),
  search_space = search_space,
  terminator = trm("evals", n_evals = 100)
)
# Initialize the tuner
tuner <- tnr("grid_search")
#
results_bet <- tuner$optimize(instance_bet)
autoplot(instance_bet,theme = theme_bw())
save(instance_bet,file="/instance_bet.RData")
best_result_bet <- instance_bet$result
print(best_result_bet$learner_param_vals)  # Check if these values make sense given your search space.

# Assuming pacific_tuna_model_data is already loaded and is a data.frame
# Splitting data into training and validation sets for demonstration purposes
set.seed(123)  # for reproducibility
# 
training_rows_bet <- sample(nrow(pacific_data_task_BET), 0.8 * nrow(pacific_data_task_BET))
training_data_bet <- pacific_data_task_BET[training_rows_bet, ]
validation_data_bet <- pacific_data_task_BET[-training_rows_bet, ]
# Create DMatrix objects
dtrain_bet <- xgb.DMatrix(data = as.matrix(training_data_bet[-which(names(training_data_bet) == "nCPUE_BET")]), label = training_data_bet$nCPUE_BET)
dvalid_bet <- xgb.DMatrix(data = as.matrix(validation_data_bet[-which(names(validation_data_bet) == "nCPUE_BET")]), label = validation_data_bet$nCPUE_BET)

# Set parameters
params_bet <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.05,
  max_depth = 9,
  gamma = 0,
  subsample = 0.9,
  colsample_bytree = 0.7666667,
  min_child_weight = 1,
  eval_metric = "rmse"
)
# Train model with early stopping
pacific_BET_xgb_model <- xgb.train(
  params = params_bet, 
  data = dtrain_bet, 
  nrounds = 128,
  watchlist = list(train = dtrain_bet, eval = dvalid_bet),
  early_stopping_rounds = 10,
  verbose = 0
)
save(pacific_BET_xgb_model,file="/pacific_BET_xgb_model.RData")
best_nrounds_bet <- pacific_BET_xgb_model$best_iteration
d_pacific_data_task_BET <- xgb.DMatrix(data = as.matrix(pacific_data_task_BET[-which(names(pacific_data_task_BET) == "nCPUE_BET")]), label = pacific_data_task_BET$nCPUE_BET)
###predict
predictions_bet <- predict(pacific_BET_xgb_model, d_pacific_data_task_BET)
predictions_bet[predictions_bet < 0] <- 0
boxplot(predictions_bet)
pacific_bet_model_data<-pacific_tuna_model_data
pacific_bet_model_data$pre_BET<-predictions_bet
save(pacific_bet_model_data,file="/pacific_bet_model_data.RData")



#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###yellowfin tuna
pacific_data_task_YFT=pacific_tuna_model_data[,c("nCPUE_YFT",
                                                 "Month","Lon","Lat",
                                                 'Temp_47', 'Temp_147', 'Sali_47', 'Sali_97', 'Sali_200', 'Sali_300',
                                                 'MLD', 'Chl_0', 'Chl_200', 'Chl_300', 'O2_47', 'O2_97', 'O2_147',
                                                 'O2_200', 'O2_245', 'O2_300', 'Nppv_147')]
boxplot(pacific_data_task_YFT$nCPUE_YFT)
task_yft <- TaskRegr$new(id = "yft_task", backend = pacific_data_task_YFT, target = "nCPUE_YFT")

# Create the learner
learner <- lrn("regr.xgboost", predict_type = "response")
# Define the search space
search_space <- ParamSet$new(list(
  ParamInt$new("nrounds", lower = 50, upper = 150),
  ParamInt$new("max_depth", lower = 3, upper = 9),
  ParamDbl$new("eta", lower = 0.01, upper = 0.1),
  ParamDbl$new("gamma", lower = 0, upper = 0.2),
  ParamDbl$new("colsample_bytree", lower = 0.5, upper = 0.9),
  ParamInt$new("min_child_weight", lower = 1, upper = 5),
  ParamDbl$new("subsample", lower = 0.5, upper = 0.9)
))
# Set the resampling strategy
resampling <- rsmp("cv", folds = 5)
instance_yft<- TuningInstanceSingleCrit$new(
  task = task_yft,
  learner = learner,
  resampling = resampling,
  measure = msr("regr.mse"),
  search_space = search_space,
  terminator = trm("evals", n_evals = 100)
)
# Initialize the tuner
tuner <- tnr("grid_search")
results_yft <- tuner$optimize(instance_yft)
# Review 
print(results_yft)
autoplot(instance_yft,theme = theme_bw())
best_result_yft <- instance_yft$result
save(instance_yft,file="/instance_yft.RData")
print(best_result_yft$learner_param_vals)  # Check if these values make sense given your search space.

# Assuming pacific_tuna_model_data is already loaded and is a data.frame
# Splitting data into training and validation sets for demonstration purposes
set.seed(123)  # for reproducibility
training_rows_yft <- sample(nrow(pacific_data_task_YFT), 0.8 * nrow(pacific_data_task_YFT))
training_data_yft <- pacific_data_task_YFT[training_rows_yft, ]
validation_data_yft <- pacific_data_task_YFT[-training_rows_yft, ]

# Create DMatrix objects
dtrain_yft <- xgb.DMatrix(data = as.matrix(training_data_yft[-which(names(training_data_yft) == "nCPUE_YFT")]), label = training_data_yft$nCPUE_YFT)
dvalid_yft <- xgb.DMatrix(data = as.matrix(validation_data_yft[-which(names(validation_data_yft) == "nCPUE_YFT")]), label = validation_data_yft$nCPUE_YFT)

# Set parameters
params_yft <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.07,
  max_depth = 9,
  gamma = 0.1111111,
  subsample = 0.9,
  colsample_bytree = 0.8555556,
  min_child_weight = 2,
  eval_metric = "rmse"
)
pacific_YFT_xgb_model <- xgb.train(
  params = params_yft, 
  data = dtrain_yft, 
  nrounds = 94,
  watchlist = list(train = dtrain_yft, eval = dvalid_yft),
  early_stopping_rounds = 10,
  verbose = 0
)
save(pacific_YFT_xgb_model,file="/pacific_YFT_xgb_model.RData")
d_pacific_data_task_YFT <- xgb.DMatrix(data = as.matrix(pacific_data_task_YFT[-which(names(pacific_data_task_YFT) == "nCPUE_YFT")]), label = pacific_data_task_YFT$nCPUE_YFT)
predictions_yft <- predict(pacific_YFT_xgb_model, d_pacific_data_task_YFT)
predictions_yft[predictions_yft < 0] <- 0
boxplot(predictions_yft)
pacific_yft_model_data<-pacific_tuna_model_data
pacific_yft_model_data$pre_YFT<-predictions_yft
save(pacific_yft_model_data,file="/pacific_yft_model_data.RData")





