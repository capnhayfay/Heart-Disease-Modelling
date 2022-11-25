# import libraries
library(data.table)
library(rpart)
library(rpart.plot)
library(caTools)
library(dtplyr)
library(dplyr)
library(tibble)
library(caret)
library(tidyverse)
library(xgboost)
library(plyr)
library(splitTools)
library(ranger)
library(Matrix)
library(slam)
library(magrittr)
library(doParallel)
library(tidymodels)
library(themis)
library(vip)

# use parallel processing for quicker computing

# doParallel::registerDoParallel(cores = 64) - replace with the number of cores you have


# set seed to ensure results are reproducible
set.seed(8)

# set project directory
setwd("C:/Users/fight/Documents/NTU/Y2S1/BC2406/Group Project")



# Pipeline
## Data is simulated using the equation and a range of values within the normal range for each
### Classify data as > intermediate risk or < intermediate risk
#### Model for the existing pki dataset and the simulated data set as is
##### Combine the two and present that they have better metrics together


# Part 1 -  Data Simulation

#create the equation
grace_eq <- function(age, pulse, bpsys, creat_mg, killip, stchange, posinit, carrst){
  
  age2 <- 0
  if(30 <= age && age < 40) {age2 = 0 + (age-30)*1.7}
  else if (40 <=age && age < 50) {age2 = 17 + (age-40)*(16/10)}
  else if (50 <=age && age < 60) {age2 = 33 + (age-50)*(17/10)}
  else if (60 <=age && age < 70) {age2 = 50 + (age-60)*(17/10)}
  else if (70 <=age && age < 80) {age2 = 67 + (age-70)*(16/10)}
  else if (80 <=age && age < 90) {age2 = 83 + (age-80)*(17/10)}
  else if (age >=90) {age2 = 100}
  
  pulse2 <-  0
  if(50 <=pulse && pulse < 60) {pulse2 = 0 + (pulse-50)*(3/10);}
  else if (60 <=pulse && pulse < 70) {pulse2 = 3 + (pulse-60)*(3/10);}
  else if (70 <=pulse && pulse < 80) {pulse2 = 6 + (pulse-70)*(3/10);}
  else if (80 <=pulse && pulse < 90) {pulse2 = 9 + (pulse-80)*(3/10);}
  else if (90 <=pulse && pulse < 100) {pulse2 = 12 + (pulse-90)*(3/10);}
  else if (100<=pulse && pulse < 110) {pulse2 = 15 + (pulse-100)*(3/10);}
  else if (110<=pulse && pulse < 150) {pulse2 = 18 + (pulse-110)*(12/40);}
  else if (150<=pulse && pulse<200) {pulse2 = 30 + (pulse-150)*(16/50);}
  else if (pulse >=200){ pulse2 = 46;}
  
  sysbp2 <- 58;
  if(80 <=bpsys && bpsys < 100)  {sysbp2 = 58 -(bpsys-80)*(10/20);}
  else if (100<=bpsys && bpsys < 110)  {sysbp2 = 48 -(bpsys-100)*(5/10);}
  else if (110<=bpsys && bpsys < 120)  {sysbp2 = 43 -(bpsys-110)*(4/10);}
  else if (120<=bpsys && bpsys < 130)  {sysbp2 = 39 -(bpsys-120)*(5/10);}
  else if (130<=bpsys && bpsys < 140)  {sysbp2 = 34 -(bpsys-130)*(5/10);}
  else if (140<=bpsys && bpsys < 150)  {sysbp2 = 29 -(bpsys-140)*(5/10);}
  else if (150<=bpsys && bpsys < 160)  {sysbp2 = 24 -(bpsys-150)*(5/10);}
  else if (160<=bpsys && bpsys < 180)  {sysbp2 = 19 -(bpsys-160)*(9/20);}
  else if (180<=bpsys && bpsys < 200)  {sysbp2 = 10 -(bpsys-180)*(10/20);}
  else if (bpsys >=200)  {sysbp2 = 0;}
  
  crt2 <- 0
  if (0.0 <=creat_mg && creat_mg < 0.2) {crt2 = 0 + (creat_mg-0)*(1/.2);}
  else if (0.2 <=creat_mg && creat_mg < 0.4)  {crt2 = 1 + (creat_mg-0.2)*(2/.2);}
  else if (0.4 <=creat_mg && creat_mg < 0.6)  {crt2 = 3 + (creat_mg-0.4)*(1/.2);}
  else if (0.6 <=creat_mg && creat_mg < 0.8)  {crt2 = 4 + (creat_mg-0.6)*(2/.2);}
  else if (0.8 <=creat_mg && creat_mg < 1.0)  {crt2 = 6 + (creat_mg-0.8)*(1/.2);}
  else if (1.0 <=creat_mg && creat_mg < 1.2)  {crt2 = 7 + (creat_mg-1.0)*(1/.2);}
  else if (1.2 <=creat_mg && creat_mg < 1.4)  {crt2 = 8 + (creat_mg-1.2)*(2/.2);}
  else if (1.4 <=creat_mg && creat_mg < 1.6)  {crt2 = 10 + (creat_mg-1.4)*(1/.2);}
  else if (1.6 <=creat_mg && creat_mg < 1.8)  {crt2 = 11 + (creat_mg-1.6)*(2/.2);}
  else if (1.8 <=creat_mg && creat_mg < 2.0)  {crt2 = 13 + (creat_mg-1.8)*(1/.2);}
  else if (2.0 <=creat_mg && creat_mg < 3.0)  {crt2 = 14 + (creat_mg-2.0)*(7/1);}
  else if (3.0 <=creat_mg && creat_mg < 4.0)  {crt2 = 21 + (creat_mg-3.0)*(7/1);}
  else if (creat_mg >=4.0) {crt2 = 28;}
  
  killips <- 0
  if(killip == 1) {killips <- 0;}
  else if(killip == 2) {killips = 20;}
  else if(killip == 3) {killips = 39;}
  else if(killip == 4) {killips = 59;}
  
  Death_pt = killips + sysbp2 + pulse2 + age2 + crt2 + 28*stchange
  + 14*posinit + 39*carrst;
  
  return(Death_pt)
}

# populate datatable with ranges as follows:
# 30 <= age <= 90
# 50 <= pulse <= 200
# 75 <= sysbp <= 200
# 0.7 <= creatinine <= 4.0
# Killip class (1,2,3,4)
# Cardiac Enzymes, ST Segment Deviation, Cardiac Arrest -> 0/1

# simulate 200K datapoints to match PKI data and get the Y

age <- array(as.integer(runif(200000, min = 30, max = 91)))
pulse <- array(as.integer(runif(200000, 50, 201)))
sysbp <- array(as.integer(runif(200000, 75, 201)))
creatinine <- array(runif(200000, 0.7, 4.0))
killip <- array(as.integer(runif(200000, 1,5)))
car_enz <- array(as.integer(runif(200000, 0,2)))
seg_dev <- array(as.integer(runif(200000, 0,2)))
car_arr <- array(as.integer(runif(200000, 0,2)))


data1 <- data.frame(age, pulse, sysbp, creatinine, killip, car_enz, seg_dev, car_arr)
data1 <- as.data.table(data1)


data1[, row_id := .I]
setkey(data1, row_id)

# create column for scores

for(i in 1:dim(data1)[1]){
  (data1[, score := grace_eq(age[i], pulse[i], sysbp[i], creatinine[i],
                             killip[i], car_enz[i], seg_dev[i], car_arr[i]), by = row_id])
}
## returns an error due to if else changes in R but all 200K entries are made

sum(!(is.na(data1$score)))

# classify risk as 1 or 0 depending on risk score
# 140 = 0.2 probability of dying
# risk < intermediate & intermediate < risk as 0 and 1

data1[, risk := ifelse(score > 140, 1, 0)]

# convert factors to factors

data1[, risk := factor(risk, ordered = T)]
data1[, killip:= factor(killip, ordered = T)]
data1[, car_enz := factor(car_enz, ordered = T)]
data1[, car_arr := factor(car_arr, ordered = T)]
data1[, seg_dev := factor(seg_dev, ordered = T)]

#verify that all scores were computed
colSums(is.na(data1))

# Probabilistic Simulation Approach

# create a new data frame with only the variables to calculate probabilities:
data2 <- data.frame(age, pulse, sysbp, creatinine, killip, car_enz, seg_dev, car_arr)
# array of coefficients from original grace model
coefficients = array(c(0.0531,0.0087,-0.0168,0.1823,0.6931,1.4586,0.4700,0.8755))
# xb is an empty score-holder array with 200000 rows
xb <- rep(NA, dim(data2)[1])
# iterate across the data to populate xb with scores
for(i in 1:dim(data2)[1]){
  xb[i] = 0
  for(j in 1:dim(data2)[2]){
    xb[i] = xb[i] + data2[i,j]*coefficients[j]
  }
  xb[i] = xb[i] - 7.7035
}
# create the probability array using the equation derived in original research
prob <- exp(1)^xb/(1+exp(1)^(xb))

# plot probabilities vs grace-score to verify curve
scatter.smooth(x = data1$score, prob)

# merge and export the data
## datacoumpounded <- data.table(data1, prob)
## fwrite(datacoumpounded, file = "data_sim.csv")
### clean up workspace - remove(data2, datacoumpounded, age, car_arr, car_enz, coefficients, creatinine, i, j, killip, pulse, seg_dev, sysbp, xb)


# import dataset for Personal Key Indicators
pki <- fread("Heart_Disease_Indicators_1.csv")
summary(pki)

# since we are considering only behavioral and lifestyle factors, we remove the following:
## anyhealthcare, nodocbccost, education, income, cholcheck

pki[, AnyHealthcare := NULL][, NoDocbcCost := NULL][, Education := NULL][, Income := NULL][, CholCheck := NULL]
# Set variables to appropriate datatypes
glimpse(pki)

# factors
pki[, HeartDiseaseorAttack := factor(HeartDiseaseorAttack, ordered = T)][
  , HighBP := factor(HighBP, ordered = T)][, HighChol := factor(HighChol, ordered = T)][
    , Smoker := factor(Smoker, ordered = T)][, Stroke := factor(Stroke, ordered = T)][
      , Diabetes := factor(Diabetes, ordered = T)][, PhysActivity := factor(PhysActivity, ordered = T)][
        , Fruits := factor(Fruits, ordered = T)][, Veggies := factor(Veggies, ordered = T)][
          , HvyAlcoholConsump := factor(HvyAlcoholConsump, ordered = T)][
            , GenHlth := factor(GenHlth, ordered = T)][, MentHlth := factor(MentHlth, ordered = T)][
              , PhysHlth := factor(PhysHlth, ordered = T)][, DiffWalk := factor(DiffWalk, ordered = T)][
                , Sex := factor(Sex)][, Age := factor(Age, ordered = T)]
# verify
glimpse(pki)

# numeric
summary(pki$BMI)
bmi_max <- IQR(pki$BMI)*1.5 + 31
# remove rows with bmi > bmi_max since in a local context, there is a lower rate of outlying BMI's
## https://www.healthhub.sg/live-healthy/764/its-not-a-small-world-after-all
pki <- pki[BMI <= bmi_max]

#resample final pki to 200K size

pki <- pki[sample(.N, 200000, replace = FALSE)]



## Part 2 - Making models on the initial datasets

# split the data into 60% training and 40% testing
splitter <- partition(data1$risk, p = c(train = 0.6, test = 0.4))
str(splitter)
# remove columns row_id and score since they are not being used for the modelling
data1.train <- data1[splitter$train]
data1.train[, row_id := NULL][, score := NULL]
data1.test <- data1[splitter$test]
data1.test[, row_id := NULL][, score := NULL]

# store the independent variables within a sparse matrix to represent categorical variables..
## .. as numeric variables (like one hot encoding but less dimensional) 
data1.train.matrix <- sparse.model.matrix(risk~., data = data.frame(data1.train))[, -1]
head(data1.train.matrix)
data1.train.output_vector <- data1.train[, risk == 1]
data1.train.output_vector <- ifelse(data1.train.output_vector == T, "Yes", "No")
data1.train.output_vector <- factor(data1.train.output_vector, ordered = T)


# set parameter for training to be adaptive cross validation with 10 repeats
train_ctrl <- trainControl(method = "adaptive_cv", number = 10, repeats = 10,
                           classProbs = T,
                           summaryFunction = twoClassSummary,
                           search = "random",
                           allowParallel = T)
# set hyperparameters for xgboost classifier
tuneGridXGB <- expand.grid(
  nrounds=c(350),
  max_depth = c(4, 6),
  eta = c(0.05, 0.1),
  gamma = c(0.01),
  colsample_bytree = c(0.75),
  subsample = c(0.50),
  min_child_weight = c(0))

# model 1 for data from grace equation
xg1 <- train(
  x = data1.train.matrix,
  y = data1.train.output_vector,
  method = 'xgbTree',
  metric = 'ROC',
  trControl = train_ctrl,
  tuneGrid = tuneGridXGB)


# convert test target variables to have the same names and type as those fed to classifier
data1.test[, risk := ifelse(risk == 1, "Yes", "No")][, risk := factor(risk, ordered = T)]

# matrix transformation for test dataset
data1.test.matrix <- sparse.model.matrix(risk~., data = data.frame(data1.test))[, -1]

# predict values from the matrix
test_predict <- predict(xg1, data1.test.matrix)

# print metrics in form of Confusion Matrix
confusionMatrix(data = test_predict, reference = data1.test$risk, positive = 'Yes')

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    No   Yes
# No  24821   213
# Yes   420 54546
# 
# Accuracy : 0.9921          
# 95% CI : (0.9914, 0.9927)
# No Information Rate : 0.6845          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9816          
# 
# Mcnemar's Test P-Value : 2.661e-16       
#                                           
#             Sensitivity : 0.9961          
#             Specificity : 0.9834          
#          Pos Pred Value : 0.9924          
#          Neg Pred Value : 0.9915          
#              Prevalence : 0.6845          
#          Detection Rate : 0.6818          
#    Detection Prevalence : 0.6871          
#       Balanced Accuracy : 0.9897          
#                                           
#        'Positive' Class : Yes

# the very high metrics in this case are because of dataset simulation
# this model was made more to get parameters


# pki modelling
# rename output column to work with xg classifier
pki[, HeartDiseaseorAttack := ifelse(HeartDiseaseorAttack == 1, "Yes", "No")]

## train-test split by row numbers
splitter.pki <- partition(pki$HeartDiseaseorAttack, p = c(train = 0.6, test = 0.4))
str(splitter.pki)
## select row nums to be train and test
pki.train <- pki[splitter.pki$train]
pki.test <- pki[splitter.pki$test]

# convert pki values into sparse matrix

pki.train.matrix <- sparse.model.matrix(HeartDiseaseorAttack~., data = data.frame(pki.train))[, -1]
head(pki.train.matrix)

pki.train.output <- pki.train[, HeartDiseaseorAttack == "Yes"]
pki.train.output <- ifelse(pki.train.output == T, "Yes", "No")
pki.train.output <- factor(pki.train.output, ordered = T)


xg2 <- train(
  x = pki.train.matrix,
  y = pki.train.output,
  method = 'xgbTree',
  metric = 'ROC',
  trControl = train_ctrl,
  tuneGrid = tuneGridXGB)

# convert test target variables to have the same names as those fed to classifier
pki.test[, HeartDiseaseorAttack := factor(HeartDiseaseorAttack, ordered = T)]

# matrix transformation for test dataset
pki.test.matrix <- sparse.model.matrix(HeartDiseaseorAttack ~., data = data.frame(pki.test))[, -1]

# predict values from the matrix
pki.test_predict <- predict(xg2, pki.test.matrix)

# print metrics in form of Confusion Matrix
confusionMatrix(data = pki.test_predict, reference = pki.test$HeartDiseaseorAttack, positive = 'Yes')

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    No   Yes
# No  71912  6649
# Yes   625   814
# 
# Accuracy : 0.9091          
# 95% CI : (0.9071, 0.9111)
# No Information Rate : 0.9067          
# P-Value [Acc > NIR] : 0.01076         
# 
# Kappa : 0.1575          
# 
# Mcnemar's Test P-Value : < 2e-16         
#                                           
#             Sensitivity : 0.10907         
#             Specificity : 0.99138         
#          Pos Pred Value : 0.56567         
#          Neg Pred Value : 0.91537         
#              Prevalence : 0.09329         
#          Detection Rate : 0.01018         
#    Detection Prevalence : 0.01799         
#       Balanced Accuracy : 0.55023         
#                                           
#        'Positive' Class : Yes             



# view models 1 and 2 for the respective tuning parameters which were chosen
print(xg1)
# The final values used for the model were nrounds = 350, max_depth = 4, eta = 0.1, gamma =
#   0.01, colsample_bytree = 0.75, min_child_weight = 0 and subsample = 0.5.

print(xg2)
# The final values used for the model were nrounds = 350, max_depth = 4, eta = 0.05, gamma =
#   0.01, colsample_bytree = 0.75, min_child_weight = 0 and subsample = 0.5.


# PART 3 - Merge Datasets to Create a new Model
# merge the 2 datasets on the following
# stroke == cardiac arrest on arrival (car_arr)
# age == age
# HeartDiseaseorAttack == risk

# by merging on these factors, there is a higher probability that patients with similar
# factors will be grouped together and hence the model will be better able to assess the
# weightage of each item thereby performing better than the individual models

# copy datasets to prevent having to redo all computations
grc_cpy <- copy(data1)
pki_cpy <- copy(pki)

glimpse(grc_cpy)
glimpse(pki_cpy)


# risk - HeartDiseaseorAttack
grc_cpy[, risk := ifelse(risk == 1, "Yes", "No")][, risk := factor(risk, ordered = T)]
pki_cpy[, HeartDiseaseorAttack := factor(HeartDiseaseorAttack, ordered = T)]

# no changes for car_arr - stroke

# age (numeric) - age (categories) 

# age ranges used in pki
# 3: 30-34
# 4: 35-39
# 5: 40-44
# 6: 45-49
# 7: 50-54
# 8: 55-59
# 9: 60-64
#10: 65-69
#11: 70-74
#12: 75-79
#13: >80O

ager <- function(age){
  age2 <- 0
  if(30 <= age && age <= 34){age2 = 3}
  else if (35 <=age && age <= 39){age2 = 4}
  else if (40 <=age && age <= 44){age2 = 5}
  else if (45 <=age && age <= 49){age2 = 6}
  else if (50 <=age && age <= 54){age2 = 7}
  else if (55 <=age && age <= 59){age2 = 8}
  else if (60 <=age && age <= 64){age2 = 9}
  else if (65 <=age && age <= 69){age2 = 10}
  else if (70 <=age && age <= 74){age2 = 11}
  else if (75 <=age && age <= 79){age2 = 12}
  else if (age >= 80) {age2 = 13}
  return(age2)
}

grc_cpy[, age := ager(age), by = row_id]

# convert age to factor
grc_cpy[, age := factor(age, ordered = T)]

# drop row_id and score for grc_cpy
grc_cpy[, row_id := NULL][, score:=NULL]


# rename columns in the copies to facilitate join
setnames(pki_cpy, c("HeartDiseaseorAttack", "Age", "Stroke"), c("risk", "age", "car_arr"))
glimpse(pki_cpy)

# order the data according to age since its the only category with multiple values
pki_cpy <- pki_cpy[order(age)]

grc_cpy <- grc_cpy[order(age)]
glimpse(grc_cpy)

grc_cpy[, row_id := .I]
pki_cpy[, row_id := .I]

# combine the data together on risk, age, car_arr
final_data = as.data.table(inner_join( pki_cpy, grc_cpy, by = c("risk", "age", "car_arr", "row_id")))

# export the combined data file
# fwrite(final_data, file = "combined_pki_grc_final.csv")

# PART 4 - MODEL ON COMBINED DATA USING


# Random Forest Classifier - 1
# XGBoost Classifier - 2

# remove unused variables
glimpse(final_data)
# row_id
final_data[, row_id := NULL]

# Train_Test Split 75-25
split_main <- partition(final_data$risk, p = c(train = 0.75, test = 0.25))
str(split_main)

main.rf <- initial_split(final_data, strata = risk)
main.train.rf <- final_data[split_main$train]
main.test.rf <- final_data[split_main$test]

# create a basic random forest classifier

# mtry1 -> number of variables to be randomly taken at each split
mtry1 <- ceiling(sqrt(dim(main.train.rf)[2] - 1))
rf1 <- ranger(dependent.variable.name = "risk", data = main.train.rf, num.trees = 500,
              importance = "impurity", mtry = mtry1, classification = T)

# metrics with test set
prob.test <- predict(rf1, data = main.test.rf, type = 'response')
RF_test <- table(testset.actual = main.test.rf$risk, prob.test$predictions, deparse.level = 2)
RF_test

confusionMatrix(data = prob.test$predictions, reference = main.test.rf$risk, positive = 'Yes')

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   No  Yes
# No  4251  126
# Yes   22  631
# 
# Accuracy : 0.9706          
# 95% CI : (0.9655, 0.9751)
# No Information Rate : 0.8495          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.878           
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.8336          
#             Specificity : 0.9949          
#          Pos Pred Value : 0.9663          
#          Neg Pred Value : 0.9712          
#              Prevalence : 0.1505          
#          Detection Rate : 0.1254          
#    Detection Prevalence : 0.1298          
#       Balanced Accuracy : 0.9142          
#                                           
#        'Positive' Class : Yes 

# looking at the metrics from the untuned random forest, the model has performed exceptionally well
# on the testing data

# lets try and tune the parameters for rf to see if we can achieve better results

# create a recipe that is untrained on data to note variables
rf1_rec <- recipe(risk ~ ., data = main.train.rf) %>%
  step_dummy(all_nominal(), -risk) %>%
  step_corr(all_predictors())
# create a trained recipe rf_prep
rf_prep <- prep(rf1_rec)
rf_prep
juiced <- juice(rf_prep)
juiced


# set tuning using n_trees 500 to match the previous run
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")
tune_spec

# create a workflow for ease of processing
tune_wf <- workflow() %>%
  add_recipe(rf1_rec) %>%
  add_model(tune_spec)

# set cross-folds for data
main.train.rf.folds <- vfold_cv(main.train.rf)

tune_res <- tune_grid(
  tune_wf,
  resamples = main.train.rf.folds,
  grid = 20
)
tune_res

# assess hyperparameter metrics by visualising Area Under Curve
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# set ranges of hyperparameters to test based on tune
rf_grid <- grid_regular(
  mtry(range = c(35, 50)),
  min_n(range = c(2, 8)),
  levels = 5
)

rf_grid

# more targeted tune using rf_grid
regular_res <- tune_grid(
  tune_wf,
  resamples = main.train.rf.folds,
  grid = rf_grid
)

regular_res

# plot AUC by number of trees
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

# choose the best model
best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf

# view variable importances

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(risk ~ .,
      data = juice(rf_prep)) %>%
  vip(geom = "point")

# final workflow
final_wf <- workflow() %>%
  add_recipe(rf1_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(main.rf)

final_res %>%
  collect_metrics()

# model metrics

# confusion matrix 
conf_mat_rf <- final_res %>%
  collect_predictions() %>%
  conf_mat(risk, .pred_class)


# XGBoost classifier using the metrics from previous runs

# The final values used for the model were nrounds = 350, max_depth = 4, eta = 0.1, gamma =
#   0.01, colsample_bytree = 0.75, min_child_weight = 0 and subsample = 0.5.

# train and test using previous splits
main.train.xg <- final_data[split_main$train]
main.test.xg <- final_data[split_main$test]


# store the independent variables within a sparse matrix to represent categorical variables..
## .. as numeric variables (like one hot encoding but less dimensional) 
final.train.matrix <- sparse.model.matrix(risk~., data = data.frame(main.train.xg))[, -1]
head(final.train.matrix)

final.ov <- factor(main.train.xg[, risk])


# train final model
xg_final <- train(x = final.train.matrix,
  y = final.ov,
  method = 'xgbTree',
  metric = 'ROC',
  trControl = train_ctrl,
  tuneGrid = tuneGridXGB)

# output column factor

final.ov.test <- factor(main.test.xg[, risk])

# matrix transformation for test dataset
final.test.matrix <- sparse.model.matrix(risk~., data = data.frame(main.test.xg))[, -1]

# predict values from the matrix
final.test.predict <- predict(xg_final, final.test.matrix)

# print metrics in form of Confusion Matrix
confusionMatrix(data = final.test.predict, reference = main.test.xg$risk, positive = 'Yes')






# Alternative Method - Monte Carlo Simulation 

# iterate through the rows of the personal key indicators given by patients and
# use the means of rows to compute predicted values for the grace predictors
# this allows patients who are unable to go for health checkups to still use the stuff

# Alternative Method - Monte Carlo Simulation 

# iterate through the rows of the personal key indicators given by patients and
# use the means of rows to compute predicted values for the grace predictors
# this allows patients who are unable to go for health checkups to still use the stuff

# doParallel::registerDoParallel(cores = 64)

pki2 <- fread("Heart_Disease_Indicators_1.csv")
pki2[, HeartDiseaseorAttack := ifelse(HeartDiseaseorAttack == 1, "Yes", "No")]

sum(pki2[,pki2$Stroke == 1])

coefficients = c(0.0531,0.0087,-0.0168,0.1823,0.6931,1.4586,0.4700,0.8755)

prob.list <- rep(NA, dim(pki2)[1])

runs <- 10000
for(i in 1:dim(pki2)[1]){
  age <- pki2$Age[i] * 5 + as.integer(runif(1, 0, 5))
  pulse = rnorm(runs,mean = 70.59, sd = 8.36)
  bpsys = rnorm(runs, mean = 128.4, sd = 19.6)
  creat_mg = (rnorm(runs, mean = 0, sd = 40))/10
  killip = rnorm(runs, mean = 1, sd = 5)
  carrst = rep(pki2$Stroke[i], runs)
  posinit = as.integer(runif(runs, 0,2))
  stchange = as.integer(runif(runs,0,2))
  grace <- data.table(age,pulse,bpsys,creat_mg,killip,carrst,posinit,stchange)
  xb = (sum(as.matrix(grace) %*% coefficients)-7.7035)/runs
  prob = 1 - exp(1)^(xb)/(1+exp(1)^(xb))
  prob.list[i] = mean(prob)
}



summary(prob.list)

pki2 <- pki2[order(HeartDiseaseorAttack)]
pki2[, probs := prob.list]
pki2[, risk := ifelse(prob.list > 0.06, "Yes", "No")]
pki2[, HeartDiseaseorAttack := factor(HeartDiseaseorAttack)][, risk := factor(risk)]
5595/(18298+5595)
accuracy <- 1 - sum(ifelse(pki2$HeartDiseaseorAttack == pki2$risk, 1, 0))/dim(pki2)[1]
accuracy

confusionMatrix(data = pki2$risk, reference = pki2$HeartDiseaseorAttack, positive = 'Yes')
# Monte Carlo simulation did not work properly in R, refer to python file for actual metrics