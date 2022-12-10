library(tidyverse)
library(tidymodels)
library(discrim)
library(baguette)
library(bonsai)
library(patchwork)

diabetes <- read_csv("../input/pima-indians-diabetes-database/diabetes.csv")
diabetes$Outcome <- as.factor(diabetes$Outcome)
diabetes %>% glimpse()
diabetes %>% head()
diabetes %>% summary()

diabetes$Pregnancies %>% summary()
hist_pregnancies <- ggplot(data = diabetes, aes(x = Pregnancies)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "Pregnancies Histogram Plot", x = "Pregnancies", y = "Count") +
  theme_minimal()

box_pregnancies <- ggplot(data = diabetes, aes(x = Pregnancies)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "Pregnancies Box Plot", x = "Pregnancies") +
  theme_minimal()

hist_pregnancies + box_pregnancies


diabetes$Glucose %>% summary()
hist_glucose <- ggplot(data = diabetes, aes(x = Glucose)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "Glucose Histogram Plot", x = "Glucose", y = "Count") +
  theme_minimal()

box_glucose <- ggplot(data = diabetes, aes(x = Glucose)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "Glucose Box Plot", x = "Glucose") +
  theme_minimal()

hist_glucose + box_glucose


diabetes$BloodPressure %>% summary()
hist_blood <- ggplot(data = diabetes, aes(x = BloodPressure)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "BloodPressure Histogram Plot", x = "BloodPressure", y = "Count") +
  theme_minimal()

box_blood <- ggplot(data = diabetes, aes(x = BloodPressure)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "BloodPressure Box Plot", x = "BloodPressure") +
  theme_minimal()

hist_blood + box_blood



diabetes$SkinThickness %>% summary()
hist_skin <- ggplot(data = diabetes, aes(x = SkinThickness)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "SkinThickness Histogram Plot", x = "SkinThickness", y = "Count") +
  theme_minimal()

box_skin <- ggplot(data = diabetes, aes(x = SkinThickness)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "SkinThickness Box Plot", x = "SkinThickness") +
  theme_minimal()

hist_skin + box_skin



diabetes$Insulin %>% summary()
hist_insulin <- ggplot(data = diabetes, aes(x = Insulin)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "Insulin Histogram Plot", x = "Insulin", y = "Count") +
  theme_minimal()

box_insulin <- ggplot(data = diabetes, aes(x = Insulin)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "Insulin Box Plot", x = "Insulin") +
  theme_minimal()

hist_insulin + box_insulin



diabetes$BMI %>% summary()
hist_bmi <- ggplot(data = diabetes, aes(x = BMI)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "BMI Histogram Plot", x = "BMI", y = "Count") +
  theme_minimal()

box_bmi <- ggplot(data = diabetes, aes(x = BMI)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "BMI Box Plot", x = "BMI") +
  theme_minimal()

hist_bmi + box_bmi



diabetes$DiabetesPedigreeFunction %>% summary()
hist_pedigree <- ggplot(data = diabetes, aes(x = DiabetesPedigreeFunction)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "DiabetesPedigreeFunction Histogram Plot", x = "DiabetesPedigreeFunction", y = "Count") +
  theme_minimal()

box_pedigree <- ggplot(data = diabetes, aes(x = DiabetesPedigreeFunction)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "DiabetesPedigreeFunction Box Plot", x = "DiabetesPedigreeFunction") +
  theme_minimal()

hist_pedigree + box_pedigree



diabetes$Age %>% summary()
hist_age <- ggplot(data = diabetes, aes(x = Age)) +
  geom_histogram(color = "darkblue", fill = "lightblue") +
  labs(title = "Age Histogram Plot", x = "Age", y = "Count") +
  theme_minimal()

box_age <- ggplot(data = diabetes, aes(x = Age)) +
  geom_boxplot(color = "darkblue", fill = "lightblue") +
  labs(title = "Age Box Plot", x = "Age") +
  theme_minimal()

hist_age + box_age



diabetes$Outcome %>% summary()
ggplot(data = diabetes, aes(x = Outcome)) +
  stat_count(fill = "steelblue") +
  labs(title = "Outcome Bar Plot", x = "Outcome", y = "Count") +
  theme_minimal()



#OUTLIER ANALYSIS
Pregnancies_out <- boxplot(diabetes$Pregnancies, plot = FALSE)
diabetes$Pregnancies[diabetes$Pregnancies <= Pregnancies_out$stats[1]] <- Pregnancies_out$stats[1]
diabetes$Pregnancies[diabetes$Pregnancies >= Pregnancies_out$stats[5]] <- Pregnancies_out$stats[5]

Glucose_out <- boxplot(diabetes$Glucose, plot = FALSE)
diabetes$Glucose[diabetes$Glucose <= Glucose_out$stats[1]] <- Glucose_out$stats[1]
diabetes$Glucose[diabetes$Glucose >= Glucose_out$stats[5]] <- Glucose_out$stats[5]

BloodPressure_out <- boxplot(diabetes$BloodPressure, plot = FALSE)
diabetes$BloodPressure[diabetes$BloodPressure <= BloodPressure_out$stats[1]] <- BloodPressure_out$stats[1]
diabetes$BloodPressure[diabetes$BloodPressure >= BloodPressure_out$stats[5]] <- BloodPressure_out$stats[5]

SkinThickness_out <- boxplot(diabetes$SkinThickness, plot = FALSE)
diabetes$SkinThickness[diabetes$SkinThickness <= SkinThickness_out$stats[1]] <- SkinThickness_out$stats[1]
diabetes$SkinThickness[diabetes$SkinThickness >= SkinThickness_out$stats[5]] <- SkinThickness_out$stats[5]

Insulin_out <- boxplot(diabetes$Insulin, plot = FALSE)
diabetes$Insulin[diabetes$Insulin <= Insulin_out$stats[1]] <- Insulin_out$stats[1]
diabetes$Insulin[diabetes$Insulin >= Insulin_out$stats[5]] <- Insulin_out$stats[5]

BMI_out <- boxplot(diabetes$BMI, plot = FALSE)
diabetes$BMI[diabetes$BMI <= BMI_out$stats[1]] <- BMI_out$stats[1]
diabetes$BMI[diabetes$BMI >= BMI_out$stats[5]] <- BMI_out$stats[5]

DiabetesPedigreeFunction_out <- boxplot(diabetes$DiabetesPedigreeFunction, plot = FALSE)
diabetes$DiabetesPedigreeFunction[diabetes$DiabetesPedigreeFunction <= DiabetesPedigreeFunction_out$stats[1]] <- DiabetesPedigreeFunction_out$stats[1]
diabetes$DiabetesPedigreeFunction[diabetes$DiabetesPedigreeFunction >= DiabetesPedigreeFunction_out$stats[5]] <- DiabetesPedigreeFunction_out$stats[5]

Age_out <- boxplot(diabetes$Age, plot = FALSE)
diabetes$Age[diabetes$Age <= Age_out$stats[1]] <- Age_out$stats[1]
diabetes$Age[diabetes$Age >= Age_out$stats[5]] <- Age_out$stats[5]



# TRAINING TESTING CROSS VALIDATION
set.seed(123)

diabetes_split <- initial_split(diabetes, prop = 0.80)
diabetes_split

diabetes_train <- training(diabetes_split)
diabetes_test  <- testing(diabetes_split)

diabetes_cv <- vfold_cv(diabetes_train, v = 10)



# preprocessing

model_recipe <- 
  recipe(Outcome ~ ., data = diabetes_train) %>%
  step_mutate(age_group = ifelse(Age %in% 21:35, 0, 1)) %>%
  step_log(Age) %>%
  step_zv(all_predictors())



#LOGISTIC REGRESSION
log_model <- 
  logistic_reg(mode = "classification",
               penalty = tune(),
               mixture = tune(),
               engine = "glmnet"
  )

set.seed(123)
log_wf <-
  workflow() %>%
  add_model(log_model) %>% 
  add_recipe(model_recipe)
log_wf

log_results <-
  log_wf %>% 
  tune_grid(resamples = diabetes_cv,
            metrics = metric_set(accuracy)
  )

log_results %>%
  collect_metrics()

param_final <- log_results %>%
  select_best(metric = "accuracy")
param_final

log_wf <- log_wf %>%
  finalize_workflow(param_final)
log_wf

log_fit <- log_wf %>%
  last_fit(diabetes_split)

test_performance <- log_fit %>% collect_predictions()
test_performance

diabetes_metrics <- metric_set(accuracy, f_meas, precision, recall)
diabetes_metrics(data = test_performance, truth = Outcome, estimate = .pred_class)

conf_mat(test_performance, Outcome, .pred_class)


#NAIVE BAYES
nb_model <- 
  naive_Bayes(mode = "classification",
              smoothness = tune(),
              Laplace = tune(),
              engine = "naivebayes"
  )

set.seed(123)
nb_wf <-
  workflow() %>%
  add_model(nb_model) %>% 
  add_recipe(model_recipe)
nb_wf

nb_results <-
  nb_wf %>% 
  tune_grid(resamples = diabetes_cv,
            metrics = metric_set(accuracy)
  )

nb_results %>%
  collect_metrics()

param_final <- nb_results %>%
  select_best(metric = "accuracy")
param_final

nb_wf <- nb_wf %>%
  finalize_workflow(param_final)
nb_wf

nb_fit <- nb_wf %>%
  last_fit(diabetes_split)

test_performance <- nb_fit %>% collect_predictions()
test_performance

diabetes_metrics <- metric_set(accuracy, f_meas, precision, recall)
diabetes_metrics(data = test_performance, truth = Outcome, estimate = .pred_class)

conf_mat(test_performance, Outcome, .pred_class)



#MULTI LAYER PERCEPTON
mlp_model <- 
  mlp(mode = "classification",
      hidden_units = tune(),
      penalty = tune(),
      epochs = tune(),
      engine = "nnet"
  )

set.seed(123)
mlp_wf <-
  workflow() %>%
  add_model(mlp_model) %>% 
  add_recipe(model_recipe)
mlp_wf

mlp_results <-
  mlp_wf %>% 
  tune_grid(resamples = diabetes_cv,
            metrics = metric_set(accuracy)
  )

mlp_results %>%
  collect_metrics()

param_final <- mlp_results %>%
  select_best(metric = "accuracy")
param_final

mlp_wf <- mlp_wf %>%
  finalize_workflow(param_final)
mlp_wf

mlp_fit <- mlp_wf %>%
  last_fit(diabetes_split)

test_performance <- mlp_fit %>% collect_predictions()
test_performance

diabetes_metrics <- metric_set(accuracy, f_meas, precision, recall)
diabetes_metrics(data = test_performance, truth = Outcome, estimate = .pred_class)

conf_mat(test_performance, Outcome, .pred_class)