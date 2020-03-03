Get Out The Vote
================
Jim Gruman
2020-03-03

## Get Out The Vote Case Study

Adapted from the Julia Silge [Supervised ML
Course](https://supervised-ml-course.netlify.com/chapter3)

Objective: use data on attitudes and beliefs in the United States to
predict the 2016 voter turnout. Apply your skills in dealing with
imbalanced data and explore more resampling options.

The Data Source is the [Democracy Fund Voter
Survey](https://www.voterstudygroup.org/publication/2016-voter-survey)

![Democracy
Fund](https://www.voterstudygroup.org/assets/build/img/main-logo.svg)

Views captured in the survey included:

1.  Life in America today for people like you compared to fifty years
    ago is better? about the same? worse?

2.  Was your vote primarily a vote in favor of your choice or was it
    mostly a vote against his/her opponent?

3.  How important are the following issues to you?

<!-- end list -->

  - Crime
  - Immigration
  - The environment
  - Gay rights

How do the reponses on the survey vary with voting behavior, at least on
three survey metrics?

``` r
voters %>%
    group_by(turnout16_2016) %>%
    summarise(`Elections don't matter` = mean(rigged_system_1_2016 <= 2, na.rm=TRUE),
              `Economy is getting better` = mean(econtrend_2016 == 1, na.rm = TRUE),
              `Crime is very important` = mean(imiss_a_2016 == 2, na.rm = TRUE))
```

    ## Error in eval(lhs, parent, parent): object 'voters' not found

``` r
library(ggplot2)
library(ggtext)
scales::show_col(viridis_pal()(2))
```

![](GetOutTheVote_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
voters %>%
  ggplot(aes(econtrend_2016, ..density.., fill = turnout16_2016))+
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1)+
  labs(title = "Overall, is the economy getting *better* or *worse* for those that <b><span style='color:#440154FF'>Voted</span></b><br> and those that <b><span style='color:#FDE725FF'>Did not vote</span></b>?", 
       caption = str_c("Jim Gruman, ", Sys.Date()),
       x = element_blank()) +
  theme(legend.position = "none"  ,
        plot.title.position = "plot",
         plot.title = element_textbox_simple(
      size = 10,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      fill = "white"
    ))+     #tip from William R Chase at RStudio Conference 2020
  scale_fill_viridis(discrete = TRUE)
```

    ## Error in eval(lhs, parent, parent): object 'voters' not found

Check the data missing-ness and find features with nearly zero variance

``` r
nearZeroVar(voters, names = TRUE)
```

    ## Error in nzv(x, freqCut = freqCut, uniqueCut = uniqueCut, saveMetrics = saveMetrics, : object 'voters' not found

``` r
Amelia::missmap(voters[,-1])
```

    ## Error in Amelia::missmap(voters[, -1]): object 'voters' not found

First fit a simple model, to take a quick look under the hood

``` r
# Remove the case_indetifier column

# Build a simple logistic regression model
simple_glm <- glm(turnout16_2016 ~ .,
                  family = "binomial", 
                  data = voters)
```

    ## Error in is.data.frame(data): object 'voters' not found

``` r
# Print a summary of significant features             

tidy(simple_glm) %>%
  filter(p.value < 0.05) %>%
  arrange(desc(estimate))
```

    ## Error in tidy(simple_glm): object 'simple_glm' not found

Splitting training and testing data for modeling, at 80/20%, stratifying
evenly on the response class turnout16\_2016

``` r
library(rsample)

vote_split<- rsample::initial_split(voters, 0.8, strata = "turnout16_2016")
```

    ## Error in is_null(names): object 'voters' not found

``` r
vote_train <- training(vote_split)
```

    ## Error in analysis(x): object 'vote_split' not found

``` r
vote_test <- testing(vote_split)
```

    ## Error in assessment(x): object 'vote_split' not found

``` r
vote_train%>%
  count(turnout16_2016)
```

    ## Error in eval(lhs, parent, parent): object 'vote_train' not found

``` r
prop.table(table(vote_train$turnout16_2016))
```

    ## Error in table(vote_train$turnout16_2016): object 'vote_train' not found

``` r
prop.table(table(vote_test$turnout16_2016))
```

    ## Error in table(vote_test$turnout16_2016): object 'vote_test' not found

Impute the missing feature values within the training set with k-nearest
neighbors.

``` r
preProcValues <- preProcess(vote_train, method = c("knnImpute"))
```

    ## Error in preProcess(vote_train, method = c("knnImpute")): object 'vote_train' not found

``` r
vote_train<-predict(preProcValues, vote_train)
```

    ## Error in predict(preProcValues, vote_train): object 'preProcValues' not found

``` r
preProcValues <- preProcess(vote_test, method = c("knnImpute"))
```

    ## Error in preProcess(vote_test, method = c("knnImpute")): object 'vote_test' not found

``` r
vote_test<- predict(preProcValues, vote_test)
```

    ## Error in predict(preProcValues, vote_test): object 'preProcValues' not found

``` r
Amelia::missmap(vote_train)
```

    ## Error in Amelia::missmap(vote_train): object 'vote_train' not found

Model with upsampling to adjust for the unbalanced class distributions

``` r
vote_glm <- train(turnout16_2016 ~ .,
                  method = "glm",
                  family = "binomial",
                  data = vote_train,
                  metric = "Kappa",
                  trControl = trainControl(method ="none",
                                           sampling = "up",
                                           summaryFunction = twoClassSummary,
                                           classProbs = TRUE
                                           ))
```

    ## Error in eval(expr, p): object 'vote_train' not found

``` r
vote_glm
```

    ## Error in eval(expr, envir, enclos): object 'vote_glm' not found

A better re-sampling approach, with cross validation, for the glm
logistic model

``` r
vote_glm <- train(turnout16_2016 ~ ., 
                  method = "glm", 
                  family = "binomial",
                  data = vote_train,
                  metric = "ROC",
                  trControl = trainControl(method = "repeatedcv", 
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE,
                         repeats = 2,
                         sampling = "up"))
```

    ## Error in eval(expr, p): object 'vote_train' not found

``` r
vote_glm
```

    ## Error in eval(expr, envir, enclos): object 'vote_glm' not found

The same re-sampling approach, with cross validation, for a random
forest model

``` r
vote_randomf <- train(turnout16_2016 ~ ., 
                  method = "rf", 
                  data = vote_train,
                  trControl = trainControl(method = "repeatedcv",
                                           repeats = 2, 
                 summaryFunction = twoClassSummary,
                 classProbs = TRUE,
                 sampling = "up"))
```

    ## Error in eval(expr, p): object 'vote_train' not found

``` r
vote_randomf
```

    ## Error in eval(expr, envir, enclos): object 'vote_randomf' not found

``` r
plot(vote_randomf)
```

    ## Error in plot(vote_randomf): object 'vote_randomf' not found

``` r
vote_train %>%
    mutate(`Logistic regression` = predict(vote_glm, vote_train)) %>%
    conf_mat(truth = turnout16_2016, estimate = "Logistic regression")
```

    ## Error in eval(lhs, parent, parent): object 'vote_train' not found

``` r
vote_train %>%
    mutate(`Random forest` = predict(vote_randomf, vote_train)) %>%
    conf_mat(truth = turnout16_2016, estimate = "Random forest")
```

    ## Error in eval(lhs, parent, parent): object 'vote_train' not found

``` r
vote_test %>%
    mutate(`Logistic regression` = predict(vote_glm, vote_test)) %>%
    conf_mat(truth = turnout16_2016, estimate = "Logistic regression")
```

    ## Error in eval(lhs, parent, parent): object 'vote_test' not found

``` r
vote_test%>%
    mutate(`Random forest` = predict(vote_randomf, vote_test)) %>%
    conf_mat(truth = turnout16_2016, estimate = "Random forest")
```

    ## Error in eval(lhs, parent, parent): object 'vote_test' not found

Which model is best? Simplest? Consider also calling sens() and spec()

``` r
library(h2o)
```

    ## 
    ## ----------------------------------------------------------------------
    ## 
    ## Your next step is to start H2O:
    ##     > h2o.init()
    ## 
    ## For H2O package documentation, ask for help:
    ##     > ??h2o
    ## 
    ## After starting H2O, you can use the Web UI at http://localhost:54321
    ## For more information visit http://docs.h2o.ai
    ## 
    ## ----------------------------------------------------------------------

    ## 
    ## Attaching package: 'h2o'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cor, sd, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     %*%, %in%, &&, ||, apply, as.factor, as.numeric, colnames,
    ##     colnames<-, ifelse, is.character, is.factor, is.numeric, log,
    ##     log10, log1p, log2, round, signif, trunc

``` r
h2o.init()
```

    ## 
    ## H2O is not running yet, starting it now...
    ## 
    ## Note:  In case of errors look at the following log files:
    ##     C:\Users\ug472\AppData\Local\Temp\RtmpMxZ9tW/h2o_UG472_started_from_r.out
    ##     C:\Users\ug472\AppData\Local\Temp\RtmpMxZ9tW/h2o_UG472_started_from_r.err
    ## 
    ## 
    ## Starting H2O JVM and connecting: . Connection successful!
    ## 
    ## R is connected to the H2O cluster: 
    ##     H2O cluster uptime:         7 seconds 679 milliseconds 
    ##     H2O cluster timezone:       America/Chicago 
    ##     H2O data parsing timezone:  UTC 
    ##     H2O cluster version:        3.28.0.4 
    ##     H2O cluster version age:    8 days  
    ##     H2O cluster name:           H2O_started_from_R_UG472_zzg229 
    ##     H2O cluster total nodes:    1 
    ##     H2O cluster total memory:   3.54 GB 
    ##     H2O cluster total cores:    4 
    ##     H2O cluster allowed cores:  4 
    ##     H2O cluster healthy:        TRUE 
    ##     H2O Connection ip:          localhost 
    ##     H2O Connection port:        54321 
    ##     H2O Connection proxy:       NA 
    ##     H2O Internal Security:      FALSE 
    ##     H2O API Extensions:         Amazon S3, Algos, AutoML, Core V3, TargetEncoder, Core V4 
    ##     R Version:                  R version 3.6.2 (2019-12-12)

``` r
voters_hf <- as.h2o(voters)
```

    ## Error in as.h2o(voters): object 'voters' not found

``` r
y<- "turnout16_2016"
x <- setdiff(colnames(voters_hf),y)
```

    ## Error in is.data.frame(x): object 'voters_hf' not found

``` r
voters_hf[,y] <- as.factor(voters_hf[,y])
```

    ## Error in is.H2OFrame(x): object 'voters_hf' not found

``` r
sframe <- h2o.splitFrame(data  = voters_hf,
                         ratios = c(0.7,0.15),
                         seed = 42)
```

    ## Error in is.H2OFrame(fr): object 'voters_hf' not found

``` r
train <- sframe[[1]]
```

    ## Error in eval(expr, envir, enclos): object 'sframe' not found

``` r
valid <- sframe[[2]]
```

    ## Error in eval(expr, envir, enclos): object 'sframe' not found

``` r
test <- sframe[[3]]
```

    ## Error in eval(expr, envir, enclos): object 'sframe' not found

``` r
h2o.describe(voters_hf)
```

    ## Error in is.H2OFrame(fr): object 'voters_hf' not found

``` r
summary(train$turnout16_2016, exact_quantiles = TRUE)
```

    ## Error in train$turnout16_2016: object of type 'closure' is not subsettable

``` r
# Train Gradient Boosted Model
gbm_model <- h2o.gbm(x = x, 
                     y = y,
                     training_frame = train, 
                     validation_frame = valid)
```

    ## Error: argument 'training_frame' must be a valid H2OFrame or key

``` r
perf <- h2o.performance(gbm_model, test)
```

    ## Error in is(model, "H2OModel"): object 'gbm_model' not found

``` r
# Extract confusion matrix
h2o.confusionMatrix(perf)
```

    ## Error in h2o.confusionMatrix(perf): object 'perf' not found

``` r
# Extract logloss
h2o.logloss(perf)
```

    ## Error in is(object, "H2OModelMetrics"): object 'perf' not found

``` r
# Train random forest model
rf_model <- h2o.randomForest(x = x,
                             y = y,
                             training_frame = train,
                             validation_frame = valid)
```

    ## Error: argument 'training_frame' must be a valid H2OFrame or key

``` r
# Calculate model performance
perf <- h2o.performance(rf_model, valid = TRUE)
```

    ## Error in is(model, "H2OModel"): object 'rf_model' not found

``` r
perf <- h2o.performance(gbm_model, test)
```

    ## Error in is(model, "H2OModel"): object 'gbm_model' not found

``` r
h2o.confusionMatrix(perf)
```

    ## Error in h2o.confusionMatrix(perf): object 'perf' not found

``` r
# Extract logloss
h2o.logloss(perf)
```

    ## Error in is(object, "H2OModelMetrics"): object 'perf' not found

``` r
stopping_params <- list(strategy = "RandomDiscrete",
                        stopping_metric = "misclassification",
                        stopping_rounds = 2,
                        stopping_tolerance = 0.1,
                        seed = 42)

###
###            AutoML
###                     must have stop criteria

automl_model <- h2o.automl(x=x, y=y,
                           training_frame = train,
                           nfolds = 6,
                           # validation_frame = valid, field is ignored, automl assumes 5 nfolds cv
                           max_runtime_secs = 1200,      ### pick something much longer than 60 seconds
                           sort_metric = "logloss",
                           seed = 42)
```

    ## Error: argument 'training_frame' must be a valid H2OFrame or key

``` r
lb<- automl_model@leaderboard
```

    ## Error in eval(expr, envir, enclos): object 'automl_model' not found

``` r
lb
```

    ## Error in eval(expr, envir, enclos): object 'lb' not found

``` r
model_ids <- as.data.frame(lb)$model_id
```

    ## Error in as.data.frame(lb): object 'lb' not found

``` r
aml_leader <-automl_model@leader
```

    ## Error in eval(expr, envir, enclos): object 'automl_model' not found

``` r
summary(aml_leader)
```

    ## Error in summary(aml_leader): object 'aml_leader' not found

``` r
# Run automatic machine learning
automl_model <- h2o.automl(x = x, 
                           y = y,
                           training_frame = train,
                           max_runtime_secs = 1200,
                           sort_metric = "mean_per_class_error",
                           nfolds = 5,
                           validation_frame = valid,
                           seed = 42)
```

    ## Error: argument 'training_frame' must be a valid H2OFrame or key

``` r
lb<- automl_model@leaderboard
```

    ## Error in eval(expr, envir, enclos): object 'automl_model' not found

``` r
lb
```

    ## Error in eval(expr, envir, enclos): object 'lb' not found

``` r
aml_leader <-automl_model@leader
```

    ## Error in eval(expr, envir, enclos): object 'automl_model' not found

``` r
h2o.performance(aml_leader, test)
```

    ## Error in is(model, "H2OModel"): object 'aml_leader' not found

``` r
summary(aml_leader)
```

    ## Error in summary(aml_leader): object 'aml_leader' not found

``` r
# Get model ids for all models in the AutoML Leaderboard

model_ids <- as.data.frame(automl_model@leaderboard$model_id)[,1]
```

    ## Error in as.data.frame(automl_model@leaderboard$model_id): object 'automl_model' not found

``` r
# Get the "All Models" Stacked Ensemble model

se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
```

    ## Error in grep("StackedEnsemble_AllModels", model_ids, value = TRUE): object 'model_ids' not found

``` r
# Get the Stacked Ensemble metalearner model

metalearner <- h2o.getModel(se@model$metalearner$name)
```

    ## Error in paste0(.h2o.__MODELS, "/", model_id): object 'se' not found

``` r
h2o.varimp(metalearner)
```

    ## Error in h2o.varimp(metalearner): object 'metalearner' not found

``` r
h2o.varimp_plot(metalearner)
```

    ## Error in h2o.varimp(model): object 'metalearner' not found

``` r
# h2o.saveModel(aml@leader, path = "./")

# h2o.download_mojo(aml@leader, path = "./")

 h2o.shutdown()
```

    ## Are you sure you want to shutdown the H2O instance running at http://localhost:54321/ (Y/N)?

    ## [1] TRUE

``` r
 # LOOK FOR THE PROMPT IN THE CONSOLE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
```
