Get Out The Vote
================
Jim Gruman
2020-03-06

## Get Out The Vote Case Study

Adapted from the Julia Silge [Supervised ML
Course](https://supervised-ml-course.netlify.com/chapter3) and updated
with Tidymodels in place of Caret.

Objective: use data on attitudes and beliefs in the United States to
predict the 2016 voter turnout. Apply skills in dealing with imbalanced
data and explore resampling options.

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
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  labs(title = "Overall, is the economy getting *better* or *worse* for those that <b><span style='color:#440154FF'>Voted</span></b> and those that <b><span style='color:#FDE725FF'>Did not vote</span></b>?", 
       caption = str_c("Jim Gruman, ", Sys.Date()),
       x = element_blank()) +
  theme(legend.position = "none"  ,
        plot.title.position = "plot",
         plot.title = element_textbox_simple(
      size = 10,
      padding = unit(c(5.5, 5.5, 5.5, 5.5), "pt"),
      margin = unit(c(0, 0, 5.5, 0), "pt"),
      fill = "white"
    ))+     #tip from William R Chase at RStudio Conference 2020
  scale_fill_viridis(discrete = TRUE)
```

    ## Error in eval(lhs, parent, parent): object 'voters' not found

Check the data missing-ness and find features with nearly zero variance

``` r
Amelia::missmap(voters[,-1])
```

    ## Error in Amelia::missmap(voters[, -1]): object 'voters' not found

``` r
#library(GGally)
#ggscatmat(voters, columns = 1:104, color = "asia", alpha = 0.5)
```

Split the data into training and testing, then center, scale, and impute
the missing fields with the recipes package functions. All the
predictors in this dataset are numeric, so no dummies are required. If
dummies were required, they would be created ahead of the centering,
scaling, and box-cox transformations.

``` r
voters_split<-voters %>% initial_split(strata = turnout16_2016)
```

    ## Error in eval(lhs, parent, parent): object 'voters' not found

``` r
voters_tr<-training(voters_split)
```

    ## Error in analysis(x): object 'voters_split' not found

``` r
voters_te<-testing(voters_split)
```

    ## Error in assessment(x): object 'voters_split' not found

``` r
voters_rec <- recipe(turnout16_2016 ~ .,
                    data = voters_tr) %>%
              step_knnimpute(all_predictors(), neighbors = 3) %>%
              step_center(all_predictors()) %>% 
              step_scale(all_predictors()) %>%
              prep(training = voters_tr, retain = TRUE) 
```

    ## Error in is_tibble(data): object 'voters_tr' not found

``` r
train_data <- bake(voters_rec,new_data= voters_tr)
```

    ## Error in bake(voters_rec, new_data = voters_tr): object 'voters_rec' not found

``` r
x_train<- voters_rec %>% juice(all_predictors())
```

    ## Error in eval(lhs, parent, parent): object 'voters_rec' not found

``` r
y_train<- voters_rec %>% juice(all_outcomes())
```

    ## Error in eval(lhs, parent, parent): object 'voters_rec' not found

``` r
test_data<- bake(voters_rec, new_data=voters_te)
```

    ## Error in bake(voters_rec, new_data = voters_te): object 'voters_rec' not found

``` r
Amelia::missmap(train_data[,-1])
```

    ## Error in Amelia::missmap(train_data[, -1]): object 'train_data' not found

First fit a simple model, to take a quick look under the hood

``` r
glm_spec<-logistic_reg(mode = "classification") %>%
         set_engine("glm")

glm_fit<- glm_spec %>%
          fit(turnout16_2016 ~ ., data = train_data)
```

    ## Error in fit.model_spec(., turnout16_2016 ~ ., data = train_data): object 'train_data' not found

``` r
rf_spec<-rand_forest(mode = "classification") %>%
         set_engine("ranger")

rf_fit<- rf_spec %>%
          fit(turnout16_2016 ~ ., data = train_data)
```

    ## Error in fit.model_spec(., turnout16_2016 ~ ., data = train_data): object 'train_data' not found

``` r
results_train <- glm_fit %>%
   predict(new_data = train_data) %>%
   mutate(
     truth = train_data$turnout16_2016,
     model = "glm"
   ) %>%
   bind_rows(rf_fit %>%
            predict(new_data = train_data) %>%
            mutate(truth = train_data$turnout16_2016,
                   model = "rf"
  )) 
```

    ## Error in eval(lhs, parent, parent): object 'glm_fit' not found

``` r
results_test <- glm_fit %>%
   predict(new_data = test_data) %>%
   mutate(
     truth = test_data$turnout16_2016,
     model = "glm"
   ) %>%
   bind_rows(rf_fit %>%
            predict(new_data = test_data) %>%
            mutate(truth = test_data$turnout16_2016,
                   model = "rf"
  )) 
```

    ## Error in eval(lhs, parent, parent): object 'glm_fit' not found

``` r
results_train %>%
  group_by(model) %>%
  metrics(truth = truth, .pred_class)
```

    ## Error in eval(lhs, parent, parent): object 'results_train' not found

``` r
results_test %>%
  group_by(model)  %>%
  metrics(truth = truth, .pred_class)
```

    ## Error in eval(lhs, parent, parent): object 'results_test' not found

Let’s look for improvement with hyperparameter tuning on the random
forest models.

``` r
vote_boot <- bake(voters_rec, voters) %>%
        bootstraps(times = 30)
```

    ## Error in bake(voters_rec, voters): object 'voters_rec' not found

``` r
rf_spec <- rand_forest(
  mode = "classification",
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_engine("ranger")

rf_grid <- tune_grid(
  turnout16_2016 ~ ., 
  model = rf_spec,
  resamples = vote_boot
)
```

    ## Error in check_rset(resamples): object 'vote_boot' not found

``` r
rf_grid %>%
  collect_metrics()
```

    ## Error in eval(lhs, parent, parent): object 'rf_grid' not found

``` r
rf_grid %>%
  show_best("roc_auc")
```

    ## Error in eval(lhs, parent, parent): object 'rf_grid' not found
