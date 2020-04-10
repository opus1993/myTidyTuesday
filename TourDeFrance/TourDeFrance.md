TourDeFrance
================
Jim Gruman
09\. April 2020

## Visualising Tour De France Data

> Inspired by the work of Alastair Rushworth at [Visualising Tour De
> France Data In
> R](https://alastairrushworth.github.io/Visualising-Tour-de-France-data-in-R/)

> and [David Robinson’s live
> screencast](https://www.youtube.com/watch?v=vT-DElIaKtE)

> and [Dr. Margaret Siple’s
> work](https://github.com/mcsiple/tidytuesday/blob/master/TourdeFrance.R)

<center>

![](https://www.letour.fr/img/dyn/event/1@2x.png)

</center>

> and by the R4DS learning community \#TidyTuesday

### Importing Libraries and Datasets

``` r
library(tidyverse)
library(paletteer)
#library(ggtext)
library(rvest)
theme_set(theme_minimal())
library(lubridate)

tuesdata <- tidytuesdayR::tt_load('2020-04-07')

tdf_winners <- tuesdata$tdf_winners %>%
       mutate(year = ymd(year(start_date),truncated = 2L),
              speed = distance / time_overall)

stage_data <- tuesdata$stage_data
tdf_stages <- tuesdata$tdf_stages %>%
  janitor::clean_names() %>%
  mutate(year = year(date))

#flagadd1<-"<img src='"
#flagadd2<-".png'  width='30' /><br>"
```

### Nationality of the Winners

``` r
tdf_nations<-tdf_winners %>%
  mutate(nationality = stringr::str_squish(nationality),
         nationality = case_when(
    nationality == "Great Britain" ~ "United-Kingdom",
    nationality == "United States" ~ "United-States-of-America",
    TRUE ~ nationality
  )) %>%  
  count(nationality, sort = TRUE) %>%
  mutate(nationality = fct_reorder(nationality,n)) %>%
  top_n(8, n) 

#map(tdf_nations$nationality, ~download.file(
#  paste0("https://www.countries-ofthe-world.com/flags-normal/flag-of-",.,".png"),
#                                           paste0(.,".png")))

#labels<-map_chr(tdf_nations$nationality, ~paste0(flagadd1,.,flagadd2,.))

pal <- RColorBrewer::brewer.pal('Set1',n=8)


nations<-tdf_nations %>%
  ggplot(aes(n, nationality))+
  geom_col(fill = "green")+
  geom_text(aes(label = nationality), 
            position = position_dodge(width = 0.3),
            hjust = -0.4)+
#  scale_y_discrete(            used with ggtext to add flag icons
#    name = NULL,
#    labels = labels
#  ) +
  expand_limits(x = c(0,45))+
#  theme(axis.text.y = element_markdown(color = "black", size = 11)) +
    hrbrthemes::theme_ft_rc() +
    theme(axis.text.y = element_blank(),
        plot.title.position = "plot")+
  labs( y = "",
        title = "Nations with the most Tour de France Wins",
        caption = paste0('@Jim_Gruman | #TidyTuesday | ', Sys.Date())
  )

nations
```

![](TourDeFrance_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Physical characteristics and race characteristics by decade

``` r
library(lubridate)
by_decade <-tdf_winners %>%
  group_by(decade = 10 * (year(year) %/% 10 )) %>%
  summarize(winner_age = mean(age, na.rm = TRUE),
            winner_height = mean(height, na.rm = TRUE),
            winner_weight = mean(weight, na.rm = TRUE),
            winner_margin = mean(time_margin, na.rm = TRUE),
            winner_time = mean(time_overall, na.rm = TRUE),
            winner_speed = mean(speed, na.rm = TRUE))

by_decade %>%  
  ggplot(aes(decade, winner_age)) +
  geom_line(color = "#9EB0FFFF", size = 3) +
  expand_limits(y = 0)+
  labs( y = "",
    title = "Average Age of Tour de France Winners By Decade",
    subtitle = 'source: Alastair Rushworths R Data Package tdf and Kaggle',
    caption = paste0('@Jim_Gruman | #TidyTuesday | ', Sys.Date())
  )+
  theme(legend.position = "")+
  scale_color_paletteer_d("nord::aurora")
```

![](TourDeFrance_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
by_decade %>%  
  ggplot(aes(decade, winner_margin))+
  geom_line(color = "#5AA3DAFF", size = 3) +
  expand_limits(y = 0)+
  labs( y = "Hours",
    title = "Average Margin of Victory of Tour de France Winners By Decade",
    subtitle = 'source: Alastair Rushworths R Data Package tdf and Kaggle',
    caption = paste0('@Jim_Gruman | #TidyTuesday | ', Sys.Date())
  )+
  theme(legend.position = "")
```

![](TourDeFrance_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
by_decade %>%  
  ggplot(aes(decade, winner_speed))+
  geom_line(color = "#2D7597FF", size = 3) +
  expand_limits(y = 0)+
  labs( y = "Hours",
    title = "Average Speed of Tour de France Winners By Decade",
    subtitle = 'source: Alastair Rushworths R Data Package tdf and Kaggle',
    caption = paste0('@Jim_Gruman | #TidyTuesday | ', Sys.Date())
  )+
  theme(legend.position = "")
```

![](TourDeFrance_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
# average margin has been shrinking
```

### Life Expectancy of TDF winners with survival analysis

extrapolated for the riders 38 still alive (not yet dead)

``` r
library(survival)
surv_model <- tdf_winners %>%
  distinct(winner_name, .keep_all = TRUE)  %>%
  transmute(birth_year = year(born),
            death_year = year(died),
            dead =  as.integer(!is.na(death_year))) %>%
  mutate(age_at_death = coalesce(death_year, 2020)- birth_year) %>%
  survfit(Surv(age_at_death, dead) ~ 1, data = .) 

surv_model %>%
  plot()
```

![](TourDeFrance_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
library(broom)

glance(surv_model)
```

    ## # A tibble: 1 x 9
    ##   records n.max n.start events rmean rmean.std.error median conf.low conf.high
    ##     <dbl> <dbl>   <dbl>  <dbl> <dbl>           <dbl>  <dbl>    <dbl>     <dbl>
    ## 1      63    63      63     38  69.6            2.69     77       71        82

Of the 63 Tour De France winners, 38 are still alive. After accounting
for survival expectations for the living, the median life expectancy of
a Tour de France winner is estimated as 77 years old.

### Stage data

``` r
p1<-stage_data %>%
  group_by(decade = 10 * (year %/% 10 )) %>%
  distinct(rider, edition, age) %>%
    ggplot(aes(decade, age)) +
  geom_jitter(color = "#9EB0FFFF", size = 0.5, alpha = 0.1)

p1 + 
  geom_line(data = by_decade, aes(decade, winner_age), 
            color = "#2D7597FF", size = 3) +
  expand_limits(y = 0)+
  labs( y = "", x= "",
    title = "Average Age of Tour de France Winners By Decade",
    subtitle = 'source: Alastair Rushworths R Data Package tdf and Kaggle',
    caption = paste0('@Jim_Gruman | #TidyTuesday | ', Sys.Date())
  )+
  theme(legend.position = "")+
  scale_color_paletteer_d("nord::aurora")
```

![](TourDeFrance_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
stages_joined<-stage_data  %>%
  tidyr::extract(stage_results_id, "stage", "stage-(.*)") %>%
  mutate(stage = if_else(year %in% 1967:1968 & stage == 0, "1a", stage),
         stage = if_else(year %in% 1967:1968 & stage == 1,"1b", stage),
         stage = if_else(year %in% 1969:2012 & stage == 0, "P", stage)) %>%
  filter(year < 2018) %>%
  left_join(tdf_stages, by = c("year","stage")) %>% 
  select(-winner, bib_number, winner_country) %>%
  mutate(rank = as.integer(rank)) %>%
  group_by(year, stage) %>%
  mutate(finishers = sum(!is.na(rank))) %>%
  ungroup() %>%
  mutate(percentile = 1- rank / finishers)


stages_joined %>%
  group_by(winner_country) %>%
  summarize(stages = n(), median_percentile = median(percentile, na.rm = TRUE)) %>%
  arrange(desc(stages))
```

    ## # A tibble: 41 x 3
    ##    winner_country stages median_percentile
    ##    <chr>           <int>             <dbl>
    ##  1 FRA             63231             0.495
    ##  2 BEL             45020             0.495
    ##  3 ITA             28970             0.496
    ##  4 NED             20185             0.496
    ##  5 ESP             17210             0.5  
    ##  6 GER             11659             0.497
    ##  7 GBR             10858             0.5  
    ##  8 SUI              6632             0.497
    ##  9 LUX              6100             0.5  
    ## 10 USA              6063             0.497
    ## # ... with 31 more rows

``` r
stages_joined %>%
  count(year, stage) %>%
  ggplot(aes(n)) +
  geom_histogram()
```

![](TourDeFrance_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# it appears that some racers are eliminated or drop out as stages are completed

total_points <- stages_joined %>%
  group_by(year, rider) %>%
  summarize(total_points = sum(points, na.rm = TRUE)) %>%
  mutate(points_rank = percent_rank(total_points)) %>%
  ungroup()
```

### Does the winner of the first stage predict their final point ranking?

``` r
stages_joined %>%
  filter(stage == "1") %>%
  inner_join(total_points, by = c("year","rider")) %>%
  select(year, rider, 
         percentile_first_stage = percentile,
         points_rank) %>%
  filter(!is.na(percentile_first_stage)) %>%
  mutate(first_stage_bin = cut(percentile_first_stage, seq(0,1,0.1))) %>%
  filter(!is.na(first_stage_bin))%>%
  ggplot(aes(first_stage_bin, points_rank)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Decile Perforance in the First Stage",
       y = "Overall Points Percentile",
       title = "Relationship of TDF First Stage Finish with Overall Finish",
       subtitle = 'source: Alastair Rushworths R Data Package tdf and Kaggle',
       caption = paste0('@Jim_Gruman | #TidyTuesday | ', Sys.Date()))
```

![](TourDeFrance_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
library(gganimate)
library(tidytext)

top_10_2016<-total_points %>%
  filter(year == 2016)%>%
  top_n(10, total_points)

stages_joined %>%
  filter(year == 2016) %>%
  semi_join(top_10_2016, by = "rider")%>%
  mutate(stage = as.integer(stage),
         points = coalesce(points, 0)) %>%
  arrange(stage)%>%
  group_by(rider) %>%
  mutate(cumulative_points = cumsum(points))%>%
  ungroup() %>%
#  mutate(rider = reorder_within(rider, cumulative_points, stage))%>%
  ggplot(aes(cumulative_points, rider,fill = cumulative_points))+
  geom_col()+
  transition_time(stage)+
  theme(legend.position = "none",
        plot.title.position = "plot")+
  labs(title = "The 2016 Tour de France Stage: {frame_time}",
       x = "Cumulative Points at Stage",
       y = "")
```

![](TourDeFrance_files/figure-gfm/unnamed-chunk-8-1.gif)<!-- -->

# Lets explore the names and life durations of the Tour de France winners

``` r
unique(tdf_winners$birth_country)
```

    ##  [1] "Italy"       "France"      "Belgium"     "Luxembourg"  "Switzerland"
    ##  [6] "Spain"       "Netherlands" "USA"         "Ireland"     "Denmark"    
    ## [11] "Germany"     "Australia"   "Kenya"       "Wales"       "Columbia"

``` r
winners<- tdf_winners %>%
  select(edition,winner_name,born,died,nickname,nationality,start_date, year) %>%
  # factor and reorder the winners by birth date
  mutate(winner_name = fct_reorder(winner_name, desc(born))) %>%
  # compute a life duration in numeric years
  mutate(life_duration = as.numeric(as.duration(ymd(born) %--%ymd(died)),"years")) %>%
  filter(!is.na(life_duration))

pal <- RColorBrewer::brewer.pal('Set1',n=8)

life_wins<-winners %>%
  ggplot()+
  geom_linerange(aes(ymin = born,
                     ymax = died,
                     x=winner_name,
                     color=life_duration),lwd=1.1) +
  coord_flip() +
  labs(x = "Winner",
       y = "Year") +
  geom_point(aes(x=winner_name,
                 y=year),
                 shape=19, size=2,color='grey')+
  scale_shape_identity('',
         labels = 'Won the \nTour de France',
         breaks=c(19),
         guide = 'legend') +
  scale_colour_gradient2('Lifetime \n(years)',
                         low = pal[1], high=pal[8], midpoint = 60) +
  labs(title='Lifespans of Tour de France winners', 
       subtitle = 'source: Alastair Rushworths R Data Package tdf and Kaggle',
       caption = paste0('@Jim_Gruman | #TidyTuesday | ', Sys.Date())) +
  guides(colour = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  hrbrthemes::theme_ft_rc() +
  theme(plot.title.position = "plot")

life_wins
```

![](TourDeFrance_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
library(patchwork)
png('TourdeFrance.png',width = 8,height = 12,units='in',res=120)

(life_wins / nations ) + plot_layout(heights = c(3,1))

dev.off()
```

    ## png 
    ##   2
