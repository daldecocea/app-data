### Setup ###
rm(list = ls())
library(stargazer) # See docs: https://cran.r-project.org/web/packages/stargazer/stargazer.pdf
# SET YOUR WORKING DIRECTORY

### Import Data ###
data = read.csv("cleaned_data.csv")
developer_data = read.csv('developer_data.csv')
new_app_data = read.csv('new_app_data.csv')

# Redefining variables for stargazer
food_drink = data$food_._drink
health_fitness = data$health_._fitness
graphics_design = data$graphics_._design
magazines_newspapers = data$magazines_._newspapers
photo_video = data$photo_._video

### 1. ATT’s App-Level Impacts in the Long Run ###
reg1a = lm(differential_excl_with_lookback ~ data_used_to_track_you + log(ratings_count + 1) + ratings_score + price + purchases + age, data = data)
reg1b = lm(differential_excl_with_lookback ~ data_used_to_track_you + log(ratings_count + 1) + ratings_score + price + purchases + age + log(excess_ratings_count + 1), data = data)
reg1c = lm(differential_excl_with_lookback ~ data_used_to_track_you + log(ratings_count + 1) + ratings_score + price + purchases + age + log(excess_ratings_count + 1) + social_networking, data = data)
stargazer(reg1a, reg1b, reg1c,
          dep.var.caption = 'Long-Run Version History Differentials (Pre v. Post-ATT)',
          covariate.labels = c('Tracking', 'Log Ratings Count', 'Ratings Score', 'Price', 'In-App Purchases', 'Minimum Age', 'Log Developer Excess Ratings Count', 'Social Networking'),
          dep.var.labels = c("LR Differential w/ Lookback"),
          type = 'html',
          notes.label = 'Significance Level:',
          single.row = TRUE,
          out = 'long_run_effects.html')

### 2. ATT’s App-Level Impacts in the Short Run ###
reg2a = lm(sr_differential_excl_with_lookback ~ data_used_to_track_you + log(ratings_count + 1) + ratings_score + price + purchases + age, data = data)
reg2b = lm(sr_differential_excl_with_lookback ~ data_used_to_track_you + log(ratings_count + 1) + ratings_score + price + purchases + age + log(excess_ratings_count + 1), data = data)
reg2c = lm(sr_differential_excl_with_lookback ~ data_used_to_track_you + log(ratings_count + 1) + ratings_score + price + purchases + age + log(excess_ratings_count + 1) + social_networking, data = data)
stargazer(reg2a, reg2b, reg2c,
          dep.var.caption = 'Short-Run Version History Differentials (Pre v. Post-ATT)',
          covariate.labels = c('Tracking', 'Log Ratings Count', 'Ratings Score', 'Price', 'In-App Purchases', 'Minimum Age', 'Log Developer Excess Ratings Count', 'Social Networking'),
          dep.var.labels = c("SR Differential w/ Lookback"),
          type = 'html',
          notes.label = 'Significance Level:',
          single.row = TRUE,
          out = 'short_run_effects.html')

### 3. Developer Characteristics Influencing New App Introduction ###
reg3a = lm(dev_new_apps ~ log(dev_total_ratings_count + 1) + dev_avg_ratings_score + dev_category_count, data=developer_data)
reg3b = lm(dev_new_apps ~ log(dev_total_ratings_count + 1) + dev_avg_ratings_score + dev_category_count + log(dev_total_ratings_count + 1)*dev_avg_ratings_score, data=developer_data)
stargazer(reg3a, reg3b,
          dep.var.caption = 'New App Introduction (Post-ATT)',
          covariate.labels = c('Log Developer Ratings Count', 'Developer Avg Ratings Score', 'Developer Category Count', 'Log Developer Ratings Count * Developer Avg Ratings Score'),
          dep.var.labels = 'Developer New Apps',
          type = 'html',
          notes.label = 'Significance Level:',
          out = 'dev_new_apps.html')

# Again redefining variables for stargazer
food_drink = new_app_data$food_._drink
health_fitness = new_app_data$health_._fitness
graphics_design = new_app_data$graphics_._design
magazines_newspapers = new_app_data$magazines_._newspapers
photo_video = new_app_data$photo_._video

### 4. New App Performance ###
reg4a = lm(log(ratings_count + 1) ~ days_active + data_used_to_track_you + ratings_score + price + purchases + age, data=new_app_data)
reg4b = lm(log(ratings_count + 1) ~ days_active + data_used_to_track_you + ratings_score + price + purchases + age + log(excess_ratings_count + 1), data=new_app_data)
reg4c = lm(log(ratings_count + 1) ~ days_active + data_used_to_track_you + ratings_score + price + purchases + age + log(excess_ratings_count + 1) + social_networking, data=new_app_data)
stargazer(reg4a, reg4b, reg4c,
          dep.var.caption = 'New App Performance (Post-ATT)',
          covariate.labels = c('Days Active', 'Tracking', 'Ratings Score', 'Price', 'In-App Purchases', 'Minimum Age', 'Log Developer Excess Ratings Count', 'Social Networking'),
          dep.var.labels = 'Log Ratings Count',
          type = 'html',
          notes.label = 'Significance Level:',
          single.row = TRUE,
          out = 'new_app_performance.html')
