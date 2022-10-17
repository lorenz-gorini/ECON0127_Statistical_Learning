# Question Set 1
# 1.a
fcbk <- read.csv("~/UCL/Statistical learning/assignments/data/fcbk.csv")
# a) How many observations are in the data? How many variables are there?

dim(fcbk)
head(fcbk)

# b) What share of the total number of observations is within the state of California?
nrow(fcbk[fcbk$state == "CA", ])

# c) In which zip codes are the most (the least) assaults per capita?
fcbk_no_na_assault <- fcbk[!is.na(fcbk$assaults17), ]
zip_min_assault <- fcbk_no_na_assault$zip[which.min(fcbk_no_na_assault$assaults17)]
zip_max_assault <- fcbk_no_na_assault$zip[which.max(fcbk_no_na_assault$assaults17)]

# Question Set 2
# Fit two bivariate linear regressions: In model A we wish to predict assaults
# using information on the share of Facebook users interested in Hip Hop music.
# In model B we wish to predict assaults using information on the
# share of Facebook users interested in first-person shooter games.

# a) Which of the two predictors accounts for more variation in the outcome variable?

# assaults17 = target
# fb_hip_hop_music = predictor
model_a <- lm(assaults17 ~ fb_hip_hop_music, data = fcbk)
coef(model_a)
summary(model_a)
# --> std = 538.3

# assaults17 = target
# fb_firstperson_shooter_games = predictor
model_b <- lm(assaults17 ~ fb_firstperson_shooter_games, data = fcbk)
coef(model_b)
summary(model_b)
# --> std = 1881.4

# --> SO.. `fb_hip_hop_music` accounts for more variation in the outcome variable

# b) Predict the assault rate for a zip code in which the share of Facebook users
# interested in in first-person
# shooter games or hip hop music is equal to the average in the data. Construct a confidence interval for
# your predicted value.
(test_fps_games <- data.frame(
    fb_firstperson_shooter_games = mean(fcbk$fb_firstperson_shooter_games)
))
(test_hiphop <- data.frame(
    fb_hip_hop_music = mean(fcbk$fb_hip_hop_music)
))
# Confidence interval for the predicted value
predict(model_b, newdata = test_fps_games, level = 0.95, interval = "prediction")
# Confidence interval for the expected value
predict(model_b, newdata = test_fps_games, level = 0.95, interval = "confidence")
predict(model_a, newdata = test_hiphop, level = 0.95, interval = "confidence")

# c) Construct a model C that leverages both the share of Facebook users interested in Hip Hop music and
# users interested in first-person shooter games to predict assaults. What is the increase in R2
# relative to the best-performing bivariate linear model?

# assaults17 = target
# fb_hip_hop_music = predictor1
# fb_firstperson_shooter_games = predictor2
model_c <- lm(assaults17 ~ fb_firstperson_shooter_games + fb_hip_hop_music, data = fcbk)
coef(model_c)
summary(model_c)

(test_both <- data.frame(
    fb_firstperson_shooter_games = mean(fcbk$fb_firstperson_shooter_games),
    fb_hip_hop_music = mean(fcbk$fb_hip_hop_music)
))
predict(model_c, newdata = test_both, level = 0.95, interval = "confidence")

# TODO: FOR ME: Why is this confidence interval smaller than the other ones, and why is it based on the predictor?

#   d) Suppose that the share of Facebook users interested in Hip Hop music increases from 20 percent to 30
# percent, i.e., a 10 percentage point increase. How many more assaults should we expected according to
# model C? What about an increase from 50 to 60 percent?
coef_model_c <- coef(model_c)
variation_per_unit <- sqrt(
    coef_model_c["fb_firstperson_shooter_games"]**2 + coef_model_c["fb_hip_hop_music"]**2
)
var_10_percent <- variation_per_unit * 0.10
var_10_percent

# Question Set 3
# Construct a linear regression model that uses all continuous variables in the dataset.
# a) What is the increase in R2
# relative to the best-performing bivariate linear model?
# b) Predict the assault rate for a typical zip code (i.e., a zip code for which all variables are centered on
# their mean) and compute the standard error. What percent of the uncertainty for the predicted value
# is due to estimation uncertainty?

predictor_cols <- setdiff(colnames(fcbk), c("state", "city", "zip", "assaults17", "sample"))

model_all <- lm(assaults17 ~ . - state - city - zip - assaults17 - sample, data = fcbk)
coef(model_all)
summary(model_all)
# Residual standard error: 581.5 on 372 degrees of freedom
#   (5 observations deleted due to missingness)
# Multiple R-squared:  0.7573,    Adjusted R-squared:  0.7188


# increase predictors
mod <- lm(assaults17 ~ fb_hip_hop_music + acs_inc25k + acs_pop25_college,
    data = fcbk
)
coef(mod)
summary(mod)

library(texreg)
screenreg(mod)

# Inference (the initial parenthesis makes R show the output stored in `new`)
(new <- data.frame(
    fb_hip_hop_music = c(0.2, 0.4, 0.6),
    acs_inc25k = mean(fcbk$acs_inc25k),
    acs_pop25_college = mean(fcbk$acs_pop25_college)
))