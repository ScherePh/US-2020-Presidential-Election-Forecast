### ----------------------------------------------------------
### Forecasting the 2020 US Presidential Elections:
### A State-Level Forecast
### Philipp Scherer & Jens Wiederspohn
### ----------------------------------------------------------

## Get packages
library(tidyverse)
library(ggplot2)
library(estimatr)
library(ggrepel)
library(ggpmisc)
library(broom)
library(MASS)


### Data Preparation --------------

## Import Data
US <- read.csv("US_Forecast_Dataset_Scherer_Wiederspohn.csv")

## Rename ï..year variable
names(US)[names(US)=="ï..year"] <- "year"

## Variable transformation + creation 
US$incumbent_party[US$incumbent_party == "Democrat" | US$incumbent_party == "Democratic-Farmer-Labor"] <- "Democrat"

US$incumbent_partynum <- NA
US$incumbent_partynum[US$incumbent_party == "Democrat" | US$incumbent_party == "Democratic-Farmer-Labor"] <- 1
US$incumbent_partynum[US$incumbent_party == "Republican"] <- -1
table(US$incumbent_partynum, US$incumbent_party, useNA = "always")

election_years <- unique(US$year)

## Rescale Cooks_PVI and incumbent2pv
US$Cooks_PVI <- US$Cooks_PVI *100
US$incumbent2pv <- US$incumbent2pv * 100

## Exclude unused elections 
US1 <- US %>% filter(year != 1948, year  != 1952, year  != 1956 , year != 1960, year != 1964, year  != 1968, year != 1972, year != 1976, year != 2020 )


### Plot relationship between response and predictors --------------

## Gallup approval rates
model_out <- lm(incumbent2pv ~ gallup_approv_jun, data = US1)
model_out_aug <- augment(model_out)
case_label <- paste(US1$state, US1$year, sep = " ")

ggplot(US1, aes(gallup_approv_jun, incumbent2pv)) + geom_point() + geom_smooth(method='lm', formula= y~x) + facet_wrap(~ state)

ggplot(US1, aes(gallup_approv_jun, incumbent2pv, label = case_label)) + geom_point() + geom_smooth(method='lm', formula= y~x) +  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.025) 

obs_id <- abs(model_out_aug$.std.resid) > 2.3
length(obs_id[obs_id == TRUE])

## Rerunning
ggplot(US1, aes(Rerunning, incumbent2pv)) + geom_point() + geom_smooth(method='lm', formula= y~x) + facet_wrap(~ state)

ggplot(US1, aes(Rerunning, incumbent2pv)) + geom_point() + geom_smooth(method='lm', formula= y~x)
ggplot(US1, aes(Rerunning, incumbent2pv, label = case_label)) + geom_point() + geom_smooth(method='lm', formula= y~x) +  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.025) 

model_out <- lm(incumbent2pv ~ Rerunning, data = US1)
model_out_aug <- augment(model_out)
obs_id <- abs(model_out_aug$.std.resid) > 1.53
length(obs_id[obs_id == TRUE])

## Gallup approval rates * Rerunning
ggplot(US1, aes(gallup_approv_jun:Rerunning, incumbent2pv)) + geom_point() + geom_smooth(method='lm', formula= y~x) + facet_wrap(~ state)

ggplot(US1, aes(gallup_approv_jun:Rerunning, incumbent2pv)) + geom_point() + geom_smooth(method='lm', formula= y~x)

ggplot(US1, aes(gallup_approv_jun*Rerunning, incumbent2pv, label = case_label)) + geom_point() + geom_smooth(method='lm', formula= y~x) +  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.025) 

model_out <- lm(incumbent2pv ~ gallup_approv_jun:Rerunning, data = US1)
model_out_aug <- augment(model_out)
obs_id <- abs(model_out_aug$.std.resid) > 1.53
length(obs_id[obs_id == TRUE])

## Unemployment
ggplot(US1, aes(d_unemployment, incumbent2pv)) + geom_point() + geom_smooth(method='lm', formula= y~x) + facet_wrap(~ state)

ggplot(US1, aes(d_unemployment, incumbent2pv)) + geom_point() + geom_smooth(method='lm', formula= y~x)
ggplot(US1, aes(d_unemployment, incumbent2pv, label = case_label)) + geom_point() + geom_smooth(method='lm', formula= y~x) +  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.025) 

model_out <- lm(incumbent2pv ~ d_unemployment, data = US1)
model_out_aug <- augment(model_out)
obs_id <- abs(model_out_aug$.std.resid) > 1.53
length(obs_id[obs_id == TRUE])

## Cooks PVI * Incumbent Party
ggplot(US1, aes(Cooks_PVI * incumbent_partynum, incumbent2pv)) + geom_point() + geom_smooth(method='lm', formula= y~x) + facet_wrap(~ state)
ggplot(US1, aes(Cooks_PVI * incumbent_partynum, incumbent2pv)) + geom_point() + geom_smooth(method='lm', formula= y~x)
ggplot(US1, aes(Cooks_PVI * incumbent_partynum, incumbent2pv, label = case_label)) + geom_point() + geom_smooth(method='lm', formula= y~x) +  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.025) 

model_out <- lm(incumbent2pv ~ Cooks_PVI * incumbent_partynum, data = US1)
model_out_aug <- augment(model_out)
obs_id <- abs(model_out_aug$.std.resid) > 1.53
length(obs_id[obs_id == TRUE])


### Run models on full sample --------------

model_out <- lm(incumbent2pv ~ gallup_approv_jun * Rerunning  + d_unemployment + Cooks_PVI:incumbent_partynum, data = US1)
summary(model_out)

## Evaluate fit
model_out_fit <- augment(model_out)
model_out_fit$state <- US1$state_po
model_out_fit$year <- US1$year
mean(abs(model_out_fit$.resid)) # mean absolute error

group_by(model_out_fit, model_out_fit$state) %>% summarise(mae = mean(abs(.resid))) # mean absolute error, by state

plot(model_out_fit$.fitted, model_out_fit$incumbent2pv, cex = .5, pch = 20)
grid()
abline(0, 1)


### 2020 forecast, without and with prediction intervals --------------

US2020 <- US %>% filter(year == 2020)
US2020$prediction <- predict(model_out, US2020)

augment(model_out, newdata = US2020)
predict_conf <- predict(model_out, US2020, se.fit = TRUE, interval = "confidence")
predict_pred <- predict(model_out, US2020, se.fit = TRUE, interval = "prediction")

ggplot(US2020,aes(x=state_po, y= prediction, label= state ))+ geom_col() +  geom_hline(yintercept=50, color="red") + labs(x ="States", y = "Vote Share in %")


### 2020 Forecast: Graphical Mapping --------------

## Calculate state tendency
US2020$prediction <- predict(model_out, US2020)

US2020$prediction1[US2020$prediction >= 50] <- 1
US2020$prediction1[US2020$prediction < 50] <- 0
table(US2020$prediction1)

US2020$CollegeInc <- US2020$prediction1 * US2020$College
sum(US2020$CollegeInc)

## Load additioanl packages
library(maps)
library(devtools)  
library(ggmap)

## Mapping state predictions 
statesMap=map_data("state")
str(statesMap)
register_google(key = "AIzaSyBlCZXGDK9dN3Vf_N1qdI6mPfFFCA34ubs")
statesMap = map_data("state")
party_colors <- c("#2E74C0", "#CB454A")
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 

predictionDataFrame <- data.frame(US2020$prediction1,US2020$state)
predictionDataFrame$region <- tolower(predictionDataFrame$US2020.state)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = US2020.prediction1))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2020")

## Predict swing states 
model_out <- lm(incumbent2pv ~ gallup_approv_jun * Rerunning  + d_unemployment + Cooks_PVI:incumbent_partynum, data = US1)

voteshare_pred <- predict(model_out, filter(US, year == 2020), type = "response", se.fit = TRUE)
se_pred <- sqrt(voteshare_pred$se.fit^2+sum((model_out$residuals^2 / model_out$df.residual)))
voteshare_pred <- as.data.frame(voteshare_pred)
voteshare_pred <- voteshare_pred %>% summarise(prob = (pt(abs((voteshare_pred$fit-50)/se_pred), df = 504)))

voteshare_pred$prob[voteshare_pred$prob <= 0.80] <- 0
voteshare_pred$prob[voteshare_pred$prob > 0.80] <- 1

## Mapping swing states predictions 
predictionDataFrame <- data.frame(voteshare_pred$prob,US2020$state)
predictionDataFrame$region <- tolower(predictionDataFrame$US2020.state)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = voteshare_pred.prob))+ geom_polygon(color = "black") + scale_fill_gradient(low = "purple", high = "grey", guide = "legend", breaks= c(0,1), labels = c("Swing State", "No Swing State"), name = "Swing States 2020")            


### Out-of-sample checks --------------------------------------------------

## Run out-of-sample predictions
model_out <- list()
model_pred <- list()
for(i in seq_along(election_years)) {
  insample <- filter(US1, year != election_years[i])
  outsample <- filter(US1, year == election_years[i])
  model_out[[i]] <- lm(incumbent2pv ~ gallup_approv_jun * Rerunning  + d_unemployment                      + Cooks_PVI:incumbent_partynum, data = insample)
  model_pred[[i]] <- augment(model_out[[i]], newdata = outsample, type.predict = "response")
}

## Evaluate fit
model_pred_df <- do.call(rbind, model_pred)
model_pred_df$voteshare <- US1$incumbent2pv

mean(abs(model_pred_df$voteshare - model_pred_df$.fitted), na.rm = TRUE) # Mean absolut error 
sqrt(mean(c(model_pred_df$voteshare - model_pred_df$.fitted)^2)) # Root mean squared error

plot(model_pred_df$.fitted, model_pred_df$voteshare,
     xlab="Predicted Vote Share", ylab="Observed Vote Share", xlim=c(0, 100), 
     ylim=c(0, 100), cex = .5, pch = 20)
grid()
abline(0, 1)


### Arriving at event probabilities / simulate Electoral College --------------

## Calculate standard errors of the predictions
model_out <- lm(incumbent2pv ~ gallup_approv_jun * Rerunning  + d_unemployment + Cooks_PVI:incumbent_partynum, data = US1)

voteshare_pred <- predict(model_out, filter(US, year == 2020), se.fit = TRUE, interval = "prediction")
se_pred <- sqrt(voteshare_pred$se.fit^2+sum((model_out$residuals^2 / model_out$df.residual)))

## Bootstrapping procedure to simulate Electoral College 
voteshare_pred_sim <- replicate(10000, rnorm(rep(1, length(voteshare_pred$fit[,"fit"])), mean = voteshare_pred$fit[,"fit"], sd = se_pred)) %>% t() %>% as.tibble

voteshare_pred_sim[voteshare_pred_sim < 50] <- 0
voteshare_pred_sim[voteshare_pred_sim >= 50] <- 1

College_sim <- voteshare_pred_sim * US2020$College

rowSums(College_sim)

names(voteshare_pred_sim) <- filter(US1, year == 2016)$state %>% as.character 

voteshare_pred_sim_new <- t(voteshare_pred_sim)

Trump <- rowSums(College_sim)
Biden <- 538 - rowSums(College_sim)

## Plot simulated Electoral College outcomes 
plot(density(Trump), xlab = "n of Electorates", ylab = "Density", main = "Density Plot" , xlim = c(50, 500), col = "red")
lines(density(Biden), col = "blue")
legend("topright", inset=.05, title="Party",
       c("Republican","Democrat"), fill= c("red", "blue") , horiz=TRUE)

quantile(Trump, c(.025, .975)) # 95% confidence interval Trump college votes 
mean(Trump) # Mean Trump college votes


## Calculate probability of Trump or Biden victory
prop.table(table(Trump > Biden))