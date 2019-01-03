setwd("C:/Users/Public/Documents/ESDA/Energy Data Analytics/Project/Project/Luke-Skywalker")

library(tidyverse)
library(lubridate)
library(survey)
library(dplyr)
library(purrrlyr)
library(magrittr)
library(kernlab)
library(kknn)
library(glmnet)
library(broom)
library(corrplot)
library(modelr)

delt <- read.csv("DSR_deltas.csv",stringsAsFactors = F)

sacl <- read.csv("sa_clean_nna2.csv",stringsAsFactors = F)

sacl$Household_id <- as.character(sacl$Household_id)
sacl_d <- subset(sacl,str_detect(Household_id,"^D"))
sacl_d$Household_id <- as.character(sacl_d$Household_id) %>% str_remove_all("D")
sacl_d$Household_id <- as.numeric(sacl_d$Household_id)
sacl_d <- sacl_d[,-1]

sacl_d <- left_join(sacl_d,delt, by=c("Household_id"="id"))

### Plotting

ggplot(sacl_d[sacl_d$high_delta<0.075&sacl_d$high_delta>-0.075,])+
  geom_boxplot(aes(x=as.factor(spec_clust1), y=as.numeric(high_delta)))

### Modelling

dsr_simple_model <- lm(combined_DRS~spec_clust1,data=sacl_d)
dsr_simp_out <- tidy(dsr_simple_model)

dsr_simp_out$p.value<.05

#### lasso

y <- sacl_d$combined_DRS
x <- sacl_d[,c(2:34,46:74)] %>% as.matrix()

lasso <- glmnet(x, y, family = "gaussian", alpha = 1)
plot(lasso,label=TRUE)
plot(lasso,xvar="lambda",label=TRUE)

cvfit = cv.glmnet(x, y)
plot(cvfit)
cvfit$lambda.min
cvfit$lambda.1se
log(cvfit$lambda.1se)
lasso.coef <- coef(cvfit, s = "lambda.1se")
round(lasso.coef,4)

sacl_d$lasso_pred <- predict(cvfit,x,s="lambda.min")

ggplot(sacl_d)+
  geom_density(aes(x=lasso_pred))+
  geom_density(aes(x=combined_DRS),color="blue")+
  xlab("black = prediction")

### OLS regression with variables from lasso

dsr_simple_model2 <- lm(combined_DRS~n_Tumble_dryer+
                         n_Tumble_dryer+
                         n_Dishwasher+              
                         n_Portable_electric_heater+ 
                         Q328+           
                         hwater_hwater_elecimm+       
                         n_bedrooms ,data=sacl_d)
dsr_simp_out2 <- tidy(dsr_simple_model2)
dsr_simp_out2
summary(dsr_simple_model2)


temp <-  sacl_d[,which(names(sacl_d) %in% c("n_Tumble_dryer","n_Dishwasher","n_Portable_electric_heater","Q328","heat_nocentralheat","hwater_hwater_elecimm","n_bedrooms"))] %>% as.data.frame()
cor(temp) %>% corrplot(method="ellipse",tl.pos="lt",tl.srt=45, tl.cex = .75)
cor(temp) %>% corrplot(order = "hclust", addrect = 2, col = "whiteblack", bg = "gold2",tl.pos="lt",tl.srt=45, tl.cex = .75)

#### Ridge
y <- sacl_d$combined_DRS
x <- sacl_d[,which(names(sacl_d) %in% c("n_Tumble_dryer","n_Dishwasher","n_Portable_electric_heater","Q328","heat_nocentralheat","hwater_hwater_elecimm","n_bedrooms"))] %>% as.matrix()

ridge <- glmnet(x, y, family = "gaussian", alpha = 0)
plot(ridge,label=TRUE)
plot(ridge,xvar="lambda",label=TRUE)

cvfit = cv.glmnet(x, y)
plot(cvfit)
cvfit$lambda.min
cvfit$lambda.1se
log(cvfit$lambda.1se)
ridge.coef <- coef(cvfit, s = "lambda.1se")
round(ridge.coef,4)

sacl_d$ridge_pred <- predict(cvfit,x,s="lambda.min")

ggplot(sacl_d)+
  geom_density(aes(x=ridge_pred))+
  geom_density(aes(x=combined_DRS),color="blue")+
  xlab("black = prediction")

sacl_d$lm_pred<- add_predictions(sacl_d,dsr_simple_model2)

sacl_d$lm_pred$combined_DRS

ggplot(sacl_d)+
  geom_density(aes(x=lm_pred$combined_DRS))+
  geom_density(aes(x=combined_DRS),color="blue")+
  xlab("black = prediction")

### Decision tree

dtree <- ctree(combined_DRS~n_Tumble_dryer+
           n_Dishwasher+              
           n_Portable_electric_heater+ 
           Q328+           
           hwater_hwater_elecimm+       
           n_bedrooms ,data=sacl_d)
plot(dtree)

dsr_simple_model3 <- lm(DRS_rank~n_Tumble_dryer+
                          n_Portable_electric_heater+
                          hwater_hwater_elecimm+       
                          n_bedrooms ,data=sacl_d)

summary(dsr_simple_model3)
