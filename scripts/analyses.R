######Packages##### ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(magrittr)
library(lme4)
library(lmerTest)
library(MASS)
library(ltm)
library(car)
library(performance)
library(glmmTMB)
library(DHARMa)
library(dplyr)
library(psych)
library(emmeans)
library(ggeffects)
################ Original Data Overview ############### ---------------------------------------------------
rawdata<-read.csv(file.choose())
View(rawdata)
#pats
nrow(rawdata)
#36,314 participants

#nations
n_distinct(rawdata$nation)
#58 nations

#pats in each nation
table(rawdata$nation)
# Number of participants in each country originally

#checking for whole countries missing data on key variables
rawdata %>%
  group_by(nation) %>%
  summarize(all_missing = all(is.na(prqc)),
            n = n()) %>%
  filter(all_missing)
#Belgium, India, Iran, Netherlands, Zimbabwe had no PRQC score
rawdata %>%
  group_by(nation) %>%
  summarize(all_missing = all(is.na(avoid)),
            n = n()) %>%
  filter(all_missing)
#No countries missing Avoidance scores
rawdata %>%
  group_by(nation) %>%
  summarize(all_missing = all(is.na(relationship.stat)),
            n = n()) %>%
  filter(all_missing)
#No countries missing relationship status
################ Initial Data Cleaning ############### ---------------------
#Adding Akalisky collectivism scores
aka_scores <- c(
  "Argentina" = 56.2,
  "Australia" = 67.7,
  "Austria" = 64.8,
  "Bangladesh" = 29.5,
  "Belgium" = NA,
  "Brazil" = 51.6,
  "Canada" = 68.3,
  "Chile" = 53.7,
  "China" = 49.4,
  "Colombia" = 46.2,
  "Costa Rica" = NA,
  "Croatia" = 51.4,
  "Cyprus" = 43.1,
  "Czech Rep." = 57.4,
  "Denmark" = 74.2,
  "Ecuador" = 43.8,
  "Estonia" = 56.4,
  "Ethiopia" = 35.3,
  "Finland" = 67.8,
  "Germany" = 67.5,
  "Greece" = 47.4,
  "Hungary" = 53.6,
  "Iceland" = 73.5,
  "India" = 38.7,
  "Indonesia" = 33.9,
  "Iran" = 37.2,
  "Israel" = NA,
  "Italy" = 55.9,
  "Japan" = 59.7,
  "Latvia" = 55.2,
  "Lebanon" = 40.2,
  "Lithuania" = 51.3,
  "Malaysia" = 42.1,
  "Malta" = NA,
  "Mexico" = 47.8,
  "Netherlands" = 71.2,
  "New Zealand" = 69.6,
  "Nigeria" = 31.7,
  "Norway" = 73.8,
  "Philippines" = 43.5,
  "Poland" = 50.9,
  "Portugal" = 58.5,
  "Romania" = 44.0,
  "Russia" = 47.0,
  "Serbia" = 47.5,
  "Slovakia" = 51.1,
  "Slovenia" = 58.1,
  "South Africa" = 48.2,
  "South Korea" = 50.9,
  "Spain" = 64.5,
  "Swaziland" = NA,
  "Switzerland" = 68.8,
  "Taiwan" = 55.2,
  "Tanzania" = NA,
  "Turkey" = 37.0,
  "United Kingdom" = 69.4,
  "United States" = 61.9,
  "Zimbabwe" = 35.5
)
rawdata <- rawdata %>%
  mutate(
    akacoll = aka_scores[nation],
    .after = mincoll
  )
View(rawdata)

#original dataset was missing Slovenia's score from Pelham et al. due to data entry error
rawdata$pelcoll[rawdata$nation == "Slovenia"] <- -0.78

#Reverse Scoring Pelham et al. so collectivism scores are high
rawdata$pelcoll <- -1 * rawdata$pelcoll

#Reverse Scoring Minkov so collectivism score is high
rawdata$mincoll <- -1 * rawdata$mincoll

#Reverse Scoring Akalisky so collectivism score is high
rawdata$akacoll <- 100 - rawdata$akacoll

#Removing Participants not in a relationship
nosingles<-rawdata%>%
  filter(relationship.stat=="coupled")
nrow(rawdata) - nrow(nosingles)
#This leaves 18,357 participants (deleted 17,957)

#Removing Participants with not avoidant attachment score
finaldata<-nosingles%>%
  filter(!if_any(c(secure,dismissive, preoccupied, fearful), is.na))
nrow(finaldata)
nrow(nosingles)
18357-17898
#This leaves 17,898 participants (deleted 459)

#Removing Participants with no Relationship Satisfaction
finaldata<-finaldata%>%
  filter(!if_any(c(prqc), is.na))
nrow(finaldata)
17898-15811
#This leaves 15,811 participants (deleted 2,087)


################ Final Data Overview#################-----------------------
View(finaldata)
#pats
nrow(finaldata)
#15,811 is the maximum # of participants the analyses can have

#nations
n_distinct(finaldata$nation)
#53 nations is the maximum number of nations an analysis can have

#pats in each nation
(fullsample_nationcounts<-table(finaldata$nation))
fullsample_nationcounts
mean(fullsample_nationcounts)
#Number of participants in each country. Ranges from 32 (China) to 3606 (US)
#mean number of people per country is 298.32

#Mean/SD age of full sample
mean(finaldata$age, na.rm = T)
sd(finaldata$age, na.rm = T)
aggregate(age~nation, data=finaldata, mean, na.rm = T)
#Mean age of full sample = 23.54, SD = 6.68
#Across countries mean age range from 19.54 (Philippines) to 32.99 (Austria)

#Sex makeup of full sample
table(finaldata$sex)
table(finaldata$nation, finaldata$sex)
finaldata %>%
  count(nation, sex) %>%
  group_by(nation) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  print(n=53)
#sex makeup of full sample: 9,903 females, 5,908 males
#Proportion female ranges from 10.2% (Hungary) to 83.7% (Denmark)

#relationshipstatus
table(finaldata$relationship.stat.spec)


################ Basic Information about Scales#################  ------------------------------------------
psych::alpha(dplyr::select(finaldata, prqc.1:prqc.6))

################Model 1.1: All nations, Hofstede, Linear################# --------------------------------
#1.1.A: Creating new dataframe for Hofstede Data
hofdata<-finaldata%>%
  filter(!is.na(hofcoll))
nrow(hofdata)
nrow(finaldata)
15811-14344
#1467 participants were in a nation with no hofstede score leaving 14,344 participants
hofdata%>%count(nation,name="n_pats")
#44 Nations have a Hofstede scores

names(finaldata)

no_nas <- finaldata |> 
  select(nation, ends_with('coll')) |> 
  pivot_longer(-nation, names_to = 'measure', values_to = 'value') |> 
  distinct() |> 
  group_by(nation) |> 
  summarise(
    NAs = sum(is.na(value))
  ) |> 
  filter(NAs == 0) |> 
  pull(nation)

finaldata |> 
  filter(nation %in% no_nas) |> 
  ggplot(aes(x = pelcoll)) + 
  geom_histogram(fill = 'firebrick', alpha = .5) + 
  geom_histogram(data = finaldata, aes(x = pelcoll), fill = 'limegreen', alpha = .5)

#1.1.1: Basic Demographics for 1.1
#age M/SD of 1.1
mean(hofdata$age, na.rm=T)
sd(hofdata$age, na.rm=T)
#Age mean and SD is 23.49 and 6.77 respectively

#Sex of 1.1
table(hofdata$sex)
#8,984 females and 5,360 males in 1.1


#1.1.2: Mean Centering Predictors of Model 1.1
#within group mean centering avoidance
hofdata%<>%
  group_by(nation)%>%
  mutate(avoid_mean = mean(avoid))
hofdata%<>%
  mutate(avoid_mc = avoid-avoid_mean)
#grand mean centering Hofstede
hofdata%<>%
  ungroup()%>%
  mutate(hofcoll_mean = mean(hofcoll, na.rm = T))
hofdata%<>%
  mutate(hofcoll_mc=hofcoll-hofcoll_mean)


#1.1.3: Running the null model
mod1.null<-lmer(prqc~1+(1|nation), data = hofdata, REML = T)
summary(mod1.null)
#ICC of null model
0.06896/(0.06896+0.88267)
#ICC suggests 7.24% of variance can be attributable to differnces among nations


#1.1.4: Assessing linearity
hofdata%>%
  ggplot()+
  geom_point(alpha = .1, mapping = aes(x = avoid_mc, y = prqc))+
  facet_wrap(~nation)
#I cant get a good look at linearity from the figure--clear cieling effects
#for prqc scores.

hofdata%>%
  group_by(nation)%>%
  ggplot(mapping = aes(x = avoid_mc, y = prqc, color = factor (nation)))+
  geom_smooth(method = lm, se = F, show.legend = T, fullrange = T)
#There does seem to be variation in intercepts/slopes


#1.1.5: L1 Independence of residuals
#Running model and pulling residuals
mod1.1<-lmer(prqc~1+avoid_mc*hofcoll_mc+(1+avoid_mc|nation), data=hofdata, REML = T)
summary(mod1.1)
hofdata%<>%
  mutate(mod1.1resids=residuals(mod1.1))

#1.1.5a Side quest: Trying different convergence method as model failed to converge
mod1.1a<-lmer(prqc~1+avoid_mc*hofcoll_mc+(1+avoid_mc|nation), data=hofdata, REML = T,
             control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e6)))
summary(mod1.1a)
AIC(mod1.1)
AIC(mod1.1a)
BIC(mod1.1)
BIC(mod1.1a)
#results are basically the same and the model converged, so I will use model 1.1a

#Actual assessing independence
hofdata%>%
  ggplot(mapping=aes(x = avoid_mc, y = mod1.1resids))+
  geom_point(alpha = .1)+
  labs(x = "avoid (mean centered)", y = "residuals")
cor.test(hofdata$avoid_mc, hofdata$mod1.1resids)
#Nonsignificant correlation = good

#1.1.6: L1 residuals are normally distributed for Model 1.1
#histogram
hofdata%>%
  ggplot(mapping=aes(x = mod1.1resids))+
  geom_histogram()
hofdata%>%
  ggplot(mapping=aes(sample=mod1.1resids))+
  stat_qq()
hist(hofdata$prqc)
#clear negative skew in relationship satisfaction--likely ceiling effect

#1.1.6: L2 residuals are independent for Model 1.1
mod1.1l2data<-hofdata%>%
  group_by(nation)%>%
  mutate(prqc_mean=mean(prqc))%>%
  dplyr::select(nation, hofcoll_mc, prqc_mean)%>%
  unique()
mod1.1l2data$intercept_resid = ranef(mod1.1)$nation[,1]
mod1.1l2data$slope_resid = ranef(mod1.1)$nation[,2]
#intercept residuals
mod1.1l2data%>%
  ggplot(mapping = aes(x = intercept_resid, y = hofcoll_mc))+
  geom_point()
cor.test(mod1.1l2data$hofcoll_mc, mod1.1l2data$intercept_resid)
#No correlation = no issues with L2 intercept independence
#slope residuals
mod1.1l2data%>%
  ggplot(mapping = aes(x = slope_resid, y = hofcoll_mc))+
  geom_point()
cor.test(mod1.1l2data$hofcoll_mc, mod1.1l2data$slope_resid)
#No correlation = no issues with L2 slope independence

#slope and intercept residuals
mod1.1l2data%>%
  ggplot(mapping=aes(x=slope_resid, y = intercept_resid))+
  geom_point()
cor.test(mod1.1l2data$intercept_resid, mod1.1l2data$slope_resid)
#No correlation = no issues with L2 slope/intercept independence


#1.1.7: Level 2 Normality of Model 1.1
#intercept residuals
mod1.1l2data%>%
  ggplot(mapping = aes(x = intercept_resid))+
  geom_histogram(binwidth = .2)
mod1.1l2data%>% 
  ggplot(mapping = aes(sample = intercept_resid)) +
  stat_qq()
#not perfect, but not problematic
#slope residuals
mod1.1l2data %>%
  ggplot(mapping=aes(x=slope_resid))+
  geom_histogram(binwidth=.005)
mod1.1l2data %>% 
  ggplot(mapping = aes(sample = slope_resid)) +
  stat_qq()
#not perfect, but not too problematic

#1.1.8: Level 2 and Level 1 residuals are independent for Mod 1.1
mod1.1npernation<-hofdata%>%
  group_by(nation)%>%
  dplyr::select(nation)%>%
  count()%>%
  ungroup()%>%
  dplyr::select(n)%>%
  unlist()
hofdata$intercept_resid <- rep(mod1.1l2data$intercept_resid, times = mod1.1npernation)
hofdata$slope_resid <- rep(mod1.1l2data$slope_resid, times = mod1.1npernation)

#level 2 intercept residuals and level 1 residuals
hofdata %>%
  ggplot(mapping = aes(x=mod1.1resids,y=intercept_resid))+
  geom_point()
cor.test(hofdata$mod1.1resids, hofdata$intercept_resid)
#No correlation: no independence between level 1 and 2 residuals

#level 1 residuals and level 2 slope residuals for Model 1.1
hofdata %>%
  ggplot(mapping = aes(x=mod1.1resids,y=slope_resid))+
  geom_point()
cor.test(hofdata$mod1.1resids, hofdata$slope_resid)
#No correlation: no independence between level 1 residuals and level 2 slopes

#level 1 residuals are independent of level 2 predictors for Model 1.1
hofdata %>%
  ggplot(mapping = aes(x=mod1.1resids,y=hofcoll_mc))+
  geom_point()
cor.test(hofdata$mod1.1resids, hofdata$hofcoll_mc)
#No correlation: L1 residuals and l2 predictors are independent

#Level 2 intercept residuals and Level 1 Predictors for Model 1.1
hofdata %>%
  ggplot(mapping = aes(x=intercept_resid,y=avoid_mc))+
  geom_point()
cor.test(hofdata$intercept_resid, hofdata$avoid_mc)
#No correlation: L2 intercept residuals and L1 predictors are independent

#Level 2 slope residuals and Level 1 Predictors
hofdata %>%
  ggplot(mapping = aes(x=slope_resid,y=avoid_mc))+
  geom_point()
cor.test(hofdata$slope_resid, hofdata$avoid_mc)
#No correlation: L2 slope residuals and L1 predictors are independent

#1.1.9: Results of Model 1.1
summary(mod1.1a)
#Main effect of avoidance: As avoidance increases, relationship satisfaction decreases
#Main effect of Hofstede scores: As collectivism increases, relationship satisfaction decreases
#No interaction effect.

#1.1.10: Visualization of Model 1.1
#Categorizing nations
mod1.1_cat <- hofdata %>%
  distinct(nation, hofcoll_mc) %>%
  mutate(hofcoll_cat = factor(ntile(hofcoll_mc, 3),
      labels = c("Low", "Medium", "High")))
hofdata <- hofdata %>%
  left_join(mod1.1_cat %>% dplyr::select(nation, hofcoll_cat), by = "nation")
#Plotting
ggplot(hofdata,aes(x = avoid_mc,y = prqc,color = hofcoll_cat)) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal()
#Who is in what category
mod1.1_cat %>%
  dplyr::select(nation, hofcoll_mc, hofcoll_cat) %>%
  arrange(hofcoll_mc)%>%
  print(n=44)

#Supports there being no interaction


################Model 1.2: All nations, Hofstede, Standard Beta ###########-------------------------
#1.2.1: Smithson-Verkuilen squeezing data
#rescaling
hofdata <- hofdata %>%
  mutate(prqc_scaled = (prqc - 1) / 6)
#squeezing
mod1.2obs<-nrow(hofdata)
hofdata<-hofdata%>%
  mutate(prqc_beta = (prqc_scaled *(mod1.2obs-1)+.5)/mod1.2obs)
#running model
mod1.2 <- glmmTMB(prqc_beta ~1+avoid_mc*hofcoll_mc+(1+avoid_mc|nation),
  family = beta_family(link = "logit"),
  data = hofdata)
#Model failed to converge
summary(mod1.2)
#Main effect of avoidance: as avoidance increases, PRQC decreases
#No main effect of collectivism
#No interaction 
VarCorr(mod1.2)
sum(hofdata$prqc_beta == 0)
sum(hofdata$prqc_beta == 1)
#Small (but not miniscule) random slope variance
################Model 1.3: All nations, Hofstede, One-inflated Beta######## --------
#1.3.1: Flipping 0 to 1s because glmmTMB  can only inflate 0's and not 1's
hofdata<-hofdata%>%
  mutate(prqc_flip = 1-prqc_scaled,
         prqc_flip = ifelse(prqc_flip == 1, 0.999999, prqc_flip))

mod1.3 <- glmmTMB(prqc_flip ~1+avoid_mc*hofcoll_mc+(1+avoid_mc|nation),
  ziformula = ~1,
  family = beta_family(link = "logit"),
  data = hofdata)
summary(mod1.3)
#Avoidance and collectivism has an inverse relationship with prqc--no interaction with Hofstede scores--Note results are flipped
#Also has tiny random slope variance, which may explain the convergence issues

###############Model 1.4: All nations, Hofstede, Ordered Beta######## -------------------------------------------------------------------------
mod1.4 <- glmmTMB(
  prqc_scaled ~ 1+avoid_mc*hofcoll_mc+(1+avoid_mc|nation),
  family = ordbeta, 
  data = hofdata)
summary(mod1.4)
#Avoidance and collectivism has an inverse relationship with prqc--no interaction with Hofstede scores--Note results are flipped
#Also has tiny random slope variance, which may explain the convergence issues
# ############Comparing Models 1.1-1.4############ ------------------------
#making new dataframe plotdata.1 with certain values
#This is back-transforming beta models
plotdata.1<-hofdata%>%mutate(
  pred_mod1.1=predict(mod1.1, type = "response"),
  
  pred_mod1.2=predict(mod1.2, type = "response"),
  pred_mod1.2original = (pred_mod1.2 *6)+1,
  
  pred_mod1.3flip=predict(mod1.3, type = "response"),
  mod1.3scaled = 1 - pred_mod1.3flip,
  pred_mod1.3original = (mod1.3scaled*6)+1,
  
  pred_mod1.4=predict(mod1.4, type = "response"),
  pred_mod1.4original = (pred_mod1.4 *6)+1)

#Comparing Models Observed vs Predicted Values
plot_long.1 <- plotdata.1 %>%
  dplyr::select(prqc,
                pred_mod1.1,
                pred_mod1.2original,
                pred_mod1.3original,
                pred_mod1.4original) %>%
  pivot_longer(-prqc,
    names_to = "Model",
    values_to = "Predicted") %>%
  mutate(Model = dplyr::recode(Model,
                   pred_mod1.1 = "LM",
                   pred_mod1.2original = "Beta",
                   pred_mod1.3original = "One-Inflated Beta",
                   pred_mod1.4original = "Ordered Beta"))
#Scatter plot comparing observed vs predicted--ideally points lie on y = x dashed line
ggplot(plot_long.1, aes(Predicted, prqc)) +
  geom_point(alpha = .5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~Model) +
  labs(title = "Observed vs Predicted") +
  theme_classic()

#beta  or ordered beta may be best

#DHARMa 
mod1.1.sim <- simulateResiduals(fittedModel = mod1.1)
mod1.2.sim  <- simulateResiduals(fittedModel = mod1.2)
mod1.3.sim  <- simulateResiduals(fittedModel = mod1.3)
mod1.4.sim <- simulateResiduals(fittedModel = mod1.4)

#Converting to Z Score
plotdata.1$mod1.1residuals<- qnorm(residuals(mod1.1.sim))
plotdata.1$mod1.2residuals<- qnorm(residuals(mod1.2.sim))
plotdata.1$mod1.3residuals<- qnorm(residuals(mod1.3.sim))
plotdata.1$mod1.4residuals<- qnorm(residuals(mod1.4.sim))

resid_long.1 <- plotdata.1 %>%
  dplyr::select(pred_mod1.1, mod1.1residuals,
                pred_mod1.2original, mod1.2residuals,
                pred_mod1.3original, mod1.3residuals,
                pred_mod1.4original, mod1.4residuals) %>%
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "Model"),
    names_pattern = "(.*)_(lm|beta|oneinf|ordbeta).*"
  ) %>%
  mutate(Model = recode(Model,
                        lm = "linear",
                        beta = "Beta Model",
                        oneinf = "One-Inflated Beta",
                        ordbeta = "Ordered Beta"))



# Model 1.4 ---------------------------------------------------------------
#1.1.A: Creating new dataframe for Hofstede Data
hofdata<-finaldata%>%
  filter(!is.na(hofcoll))

#Model 1.4
mod1.4 <- glmmTMB(
  prqc_scaled ~ 1+age+avoid_mc*hofcoll_mc+(1+avoid_mc|nation),
  family = ordbeta, 
  data = hofdata)
summary(mod1.4)

# Model 2.4 -------------------------------------
#Creating new dataframe for Pelham Data (Model 2.4)
peldata<-finaldata%>%
  filter(!is.na(pelcoll))
nrow(peldata)
nrow(finaldata)
#No participants were missing Pelham scores (so there is 15,811 participants)
peldata%>%count(nation,name="n_pats")
#53 Nations have Pelham scores

#2.1.1: Basic Demographics for 2.1
#age M/SD of 1.1
mean(peldata$age, na.rm=T)
sd(peldata$age, na.rm=T)
#Age mean and SD is 23.54 and 6.78, respectively

#Sex of 2.1
table(peldata$sex)
#9,903 females and 5,908 males in 2.1

#2.1.2: Mean Centering Predictors of Model 1.1
#within group mean centering avoidance
peldata%<>%
  group_by(nation)%>%
  mutate(avoid_mean = mean(avoid))
peldata%<>%
  mutate(avoid_mc = avoid-avoid_mean)
#grand mean centering Hofstede
peldata%<>%
  ungroup()%>%
  mutate(pelcoll_mean = mean(pelcoll, na.rm = T))
peldata%<>%
  mutate(pelcoll_mc=pelcoll-pelcoll_mean)

#creating scaled PRQC for Pelham
peldata <- peldata %>%
  mutate(prqc_scaled = (prqc - 1) / 6)

#Model 2.4
mod2.4 <- glmmTMB(
  prqc_scaled ~ 1+age+avoid_mc*pelcoll_mc+(1+avoid_mc|nation),
  family = ordbeta, 
  data = peldata)
summary(mod2.4)

# Model 3.4 ---------------------------------------------------------------
#Creating new dataframe for Minkov Data (Model 3.4)
mindata<-finaldata%>%
  filter(!is.na(mincoll))
nrow(mindata)
nrow(finaldata)
15811-13285
#2526 participants were missing Minkov scores (so there is 13,285 participants)
mindata%>%count(nation,name="n_pats")
#37 Nations have Minkov scores

#3.1.1: Basic Demographics for 3.1
#age M/SD of 3.1
mean(mindata$age, na.rm=T)
sd(mindata$age, na.rm=T)
#Age mean and SD is 23.67 and 6.96, respectively

#Sex of 3.1
table(mindata$sex)
#8,365 females and 4,920 males in 3.1

#3.1.2: Mean Centering Predictors of Model 1.1
#within group mean centering avoidance
mindata%<>%
  group_by(nation)%>%
  mutate(avoid_mean = mean(avoid))
mindata%<>%
  mutate(avoid_mc = avoid-avoid_mean)
#grand mean centering Minkov
mindata%<>%
  ungroup()%>%
  mutate(mincoll_mean = mean(mincoll, na.rm = T))
mindata%<>%
  mutate(mincoll_mc=mincoll-mincoll_mean)

#creating scaled PRQC for minham
mindata <- mindata %>%
  mutate(prqc_scaled = (prqc - 1) / 6)

#Model 3.4
mod3.4 <- glmmTMB(
  prqc_scaled ~ 1+age+avoid_mc*mincoll_mc+(1+avoid_mc|nation),
  family = ordbeta, 
  data = mindata)
summary(mod3.4)
#Failed to converge

#Testing model without random intercept to see if there...
#...is no random slope to be modeled.
mod3.4test <- glmmTMB(
  prqc_scaled ~ 1 + age + avoid_mc*mincoll_mc +
    (1 | nation),
  family = ordbeta,
  data = mindata
)
summary(mod3.4test)
#It ran indicating the random slopes are causing the convergence issues
# Model 4.4 -------------------------------------
#Creating new dataframe for Aka Data (Model 4.4)
akadata<-finaldata%>%
  filter(!is.na(akacoll))
nrow(akadata)
nrow(finaldata)
15811-15202
#609 participants were missing Minkov scores (so there is 15,202 participants)
akadata%>%count(nation,name="n_pats")
#48 Nations have Aka scores

#4.1.1: Basic Demographics for 4.1
#age M/SD of 4.1
mean(akadata$age, na.rm=T)
sd(akadata$age, na.rm=T)
#Age mean and SD is 23.51 and 6.71, respectively

#Sex of 4.1
table(akadata$sex)
#9,532 females and 5,670 males in 4.1

#4.1.2: Mean Centering Predictors of Model 4.1
#within group mean centering avoidance
akadata%<>%
  group_by(nation)%>%
  mutate(avoid_mean = mean(avoid))
akadata%<>%
  mutate(avoid_mc = avoid-avoid_mean)
#grand mean centering Hofstede
akadata%<>%
  ungroup()%>%
  mutate(akacoll_mean = mean(akacoll, na.rm = T))
akadata%<>%
  mutate(akacoll_mc=akacoll-akacoll_mean)

#creating scaled PRQC for Pelham
akadata <- akadata %>%
  mutate(prqc_scaled = (prqc - 1) / 6)

#Model 4.4
mod4.4 <- glmmTMB(
  prqc_scaled ~ 1+age+avoid_mc*akacoll_mc+(1+avoid_mc|nation),
  family = ordbeta, 
  data = akadata)
summary(mod4.4)

# Model 5.4 ---------------------------------------------------------------
#5.1.A: Creating country-level (Level 2) variable for avoidance
finaldata <- finaldata %>%
  group_by(nation) %>%
  mutate(avoid_level2 = mean(avoid, na.rm = TRUE)) %>%
  ungroup()
View(finaldata)
nrow(finaldata)
#15,811 participants
#53 nations

#5.1.B: Seeing the values for each country
finaldata%>%
  distinct(nation, avoid_level2) %>%
  arrange(desc(avoid_level2)) %>%
  print (n = 53)

#5.1.1: Basic Demographics for 5.1
#age M/SD of 5.1
mean(finaldata$age, na.rm=T)
sd(finaldata$age, na.rm=T)
#Age mean and SD is 23.54 and 6.78, respectively

#Sex of 5.1
table(finaldata$sex)
#9,903 females and 5,908 males in 2.1

#5.1.2: Mean Centering Predictors of Model 5.1
#within group mean centering avoidance (L1)
finaldata<- finaldata %>%
  group_by(nation)%>%
  mutate(avoid_mean = mean(avoid))
finaldata<- finaldata %>%
  mutate(avoid_mc = avoid-avoid_mean)
#grand mean centering Level 2 avoidance
finaldata <- finaldata %>%
  ungroup() %>%
  mutate(avoid_level2_mean=mean(avoid_level2, na.rm = T))
finaldata<- finaldata %>%
    mutate(avoid_level2_mc=avoid_level2-avoid_level2_mean)

#creating scaled PRQC for Pelham
finaldata <- finaldata %>%
  mutate(prqc_scaled = (prqc - 1) / 6)

#Model 5.4
mod5.4 <- glmmTMB(
  prqc_scaled ~ 1+age+avoid_mc*avoid_level2_mc+(1+avoid_mc|nation),
  family = ordbeta, 
  data = finaldata)
summary(mod5.4)

# Model 6.4 ---------------------------------------------------------------
#6.1.A: Creating dataframe
mod6data <- finaldata %>%
  filter(nation %in% c("United States",
                       "Mexico",
                       "Spain",
                       "Turkey",
                       "Israel"))
#Number of participants
nrow(mod6data)
table(mod6data$nation)
#4,433 participants: US: 3606; Israel: 180; Mexico: 85; Spain: 378; Turkey:184

#6.1.1: Basic Demographics for 6.1
#age M/SD of 6.1
mean(mod6data$age, na.rm=T)
sd(mod6data$age, na.rm=T)
#Age mean and SD is 22.37 and 6.27, respectively

#Sex of 6.1
table(mod6data$sex)
#2,937 females and 1,496 males in 6.1

#6.1.2: creating a new avoidance mean center variable (overwriting the existing one that 
#is the mean of the finaldata data frame)
mod6data<- finaldata %>%
  group_by(nation)%>%
  mutate(avoid_mean = mean(avoid))
mod6data%<>%
  mutate(avoid_mc = avoid-avoid_mean)
View(mod6data)

mod6.4 <- glmmTMB(
  prqc_scaled ~ avoid_mc * nation+age,
  family = ordbeta(link = "logit"),
  data = mod6data
)
summary(mod6.4)

#comparing slopes with emmeans
slopes6.4 <- emtrends(
  mod6.4,
  specs = "nation",
  var = "avoid_mc"
)

summary(slopes6.4)
pairs(slopes6.4)
#Figure of mod6.4
pred6.4<- ggpredict(
  mod6.4,
  terms = c("avoid_mc [-2:2 by=0.1]", "nation"))
plot(pred6.4)
