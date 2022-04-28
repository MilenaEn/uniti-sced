library(readr)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(dplyr)
library(digest)
library(skimr)
library(timetk)

library(lme4)
library(performance)
library(patchwork)
library(modelr)
library(lmerTest)
library(rstatix)
library(effectsize)
library(visdat)
library(Hmisc)
library(table1)
library(ggdist)
library(gghalves)


# 1. load data frame sced1 ----

sced1 <- readRDS(file = "sced1.RDS") 
sced <- sced1

# 2. subjects: demographics, app use & thi diff ----

subjects <- sced %>%
  group_by(case_id) %>%
  arrange(con_days) %>%
  filter(row_number()==1) %>%
  select(1:3,6:8,17:43) %>% 
  arrange(case_id) %>% 
  ungroup()

#demographics
table1(~ age + sex + thi_pre + duration_tin_mon + phq9_score + guef_score, data = subjects)

#tinnitus characteristics
subjects %>% count(education)
subjects %>% count(vertigo) 
subjects %>% count(tin_num) 
subjects %>% count(tin_on) 
subjects %>% count(tin_change) 
subjects %>% count(tin_qual) 
subjects %>% count(tin_pitch) 
subjects %>% count(tin_loc) 
subjects %>% count(tin_care) 

#app use
table1(~ n_tinedu + n_son + n_diary, data = subjects)

#thi diff: first switch to long format for boxplot
subjects %>% mutate(thi_diff = thi_pre - thi_post) -> subjects
thi <- select(subjects, case_id, thi_pre, thi_post)
thi <- gather(thi, thi, value, thi_pre:thi_post)
thi$thi <- factor(thi$thi,levels = c('thi_pre','thi_post'),ordered = TRUE)

ggplot(thi, aes(thi, value, fill = thi)) + geom_boxplot(show.legend = FALSE) + ylab("thi value") + xlab("time") + theme_classic(base_size = 14) + scale_fill_manual(values=c("#12737b", "#f2953d"))

ggplot(thi, aes(thi, value, fill = thi)) + ylab("thi value") + xlab("time") + theme_classic(base_size = 14) + scale_fill_manual(values=c("#12737b", "#f2953d")) +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA, show.legend = FALSE) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, show.legend = FALSE
  ) +
  geom_point(
    size = 1.3, show.legend = FALSE,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")


#mean thi diff
table1(~ thi_pre + thi_post + thi_diff + phq9_score, data = subjects)
t.test(subjects$thi_pre, subjects$thi_post, paired = TRUE)
cohens_d(subjects$thi_pre, subjects$thi_post, paired = TRUE)

subjects %>% count(thi_diff >= 7)

# 3. questionnaire data: visual analysis and MLM ----
app <- select(sced, case_id, group_id, questionnaire, age, sex, duration_tin_mon, thi_pre, phq9_score, bfi2_extraversion, bfi2_neg_emotion, con_days, base_days, int_days, magic_days, loudness_cur:emotion_tod)
app <- filter(app, questionnaire == "diary")
sced %>% count(questionnaire)

#remove duplicates by con_days and case_id -> 79 duplicates removed
app <- distinct(app, case_id, con_days, .keep_all= TRUE) 

#complete data set with missing values and refill demographics
complete(app, case_id, con_days) -> df_imp
df_imp %>% arrange(case_id, con_days)

df_imp %>% group_by(case_id) %>% fill(group_id, questionnaire, age, sex, duration_tin_mon, thi_pre, phq9_score, bfi2_extraversion, bfi2_neg_emotion, .direction = c("downup")) -> df_imp

#filter dataset by time: 12 weeks of intervention for each group
test1 <- filter(df_imp[which(df_imp$group_id==1),], con_days <= 95)
test2 <- filter(df_imp[which(df_imp$group_id==2),], con_days <= 103)
test3 <- filter(df_imp[which(df_imp$group_id==3),], con_days <= 100)
test4 <- filter(df_imp[which(df_imp$group_id==4),], con_days <= 92)

df_imp <- rbind(test1, test2, test3, test4)

#add intervention variable to dataset by group and baseline length
df_imp$intervention <- 99
df_imp <- df_imp %>% relocate(intervention, .after = group_id)

df_imp$intervention[df_imp$group_id == "1" & df_imp$con_days < 11] <- "0"
df_imp$intervention[df_imp$group_id == "1" & df_imp$con_days >= 11] <- "1"
df_imp$intervention[df_imp$group_id == "2" & df_imp$con_days < 19] <- "0"
df_imp$intervention[df_imp$group_id == "2" & df_imp$con_days >= 19] <- "1"
df_imp$intervention[df_imp$group_id == "3" & df_imp$con_days < 16] <- "0"
df_imp$intervention[df_imp$group_id == "3" & df_imp$con_days >= 16] <- "1"
df_imp$intervention[df_imp$group_id == "4" & df_imp$con_days < 8] <- "0"
df_imp$intervention[df_imp$group_id == "4" & df_imp$con_days >= 8] <- "1"

#NAs for cases
df_imp %>% group_by(case_id) %>% summarise_all(~ sum(is.na(.)))

#vis_dat: estimate missingness at random 
case03 <- subset(df_imp, case_id == "03") 
vis_miss(case03) #not missing at random

case04 <- subset(df_imp, case_id == "04")
vis_miss(case04) #missing at random (end day 92)

case05 <- subset(df_imp, case_id == "05")
vis_miss(case05) #missing at random

case06 <- subset(df_imp, case_id == "06")
vis_miss(case06) #not

case07 <- subset(df_imp, case_id == "07")
vis_miss(case07) #missing at random

case08 <- subset(df_imp, case_id == "08")
vis_miss(case08) #not

case10 <- subset(df_imp, case_id == "10")
vis_miss(case10) #missing at random

case11 <- subset(df_imp, case_id == "11")
vis_miss(case11) #missing at random

case12 <- subset(df_imp, case_id == "12")
vis_miss(case12) #missing at random (end day 88)

case13 <- subset(df_imp, case_id == "13")
vis_miss(case13) #missing at random (end day 84)

case14 <- subset(df_imp, case_id == "14")
vis_miss(case14) #missing at random 

case15 <- subset(df_imp, case_id == "15")
vis_miss(case15) #missing at random (end day 67)

case16 <- subset(df_imp, case_id == "16")
vis_miss(case16) #missing at random

case17 <- subset(df_imp, case_id == "17")
vis_miss(case17) #missing at random

case18 <- subset(df_imp, case_id == "18")
vis_miss(case18) #missing at random

case19 <- subset(df_imp, case_id == "19")
vis_miss(case19) #missing at random

case20 <- subset(df_imp, case_id == "20")
vis_miss(case20) #missing at random

case21 <- subset(df_imp, case_id == "21")
vis_miss(case21) # not missing at random

#select cases where NAs not missing at random & delete NAs at the end
df <- filter(df_imp, !(case_id %in% c("03","06","08","21")))

df %>% arrange(case_id, con_days) -> df
df[-c(93:100, 570:576, 661:671, 841:868),] -> df

#impute: aregImpute
imp_arg <- aregImpute(~ loudness_cur + distress_cur + jaw_tension + neck_tension + 
                        tin_thoughts + distress_tod + loudness_tod + movement_tod + stress_tod + emotion_tod,
                      x = TRUE, data = df, n.impute = 5)

df_imp <- impute.transcan(imp_arg, imputation=1, data=df, list.out=TRUE,pr=FALSE, check=FALSE) 

as.data.frame(do.call(cbind,df_imp)) -> imp

glimpse(df)
df[,16:25] <- imp

#visual analysis: distress & loudness
ggplot(df, aes(con_days, distress_cur)) + geom_line(aes(colour = case_id), show.legend = FALSE) +
  facet_wrap(~ case_id, nrow = 7) +
  xlab("days") + ylab("tinnitus distress") + theme_ggdist()

ggplot(df, aes(con_days, loudness_cur)) + geom_line(aes(colour = case_id), show.legend = FALSE) +
  facet_wrap(~ case_id, nrow = 7) +
  xlab("days") + ylab("tinnitus loudness") + theme_ggdist()

ggplot(df) + geom_line(aes(con_days, distress_cur), colour = "#12737b") + geom_line(aes(con_days, loudness_cur), colour = "#f2953d") + facet_wrap(~ case_id, nrow = 4) + theme_ggdist()

# Mixed effect model
mod2 <- lmer(distress_cur ~ con_days + 
               intervention +
               age + 
               sex +
               thi_pre +
               phq9_score +
               (1|case_id),
             data = df)
summary(mod2)
performance(mod2)
confint(mod2)
check_model(mod2)

# Data split ----

library(tidymodels)
innitial_split <- initial_split(df, prop = 4/5)
training <- training(innitial_split)
testing <- testing(innitial_split)
mod2 <- lmer(distress_cur ~ con_days + 
               intervention +
               age + 
               sex +
               thi_pre +
               phq9_score +
               (1|case_id),
             data = training)

test_results <- as.data.frame(predict(mod2, testing) %>% cbind(testing$distress_cur))
rsq_vec(estimate = test_results$`.`, truth = test_results$V2)

ggplot(test_results, aes(x = `.`, y = V2)) +
         geom_point()

       