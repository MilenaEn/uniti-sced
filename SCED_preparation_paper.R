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


# 1. import dataframe ----

mydf <- read_csv("data/answers-13.10.21.csv", 
) %>%
  mutate(answers = str_replace_all(answers, '""', '"'),
         answers = str_replace_all(answers, '\\"\\[', '['),
         answers = str_replace_all(answers, '\\]\\"', ']'),
         client = str_replace_all(client, '""', '"'),
         client = str_replace_all(client, '\\"\\{', '{'),
         client = str_replace_all(client, '\\}\\"', '}')) %>% as_tibble()


mydf <- mydf[-22910,]
mydf <- mydf[-20749,]


json_list<- list()
for(i in 1:nrow(mydf)){
  json_list[[i]] <- pivot_wider(fromJSON(mydf$answers[i]), names_from = "label", values_from = c("value", "collected_at")) %>% as.tibble()
} # should have used purrr inside the previous mutate, but could not make it work :-(



# 2. Track Your Tinnitus ----

tyt <- mydf %>%
  mutate(app_usage = json_list) %>%
  select(everything(), -c(answers, sensordata, flags, deleted_at)) %>%
  filter(questionnaire_id == 17) %>%
  mutate(app_usage = map(app_usage, ~ select(., #"collected_at",
                                             "value_loudness", "value_cumberness", "value_jawbone",  "value_neck", "value_tin_day", "value_tin_cumber", "value_tin_max", "value_movement", "value_stress", "value_emotion",
                                             "collected_at_loudness", "collected_at_cumberness", "collected_at_jawbone",  "collected_at_neck", "collected_at_tin_day", "collected_at_tin_cumber", "collected_at_tin_max", "collected_at_movement", "collected_at_stress", "collected_at_emotion")))

for(i in 1:nrow(tyt)){
  tyt$app_usage[[i]] <-
    tyt$app_usage[[i]] %>%
    mutate(across(where(is.character), as.numeric))
} # should have used purrr inside the previous mutate, but could not make it work :-(

tyt <- tyt[-4307,]

tyt <- tyt %>%
  unnest(app_usage) %>%
  mutate(across(contains("collected_at_"), as_datetime))



# 3. TinEdu ----

tinedu <- mydf %>%
  mutate(app_usage = json_list) %>%
  select(everything(), -c(answers, sensordata, flags, deleted_at)) %>%
  filter(questionnaire_id == 1) %>%
  unnest(app_usage) %>%
  select(everything(), tip = 9, collected_at_tip = 10) %>%
  mutate(across(contains("collected_at_"), as_datetime))



# 4. Shades of Noise ----

son <- mydf %>%
  mutate(app_usage = json_list) %>%
  select(everything(), -c(answers, sensordata, flags, deleted_at)) %>%
  filter(questionnaire_id == 15)

gambiarra <- map_df(son$app_usage, ~ map_chr(.x, ~ if(is.list(.x)){as.character(.x)}else{.x})) %>%
  mutate(across(contains("collected_at_"), as.numeric),
         across(contains("collected_at_"), as_datetime))

son <- cbind(son[,1:8], gambiarra)



# 5. clean environment ----

rm(json_list, i, gambiarra)

#a <-str_replace_all(son$value_beachwaves[[1]], '"', '')



# 6. create subsets of the sced study by user_id ----

mydf_sced <- filter(mydf, user_id>=18109 & user_id<=18184 & !user_id == 18133 & !user_id == 18136)

mydf_son <- filter(son, user_id>=18109 & user_id<=18184 & !user_id == 18133 & !user_id == 18136)

mydf_tinedu <- filter(tinedu, user_id>=18109 & user_id<=18184 & !user_id == 18133 & !user_id == 18136)

mydf_tyt <- filter(tyt, user_id>=18109 & user_id<=18184 & !user_id == 18133 & !user_id == 18136)



# 7. read questionnaire data + merge with mydf_sced_raw (sex, age ...) ----

questionnaires <- read_csv2("questionnaires/data.csv")
mydf_sced <- merge(mydf_sced, questionnaires, by="user_id", all = TRUE)



# 8. mydf_sced_raw: create new variables ----

#case ID

mydf_sced$case_id <- 99
mydf_sced <- mydf_sced %>% relocate(case_id, .after = user_id)

mydf_sced$case_id[mydf_sced$user_id <= "18112"] <- "03"
mydf_sced$case_id[mydf_sced$user_id >= "18113" & mydf_sced$user_id <= "18116"] <- "04"
mydf_sced$case_id[mydf_sced$user_id >= "18117" & mydf_sced$user_id <= "18120"] <- "05"
mydf_sced$case_id[mydf_sced$user_id >= "18121" & mydf_sced$user_id <= "18124"] <- "06"
mydf_sced$case_id[mydf_sced$user_id >= "18125" & mydf_sced$user_id <= "18128"] <- "07"
mydf_sced$case_id[mydf_sced$user_id >= "18129" & mydf_sced$user_id <= "18132"] <- "08"
mydf_sced$case_id[mydf_sced$user_id >= "18137" & mydf_sced$user_id <= "18140"] <- "10"
mydf_sced$case_id[mydf_sced$user_id >= "18141" & mydf_sced$user_id <= "18144"] <- "11"
mydf_sced$case_id[mydf_sced$user_id >= "18145" & mydf_sced$user_id <= "18148"] <- "12"
mydf_sced$case_id[mydf_sced$user_id >= "18149" & mydf_sced$user_id <= "18152"] <- "13"
mydf_sced$case_id[mydf_sced$user_id >= "18153" & mydf_sced$user_id <= "18156"] <- "14"
mydf_sced$case_id[mydf_sced$user_id >= "18157" & mydf_sced$user_id <= "18160"] <- "15"
mydf_sced$case_id[mydf_sced$user_id >= "18161" & mydf_sced$user_id <= "18164"] <- "16"
mydf_sced$case_id[mydf_sced$user_id >= "18165" & mydf_sced$user_id <= "18168"] <- "17"
mydf_sced$case_id[mydf_sced$user_id >= "18169" & mydf_sced$user_id <= "18172"] <- "18"
mydf_sced$case_id[mydf_sced$user_id >= "18173" & mydf_sced$user_id <= "18176"] <- "19"
mydf_sced$case_id[mydf_sced$user_id >= "18177" & mydf_sced$user_id <= "18180"] <- "20"
mydf_sced$case_id[mydf_sced$user_id >= "18181" & mydf_sced$user_id <= "18184"] <- "21"

#intervention

mydf_sced <- mydf_sced %>%
  mutate(intervention = case_when(
    user_id %% 2 == 0 ~ "1",
    TRUE ~ "0"
  )) 

mydf_sced <- mydf_sced %>% relocate(intervention, .after = case_id)

#group

mydf_sced$group_id <- 99
mydf_sced <- mydf_sced %>% relocate(group_id, .after = case_id)

mydf_sced$group_id[mydf_sced$case_id == "05"] <- "1"
mydf_sced$group_id[mydf_sced$case_id == "12"] <- "1"
mydf_sced$group_id[mydf_sced$case_id == "13"] <- "1"
mydf_sced$group_id[mydf_sced$case_id == "15"] <- "1"

mydf_sced$group_id[mydf_sced$case_id == "06"] <- "2"
mydf_sced$group_id[mydf_sced$case_id == "07"] <- "2"
mydf_sced$group_id[mydf_sced$case_id == "14"] <- "2"
mydf_sced$group_id[mydf_sced$case_id == "19"] <- "2"

mydf_sced$group_id[mydf_sced$case_id == "04"] <- "3"
mydf_sced$group_id[mydf_sced$case_id == "08"] <- "3"
mydf_sced$group_id[mydf_sced$case_id == "16"] <- "3"
mydf_sced$group_id[mydf_sced$case_id == "18"] <- "3"
mydf_sced$group_id[mydf_sced$case_id == "21"] <- "3"

mydf_sced$group_id[mydf_sced$case_id == "03"] <- "4"
mydf_sced$group_id[mydf_sced$case_id == "10"] <- "4"
mydf_sced$group_id[mydf_sced$case_id == "11"] <- "4"
mydf_sced$group_id[mydf_sced$case_id == "17"] <- "4"
mydf_sced$group_id[mydf_sced$case_id == "20"] <- "4"

#time factor

mydf_sced %>% 
  mutate(date = as_date(collected_at),
         hour = hour(collected_at),
         weekday = weekdays(collected_at),
         weekend = case_when(weekday == "Samstag" | weekday == "Sonntag" ~ "we",
                                  TRUE ~ "udw")) -> mydf_sced

con_days = rep(0, length(mydf_sced$date)) #storage vector

for(i in 1:length(mydf_sced$date)){ #loop through the vector
  tmp_juldays = julian(as.Date(mydf_sced$date))
  tmp_lfdays = tmp_juldays - min(tmp_juldays) + 1
  con_days = tmp_lfdays
  print(i)
}

mydf_sced = data.frame(mydf_sced, con_days)
mydf_sced <- mydf_sced %>% relocate(date, hour, weekday, weekend, con_days, .after = collected_at)

#base_days, int_days, magic_days (baseline backwards) 

base_days = rep(NA, dim(mydf_sced)[1])
int_days = rep(NA, dim(mydf_sced)[1])
magic_days = rep(NA, dim(mydf_sced)[1])

cases = unique(mydf_sced$case_id)

for(i in 1:length(cases)){
  tmp = subset(mydf_sced, case_id == cases[i])
  tmp_b = subset(tmp, intervention == 0)
  tmp_i = subset(tmp, intervention == 1)
  
  tmp_lfday_b = tmp_b$con_days
  tmp_lfday_i = tmp_i$con_days
  
  ### hier der entscheidende Schritt: Tag 1 der Intervention ist lfday_i = 1
  tmp_lfday_i = tmp_lfday_i - min(tmp_lfday_i) +1
  
  ### für lfday_magic: die baseline Tage werden rückwärts gezöhlt
  tmp_lfday_magic = tmp_lfday_b
  tmp_lfday_magic = tmp_lfday_magic - max(tmp_lfday_magic)
  
  tmp_indx_b = match(tmp_b$id, mydf_sced$id)
  tmp_indx_i = match(tmp_i$id, mydf_sced$id)
  
  base_days[tmp_indx_b] = tmp_lfday_b
  int_days[tmp_indx_i] = tmp_lfday_i
  magic_days[tmp_indx_b] = tmp_lfday_magic
  magic_days[tmp_indx_i] = tmp_lfday_i
  
}

mydf_sced = data.frame(mydf_sced, base_days, int_days, magic_days)

mydf_sced %>% 
  relocate(c("base_days", "int_days", "magic_days"), .after = con_days) -> mydf_sced




# 9. sced_raw: merge mydf with tyt, tinedu & son ----

sced_raw <- merge(mydf_sced, mydf_tyt, by="id", all = TRUE)
sced_raw <- merge(sced_raw, mydf_tinedu, by="id", all = TRUE)
sced_raw <- merge(sced_raw, mydf_son, by="id", all = TRUE)


# 10. sced: filter relevant variables + rename ----

sced <- select(sced_raw, 2:6,13:20,24:49,57:66,84)
sced <- rename(sced, user_id = user_id.x)
sced <- rename(sced, questionnaire_id = questionnaire_id.x)
sced <- rename(sced, tinedu = tip)
sced <- rename(sced, loudness_cur = value_loudness)
sced <- rename(sced, distress_cur = value_cumberness)
sced <- rename(sced, jaw_tension = value_jawbone)
sced <- rename(sced, neck_tension = value_neck)
sced <- rename(sced, tin_thoughts = value_tin_day)
sced <- rename(sced, distress_tod = value_tin_cumber)
sced <- rename(sced, loudness_tod = value_tin_max)
sced <- rename(sced, movement_tod = value_movement)
sced <- rename(sced, stress_tod = value_stress)
sced <- rename(sced, emotion_tod = value_emotion)
sced <- rename(sced, duration_tin_mon = tschq_months_since_begin_tinnitus)
sced <- rename(sced, questionnaire = questionnaire_id)
sced <- rename(sced, vertigo = esitsq_q_a9_vertigo)
sced <- rename(sced, tin_freq = esitsq_q_b1_frequency)
sced <- rename(sced, tin_num = esitsq_q_b6_number_sounds)
sced <- rename(sced, tin_on = esitsq_q_b7_onset_char)
sced <- rename(sced, tin_change = esitsq_q_b12_loudness_changes)
sced <- rename(sced, tin_qual = esitsq_q_b13_quality)
sced <- rename(sced, tin_pitch = esitsq_q_b14_pitch)
sced <- rename(sced, tin_loc = esitsq_q_b15_localisation)
sced <- rename(sced, tin_rhyt = esitsq_q_b16_rhythmic)
sced <- rename(sced, tin_care = esitsq_q_b20_tinnitus_healthcare)

sced <- sced %>% mutate(weekday=recode(weekday, `Montag`="Monday", `Dienstag`="Tuesday", `Mittwoch`="Wednesday", `Donnerstag`="Thursday", `Freitag`="Friday", `Samstag`="Saturday", `Sonntag`="Sunday"))


# 11. filter relevant time ----
sced <- filter(sced, magic_days <= 84)


# 12. create variable: app usage ----

sced <- sced %>% mutate(questionnaire=recode(questionnaire, `1`="tinedu", `15`="son", `17`="diary"))
sced <- filter(sced, !is.na(questionnaire))

sced <- sced %>% 
  group_by(case_id) %>% 
  mutate(n_tinedu = sum(questionnaire == "tinedu"), n_son = sum(questionnaire == "son"), n_diary = sum(questionnaire == "diary")) %>% 
  relocate(n_tinedu, .after = questionnaire) %>% 
  relocate(n_son, .after = n_tinedu) %>% 
  relocate(n_diary, .after = n_son) %>%
  ungroup()

# 13. create variable: TinEdu chapter ----

sced$tinchap <- NA
sced$tinchap[sced$tinedu == "book-1-chapter-1-section-8"] <- 01
sced$tinchap[sced$tinedu == "book-1-chapter-2-section-7"] <- 02
sced$tinchap[sced$tinedu == "book-1-chapter-3-section-7"] <- 03
sced$tinchap[sced$tinedu == "book-1-chapter-4-section-7"] <- 04
sced$tinchap[sced$tinedu == "book-1-chapter-5-section-7"] <- 05
sced$tinchap[sced$tinedu == "book-1-chapter-6-section-7"] <- 06
sced$tinchap[sced$tinedu == "book-1-chapter-7-section-7"] <- 07
sced$tinchap[sced$tinedu == "book-1-chapter-8-section-7"] <- 08
sced$tinchap[sced$tinedu == "book-1-chapter-9-section-7"] <- 09
sced$tinchap[sced$tinedu == "book-1-chapter-10-section-7"] <- 10
sced$tinchap[sced$tinedu == "book-1-chapter-11-section-7"] <- 11
sced$tinchap[sced$tinedu == "book-1-chapter-12-section-7"] <- 12

sced1 <- sced %>% 
  relocate(tinchap, .after = tinedu)


# 14. save sced1 ----
saveRDS(sced1, file = "sced1.RDS") 




