library(tidyverse)
library(foreign)
library(readxl)
library(stringr)
library(naniar)
library(scales)
library(qdapTools)
library(quanteda)
library(quanteda.textstats)


"%notin%" <- function(x, y) !(x %in% y)

trust_levels <- c("Completely distrust", "Mostly distrust", "Somewhat distrust",
                  "Neither trust nor distrust",
                  "Somewhat trust", "Mostly trust", "Completely trust")

#note: fixed another typo in raw on 1/19 so this data is replaced
#I have replaced this as the raw data on google drive
dat_raw <- read.csv("Poland+Media+Project_January+19,+2024_21.18.csv",
                    na.strings=c("-99"))

#note: we may want to remove this na strings eventually so we can see which trials were seen but unanswered

questions <- as.character(dat_raw[1,])
vars <- colnames(dat_raw)
codebook<- as.data.frame(cbind(vars, questions))

dat_raw <- dat_raw[-(1:2),]

dat_full <- dat_raw %>%
  filter(
    consent %in% "I consent, begin the study",
    screener_captcha %in% 15,
    screener_disagree %in% "Somewhat disagree",
    screener_number %in% "25",
  ) %>%
  rowwise() %>%
  mutate(
    #attention check
    attention1 = ifelse(
      (screener_web_1 == "pudelek.pl" & screener_web_17 == "echodnia.eu" & 
         screener_web_1 == "0" & screener_web_2 == "0" & screener_web_3 == "0" & screener_web_4 == "0" &
         screener_web_5 == "0" & screener_web_6 == "0" & screener_web_7 == "0" & screener_web_8 == "0" &
         screener_web_9 == "0" & screener_web_10 == "0" &
         screener_web_11 == "0" & screener_web_12 == "0" & screener_web_13 == "0" &
         screener_web_14 == "0" & screener_web_15 == "0" & screener_web_16 == "0" &
         screener_web_18 == "0"), 1, 0),
    attention2 = ifelse(
      (screener_color_3 == "Red" & screener_color_5 == "Green" & screener_color_1 == "0" &
         screener_color_2 == "0" & screener_color_4 == "0" & screener_color_6 == "0"), 1, 0),
    attention_score = (attention1 + attention2) / 2,
    
    start_date = as.Date(StartDate),
    
    #partisanship and political attitudes
    voted = ifelse(voted == "Yes", 1, 0),
    party = case_when(voted == 1 & party_voted %notin% c("forgot") ~ party_voted,
                      voted == 0 ~ party_supported),
    pisvoted = ifelse(party_voted == "pis", 1, 0),
    pissupported = ifelse(party_supported == "pis", 1, 0),
    pro_pis = ifelse(pisvoted == 1 | pissupported == 1, 1, 0 ),
    
    antipisvoted = ifelse(party_voted %in% c("ko", "nl", "td"), 1, 0),
    antipissupported = ifelse(party_supported %in% c("ko", "nl", "td"), 1, 0),
    anti_pis = ifelse(antipisvoted == 1 | antipissupported == 1, 1, 0 ),
    
    
    #demographics
    education = factor(education, levels = c("Primary school or lower", "Vocational secondary school", "General secondary school",
                                             "Bachelor's degree", "Advanced degree (Master's, doctorate etc.")),
    ba_grad = ifelse(education %in% c("Bachelor's degree", "Advanced degree (Master's, doctorate etc."), 1, 0),
    
    female = ifelse(gender == "Female", 1, 0),
    income = factor(income),
    income = droplevels(income, "Prefer not to say"),
    
    age = as.numeric(age),
    
    #Media Use
    
    media_pref_tv = ifelse(media_pref == "tv", 1, 0),
    media_pref_online = ifelse(media_pref == "online", 1, 0),
    
    use_tvp = ifelse(news_use_1 == "tvp", 1, 0),
    use_tvn = ifelse(news_use_2 == "tvn", 1, 0),
    
    
    reg_tvp = ifelse(news_use_followup_1 %in% c("A few times per week", "Everyday or almost everyday"), 1, 0),
    reg_tvn = ifelse(news_use_followup_2 %in% c("A few times per week", "Everyday or almost everyday"), 1, 0),
    
    
    pol_interest = factor(pol_interest,
                          levels = c("Not at all interested", "Slightly interested",
                                     "Moderately interested", "Very interested")),
    pol_interest_n = rescale(as.numeric(pol_interest), from = c(1, 4), to = c(0, 1)),
    
    #media trust
    trust_tvp = factor(trust_tvp, levels = trust_levels),
    trust_tvn = factor(trust_tvn, levels = trust_levels),
    trust_polsat = factor(trust_polsat, levels = trust_levels),
    trust_republika = factor(trust_republika, levels = trust_levels),
    trust_trwam = factor(trust_trwam, levels = trust_levels),
    
    trust_tvp_n = as.numeric(trust_tvp),
    trust_tvn_n = as.numeric(trust_tvn),
    trust_polsat_n = as.numeric(trust_polsat),
    trust_republika_n = as.numeric(trust_republika),
    trust_trwam_n = as.numeric(trust_trwam),
    
    #outsider outcomes
    threat_traditions = rescale(as.numeric(outsider_threat_1), from = c(0,10), to = c(0,1)),
    threat_organizations = rescale(as.numeric(outsider_threat_2), from = c(0,10), to = c(0,1)),
    threat_values = rescale(as.numeric(outsider_threat_4), from = c(0,10), to = c(0,1)),
    threat_safety = rescale(as.numeric(outsider_threat_5), from = c(0,10), to = c(0,1)),
    
    open_integration = rescale(as.numeric(outsider_openness_m_1), from = c(0,10), to = c(1,0)),
    open_organizations = rescale(as.numeric(outsider_openness_m_2), from = c(0,10), to = c(1,0)),
    open_positive = rescale(as.numeric(outsider_openness_m_4), from = c(0,10), to = c(1,0)),
    open_admit = rescale(as.numeric(outsider_openness_m_5), from = c(0,10), to = c(1,0)),
    
    #Democratic Opposition distrust outcomes
    
    oppo_institutions = rescale(as.numeric(tusk_gov1_1), from = c(0,10), to = c(0,1)),
    oppo_corruption = rescale(as.numeric(tusk_gov1_2), from = c(0,10), to = c(0,1)),
    oppo_discord = rescale(as.numeric(tusk_gov1_3), from = c(0,10), to = c(0,1)),
    
    
    #outsider meta outcomes
    
    #note these are scaled so that 1 = high threat
    m_threat_traditions = rescale(as.numeric(outsider_threat_m_1), from = c(0,10), to = c(0,1)),
    m_threat_organizations = rescale(as.numeric(outsider_threat_m_2), from = c(0,10), to = c(0,1)),
    m_threat_values = rescale(as.numeric(outsider_threat_m_4), from = c(0,10), to = c(0,1)),
    m_threat_safety = rescale(as.numeric(outsider_threat_m_5), from = c(0,10), to = c(0,1)),
    
    #note, these are rescaled so 1 = less open
    m_open_integration = rescale(as.numeric(outsider_openness_m_1), from = c(0,10), to = c(1,0)),
    m_open_organizations = rescale(as.numeric(outsider_openness_m_2), from = c(0,10), to = c(1,0)),
    m_open_positive = rescale(as.numeric(outsider_openness_m_4), from = c(0,10), to = c(1,0)),
    m_open_admit = rescale(as.numeric(outsider_openness_m_5), from = c(0,10), to = c(1,0)),
    
    #Democratic Opposition distrust meta outcomes
    
    m_oppo_institutions = rescale(as.numeric(tusk_gov1_m_1), from = c(0,10), to = c(0,1)),
    m_oppo_corruption = rescale(as.numeric(tusk_gov1_m_2), from = c(0,10), to = c(0,1)),
    m_oppo_discord = rescale(as.numeric(tusk_gov1_m_4), from = c(0,10), to = c(0,1)),
    
    politicians_trust_interests = rescale(as.numeric(politicians_trust_1), from = c(0,10), to = c(0,1)),
    politicians_trust_corrupt = rescale(as.numeric(politicians_trust_2), from = c(0,10), to = c(0,1)),
    
    party_coop = rescale(as.numeric(party_coop_1), from = c(0,10), to = c(0,1)),
    
    Finished = case_when(Finished == "True" ~ 1,
                         Finished == "False" ~ 0)
  ) %>%
  
  rowwise() %>%
  mutate(
    outsider_threat = mean(threat_traditions, threat_organizations, threat_values, threat_safety,
                           open_integration, open_organizations, open_positive, open_admit, na.rm = T),
    oppo_distrust = mean(oppo_institutions, oppo_corruption, oppo_discord, na.rm = T),
    m_outsider_threat = mean(m_threat_traditions, m_threat_organizations, m_threat_values, m_threat_safety,
                             m_open_integration, m_open_organizations, m_open_positive, m_open_admit, na.rm = T),
    m_oppo_distrust = mean(m_oppo_institutions, m_oppo_corruption, m_oppo_discord, na.rm = T)
  ) %>%
  mutate(
    source_rec_tvp = ifelse(source_rec_1 == "tvp", 1, 0),
    source_rec_tvn = ifelse(source_rec_2 == "tvn", 1, 0)
  )




#Main data set for those who finished the survey
dat <- dat_full %>% filter(Finished == 1) %>%
  ungroup() %>%
  mutate(subj_id = row_number()) %>%
  # GC: adding the counter and aligned source vars here 
  mutate(counter_source = 
           case_when(source == "tvp" & anti_pis == 1 ~ 1,
                     source == "tvn" & pro_pis == 1 ~ 1,
                     source == "tvp" & pro_pis == 1 ~ 0,
                     source == "tvn" & anti_pis == 1 ~ 0),
         aligned_source = 
           case_when(source == "tvn" & anti_pis == 1 ~ 1,
                     source == "tvp" & pro_pis == 1 ~ 1,
                     source == "tvn" & pro_pis == 1 ~ 0,
                     source == "tvp" & anti_pis == 1 ~ 0))

#create small data set of trust data only
trust_dat <- dat_full %>% 
  ungroup() %>%
  dplyr::select(ResponseId, matches("^tvp|^tvn")) %>%
  gather("var", "value", -ResponseId) %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value)) %>%
  mutate(value = scales::rescale(value)) %>% 
  separate(var, into = c("excerpt", "var")) %>%
  pivot_wider(id_cols = c("ResponseId", "excerpt"),
              names_from = "var", 
              values_from = "value") %>%
  rowwise() %>%
  mutate(trust_index = mean(c(objective, trustworthy, truthful))) %>%
  unite(trial_id, c(ResponseId, excerpt), remove = FALSE)

#create small data set of trial order data only
trial_order_dat <- dat_full %>% 
  ungroup() %>%
  dplyr::select(ResponseId, matches("^(FL_190_DO|FL_196_DO|FL_424_DO|FL_455_DO|FL_466_DO|FL_509_DO)")) %>%
  gather("x", "order", -ResponseId) %>%
  mutate(order = as.numeric(order)) %>%
  filter(!is.na(order)) %>% 
  mutate(excerpt = sub(".+DO_(.+)", "\\1", x),
         excerpt = gsub("_", "", excerpt)) %>%
  dplyr::select(-x) %>%
  unite(trial_id, c(ResponseId, excerpt), remove = FALSE)

dat_long_full <- left_join(trust_dat, trial_order_dat)

#note: qualtrics assigns trial order even for trials not seen
#this causes more rows in the order dat than actual trials completed



# add ind-level variables to long data
dat_long_full <- dat_long_full %>% 
  left_join(dat %>% dplyr::select(
    "Finished",
    "ResponseId", "treatment", "source", "topic", "counter_source",
    "voted", "party", "party_voted", "party_supported",
    "pro_pis", "anti_pis",
    "pol_interest_n",
    "use_tvp", "use_tvn",
    "reg_tvp", "reg_tvn",
    "media_pref",
    contains("trust_"),
    "attention_score",
    "start_date",
    "age", "female", "income",
    contains("threat_"),
    contains("open_"),
    contains("oppo_"),
    contains("politicians_"),
    "party_coop",
    "source_rec_tvp",
    "source_rec_tvn",
    "education", "ba_grad", "employment"),
    by = "ResponseId")

######
#add in text data

load("text_df.Rdata")

dat_long_full <- left_join(dat_long_full, text_df)

dat_long <- dat_long_full %>%
  filter(Finished == 1)

#dat is the main respondent-level data set and includes only those who finish the study
#dat full is everyone who consented to participate and passed the initial screens but includes people who dropped out in the middle of the study
#dat long is trial level data, excluding those who dropped out of the survey
#dat_full_long is trial level data, including dropouts
save(dat, file = "dat.RData")
save(dat_full, file = "dat_full.RData")
save(dat_long, file = "dat_long.RData")
save(dat_long_full, file = "dat_long_full.RData")
