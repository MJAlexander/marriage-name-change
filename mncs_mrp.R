########## MRP example using martial name change survey #################
########## Monica Alexander, August, 2019 ##############################

library(tidyverse)
library(haven)
library(brms)
library(tidybayes)
library(statebins)


# 1. Read in and tidy up data ---------------------------------------------


# read in MNCS
# this file can be downloaded here: https://osf.io/uzqdn/
d <- read_dta("MNCS-PV2.dta")

# keep variables of interest and only those responses with a year and age

d <- d %>% 
  select(yrmar,
         agemar,
         agemarc,
         genmar,
         spgenmar,
         namechg,
         ednow,
         state) %>% 
  filter(!is.na(agemar), !is.na(yrmar))

# keep only hetero women, make age group, education and decade married variables

d <- d %>% 
  filter(genmar==2&spgenmar==1) %>% 
  mutate(kept_name = as.numeric(namechg==1),
         state_name = str_to_lower(as.character(factor(state, 
                                                       levels = attributes(d$state)$labels, 
                                                       labels = names(attributes(d$state)$labels)))),
         age = agemar + (2019-yrmar),
         age_group = (as.character(cut(age, 
                                       breaks = c(seq(15, 80, by = 5), Inf),
                                       labels = seq(15, 80, by = 5), right = FALSE
         ))),
         decade_married = (as.character(cut(yrmar, 
                                            breaks = c(seq(1969, 2019, by = 10), Inf),
                                            labels = seq(1969, 2019, by = 10), right = FALSE
         ))),
         educ_group = case_when(
           ednow<5 ~ "<BA",
           ednow==5 ~ "BA",
           ednow>5 ~ ">BA",
           TRUE ~ "NA"
         ))


# keep only what we want, tidy up and save
# I only looked at 25+ year olds to give people a chance to have finished a BA. 
# 80+ and 1969 decade filtered out as these only have one observation

d_mncs <- d %>% 
  select(kept_name, state_name, age_group, decade_married, educ_group) %>% 
  filter(age_group>20, age_group<80, decade_married>1969)

saveRDS(d_mncs, "d_mncs.RDS")

# read in the ACS data
# downloaded from IPUMS USA as a .dta file.
# you will want to download AGE, SEX, STATEFIP, MARST, YRMARR, EDUC for the ACS 2017 5 year sample. 

dc <- read_dta("PATH/TO/YOUR/FILE")

# create age groups, decade married and education levels

d_acs <- dc %>% 
  filter(sex==2, age>14, marst!=6, yrmarr>1968) %>% 
  mutate(age_group = (as.character(cut(age, 
                                       breaks = c(seq(15, 80, by = 5), Inf),
                                       labels = seq(15, 80, by = 5), right = FALSE
  ))),
  decade_married = (as.character(cut(yrmarr, 
                                     breaks = c(seq(1969, 2019, by = 10), Inf),
                                     labels = seq(1969, 2019, by = 10), right = FALSE
  ))),
  educ_group = case_when(
    educ<10 ~ "<BA",
    educ==10~"BA",
    educ>10 ~">BA",
    TRUE~ "NA"
  )) 


# 2. Calculate counts and proportions from ACS to post-stratify on --------


# calculate cell counts and save

cell_counts <- d_acs %>% 
  group_by(age_group, statefip, educ_group, decade_married) %>% 
  summarise(n = sum(perwt))%>% 
  mutate(state_name = str_to_lower(as.character(factor(statefip, 
                                                       levels = attributes(d_acs$statefip)$labels, 
                                                       labels = names(attributes(d_acs$statefip)$labels))))) %>% 
  ungroup() %>% 
  select(-statefip) %>% 
  select(state_name, age_group, decade_married, educ_group, n) %>% 
  filter(age_group>20, age_group<80, decade_married>1969)

saveRDS(cell_counts, "cell_counts.RDS")

# calculate proportions by age educ state decade 

state_prop <- cell_counts %>% 
  ungroup() %>% 
  group_by(state_name) %>% 
  mutate(prop = n/sum(n))  %>% 
  ungroup()

age_prop <- cell_counts %>% 
  ungroup() %>% 
  group_by(age_group) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup()

decade_prop <- cell_counts %>% 
  ungroup() %>% 
  group_by(decade_married) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup()

educ_prop <- cell_counts %>% 
  ungroup() %>% 
  group_by(educ_group) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup()


# 3. Run model ------------------------------------------------------------



mod <- brm(kept_name ~ (1|age_group) + (1|educ_group)
           + (1|state_name) + (1|decade_married), 
           data = d_mncs, family=bernoulli(), 
           control = list(adapt_delta = 0.98))

summary(mod)



# 4. Get results and plot -------------------------------------------------


# by age
res_age <- mod %>%
  add_predicted_draws(newdata=age_prop %>% 
                        filter(age_group>20, 
                               age_group<80, 
                               decade_married>1969),
                      allow_new_levels=TRUE) %>%
  rename(kept_name_predict = .prediction) %>% 
  mutate(kept_name_predict_prop = kept_name_predict*prop) %>% 
  group_by(age_group, .draw) %>% 
  summarise(kept_name_predict = sum(kept_name_predict_prop)) %>% 
  group_by(age_group) %>% 
  summarise(mean = mean(kept_name_predict), 
            lower = quantile(kept_name_predict, 0.025), 
            upper = quantile(kept_name_predict, 0.975))


res_age %>% 
  ggplot(aes(y = mean, x = forcats::fct_inorder(age_group), color = "MRP estimate")) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) + 
  ylab("proportion keeping name") + 
  xlab("age in 2019") + 
  geom_point(data = d_mncs %>% 
               group_by(age_group, kept_name) %>%
               summarise(n = n()) %>% 
               group_by(age_group) %>% 
               mutate(prop = n/sum(n)) %>% 
               filter(kept_name==1, age_group<80, age_group>20), 
             aes(age_group, prop, color = "MNCS raw data")) +
  scale_color_manual(name = "", values = c("MRP estimate" = "black", "MNCS raw data" = "red")) + 
  theme_bw(base_size = 14) 
ggsave("age_plot.png", width = 8, height = 6)

# by decade married
res_dec <- mod %>%
  add_predicted_draws(newdata=decade_prop %>% 
                        filter(age_group>20, age_group<80, decade_married>1969),
                      allow_new_levels=TRUE) %>%
  rename(kept_name_predict = .prediction) %>% 
  mutate(kept_name_predict_prop = kept_name_predict*prop) %>% 
  group_by(decade_married, .draw) %>% 
  summarise(kept_name_predict = sum(kept_name_predict_prop)) %>% 
  group_by(decade_married) %>% 
  summarise(mean = mean(kept_name_predict), 
            lower = quantile(kept_name_predict, 0.025), 
            upper = quantile(kept_name_predict, 0.975))


res_dec %>% 
  ggplot(aes(y = mean, x = forcats::fct_inorder(decade_married), color = "MRP estimate")) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) + 
  ylab("proportion keeping name") + 
  xlab("decade married") + 
  geom_point(data = d_mncs %>% 
               group_by(decade_married, kept_name) %>%
               summarise(n = n()) %>% 
               group_by(decade_married) %>% 
               mutate(prop = n/sum(n)) %>% 
               filter(kept_name==1, decade_married>1969, decade_married<2019), #, age_group<80, age_group>20), 
             aes(decade_married, prop, color = "MNCS raw data")) +
  scale_color_manual(name = "", values = c("MRP estimate" = "black", "MNCS raw data" = "red")) + 
  theme_bw(base_size = 14) 
ggsave("dec_plot.png", width = 8, height = 6)

# by state
res_state <- mod %>%
  add_predicted_draws(newdata=state_prop %>% 
                        filter(age_group>20, age_group<80, decade_married>1969),
                      allow_new_levels=TRUE) %>%
  rename(kept_name_predict = .prediction) %>% 
  mutate(kept_name_predict_prop = kept_name_predict*prop) %>% 
  group_by(state_name, .draw) %>% 
  summarise(kept_name_predict = sum(kept_name_predict_prop)) %>% 
  group_by(state_name) %>% 
  summarise(mean = mean(kept_name_predict), 
            lower = quantile(kept_name_predict, 0.025), 
            upper = quantile(kept_name_predict, 0.975))


res_state %>% 
  arrange(-mean) %>% 
  left_join(d %>%
              group_by(state_name, kept_name) %>%
              summarise(n = n()) %>%
              group_by(state_name) %>%
              mutate(prop = n/sum(n)) %>%
              filter(kept_name==1)) %>% 
  ggplot(aes(y = mean, x = forcats::fct_inorder(state_name), color = "MRP estimate")) + 
  geom_point() +
  coord_flip() + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) + 
  ylab("proportion keeping maiden name") + 
  xlab("state") + 
  geom_point(aes(state_name, prop, color = "raw data")) +
  scale_color_manual(name = "", values = c("MRP estimate" = "black", "raw data" = "red")) + 
  theme_bw() +
  ggtitle("Proportion of women keeping maiden name after marriage")


res_state %>% 
  mutate(statename = str_to_title(state_name)) %>% 
  #mutate(statename = ifelse(state_name=="district of columbia", "District of Columbia", statename)) %>% 
  ggplot(aes(fill = mean, state = statename)) + 
  geom_statebins() +
  scale_fill_viridis_c(name = "proportion keeping name") +
  theme_void()
ggsave("state_plot.png", width = 8, height = 6)  

# educ

res_educ <- mod %>%
  add_predicted_draws(newdata=educ_prop %>% 
                        filter(age_group>20, age_group<80, decade_married>1969),
                      allow_new_levels=TRUE) %>%
  rename(kept_name_predict = .prediction) %>% 
  mutate(kept_name_predict_prop = kept_name_predict*prop) %>% 
  group_by(educ_group, .draw) %>% 
  summarise(kept_name_predict = sum(kept_name_predict_prop)) %>% 
  group_by(educ_group) %>% 
  summarise(mean = mean(kept_name_predict), 
            lower = quantile(kept_name_predict, 0.025), 
            upper = quantile(kept_name_predict, 0.975))


res_educ %>% 
  arrange(mean) %>% 
  ggplot(aes(y = mean, x = forcats::fct_inorder(educ_group), color = "MRP estimate")) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) + 
  ylab("proportion keeping name") + 
  xlab("education group") + 
  geom_point(data = d_mncs %>% 
               group_by(educ_group, kept_name) %>%
               summarise(n = n()) %>% 
               group_by(educ_group) %>% 
               mutate(prop = n/sum(n)) %>% 
               filter(kept_name==1, educ_group!="NA"), #, age_group<80, age_group>20), 
             aes(educ_group, prop, color = "MNCS raw data")) +
  scale_color_manual(name = "", values = c("MRP estimate" = "black", "MNCS raw data" = "red")) + 
  theme_bw(base_size = 14) 
ggsave("educ_plot.png", width = 8, height = 6)
