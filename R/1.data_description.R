################################################################################
## DATA DESCRIPTION
################################################################################
rm(list = ls())
library(tidyverse)
library(readxl)

source("R/helper_functions.R")

################################################################################
## Load data
################################################################################
# Azithromycine consumption-----------------------------------------------------
sheets = c("Azithromycine-Etablissement", "Azithromycine-Chirurgie", "Azithromycine-Médecine",
           "Azithromycine-Obstétrique", "Azithromycine-Réanimation")

azi = data.frame()
for (s in sheets) {
  azi = bind_rows(
    azi,
    read_excel("data/Azithromycine 2012-2023.xlsx", sheet = s) %>%
      mutate(ward = gsub("Azithromycine-", "", s), Date_month = as_date(sprintf('%d/%02d/01', Année, Mois))) %>%
      mutate(ward = case_when(ward == "Chirurgie" ~ "Surgery", ward == "Etablissement" ~ "Hospital",
                              ward == "Médecine" ~ "Medicine", ward == "Obstétrique" ~ "Gynecology",
                              ward == "Réanimation" ~ "ICU")) %>%
      rename(bd = JH, ddd_bd = `Nb de DDJ/1000 JH`) %>%
      select(ward, Date_month, bd, ddd_bd) 
  ) 
}

# Antibiotic consumption--------------------------------------------------------
# 2011-2016
atb_first_period = read_excel("data/tableau K & K rea septembre 2016.xlsx") %>%
  rename(Date_month = date, sha = sha_rea, atb = atb_rea, pena = pena_rea, penaac = penaac_rea, 
         cefotaxime = cefot_rea, ceftriaxone = ceftri_rea, pip_taz = piptaz_rea, 
         carbapenem = carba_rea, fluoroquinolone = fq_rea, 
         macrolide = macro_rea, amikacin = amik_rea, gentamicin = genta_rea, ciprofloxacin = cipro_rea,
         metronidazole = metro_rea, vancomycin = vanco_rea, bd = nb_jh_rea) %>%
  mutate(ward = "ICU", Date_month = as_date(Date_month), covid_period = 0, 
         sha_bd = (sha/1000)/bd*1000) %>%
  select(ward, Date_month, covid_period, sha, sha_bd, atb, pena, penaac, cefotaxime, ceftriaxone,
         pip_taz, carbapenem, fluoroquinolone, macrolide, amikacin, gentamicin, ciprofloxacin,
         metronidazole, vancomycin, bd, intervention)


# 2017-2023
atb = data.frame()
for (f in list.files("data", "Monthly antibiotic.*", full.names = T)) {
  atb = bind_rows(
    atb,
    read_excel(f) %>%
      mutate(ward = gsub(".*HAI_|_2017-.*", "", f), 
             Date_month = as_date(sprintf('%d/%02d/01', year, month)), 
             intervention = 1) %>%
      mutate(ward = case_when(ward == "chirurgie" ~ "Surgery", ward == "ETS" ~ "Hospital",
                              ward == "medecine" ~ "Medicine", ward == "gynéco-obstetrique" ~ "Gynecology",
                              .default = "ICU")) %>%
      rename(covid_period = `COVID Period`, sha_bd = `sha_DDD/1000BD Col F/Z`,
             sha = `raw hydroalcoholic hand rub solution consumption (sha) en ML`, 
             atb = `total atb (DDD/1000BD)`, pena = `pena (DDD/1000BD)`, penaac = `penaac (DDD/1000BD)`, 
             cefotaxime = `cefotaxime (DDD/1000BD)`, ceftriaxone = `ceftriaxone (DDD/1000BD)`,
             c3g_tot = `C3G_tot (DDD/1000BD) Col J+K`, pip_taz = `pip_taz (DDD/1000BD)`, 
             carbapenem = `carbapenem (DDD/1000BD)`, fluoroquinolone = `Fluoroquinolones (DDD/1000BD)`, 
             macrolide = `macrolides (DDD/1000BD)`, amikacin = `amikacine (DDD/1000BD)`, 
             gentamicin = `gentamicine (DDD/1000BD)`, ciprofloxacin = `ciprofloxacine (DDD/1000BD)`,
             metronidazole = `metronidazole (DDD/1000BD)`, vancomycin = `vanco (DDD/1000BD)`, 
             ceftazidime = `ceftazidime (DDD/1000BD)`, c3g_glob = `C3G_glob (DDD/1000BD) col J+K+V`, 
             cefepime = `cefepime (DDD/1000BD)`, cephalosporin = `cephalosporines (DDD/1000BD)`, 
             bd = `1000 BD (1000 bed-days)`) %>%
      select(ward, Date_month, covid_period, sha_bd, sha, atb, pena, penaac, cefotaxime, ceftriaxone,
             c3g_tot, pip_taz, carbapenem, fluoroquinolone, macrolide, amikacin, gentamicin, ciprofloxacin,
             metronidazole, vancomycin, ceftazidime, c3g_glob, cefepime, cephalosporin, bd) 
  ) 
}

# Concatenate
atb = bind_rows(atb_first_period, atb)

# HAI --------------------------------------------------------------------------
# 2011-2016
hai_first_period = read_excel("data/tableau K & K rea septembre 2016.xlsx", sheet = "Feuil1", range = "A1:AS70") %>%
  rename(Date_month = date, 
         bd = nb_jh_rea, mean_age = age_moy_rea, dms = dms_rea,
         nb_stays = nb_sej_rea, occup_rate = tx_occup_rea, cd = cd_rea, 
         nb_pat = nb_pat_rea#, 
         # hemoc_c3gs_rea, inf_c3gs_rea, inf_blse_rea, hemoc_blse_rea,
         # blse_rectal_rea,
         # inf_sarm_read = inf_sarm_rea...33, sarm_nasal_rea, hemoc_sasm_rea, hemoc_sarm_rea, inf_sarm_rea...33, 
         # hemoc_candida_rea,
         # inf_hyper_rea,  hemoc_hyper_rea, 
         # inf_erc3g_rea, hemoc_erc3g_rea, 
         ) %>%
  mutate(ward = "ICU", Date_month = as_date(Date_month), covid_period = 0,
         nb_covid = 0, total_covid = 0) %>%
  select(ward, Date_month, covid_period, intervention, bd, nb_urg, nb_dial, nb_chimio,
         nb_cancer, nb_diab, nb_covid, total_covid, mean_age, dms, nb_stays, #stay_length,deaths, age75, age80,
         occup_rate, cd, nb_pat)

# 2017-2023
wards = c("CHIRURGIE", "MEDECINE", "REANIMATION", "MATERNITE")
hai = data.frame()
for (w in wards) {
  hai = bind_rows(
    hai,
    read_excel("data/DIM_Données_VF.xlsx", sheet = w) %>%
      mutate(ward = w, Date_month = as_date(sprintf('%d/%02d/01', year, month)), intervention = 1) %>%
      mutate(ward = case_when(ward == "CHIRURGIE" ~ "Surgery", ward == "MEDECINE" ~ "Medicine",
                              ward == "MATERNITE" ~ "Gynecology", .default = "ICU")) %>%
      rename(covid_period = `COVID Period`, 
             bd = `1000BD (1000 bed-days)`,
             nb_urg = `number of admissions in emergency department`, 
             nb_dial = `number of dialysis performed`, 
             nb_chimio = `number of chimiotherapies performed`, 
             nb_cancer = `number of patients admitted with cancer`, 
             nb_diab = `number of patients admitted with diabetes`, 
             nb_covid = `number of patients admitted with COVID-19`,
             total_covid = `total COVID-19 patients (workload)`,
             mean_age = `age_medium`,
             #dms, 
             nb_stays = `number of stays`,
             stay_length = `average length of stay (days)`,
             occup_rate = `tx_occup rate`, 
             #cd, 
             nb_pat = `admitted patients count`,
             deaths = `deaths cases`,
             age75 = `age_75 cases`,
             age80 = `age_80 cases`
             ) %>%
      select(ward, Date_month, covid_period, intervention, bd, nb_urg, nb_dial, nb_chimio,
             nb_cancer, nb_diab, nb_covid, total_covid, mean_age, nb_stays, stay_length, #dms, cd,
             occup_rate, nb_pat, deaths, age75, age80)
  ) 
}

# Concatenate data
hai = bind_rows(hai_first_period, hai)

################################################################################
## Save concatenated data 
################################################################################
# Antibiotics data
atb %>%
  left_join(., azi %>% select(-bd) %>% rename(azi = ddd_bd), by = c("Date_month", "ward")) %>%
  write.csv2(., "data/antibiotics.csv", row.names = F, quote = F)

# HAI data
write.csv2(hai, "data/hai.csv", row.names = F, quote = F)

################################################################################
## Plots
################################################################################
# Antibiotic consumption--------------------------------------------------------
# Azithromycin
ggplot(azi, aes(x = as_date(Date_month), y = ddd_bd, col = ward)) +
  geom_line() +
  facet_wrap(facets = vars(ward), ncol = 3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Azithromycin (DDD/1,000 bed-days)", x = "", col = "")
ggsave("figures/time_series/azithromycine.png", height = 4, width = 6)

# Other antibiotics - full period
atb %>% 
  filter(ward == "ICU", !is.na(Date_month)) %>%
  select(-c(sha, sha_bd, bd, c3g_glob, c3g_tot, ceftazidime, cefepime, cephalosporin)) %>%
  pivot_longer(-c(ward, Date_month, intervention, covid_period), names_to = "atb", values_to = "cons") %>% 
  mutate(atb = recode(atb, !!!dict_atb)) %>%
  ggplot(., aes(x = Date_month, y = cons)) +
  geom_line() +
  facet_wrap(facets = vars(atb), scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Antibiotic consumption (DDD/1000 bed-days)", title = "ICU")
ggsave("figures/time_series/antibiotics_fullperiod.png", height = 6, width = 10)

# Other antibiotics - second period
atb %>% 
  select(ward, Date_month, c3g_glob, c3g_tot, ceftazidime, cefepime, cephalosporin) %>%
  pivot_longer(-c(ward, Date_month), names_to = "atb", values_to = "cons") %>% 
  filter(ward == "ICU", !is.na(Date_month), !is.na(cons)) %>%
  mutate(atb = recode(atb, !!!dict_atb)) %>%
  ggplot(., aes(x = Date_month, y = cons)) +
  geom_line() +
  facet_wrap(facets = vars(atb), scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Antibiotic consumption (DDD/1000 bed-days)", title = "ICU")
ggsave("figures/time_series/antibiotics_2ndperiod.png", height = 4, width = 8)

# SHA---------------------------------------------------------------------------
atb %>% 
  filter(ward == "ICU", !is.na(Date_month)) %>%
  select(ward, Date_month, sha_bd, intervention, covid_period) %>%
  ggplot(., aes(x = Date_month, y = sha_bd)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Hand rub (L/1000 bed-days)", title = "ICU")
ggsave("figures/time_series/sha.png", height = 3, width = 4)

# Infections--------------------------------------------------------------------
# # Nosocomial infections
# db %>%
#   dplyr::select(date, nb_jh_rea, matches("^inf_")) %>%
#   pivot_longer(cols = matches("^inf_"), names_to = "bacteria", values_to = "n") %>%
#   mutate(incidence = n / nb_jh_rea * 1000 ) %>%
#   ggplot(., aes(x =date, y = incidence)) +
#   geom_line() +
#   facet_wrap(facets = vars(bacteria)) +
#   theme_bw()
# ggsave("figures/time_series/hai.png", height = 4, width = 10)
# 
# 
# # Bloodstream infections
# db %>%
#   dplyr::select(date, nb_jh_rea, matches("^hemoc_")) %>%
#   pivot_longer(cols = matches("^hemoc_"), names_to = "bacteria", values_to = "n") %>%
#   mutate(incidence = n / nb_jh_rea * 1000 ) %>%
#   ggplot(., aes(x =date, y = incidence)) +
#   geom_line() +
#   facet_wrap(facets = vars(bacteria)) +
#   theme_bw() +
#   labs(x = "", y = "Monthly incidence (per 1,000 bed-days)")
# ggsave("figures/time_series/bloodstream_infections.png", 
#        height = 6, width = 10)
# 
# 
# # Colonisation at admission
# db %>%
#   dplyr::select(date, nb_jh_rea, blse_rectal_rea, sarm_nasal_rea) %>%
#   pivot_longer(cols = c(blse_rectal_rea, sarm_nasal_rea), names_to = "bacteria", values_to = "n") %>%
#   mutate(incidence = n / nb_jh_rea * 1000 ) %>%
#   ggplot(., aes(x =date, y = incidence)) +
#   geom_line() +
#   facet_wrap(facets = vars(bacteria)) +
#   theme_bw() +
#   labs(x = "", y = "Monthly incidence (per 1,000 bed-days)")
# ggsave("figures/time_series/colonisation_at_admission.png", height = 3, width = 7)
