pacman::p_load(tidyverse, rvest, httr, purrr, haven, asciiSetupReader, geofacet)

devtools::load_all(pkg = "../tidyICPSR")

options("icpsr_email" = Sys.getenv("icpsr_email"), "icpsr_password" = Sys.getenv("icpsr_password"))

# This downloads the data from https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/36151 on India.
#

#icpsr_download(file_id = 36151, download_dir = "data")

ind11 <- read_tsv(file = "data/ICPSR_36151/DS0004/36151-0004-Data.tsv")
ind11_recode <- icpsr_sas_recode(file = "data/ICPSR_36151/DS0004/36151-0004-Setup.sas", trailing_f = 1)
ind11_rename <- icpsr_sas_rename(file = "data/ICPSR_36151/DS0004/36151-0004-Setup.sas")

## Need to deal with recode when they are replacing text to a numeric.  Fix by hand
## 

# VALUE bh8affff  55='(55) <1 month 55';
# VALUE bh6bffff  88='(88) Don''t Know';
ind11_recode <- ind11_recode[!names(ind11_recode) %in% c("bh8a", "bh6b")]



ind11 <- ind11 %>%
  rename_all(str_to_lower) %>%
  mutate_all(icpsr_missing) %>%
  recode_all(y = ind11_recode) %>%
  mutate(bh8a = ifelse(bh8a == 55, .5, bh8a), bh6b = ifelse(bh6b == 88, NA, bh6b))


  
#  rename(!!ind11_rename)

dat <- ind11 %>%
  mutate(stateid = str_remove_all(stateid, " [:digit:]+") %>% 
           str_replace_all("&", "and") %>% str_replace_all("Orissa", "Odisha"),
         bh4 = str_remove_all(bh4, " [:digit:]+"), bh7 = str_remove_all(bh7, " [:digit:]+")) %>%
  select(stateid, idhh, hhid, distid, psuid, mother_id = ew3, birth_id = bh1, age_years = bh6a, 
         sex = bh4, location = bh7, age_death_years = bh8a, age_death_months = bh8b,
         nbirths = bhed) %>%
  mutate(age_death_years = ifelse(age_years < age_death_years, NA, age_death_years))

write_rds(dat, path = "data/artifacts/2011_36151_India_births.Rds")


dat %>%
  group_by(idhh, stateid, mother_id) %>%
  summarise(births = max(nbirths)) %>%
  ungroup() %>%
  mutate(stateid = fct_reorder(stateid, births, quantile, probs = .75)) %>%
  ggplot(aes(x = stateid, y = births)) +
  geom_boxplot() + 
  coord_flip() +
  theme_bw()


# location <- dat %>%
#   #filter(age_years < 10) %>%
#   mutate(location = recode(location, !!!list(Elsewhere = "Alive", `With Respondent` = "Alive"))) %>%
#   group_by(idhh, stateid, mother_id, location) %>%
#   summarise(n = n(), nbirths = max(nbirths), age_death_years = mean(age_death_years, na.rm = TRUE), age_years = mean(age_years),
#             prop = n/max(nbirths)) %>%
#   ungroup() %>%
#   mutate(stateid = fct_reorder(stateid, prop, min))

# 
# location %>%
#   ggplot(aes(x = nbirths, y = n/nbirths )) +
#   geom_jitter(width = .25, height = .01) +
#   geom_smooth() +
#   scale_x_continuous(breaks = seq(1, 25, 2)) +
#   facet_wrap(~location, ncol = 1)
# 


# location %>%
#   ggplot(aes(x = stateid, y = prop )) +
#   geom_boxplot() +
#   geom_jitter(width = .25, height = .1) +
#   facet_wrap(~location, ncol = 1)
            