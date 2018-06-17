pacman::p_load(tidyverse, rvest, httr, purrr, haven, asciiSetupReader, geofacet)

devtools::load_all(pkg = "../tidyICPSR")

options("icpsr_email" = Sys.getenv("icpsr_email"), "icpsr_password" = Sys.getenv("icpsr_password"))

# This downloads the data from https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/36151 on India.
#

#icpsr_download(file_id = 36151, download_dir = "data")

ind11 <- read_tsv(file = "data/ICPSR_36151/DS0001/36151-0001-Data.tsv")
ind11_recode <- icpsr_sas_recode(file = "data/ICPSR_36151/DS0001/36151-0001-Setup.sas", trailing_f = 1)
ind11_rename <- icpsr_sas_rename(file = "data/ICPSR_36151/DS0001/36151-0001-Setup.sas")



ind11 <- ind11 %>%
  rename_all(str_to_lower) %>%
  rename(distric = distid) %>% # the recode variable name isn't the same as the column in the dat
  mutate_all(icpsr_missing) %>%
  recode_all(y = ind11_recode) %>%
  rename(distid = distric)

write_rds(ind11, path = "data/artifacts/2011_36151_India.Rds")
  
#  rename(!!ind11_rename)

dat <- ind11 %>%
  mutate(stateid = str_remove_all(stateid, " [:digit:]+") %>% 
           str_replace_all("&", "and") %>% str_replace_all("Orissa", "Odisha"),
         ro3 = str_remove_all(ro3, " [:digit:]+")) %>%
  select(idperson, stateid, hhid, distid, psuid, hhsplitid, idhh, 
         anthro_id = ap3, age = ro5, height1 = ap5, height2 = ap6, 
         anthro_position = ap7, weight1 = ap8, weight2 = ap9, 
         sex = ro3, relation_head = ro4, diarrhea = sm7, 
         hours_work_year = wkhours, annual_wages = wsearn)

household <- dat %>%
  group_by(hhid, stateid, distid, psuid, hhsplitid, idhh) %>%
  summarise(n = n(), children = sum(age <= 18, na.rm = TRUE), 
            household_wages = sum(annual_wages, na.rm = TRUE),
            household_hours = sum(hours_work_year, na.rm = TRUE)) %>%
  ungroup()


kids <- dat %>%
  filter(age <= 5) %>% 
  left_join(household)


# The household income numbers are close to the reports I could find.
# - https://cmie.com/kommon/bin/sr.php?kall=warticle&dt=2016-07-04%2013:45:29&msec=170
# - https://www.quora.com/What-is-the-median-income-of-households-in-different-states-of-India
state_labels <- household %>% 
  group_by(stateid) %>%
  summarise(sum = sum(n), mean = mean(household_wages, na.rm = TRUE), median = median(household_wages, na.rm = TRUE))

median(household$household_wages)



household %>%
  ggplot(aes(x = household_wages/10000)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Household wages (10,000 INR)", y = "Count", title = "Distribution of Household Income") +
  facet_geo(~stateid, grid = india_grid2, scales = "free") 

# The household count numbers seem to make sense based on numbers reported at the following.
# - https://en.wikipedia.org/wiki/Indian_states_ranking_by_household_size
# - http://www.arcgis.com/home/item.html?id=6cf22970ea8c4b338a196879397a76e4
state_labels <- household %>% 
  group_by(stateid) %>%
  summarise(sum = sum(n), mean = mean(n, na.rm = TRUE), median = median(n, na.rm = TRUE))

household %>%
  ggplot(aes(x = n)) +
  geom_histogram(color = "white") +
# geom_vline(aes(xintercept = median), data = state_labels) +
  theme_bw() +
  labs(x = "Number in household", y = "Count", title = "Distribution of Household Size") +
  scale_x_continuous(breaks = seq(0,30, by = 5), limits = c(0, 15)) +
  theme(panel.grid.minor = element_blank()) +
  facet_geo(~stateid, grid = india_grid2,  scales = "free_y") 


# Household heights by age
kids %>%
  filter(age <= 5) %>%
  ggplot(aes(y = height2 * 0.0328084, x = factor(age)), fill = "darkgrey") +
  geom_boxplot() +
#  geom_jitter(aes(color = sex), alpha = .5, width = .25) +
  facet_geo(~stateid, grid = india_grid2) +
  theme_bw() +
  scale_color_brewer(type = "qual") +
  coord_cartesian(ylim = c(0, 4)) +
  labs(y = "height (ft)", x = "gender", color = "gender", title = "Distribution of height by age group")

kids %>%
  filter(age <= 1) %>%
  mutate(income_groups = 
           case_when( household_wages/ 10000 <= 2.5 ~ "less than 25k",
                      household_wages/ 10000 <= 5 ~ "between 25k & 50k",
                      household_wages/ 10000 <= 10 ~ "between 50k & 100k",
                      household_wages/ 10000 <= 25 ~ "between 100k & 250k",
                      household_wages/ 10000 > 25 ~ "greater than 250k")) %>%
  ggplot(aes(y = weight2 * 2.200462, x = income_groups)) +
  scale_y_continuous(limits = c(0, 25)) + 
  geom_boxplot() +
  facet_geo(~stateid, grid = india_grid2) +
  labs(x = "Household wages", y = "height (ft)") +
  coord_flip() +
  theme_bw()

# Household weights by age
kids %>%
  filter(age <= 5) %>%
  ggplot(aes(y = weight2 * 2.20462, x = factor(age)), fill = "darkgrey") +
  geom_boxplot() +
  #  geom_jitter(aes(color = sex), alpha = .5, width = .25) +
  facet_geo(~stateid, grid = india_grid2) +
  theme_bw() +
  scale_color_brewer(type = "qual") +
  coord_cartesian(ylim = c(0, 45)) +
  labs(y = "weight (lbs)", x = "gender", color = "gender", title = "Distribution of height by age group")


  
  




height_weight_age <- kids %>%
  filter(age <= 5) %>%
  ggplot(aes(x = height1 * 0.0328084, y = weight1 * 2.20462)) +
  geom_point(aes(color = sex)) +
#  geom_rug(alpha = .25) +
  facet_wrap(~age, nrow = 1, labeller = "label_both") +
  theme_bw() +
  scale_color_brewer(type = "qual") +
  labs(x = "height (ft)", y = "weight (lbs)", color = "gender", title = "Distribution of height/weight by age group")

height_wa_zoom <- height_weight_age + coord_cartesian(ylim = c(0, 50))


ggsave(filename = "inst/ignore/image/scatter.png", plot = height_weight_age, width = 15, height = 8)
ggsave(filename = "inst/ignore/image/scatter_zoom.png", plot = height_wa_zoom, width = 15, height = 8)



