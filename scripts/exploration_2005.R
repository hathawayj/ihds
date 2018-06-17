pacman::p_load(tidyverse, rvest, httr, purrr, haven, asciiSetupReader, tidyICPSR, trelliscopejs)

options("icpsr_email" = Sys.getenv("icpsr_email"), "icpsr_password" = Sys.getenv("icpsr_password"))

# This downloads the data from https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/22626 on India.
#
# set data folder to be ignored

# icpsr_download(file_id = 22626, download_dir = "data")

ind5 <- read_tsv(file = "data/ICPSR_22626/DS0001/22626-0001-Data.tsv")
ind5_recode <- icpsr_sas_recode(file = "data/ICPSR_22626/DS0001/22626-0001-Setup.sas")
ind5_rename <- icpsr_sas_rename(file = "data/ICPSR_22626/DS0001/22626-0001-Setup.sas")



ind5 <- ind5 %>%
  rename_all(str_to_lower) %>%
  mutate_all(icpsr_missing) %>%
  recode_all(y = ind5_recode) 

# rename(!!ind5_rename)



dat <- ind11 %>%
  mutate(stateid = str_remove_all(stateid, " [:digit:]+") %>% 
           str_replace_all("&", "and") %>% str_replace_all("Orissa", "Odisha"),
         ro3 = str_remove_all(ro3, " [:digit:]+")) %>%
  select(idperson, stateid, hhid, distid, psuid, hhsplitid, idhh, 
         anthro_id = ap3, age = ro5, height1 = ap5, height2 = ap6, 
         anthro_position = ap7, weight1 = ap8, weight2 = ap9, 
         sex = ro3, relation_head = ro4, diarrhea = sm7, 
         hours_work_year = wkhours, annual_wages = ws10annual)

rename(anthro_id = ap1, age = ro5, height = ap2, anthro_position = ap3,
       weight1 = ap4, weight2 = ap5, sex = ro3, relation_head = ro4)

household <- dat %>%
  group_by(hhid, stateid, distid, psuid, hhsplitid, idhh) %>%
  summarise(n = n(), children = sum(age <= 18, na.rm = TRUE), 
            household_wages = sum(annual_wages, na.rm = TRUE),
            household_hours = sum(hours_work_year, na.rm = TRUE))


kids <- dat %>%
  filter(age <= 5) %>% 
  left_join(household)



household %>%
  ggplot(aes(x = household_wages)) +
  geom_histogram() +
  theme_bw() +
  facet_geo(~stateid, grid = india_grid2, scales = "free")








ind5 <- ind5 %>%
  rename_all(str_to_lower) %>%
  select(personid, stateid, distname, ro3, ro4, ro5, contains("AP"),
         npersons, nchildren, nteens, nadults,
         income, incfarm) %>%
  mutate_all(my_na) %>%
  mutate(ro3 = recode(ro3, !!!ind5_recode[[str_which(names(ind5_recode), "ro3")]],),
         ro4 = recode(ro4, !!!ind5_recode[[str_which(names(ind5_recode), "ro4")]]),
         stateid = recode(stateid, !!!ind5_recode[[str_which(names(ind5_recode), "stateid")]]),
         ap3 = recode(ap3, !!!ind5_recode[[str_which(names(ind5_recode), "ap3")]]),
         distname = recode(distname, !!!ind5_recode[[str_which(names(ind5_recode), "distname")]])) %>%



height_weight_age <- ind5 %>%
  filter(age <= 5) %>%
  ggplot(aes(x = height * 0.0328084, y = weight1 * 2.20462)) +
  geom_point(aes(color = sex)) +
  geom_rug(alpha = .25) +
  facet_wrap(~age, nrow = 1, labeller = "label_both") +
  theme_bw() +
  scale_color_brewer(type = "qual") +
  labs(x = "length/height (ft)", y = "weight (lbs)", color = "gender", title = "Distribution of height/weight by age group")

height_wa_zoom <- height_weight_age + coord_cartesian(ylim = c(0, 50))


ggsave(filename = "inst/ignore/image/scatter.png", plot = height_weight_age, width = 15, height = 8)
ggsave(filename = "inst/ignore/image/scatter_zoom.png", plot = height_wa_zoom, width = 15, height = 8)



