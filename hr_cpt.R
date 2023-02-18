library(tidyverse)
library(janitor)
library(hdbinseg)

teams <- read_csv("data/Teams.csv") %>% 
  clean_names()


team_names <- teams %>% 
  group_by(franch_id) %>% 
  count() %>% 
  filter(n > 120) %>% 
  pull(franch_id)

teams %>% 
  filter(franch_id %in% team_names) %>% 
  group_by(franch_id) %>% 
  summarise(min(year_id)) ## filter after 1901

old_teams_full <- teams %>% 
  filter(franch_id %in% team_names & year_id > 1900) %>% 
  select(year_id, franch_id, w, l, r, ab, h, hr, bb, so)


teams_wide <- old_teams_full %>% 
  pivot_wider(id_cols = franch_id, 
              names_from = year_id, 
              values_from = hr)

## find thresholding value for cpt using bootstrapping
thrs <- dcbs.thr(as.matrix(teams_wide[-1]), interval = c(1, dim(teams_wide[-1])[2]))
### if I lower the threshold more changepoints
hr_cpt_dcbs <- dcbs.alg(as.matrix(teams_wide[-1]), cp.type = 1, thr = thrs)
hr_cpt_dcbs$ecp
hr_cpt_sbs <- sbs.alg(as.matrix(teams_wide[-1]), cp.type = 1)         
hr_cpt_sbs$ecp

cpt_years <- as.numeric(names(teams_wide[-1])[hr_cpt_dcbs$ecp])

ggplot(old_teams_full, aes(year_id, hr, color = franch_id)) +
  geom_line() +
  geom_vline(xintercept = cpt_years)

