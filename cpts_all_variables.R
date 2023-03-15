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

old_teams_full <- teams %>% 
  filter(franch_id %in% team_names & year_id > 1900) %>% 
  select(year_id, franch_id,g, r, ab, sb, hr, bb, so, attendance)

names(old_teams_full)[-c(1:2)]
## 16 missing values from SO
apply(old_teams_full, 2,function(x) sum(is.na(x)))


imp <- mice::mice(old_teams_full[-c(1:2)])
teams_full_nomiss <- complete(imp)
apply(teams_full_nomiss, 2,function(x) sum(is.na(x)))

full_teams <- bind_cols(old_teams_full[1:2],teams_full_nomiss) %>% 
  mutate(hr_per_game = hr/g, 
         so_per_game = so/g,
         bb_per_game = bb/g,
         att_per_game = attendance/g,
         sb_per_game = sb/g, 
         r_per_game = r/g) %>% 
  select(year_id, franch_id, hr_per_game, so_per_game, bb_per_game, att_per_game, sb_per_game, r_per_game)


# cpt <- function(dataset, var){
#   wide <- dataset %>% 
#     pivot_wider(id_cols = franch_id, 
#                 names_from = year_id, 
#                 values_from = var)
#   
#   ## find thresholding value for cpt using bootstrapping
#   thrs <- dcbs.thr(as.matrix(wide[-1]), interval = c(1, dim(wide[-1])[2]))
#   ### if I lower the threshold more changepoints
#   hr_cpt_dcbs <- dcbs.alg(as.matrix(wide[-1]), cp.type = 1, thr = thrs)
#   cpt_years <- as.numeric(names(wide[-1])[hr_cpt_dcbs$ecp])
#   return(list(threshold = thrs,
#               years = cpt_years),
#               variable = var)
#   
# }

changes <- lapply(names(full_teams)[-c(1:2)], 
                  function(i){
                    wide <- full_teams %>% 
                      pivot_wider(id_cols = franch_id, 
                                  names_from = year_id, 
                                  values_from = i)
                    
                    ## find thresholding value for cpt using bootstrapping
                    thrs <- dcbs.thr(as.matrix(wide[-1]), interval = c(1, dim(wide[-1])[2]))
                    ### if I lower the threshold more changepoints
                    cpt_dcbs <- dcbs.alg(as.matrix(wide[-1]), cp.type = 1, thr = thrs-1)
                    cpt_years <- as.numeric(names(wide[-1])[cpt_dcbs$ecp])
                    return(tibble(threshold = thrs,
                                years = cpt_years,
                                variable = i))
                    
                  })

time_cpts <- changes %>% 
  tibble() %>% 
  unnest(cols = c(.)) 


full_teams %>% 
  pivot_longer(cols = -c(year_id, franch_id), names_to = "variable", values_to = "value") %>% 
  ggplot(aes(year_id, value, color = franch_id)) +
  geom_line() +
  geom_vline(data = time_cpts, aes(xintercept = years)) +
  facet_wrap(~ variable, scales = "free_y")

