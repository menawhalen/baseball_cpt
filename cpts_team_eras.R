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
  select(year_id, franch_id, w, l, r, ab, h, hr, bb, g) %>% 
  pivot_longer(cols = -c(year_id, franch_id, g), names_to = "variable", values_to = "value") %>% 
  mutate(game_scale = value/g) %>% 
  pivot_wider(id_cols = c(year_id, franch_id), names_from = variable, values_from = game_scale)

franchise_names <- unique(old_teams_full$franch_id)

# old_teams_full %>% 
#   pivot_longer(cols = 3:10, names_to = "stat", values_to = "value") %>% 
#   filter(franch_id == "NYY") %>% 
#   select(-franch_id) %>% 
#   pivot_wider(id_cols = stat, 
#               names_from = year_id, 
#               values_from = value)

wide_teams <- lapply(franchise_names, function(i){
  wide <- old_teams_full %>% 
    pivot_longer(cols = 3:9, names_to = "stat", values_to = "value") %>% 
    filter(franch_id == i) %>% 
    select(-franch_id) %>% 
    pivot_wider(id_cols = stat, 
                names_from = year_id, 
                values_from = value)
  return(wide)
})

names(wide_teams) <- franchise_names

## checking about missingness
lapply(lapply(seq_along(wide_teams), function(x){
  apply(wide_teams[[x]], MARGIN = 2, function(i) sum(is.na(i)))
}), sum)  
## 8 of the teams are missing two years of data from 1911 and 1912 always so

changes_team <- lapply(wide_teams, 
                  function(x){
                    ## find thresholding value for cpt using bootstrapping
                    thrs <- dcbs.thr(as.matrix(x[-1]), interval = c(1, dim(x[-1])[2]))
                    ### if I lower the threshold more changepoints
                    hr_cpt_dcbs <- dcbs.alg(as.matrix(x[-1]), cp.type = 1, thr = thrs -3)
                    cpt_years <- as.numeric(names(x[-1])[hr_cpt_dcbs$ecp])
                    return(tibble(threshold = thrs,
                                  years = cpt_years))
                    
                  })

teams_cpt_time <- changes_team %>% 
  bind_rows(.id = "franch_id")

old_teams_full %>% 
  pivot_longer(cols = -c(year_id, franch_id), names_to = "variable", values_to = "value") %>% 
  ggplot(aes(year_id, value, color = variable)) +
    geom_line() +
    geom_vline(data = teams_cpt_time, aes(xintercept = years)) +
    facet_wrap(~ franch_id, scales = "free_y")
  
