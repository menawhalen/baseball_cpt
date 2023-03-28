library(tidyverse)
library(janitor)
library(hdbinseg)


teams <- read_csv("data/Teams.csv") %>% 
  clean_names() %>% 
  select(-x1)

current_teams <- teams %>% filter(year_id == 2021) %>% pull(franch_id)

teams %>% 
  filter(franch_id %in% current_teams) %>% 
  map(~sum(is.na(.)))

imp <- mice::mice(filter(teams, franch_id %in% current_teams)[15:40])
teams_full_nomiss <- complete(imp)
apply(teams_full_nomiss, 2,function(x) sum(is.na(x)))

clean_modern_teams <- bind_cols(filter(teams, franch_id %in% current_teams)[1:14],
                                teams_full_nomiss,
          filter(teams, franch_id %in% current_teams)[41:48])

## divide by season average for team stats ??
clean_modern_teams %>% 
  group_by(year_id) %>% 
  mutate(r_season = mean(r),
         ab_season = mean(ab),
         h_season = mean(h),
         hr_season = mean(hr),
         bb_season = mean(bb),
         so_season = mean(so),
         sb_season = mean(sb),
         w_season = mean(w),
         ha_season = mean(ha),
         bba_season = mean(bba),
         soa_season = mean(soa),
         hra_season = mean(hra),
         att_season = mean(attendance),
         r_per_season = r/r_season,
         ab_per_season = ab/ab_season,
         h_per_season = h/h_season,
         hr_per_season = hr/hr_season,
         bb_per_season = bb/bb_season,
         so_per_season = so/so_season,
         sb_per_season = sb/sb_season,
         hra_per_season = hra/hra_season,
         w_per_season = w/w_season,
         ha_per_season = ha/ha_season,
         bba_per_season = bba/bba_season,
         soa_per_season = soa/soa_season,
         att_per_season = attendance/att_season) %>% 
  pivot_longer(cols = ends_with("per_season"), names_to = "stat_type", values_to = "stat") %>% 
  ggplot(aes(year_id, stat, color = stat_type)) +
  geom_line() +
  facet_wrap(~franch_id, scales = "free_x")

### standardize teams by season average then by number of games
std_teams <- clean_modern_teams %>% 
  group_by(year_id) %>% 
  mutate(r_season = mean(r),
         ab_season = mean(ab),
         h_season = mean(h),
         hr_season = mean(hr),
         bb_season = mean(bb),
         so_season = mean(so),
         sb_season = mean(sb),
         w_season = mean(w),
         ha_season = mean(ha),
         bba_season = mean(bba),
         soa_season = mean(soa),
         hra_season = mean(hra),
         att_season = mean(attendance)) %>% 
  ungroup() %>% 
  mutate(r_per_season = (r-r_season)/g,
         ab_per_season = (ab-ab_season)/g,
         h_per_season = (h-h_season)/g,
         hr_per_season = (hr-hr_season)/g,
         bb_per_season = (bb-bb_season)/g,
         so_per_season = (so-so_season)/g,
         sb_per_season = (sb-sb_season)/g,
         hra_per_season = (hra-hra_season)/g,
         w_per_season = (w-w_season)/g,
         ha_per_season = (ha-ha_season)/g,
         bba_per_season = (bba-bba_season)/g,
         soa_per_season = (soa-soa_season)/g) 

std_teams %>% 
  pivot_longer(cols = ends_with("per_season"), names_to = "stat_type", values_to = "stat") %>% 
  ggplot(aes(year_id, stat, color = stat_type)) +
  geom_line() +
  facet_wrap(~franch_id, scales = "free_x")


## get list of all teams with their data
curt_teams_data <- current_teams |>
  map(\(x) filter(std_teams, franch_id == x))
names(curt_teams_data) <- current_teams


cpt_fct <- function(dat){
  ## find thresholding value for cpt using bootstrapping
  thrs <- dcbs.thr(as.matrix(dat), interval = c(1, dim(dat)[2]))
  ### if I lower the threshold more changepoints
  cpt_dcbs <- dcbs.alg(as.matrix(dat), cp.type = 1, thr = thrs)
  cpt_years <- as.numeric(names(dat)[cpt_dcbs$ecp])
  return(tibble(threshold = thrs,
                years = cpt_years))
}

cpt <- lapply(1:30, function(i){
  select(curt_teams_data[[i]], year_id, ends_with("per_season")) %>% 
    pivot_longer(-year_id, names_to = "stat") %>% 
    pivot_wider(names_from = "year_id", values_from = "value") %>% 
    mutate(stat = str_remove_all(stat, "_per_season")) %>% 
    select(-1) %>% 
    cpt_fct()
})
names(cpt) <- current_teams 
mean_change <- bind_rows(cpt, .id = "franch_id")

cpt_var_fct <- function(dat){
  ## find thresholding value for cpt using bootstrapping
  thrs <- dcbs.thr(as.matrix(dat), interval = c(1, dim(dat)[2]))
  ### if I lower the threshold more changepoints
  cpt_dcbs <- dcbs.alg(as.matrix(dat), cp.type = 2, thr = thrs)
  cpt_years <- as.numeric(names(dat)[cpt_dcbs$ecp])
  return(tibble(threshold = thrs,
                years = cpt_years))
}
cpt_var <- lapply(1:30, function(i){
  select(curt_teams_data[[i]], year_id, ends_with("per_season")) %>% 
    pivot_longer(-year_id, names_to = "stat") %>% 
    pivot_wider(names_from = "year_id", values_from = "value") %>% 
    mutate(stat = str_remove_all(stat, "_per_season")) %>% 
    select(-1) %>% 
    cpt_var_fct()
})
names(cpt_var) <- current_teams 
var_change <- bind_rows(cpt_var, .id = "franch_id")

total_values <- bind_rows(mean_change, var_change, .id = "type") %>% 
  mutate(type = ifelse(type == 1, "mean", "variance"))

std_teams %>% 
  pivot_longer(cols = ends_with("per_season"), names_to = "stat_type", values_to = "stat") %>% 
  ggplot(aes(year_id, stat, color = stat_type)) +
  geom_line() +
  geom_vline(data = total_values, aes(xintercept = years, linetype = type)) +
  facet_wrap(~franch_id, scales = "free_x")
