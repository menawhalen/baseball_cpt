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
## missing values in so and sb that we need to fill in
imp <- mice::mice(teams[c("r","h","hr","bb","so", "ab", "sb", 
                          "ra", "ha", "hra", "bba", "soa")], method = "cart")
teams_full_nomiss <- complete(imp)
#apply(teams_full_nomiss, 2,function(x) sum(is.na(x)))

teams_full_nomiss$bb[imp$where[,c("bb")]] #no negative values now capped at "real" values
teams_full_nomiss$sb[imp$where[,c("sb")]]
teams_full_nomiss$so[imp$where[,c("so")]]

clean_teams <- bind_cols(teams[c("year_id", "franch_id")],
                                teams_full_nomiss)

### standardize teams by season average then by number of games
std_teams <- clean_teams %>% 
  group_by(year_id) %>% 
  mutate(r_season = (r- mean(r))/sd(r),
         #ab_season = (ab - mean(ab))/sd(ab),
         h_season = (h - mean(h))/sd(h),
         hr_season = (hr - mean(hr))/sd(hr),
         bb_season = (bb - mean(bb))/sd(bb),
         so_season = (so - mean(so))/sd(so),
         #sb_season = (sb - mean(sb))/sd(sb),
         #w_season = (w - mean(w))/sd(w),
         ra_season = -(ra - mean(ra))/sd(ra),
         ha_season = -(ha - mean(ha))/sd(ha),
         bba_season = -(bba - mean(bba))/sd(bba),
         soa_season = -(soa - mean(soa))/sd(soa),
         hra_season = -(hra - mean(hra))/sd(hra))


  
std_teams %>% 
  filter(franch_id %in% current_teams) %>% 
  pivot_longer(cols = ends_with("_season"), names_to = "stat_type", values_to = "stat") %>% 
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
  select(curt_teams_data[[i]], year_id, ends_with("_season")) %>% 
    pivot_longer(-year_id, names_to = "stat") %>% 
    pivot_wider(names_from = "year_id", values_from = "value") %>% 
    mutate(stat = str_remove_all(stat, "_season")) %>% 
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
  select(curt_teams_data[[i]], year_id, ends_with("_season")) %>% 
    pivot_longer(-year_id, names_to = "stat") %>% 
    pivot_wider(names_from = "year_id", values_from = "value") %>% 
    mutate(stat = str_remove_all(stat, "_season")) %>% 
    select(-1) %>% 
    cpt_var_fct()
})
names(cpt_var) <- current_teams 
var_change <- bind_rows(cpt_var, .id = "franch_id")

total_values <- bind_rows(mean_change, var_change, .id = "type") %>% 
  mutate(type = ifelse(type == 1, "mean", "variance"))

std_teams %>% 
  filter(franch_id %in% current_teams) %>% 
  pivot_longer(cols = ends_with("_season"), names_to = "stat_type", values_to = "stat") %>%  
  ggplot(aes(year_id, stat, color = stat_type)) +
  geom_line() +
  geom_vline(data = total_values, aes(xintercept = years, linetype = type)) +
  facet_wrap(~franch_id, scales = "free_x")

## print out table of results
total_values %>% 
  group_by(type, franch_id) %>% 
  summarise(cpt = paste(years, collapse = ", ")) %>% 
  kableExtra::kable(format = "markdown")

