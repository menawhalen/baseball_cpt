#### Image file for paper

library(tidyverse)
library(janitor)
library(hdbinseg)

######################################################
########### Load and clean up data for analysis
teams <- read_csv("data/Teams.csv") %>% 
  clean_names()

## list of old teams or current teams
team_names <- teams %>% 
  group_by(franch_id) %>% 
  count() %>% 
  filter(n > 120) %>% 
  pull(franch_id)

old_teams_full <- teams %>% 
  filter(franch_id %in% team_names & year_id > 1900) %>% 
  select(year_id, franch_id,g, r, ab, sb, hr, bb, so, attendance)

current_teams <- teams %>% filter(year_id == 2021) %>% pull(franch_id)

## clean up missingness
imp <- mice::mice(teams[c("r","h","hr","bb","so", "ab", "sb", "g", "attendance", 
                          "ra", "ha", "hra", "bba", "soa")], method = "cart")
teams_full_nomiss <- complete(imp)

## full team standardized
full_teams <- bind_cols(teams[c("year_id", "franch_id")], teams_full_nomiss) %>% 
  filter(franch_id %in% team_names & year_id > 1900) %>% 
  mutate(hr_per_game = hr/g, 
         so_per_game = so/g,
         bb_per_game = bb/g,
         att_per_game = attendance/g,
         sb_per_game = sb/g, 
         r_per_game = r/g) %>% 
  select(year_id, franch_id, hr_per_game, so_per_game, bb_per_game, att_per_game, sb_per_game, r_per_game)

## current standardized teams
std_teams <- bind_cols(teams[c("year_id", "franch_id")],
                         teams_full_nomiss) %>% 
  group_by(year_id) %>% 
  mutate(r_season = (r- mean(r))/sd(r),
         h_season = (h - mean(h))/sd(h),
         hr_season = (hr - mean(hr))/sd(hr),
         bb_season = (bb - mean(bb))/sd(bb),
         so_season = (so - mean(so))/sd(so),
         ra_season = -(ra - mean(ra))/sd(ra),
         ha_season = -(ha - mean(ha))/sd(ha),
         bba_season = -(bba - mean(bba))/sd(bba),
         soa_season = -(soa - mean(soa))/sd(soa),
         hra_season = -(hra - mean(hra))/sd(hra))

###################################################################
############# old teams analysis
changes <- lapply(names(full_teams)[-c(1:2)], 
                  function(i){
                    wide <- full_teams %>% 
                      pivot_wider(id_cols = franch_id, 
                                  names_from = year_id, 
                                  values_from = i)
                    
                    ## find thresholding value for cpt using bootstrapping
                    thrs <- dcbs.thr(as.matrix(wide[-1]), interval = c(1, dim(wide[-1])[2]))
                    ### if I lower the threshold more changepoints
                    cpt_dcbs <- dcbs.alg(as.matrix(wide[-1]), cp.type = 1, thr = thrs)
                    cpt_years <- as.numeric(names(wide[-1])[cpt_dcbs$ecp])
                    return(tibble(threshold = thrs,
                                  years = cpt_years,
                                  variable = i))
                    
                  })

changes_var <- lapply(names(full_teams)[-c(1:2)], 
                      function(i){
                        wide <- full_teams %>% 
                          pivot_wider(id_cols = franch_id, 
                                      names_from = year_id, 
                                      values_from = i)
                        
                        ## find thresholding value for cpt using bootstrapping
                        thrs <- dcbs.thr(as.matrix(wide[-1]), interval = c(1, dim(wide[-1])[2]))
                        ### if I lower the threshold more changepoints
                        cpt_dcbs <- dcbs.alg(as.matrix(wide[-1]), cp.type = 2, thr = thrs)
                        cpt_years <- as.numeric(names(wide[-1])[cpt_dcbs$ecp])
                        return(tibble(threshold = thrs,
                                      years = cpt_years,
                                      variable = i))
                        
                      })

time_cpts <- changes %>% 
  tibble() %>% 
  unnest(cols = c(.)) %>% 
  mutate(change = "Change in Mean",
         variable = factor(variable, levels = c("att_per_game", "bb_per_game", "hr_per_game", "r_per_game", "sb_per_game", "so_per_game"),
                           labels = c("Attendance", "Balls", "Home Runs", "Runs", "Stolen Bases", "Strikeouts")))
time_cpts_var <- changes_var %>% 
  tibble() %>% 
  unnest(cols = c(.)) %>% 
  mutate(change = "Change in Variance",
         variable = factor(variable, levels = c("att_per_game", "bb_per_game", "hr_per_game", "r_per_game", "sb_per_game", "so_per_game"),
                           labels = c("Attendance", "Balls", "Home Runs", "Runs", "Stolen Bases", "Strikeouts")))

#################### save
full_teams %>% 
  pivot_longer(cols = -c(year_id, franch_id), names_to = "variable", values_to = "value") %>% 
  mutate(variable = factor(variable, labels = c("Attendance", "Balls", "Home Runs", "Runs", "Stolen Bases", "Strikeouts"))) %>% 
  ggplot(aes(year_id, value, color = franch_id)) +
  geom_line() +
  geom_vline(data = bind_rows(time_cpts, time_cpts_var), aes(xintercept = years, linetype = change)) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Changes Across Time by Statistic",
       x = "Year",
       y = "Value of Statistic",
       color = "Franchise",
       linetype = "Changepoint Type")

#################################################################
######## season averages over all baseball
szn_ave <- full_teams %>% 
  group_by(year_id) %>% 
  summarise(szn_ave_hr = mean(hr_per_game),
            szn_ave_so = mean(so_per_game),
            szn_ave_bb = mean(bb_per_game),
            szn_ave_sb = mean(sb_per_game)) %>% 
  pivot_longer(cols = -year_id, names_to = "stat", values_to = "value") 

wide <- szn_ave %>% 
  pivot_wider(id_cols = stat, 
              names_from = year_id, 
              values_from = value)


## find thresholding value for cpt using bootstrapping
thrs <- dcbs.thr(as.matrix(wide[-1]), interval = c(1, dim(wide[-1])[2]))
### if I lower the threshold more changepoints
cpt_dcbs <- dcbs.alg(as.matrix(wide[-1]), cp.type = 1, thr = thrs)
cpt_years <- as.numeric(names(wide[-1])[cpt_dcbs$ecp])

#cpt_years                   


## variance
## find thresholding value for cpt using bootstrapping
thrs_var <- dcbs.thr(as.matrix(wide[-1]), interval = c(1, dim(wide[-1])[2]))
### if I lower the threshold more changepoints
cpt_dcbs_var <- dcbs.alg(as.matrix(wide[-1]), cp.type = 2, thr = thrs_var)
cpt_years_var <- as.numeric(names(wide[-1])[cpt_dcbs_var$ecp])

#cpt_years_var  

cpt_szn <- bind_rows(tibble(year = cpt_years, cpt_stat = "mean"), 
                     tibble(year = cpt_years_var, cpt_stat = "var"))

szn_ave <- szn_ave %>% 
  mutate(stat = factor(stat, levels = c("szn_ave_bb", "szn_ave_hr", "szn_ave_sb", "szn_ave_so"), labels = c("Walks", "Home Runs", "Stolen Bases", "Strikeouts")))

cpt_szn <- cpt_szn %>% 
  mutate(cpt_stat = factor(cpt_stat, levels = c("mean", "var"), labels = c("Change in Mean", "Change in Variance")))

########################### save
ggplot(szn_ave) +
  geom_line(aes(year_id, value, color = stat)) +
  geom_vline(data = cpt_szn, aes(xintercept = year, linetype = cpt_stat)) +
  labs(title = "Season Average Changes Across Teams",
       subtitle = "By 4 Baseball Metrics",
       x = "Year",
       y = "Average Value",
       color = "Season Average Metric",
       linetype = "Changepoint Type")


##############################################################################
################## current teams 
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
  mutate(type = ifelse(type == 1, "Change in Mean", "Change in Variance"))
### saving but need to cut down
std_teams %>% 
  filter(franch_id %in% current_teams) %>% 
  pivot_longer(cols = ends_with("_season"), names_to = "stat_type", values_to = "stat") %>%  
  ggplot(aes(year_id, stat, color = stat_type)) +
  geom_line() +
  geom_vline(data = total_values, aes(xintercept = years, linetype = type)) +
  facet_wrap(~franch_id, scales = "free_x")

library(teamcolors)
data("teamcolors")

team_colors <- teams %>% 
  filter(franch_id %in% current_teams) %>% 
  select(franch_id, name) %>% 
  distinct() %>% 
  arrange(franch_id) %>%
  inner_join(teamcolors) 
team_colors <- team_colors[-c(4,20,27),]

team_time <- teams %>% 
  filter(franch_id %in% current_teams) %>% 
  group_by(franch_id) %>% 
  summarise(min = min(year_id),
            max = max(year_id)) %>% 
  inner_join(team_colors) %>% 
  left_join(total_values) %>% 
  select(franch_id, min, max, name, years, type) %>% 
  mutate(name = factor(name))
  


ggplot(team_time) +
  geom_segment(data = distinct(select(team_time, name, min, max)), aes(x = min, y = name, xend = max, yend = name), alpha = 0.2,
               size = 3) +
  geom_point(aes(x = years, y = name, shape = type, color = name)) +
  scale_color_manual(values = team_colors$primary, 
                     guide = "none") +
  scale_shape(na.translate = F) +
  scale_y_discrete(limits = rev(levels(team_time$name))) +
  scale_x_continuous(breaks = seq(1875, 2021, 25)) +
  labs(title = "Changepoint Locations of Modern Teams",
       y = "",
       x = "Time",
       shape = "Type of Changepoint") +
  theme(legend.position="bottom")
