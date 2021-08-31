dt_daily <- dt_case_state %>%
  select(date, state, cases_new, cases_recovered) %>%
  left_join(dt_death_state) %>%
  left_join(dt_hospital) %>%
  left_join(dt_icu %>%
              mutate(beds_icu_covid = if_else((beds_icu_covid == 0) & (state == "Johor"), as.integer(10), beds_icu_covid))) %>%
  left_join(dt_pkrc %>%
              rename(
                pkrc_beds = beds,
                pkrc_admitted_pui = admitted_pui,
                pkrc_admitted_covid = admitted_covid,
                pkrc_admitted_total = admitted_total,
                pkrc_discharge_pui = discharge_pui,
                pkrc_discharge_covid = discharge_covid,
                pkrc_discharge_total = discharge_total,
              )) %>%
  mutate(date = as.Date(date)) %>%
  left_join(dt_vac_state %>% 
              select(date, state, daily_full, cumul_full, contains("2"), cansino) %>% 
              mutate(date = as.Date(date) + 14)) %>% ## delay 14 days for full
  left_join(dt_population %>% select(state, pop))

temp_nation <- dt_daily %>%
  group_by(date) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(state = "All") %>%
  ungroup()

dt_daily_state <- dt_daily %>%
  add_row(temp_nation) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate(
    hosp_netflow = admitted_total - discharged_total,
    pkrc_netflow = pkrc_admitted_total - pkrc_discharge_total
  ) %>%
  group_by(state) %>%
  mutate(
    ## hospital
    cumul_hosp_netflow = cumsum(hosp_netflow),
    cumul_remain_bed = beds_covid - cumul_hosp_netflow,
    cumul_hosp_netflow_rate = round(cumul_hosp_netflow*100/beds, digits = 2),
    cumul_remain_bed_rate = round(cumul_remain_bed*100/beds, digits = 2),
    
    ## pkrc
    cumul_pkrc_netflow = cumsum(pkrc_netflow),
    cumul_remain_pkrc_bed = pkrc_beds - cumul_pkrc_netflow,
    cumul_pkrc_netflow_rate = round(cumul_pkrc_netflow*100/pkrc_beds, digits = 2),
    cumul_remain_pkrc_rate = round(cumul_remain_pkrc_bed*100/pkrc_beds, digits = 2),
    
    ## icu & vent
    remain_icu_covid_rate = round((beds_icu_covid - icu_covid - icu_pui - icu_noncovid)*100/beds_icu_covid, digits = 2),
    remain_vent_covid_rate = round((vent - vent_covid - vent_pui - vent_noncovid)*100/vent, digits = 2)
    
  ) %>%
  ungroup() %>%
  ## Per population
  mutate(
    cumul_not_full = pop - cumul_full, ## not having fully vaccination
    cases_new_adjust = cases_new*100000/pop,
    deaths_new_adjust = deaths_new*1000000/pop,
    daily_full_adjust = daily_full*10000/pop,
    cumul_full_adjust = cumul_full*100000/pop,
    cumul_not_full_adjust = cumul_not_full*100/pop,
    beds_adjust = beds*100000/pop,
    beds_covid_adjust = beds_covid*100000/pop,
    beds_noncrit_adjust = beds_noncrit*100000/pop,
    beds_icu_total_adjust = beds_icu_total*100000/pop,
    beds_icu_covid_adjust = beds_icu_covid*100000/pop,
    vent_adjust = vent*100000/pop,
    pkrc_beds_adjust = pkrc_beds*100000/pop
  ) %>%
  mutate_if(is.numeric, ~if_else(. %nin% c(Inf, -Inf, NaN), ., NULL)) %>%
  arrange(date, state)

rm(temp_nation, dt_daily)

## Cumulative
sum_cum <- dt_daily_state %>%
  group_by(state) %>%
  summarise(
    cumul_cases_new = sum(cases_new, na.rm = TRUE),
    cumul_cases_new_adjust = sum(cases_new_adjust, na.rm = TRUE),
    cumul_deaths_new = sum(deaths_new, na.rm = TRUE),
    cumul_deaths_new_adjust = sum(deaths_new_adjust, na.rm = TRUE),
    # cum_recover = sum(cases_recovered, na.rm = TRUE)
  ) %>%
  # mutate(
  #   cum_active = cum_case - cum_recover,
  #   cum_active_adj = cum_active*100000/32657400
  # ) %>%
  rename(name = state)
