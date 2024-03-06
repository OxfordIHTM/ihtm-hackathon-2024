##calculate these statistics for each state------------------------------
calculate_statistics <- function(maternal_data){
  
  stats_by_state <- maternal_data %>% 
    group_by(state_name) %>% 
    summarise(
      median_gest_age = median(ancGestAge, na.rm = TRUE),
      median_attendance = median(visitsANC, na.rm = TRUE),
      percent_anc4 = sum(anc4 == "1" , na.rm = TRUE)/n() * 100,
      percent_anyIFA = sum (anyIFA == "1", na.rm = TRUE)/n() * 100,
      percent_IFA90 = sum(ifa90 == "1", na.rm = TRUE)/n()* 100,
      percent_tetanus = sum (tetanusDoses == "1", na.rm = TRUE)/n() * 100
    )
  
  return(stats_by_state)
}