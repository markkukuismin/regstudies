#' Count the days in in-hospital care from the start of the washout period.
#'
#' Can be used for filtering hospitalizations during the wash-out period. The function extends the original data `.data` with a column describing the days in-hospital care during the washout-period.
#' @param .data cohort data containing at least id-numbers and index days
#' @param user_data register data
#' @param idnum name of the column/variable holding the person IDs
#' @param adm_date the variable name of the date of admission
#' @param disc_date the name of the variable holding the date of discharge
#' @param index_date the name of the variable which is the reference point around which to search
#' @param wolen washout length in days. Default is one year, `365`
#' @param ongoing_end_time the time (days) persons were hospitalized at the end of the washout period. Default is three months, `90`
#' @return returns the `.data` tibble extended with two columns: `wo_total_time_hosp` which is the total number of days in hospital during the washout period, and `wo_end_time_hosp` which is the number of days in the hospital at the end of the washout period.
#' 
#' @keywords regstudies, dplyr, tidyr
#' @export
#' @examples
#' \dontrun{
#' Df <- regstudies::sample_regdata %>% 
#'   dplyr::left_join(sample_cohort %>% select(personid, postingdate), by="personid")
#'
#' d <- Df %>% regstudies::sum_stay_length(., regstudies::sample_cohort,
#'                                         idnum = personid,
#'                                         adm_date = adm_date, 
#'                                         disc_date = disc_date, 
#'                                         index_date = postingdate, 
#'                                         wolen = 2*365, 
 #'                                        ongoing_end_time = 60)
#' }
#'
#' @export
#'
sum_stay_length <- function(.data, user_data, idnum,
                            adm_date, 
                            disc_date, 
                            index_date, 
                            wolen = 365, 
                            ongoing_end_time = 90
                            ){
  
  .data <- dplyr::mutate(.data, rel_adm = as.numeric({{adm_date}} - {{index_date}}),
                  rel_dis = as.numeric({{disc_date}} - {{index_date}}))
  
  .data <- .data %>% 
    dplyr::mutate(
      rel_adm = dplyr::if_else(rel_adm < -wolen, -wolen, rel_adm)
    )
  
  .data <- .data %>% dplyr::mutate(
    rel_dis = dplyr::if_else(rel_dis > 0, 0, rel_dis)
  )
  
  .data <- .data %>% dplyr::select({{idnum}}, rel_dis, rel_adm)
  
  ind1 <- user_data %>% dplyr::pull({{idnum}}) 
  
  for(day in 1:wolen){
    
    tmp <- .data %>% dplyr::filter(rel_dis >= day - wolen - 1, rel_adm <= day - wolen - 1)
	
	ind2 <- tmp %>% dplyr::pull({{idnum}})
    
    hospday <- paste0("hday", day)
    
    user_data[ , hospday] <- 0
    
    user_data[ind1 %in% ind2, hospday] <- 1
    
  }
  
  user_data$wo_total_time_hosp <- rowSums(user_data %>% dplyr::select(tidyr::contains("hday"))) 
  
  ongoing_end_days <- paste0("hday", (wolen - ongoing_end_time + 1):wolen)
    
  user_data$wo_end_time_hosp <- rowSums(user_data %>% dplyr::select(tidyr::contains(ongoing_end_days)))
  
  user_data <- user_data %>% dplyr::select(-contains("hday"))
  
  return(user_data)
  
}