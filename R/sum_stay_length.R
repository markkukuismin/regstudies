#' Count the days in in-hospital care from the start of the washout period.
#'
#' Can be used for filtering hospitilazations during the wash-out period. The function extends the original data `.data` with a column describing the days in-hospital care during the washout-period.
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
#' d <- regstudies::sample_cohort %>% sum_stay_length(., regstudies::sample_regdata, idnum = personid, 
#'                                                     adm_date = adm_date, 
#'                                                     disc_date = disc_date,
#'                                                     index_date = postingdate,
#'                                                     wolen = 365,
#'                                                     ongoing_end_time = 90)
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
  
  tmp <- user_data %>%
    dplyr::inner_join(.data, by = rlang::quo_name(rlang::enquo(idnum)))
	
  adm_date <- rlang::quo_name(rlang::enquo(adm_date))
  
  disc_date <- rlang::quo_name(rlang::enquo(disc_date))
  
  index_date <- rlang::quo_name(rlang::enquo(index_date))
  
  tmp <- dplyr::mutate(tmp, rel_adm = as.numeric(adm_date - index_date),
                  rel_dis = as.numeric(disc_date - index_date))
  
  tmp <- tmp %>% 
    dplyr::mutate(
      rel_adm = dplyr::if_else(rel_adm < -wolen, -wolen, rel_adm)
    )
  
  tmp <- tmp %>% dplyr::mutate(
    rel_dis = dplyr::if_else(rel_dis > 0, 0, rel_dis)
  )
  
  tmp <- tmp %>% dplyr::select({{idnum}}, rel_dis, rel_adm)
  
  for(day in 1:wolen){
    
    tmp.2 <- tmp %>% dplyr::filter(rel_dis >= day - wolen - 1, rel_adm <= day - wolen - 1)
    
    hospday <- paste0("hday", day)
    
    .data[ , hospday] <- 0
    
    ind1 <- .data %>% dplyr::pull({{idnum}}) 
    
    ind2 <- tmp.2 %>% dplyr::pull({{idnum}})
    
    .data[ind1 %in% ind2, hospday] <- 1
    
  }
  
  .data$wo_total_time_hosp <- rowSums(.data %>% dplyr::select(tidyr::contains("hday"))) 
  
  ongoing_end_days <- paste0("hday", (wolen - ongoing_end_time + 1):wolen)
    
  .data$wo_end_time_hosp <- rowSums(.data %>% dplyr::select(tidyr::contains(ongoing_end_days)))
  
  .data <- .data %>% dplyr::select(-contains("hday"))
  
  return(.data)
  
}