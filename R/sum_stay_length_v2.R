#' Alternative procedure for counting the days in in-hospital care.
#'
#' Can be used for filtering hospitalizations during the wash-out period. The function extends the original data `.data` with a column describing the days in-hospital care during the washout-period, at the end of it and total number of days the person has been in hospital care.
#' @param .data cohort data set containing information about the persons: ID, gender, and index day.
#' @param user_data register data
#' @param idnum name of the column/variable holding the person IDs
#' @param adm_date the variable name of the date of admission
#' @param disc_date the name of the variable holding the date of discharge
#' @param index_date the name of the variable which is the reference point around which to search
#' @param wolen washout length in days. Default is one year, `365`
#' @param ongoing_end_time the time (days) persons were hospitalized at the end of the washout period. Default is three months, `90`
#' 
#' @return returns the `.data` tibble extended with three columns: `wo_total_days` which is the total number of days in hospital during the washout period, `wo_end_days` which is the number of days in the hospital at the end of the washout period, and `total_days` which is the total number of days in hospital care.
#' 
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom tidyr replace_na
#' 
#' @examples
#' \dontrun{
#' d <- regstudies::sample_cohort %>% 
#' sum_stay_length_v2(., user_data = regstudies::sample_regdata, 
#'                    idnum = personid, 
#'                    adm_date = adm_date, 
#'                    disc_date = disc_date,
#'                    index_date = postingdate,
#'                    wolen = 365,
#'                    ongoing_end_time = 90)
#' }
#'
#' @rdname sum_stay_length_v2
#' 
#' @author Markku Kuismin, Juho Kopra, Jani Miettinen, Reijo Sund
#' @export
#'
sum_stay_length_v2 <- function(.data, user_data, 
                               idnum,  
                               adm_date, disc_date, index_date,
                               wolen = 365, ongoing_end_time = 90){
  
  user_data <- user_data %>% dplyr::left_join(.data)
  
  user_data <- user_data %>%
    dplyr::mutate(wo_interval = lubridate::interval({{index_date}} - lubridate::days(wolen), {{index_date}} - days(1)),
                  wo_end_interval = lubridate::interval({{index_date}} - lubridate::days(ongoing_end_time), {{index_date}} - days(1))
                  )
  
  user_data <- user_data %>% dplyr::rowwise() %>% 
    dplyr::mutate(wo_total_days = list(seq({{adm_date}}, {{disc_date}}, "1 day") %within% wo_interval),
                  wo_end_days = list(seq({{adm_date}}, {{disc_date}}, "1 day") %within% wo_end_interval)) %>%
    dplyr::mutate(wo_total_days = list(seq({{adm_date}}, {{disc_date}}, "1 day")[unlist(wo_total_days)]),
                  wo_end_days = list(seq({{adm_date}}, {{disc_date}}, "1 day")[unlist(wo_end_days)]),
                  total_days = list(seq({{adm_date}}, {{disc_date}}, "1 day"))
                  ) %>% 
    dplyr::select(-wo_interval, -wo_end_interval)
  
  user_data <- user_data %>% 
    dplyr::group_by({{idnum}}) %>% 
    dplyr::mutate(wo_total_days = length(base::table(unlist(wo_total_days))),
                  wo_end_days = length(base::table(unlist(wo_end_days))),
                  total_days = length(base::table(unlist(total_days)))
    ) %>% 
    dplyr::distinct(., {{idnum}}, .keep_all = T) %>%
    dplyr::select({{idnum}}, wo_total_days, wo_end_days, total_days)
  
  .data <- left_join(.data, user_data, by = rlang::quo_name(rlang::enquo(idnum))) %>% 
    dplyr::mutate(wo_total_days = tidyr::replace_na(wo_total_days, 0),
                  wo_end_days = tidyr::replace_na(wo_end_days, 0),
                  total_days = tidyr::replace_na(total_days, 0)
                  )
  
  return(.data)
  
}
