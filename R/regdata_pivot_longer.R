#' Extends the data with variable describing the code version and returns the data in a long format. This function only works with regular expression tables
#'
#' The function adds a variable describing the version of the ICD, SII or ATC codes based on the  classification table.
#' If the ICD, SII or ATC codes are found, the function adds ICD (either icd9 or icd10), ATC or SII code name to the input data set and returns a data in a long format. 
#' This long format data can then be analyzed with functions such as `regstudies::classify_elixhauser`, 
#' `regstudies::classify_charlson` or `regstudies::classify_table`.
#' @param .data tibble in wide-format where class of date variables are dates, class of variables holding atc codes are characters and class of id-numbers is double or int
#' @param idnum name of the variable holding identification codes of subjects
#' @param diag_tbl tibble which holds the classification details: needs to have variables 'regex' and 'label' 'regex' must hold a string with a regular expression defining classes. 'regex.rm' is optional, defines exceptions to 'regex' (these are not in the group they are named in) 'label' defines the names of the variables of classes (e.g. comorbidity indicators)
#' 
#' @return returns a tibble object extended with a variable describing the ICD code version. The data is in long format. ICD codes themself are stored into a new variable `CODE`.
#' 
#' @importFrom dplyr distinct
#' @importFrom dplyr rename
#' @importFrom dplyr select_if
#' @importFrom dplyr bind_cols
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate_all
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom tidyr pivot_longer
#' 
#' @examples
#' \dontrun{
#' # Use your own icd codes,
#' 
#' my_classes <-
#' tribble(~class_myclass,         ~label_myclass,                                 ~icd10,                 ~icd9, ~score,
#'         "aids",          "AIDS or HIV",                  "^B20|^B21|^B22|^B24",      "^042|^043|^044",      7,
#'         "dementia",             "Dementia", "^F00|^F01|^F02|^F03|^F051|^G30|^G311",    "^290|^2941|^3312",      3,
#'         "pud", "Peptic ulcer disease",                  "^K25|^K26|^K27|^K28", "^531|^532|^533|^534",      1
#' )
#'
#' regdata_long <- regdata %>% 
#'   regdata_pivot_longer(idnum = personid, 
#'                        diag_tbl = my_classes %>% select(icd10, icd9)
#'   )
#'
#' D <- regdata_long %>% 
#'   regstudies::classify_codes(codes = CODE, 
#'                              diag_tbl = read_classes(my_classes)
#'   ) 
#' }
#'
#' @export
#'
#' @author Markku Kuismin, Juho Kopra, Jani Miettinen, Reijo Sund
#'

regdata_pivot_longer = function(.data, idnum, diag_tbl = NULL){
  
  idnum <- rlang::quo_name(rlang::enquo(idnum))
  
  diag_tbl <- diag_tbl %>% 
    dplyr::distinct(icd9, .keep_all = T) %>% 
    dplyr::distinct(icd10, .keep_all = T) %>% # Remove duplicate icd9 and icd10 codes 
    dplyr::mutate(across(where(is.factor), as.character)) # Some variables are turned into factors and integers...
  
  diag_tbl <- dplyr::as_tibble(diag_tbl)
  
  B <- .data %>% dplyr::select(where(is.character))
  
  B <- B %>% dplyr::mutate_all(na_if,"") # Replace blank with NA
  
  B2 <- B %>% tidyr::pivot_longer(tidyr::everything())

  B2 <- B2 %>% dplyr::rename(Diagnosis = name, CODE = value)
  
  icd <- rep(NA_character_, nrow(B2))
  
  icd_class <- colnames(diag_tbl)
  
  d <- c(which(!stringr::str_detect(icd_class, "^icd")),
         which(stringr::str_detect(icd_class, "^icd"))) # Just put ICD version names last
  
  icd_class <- icd_class[d]
  
  for(icd_version in icd_class){
    
    icd <- if_else(
      stringr::str_detect(B2 %>% dplyr::pull(CODE), paste0(diag_tbl %>% dplyr::pull(icd_version), collapse = "|")), 
      icd_version, icd
      )
    
  }
  
  B2 <- dplyr::bind_cols(B2, dplyr::as_tibble(list(icd = icd)))
  
  t = ncol(B)
  
  data_long <- .data %>%
    dplyr::select_if(sapply(., class) != "character") %>%
    dplyr::slice(rep(1:n(), each = t))
  
  data_long <- data_long %>%
    dplyr::bind_cols(B2)
  
  return(data_long)
  
}
