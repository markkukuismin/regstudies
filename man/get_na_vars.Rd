% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/na_operations.R
\name{get_na_vars}
\alias{get_na_vars}
\title{Function for extracting names of variables which have any NA values}
\usage{
get_na_vars(.data)
}
\arguments{
\item{.data}{data to be studied}
}
\value{
returns a two-column tibble holding variable name as first column ('variable') and 'contains_na' as second column.
}
\description{
Function for extracting names of variables which have any NA values
}
\examples{
\dontrun{
 d<-left_join(sample_cohort,sample_regdata)
 get_na_vars(d)
}

}
\seealso{
Other help functions: 
\code{\link{get_classification_name}()},
\code{\link{get_var_types}()},
\code{\link{left_join_replace_na}()},
\code{\link{make_regex}()},
\code{\link{replace_na_by_type}()}
}
\concept{help functions}
