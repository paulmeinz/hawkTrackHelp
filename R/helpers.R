#' @export

cohortMessage <- function(year, definition) {
  paste("Displaying data for the <strong>", year, "</strong> fall cohort (",
        definition, " students) ",
        "If you would like to disaggregate or conduct a comparison, select
        'Yes' in the options below")
}
