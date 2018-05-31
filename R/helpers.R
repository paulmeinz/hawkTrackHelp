#' @export

cohortMessage <- function(year, definition) {
  paste("Displaying data for the <strong>", year, "</strong> fall cohort (",
        definition, " students). ",
        "If you would like to disaggregate or conduct a comparison, select
        'Yes' in the options below.", sep = '')
}


cohortSelectData <- function(year, definition, demographic, data) {

  cohorts <- data

  # Pull selected cohort
  temp <- cohorts %>% filter(cohortyear == year & term == 1)

  # Specify filter column based on cohort selection
  names(temp)[names(temp) == definition] <- 'filt'

  plotSet <- temp %>%
    filter(!is.na(filt)) %>%
    group_by_(demographic) %>%
    summarise(headcount = n()) %>%
    mutate(percent = headcount/sum(headcount), headcount,
           total = sum(headcount))

  plotSet
}

makeDemoToolTip <- function(num = 'headcount', den = 'total') {
  paste(
    "#!
    function(key, x, y, e) {
    return '<p> <strong>' + x + '</strong> </p>' +
    '<p>' +
    <strong>' + y + '% </strong>' +
    '</p>' +
    '<p>' +
    e.point.headcount + 'out of' + e.point.den + 'students' +
    '</p>'
    } !#"
  )
}
