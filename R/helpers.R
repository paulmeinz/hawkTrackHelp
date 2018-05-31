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

  filtTest <- temp %>% filter(!is.na(filt))
  num <- nrow(filtTest)

  if (num >= 1) {
    plotSet <- temp %>%
      filter(!is.na(filt)) %>%
      group_by_(demographic) %>%
      summarise(headcount = n()) %>%
      mutate(percent = headcount/sum(headcount) * 100, headcount,
             total = sum(headcount)) %>%
      complete_(demographic)
  } else {
    plotSet <- temp %>%
      mutate(none = as.numeric(is.na(emplid))) %>%
      group_by_(demographic) %>%
      summarise(headcount = sum(none)) %>%
      mutate(percent = headcount/sum(headcount) * 100, headcount,
             total = sum(headcount)) %>%
      complete_(demographic)
  }



  plotSet[is.na(plotSet)] <- 0

  plotSet
}

makeDemoToolTip <- function(type = 'bar') {
  if (type == 'bar') {
    paste(
      "#!
      function(key, x, y, e) {
      return '<p> <strong>' + x + '</strong> </p>' +
      '<p>' +
      '<strong>' + y + '% </strong>' +
      '</p>' +
      '<p>' +
      e.point.headcount + ' out of ' + e.point.total + ' students' +
      '</p>'
      } !#"
    )
  } else {
      "#!
      function(key, y, e, graph) {
      return '<p> <strong>' + key + '</strong> </p>' +
      '<p>' +
      '<strong>' + y + '% </strong>' +
      '</p>' +
      '<p>' +
      e.point.headcount + ' out of ' + e.point.total + ' students' +
      '</p>'
    } !#"
  }
}
