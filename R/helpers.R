#' @export


# Create a message for cohort data that displays on the top of sidebars
cohortMessage <- function(year, definition) {
  paste("Displaying data for the <strong>", year, "</strong> fall cohort (",
        definition, " students). ",
        "If you would like to disaggregate or conduct a comparison, select
        'Yes' in the options below.", sep = '')
}

# Function that disaggregates data for the select a cohort tab
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


# Creates tooltips for demographic plots in cohort tab (may be expanded)
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


createTermString <- function(term) {
  term <- as.character(term)

  lookup <- c('1' = '1st',
              '2' = '2nd',
              '3' = '3rd',
              '4' = '4th',
              '5' = '5th',
              '6' = '6th')

  term <- lookup[term]
}
