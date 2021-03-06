#' @export

################################################################################

# Create a message for cohort data that displays on the top of sidebars

################################################################################


cohortMessage <- function(year, definition, term) {
  paste("Displaying data for the <strong>", year, "</strong> cohort (",
        definition, " students). ",
        "Select 'Yes' to compare, disaggregate or look at other variables
        ", sep = '')
}


################################################################################

# Function that disaggregates data for the select a cohort tab

################################################################################


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


################################################################################

# Creates tooltips for demographic plots in cohort tab (may be expanded)

################################################################################


makeDemoToolTip <- function(type = 'bar', tool = '%') {
  if (type == 'bar') {
    text <- "#!
    function(key, x, y, e) {
    return '<p> <strong>' + x + '</strong> </p>' +
    '<p>' +
    '<strong>' + y + '% </strong>' +
    '</p>' +
    '<p>' +
    e.point.headcount + ' out of ' + e.point.total + ' students' +
    '</p>'
    } !#"
  }

  if (type == 'baravg') {
    text <- "#!
    function(key, x, y, e) {
    return '<p> <strong>' + x + '</strong> </p>' +
    '<p>' +
    'Average: ' + '<strong>' + y + '</strong>' +
    '</p>'
    } !#"
  }

  if (type == 'pie') {
    text <- "#!
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

  if (type == 'demo') {
    text <- "#!
    function(key, x, y, e) {
    return '<p> <strong>' + key + '</strong> </p>' +
    '<p>' + x + ': <strong>' + y + '% </strong> </p>' +
    '<p>' +
    e.point.headcount + ' out of </br>' +
    e.point.total + ' students'
    '</p>'
    } !#"
  }

  if (type == 'demoavg') {
    text <- "#!
    function(key, x, y, e) {
    return '<p> <strong>' + key + '</strong> </p>' +
    '<p>' + x + ': <strong>' + y + '</strong> </p>'
    } !#"
  }

  if (type == 'equity' & tool == '%') {
    text <- "#!
    function(key, x, y, e) {
    return '<p>' + '<strong>' + key + '</strong>' + '</p>' +
    '<p>' +
    x + ': ' + '<strong>' + y  + '</strong>' +
    '</p>' +
    '<p>' +
    e.point.groupp +
    '% minus the average rate of ' + e.point.overalll + '%'
    '<br/>' +
    'in the cohort.'
    '</p>'
    } !#"
  }

  if (type == 'equity' & tool != '%') {
    text <- "#!
    function(key, x, y, e) {
    return '<p>' + '<strong>' + key + '</strong>' + '</p>' +
    '<p>' +
    x + ': ' + '<strong>' + y  + '</strong>' +
    '</p>' +
    '<p>' +
    e.point.groupp +
    ' minus the average of ' + e.point.overalll +
    '<br/>' +
    'for the full cohort.'
    '</p>'
    } !#"
  }

  text
}


################################################################################

# Creates a term name string

################################################################################


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


################################################################################

# Funciton that disaggregates data for the comparison plots

################################################################################


outcomeDisag <- function(outcome,
                         comparison,
                         cohort,
                         definition,
                         term,
                         filtCol,
                         filtOpt,
                         equity = 'No',
                         demo = 'None', data, type = '%') {
  temp <- data
  names(temp)[names(temp) == definition] <- 'filt'

  if(filtCol != 'None' & filtOpt != 'None') {
    preserve <- names(temp)[names(temp) == filtCol]
    names(temp)[names(temp) == filtCol] <- 'filt2'

    # check to see if the opts changed
    if(filtOpt %in% unique(temp$filt2)) {
      temp <- temp[temp$filt2 == filtOpt,]

    # if they didnt....
    } else {
      default <- unique(temp$filt2)[1]
      temp <- temp[temp$filt2 == default,]
    }

    names(temp)[names(temp) == 'filt2'] <- preserve
  }

  temp <- temp[!is.na(temp$filt),]

  names(temp)[names(temp) == outcome] <- 'out'


  if(outcome == 'None') {
    temp <- data.frame(outcome = 0, order = 0)
    return(temp)
    }

  temp <- temp[!is.na(temp$out),]
  if (comparison == 'years') {
    temp <- temp[temp$cohortyear == cohort,]

    # Create an ordered strm descr factor
    new <- unique(temp[,c('term', 'termdescr')])
    new <- new$termdescr[order(new$term)]
    temp$termdescr <- factor(temp$termdescr, levels = new, ordered = TRUE)


    names(temp)[names(temp) == 'termdescr'] <- 'order'
    temp <- temp[temp$term <= term,]
  }

  if (comparison == 'cohorts') {
    temp <- temp[temp$term == term,]
    comps <- unique(temp$cohortyear)[order(unique(temp$cohortyear))]
    cur <- match(cohort,comps)
    bot <- ifelse(cur - 4 > 0, cur - 4, 1)
    comps <- comps[bot:cur]
    temp <- temp[temp$cohortyear %in% comps,]
    names(temp)[names(temp) == 'cohortyear'] <- 'order'
  }

  if (demo == 'None') {
    final <- temp %>%
      group_by(order) %>%
      summarise(outcome = mean(out, na.rm = TRUE),
                headcount = sum(out, na.rm= TRUE),
                total = n()) %>%
      mutate(outcome = outcome * ifelse(type == '%', 100, 1))
  }

  if (demo != 'None') {
    names(temp)[names(temp) == demo] <- 'demo'
    final <- temp %>%
      group_by(order, demo) %>%
      summarise(outcome = mean(out, na.rm = TRUE),
                headcount = sum(out, na.rm = TRUE),
                total = n()) %>%
      mutate(outcome = outcome * ifelse(type == '%', 100, 1))
  }

  if (equity == 'Yes') {
    comp <- temp %>%
      group_by(order) %>%
      summarise(outcome2 = mean(out, na.rm = TRUE),
                headcount2 = sum(out, na.rm = TRUE),
                total2 = n()) %>%
      mutate(outcome2 = outcome2 * ifelse(type == '%', 100, 1))

    final <- final %>%
      left_join(comp) %>%
      mutate(groupp = round(outcome/100, 3) * 100,
             overalll = round(outcome2/100, 3) * 100,
             group = round(outcome/100, 1),
             overall = round(outcome2/100, 1),
             outcome = round(outcome - outcome2, 1),
             headcount = round(headcount/headcount2 * 100, 1),
             total = round(total/total2 * 100, 1)) %>%
      select(c('order', 'demo', 'outcome', 'headcount', 'total', 'group',
               'overall', 'groupp','overalll'))
  }

  final
}


################################################################################

#               Asterisc Order Variable from outcomeDisAg

################################################################################


activeData <- function(table, term, data, type = 'enroll') {

  cohorts <- data

  activeTerms <- unique(cohorts$termdescr[])

  if(!is.factor(table$order)) {table$order <- as.factor(table$order)}

  if (type == 'enroll') {
    activeTerms <- unique(cohorts$termdescr[cohorts$livestatusenroll == 'Live'])
    activeCohorts <- unique(cohorts$cohortyear[cohorts$livestatusenroll == 'Live'
                                               & cohorts$term == term])
    levels(table$order)[levels(table$order) %in% activeTerms |
                        levels(table$order) %in% activeCohorts] <-
      paste(levels(table$order)[levels(table$order) %in% activeTerms |
                                levels(table$order) %in% activeCohorts],
            '*', sep = '')
  }

  if (type == 'comp') {
    activeTerms <- unique(cohorts$termdescr[cohorts$livestatuscomp == 'Live'])
    activeCohorts <- unique(cohorts$cohortyear[cohorts$livestatuscomp == 'Live'
                                               & cohorts$term == term])

    levels(table$order)[levels(table$order) %in% activeTerms |
                        levels(table$order) %in% activeCohorts] <-
      paste(levels(table$order)[levels(table$order) %in% activeTerms |
                                levels(table$order) %in% activeCohorts],
            '*', sep = '')
  }

  table
}
