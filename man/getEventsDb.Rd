% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getData.R
\name{getEventsDb}
\alias{getEventsDb}
\title{getEventsDb}
\usage{
getEventsDb(con, groups, countries, date, metrics)
}
\arguments{
\item{con}{(DBI) database connection}

\item{groups}{(character)}

\item{countries}{(character)}

\item{date}{(character)}

\item{metrics}{(character)}
}
\value{
dataframe
}
\description{
get the events data from the database
}
