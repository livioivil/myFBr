% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getWall_summary.R
\name{getWall_summary}
\alias{getWall_summary}
\title{conoscere il numero delle varie attivit? sul wall}
\usage{
getWall_summary(wall)
}
\arguments{
\item{wall}{result of a call of \code{getWall}.}

\item{percorso}{stringa che indica il percorso della cartella dei dati del profilo}

\item{dataI}{data di Inizio (-Inf by default)}

\item{dataF}{data di Fine (+Inf by default)}
}
\value{
getWall un dataset a con tre colonne: time, user, text
getWall_summary: dataset (1x7) contenente le informazioni delle attivit? sul Wall dopo la data di riferimento
}
\description{
Funzione che dato il percorso dei dati del profilo facebook ritorna il numero delle differenti attivit? che compaiono sul wall
tra cui amicizie, stati, like, piace, condivisioni, link e giocato dopo una certa data.
Funzione NON valida per profili scritti in lingua inglese o spagnola, facile da aggiornare
}
\author{
Davide Meneghetti, Livio Finos
}
