% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pobierz_rspo.R
\name{pobierz_rspo}
\alias{pobierz_rspo}
\title{Pobierz dane z RSPO}
\usage{
pobierz_rspo(rownolegle = TRUE)
}
\arguments{
\item{rownolegle}{wartość logiczna - czy dane z poszczególnych województw
mają być zbierane równolegle (z użyciem pakietu \code{snow})?}
}
\value{
data frame z bazą RSPO
}
\description{
Funkcja pobiera z Rejestru Szkół i Placówek Oświatowych dane:
\itemize{
  \item{szkół podstawowych,}
  \item{gimnazjów,}
  \item{liceów ogólnokształcących,}
  \item{techników,}
  \item{oraz zespołów, do których należą.}
}
}

