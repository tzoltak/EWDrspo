% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sprawdz_baze.R
\name{sprawdz_baze}
\alias{sprawdz_baze}
\title{Sprawdz spojnosc pobranej bazy}
\usage{
sprawdz_baze(baza)
}
\arguments{
\item{baza}{data frame zawierająca ściągniętą bazę RSPO}
}
\value{
Funkcja nie zwraca nic jeśli nie wykryje błędów. W przeciwnym wypadku
pluje komunikatami o błędach.
}
\description{
Funkcja sprawdza spójność pobranej bazy pod kątem:
\itemize{
  \item{kompletności i unikalności id RSPO,}
  \item{występowania w bazie jednostek nadrzędnych dla wszystkich jednostek
        podrzędnych, które są w bazie.}
}
}

