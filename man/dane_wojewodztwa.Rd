\encoding{utf-8}
\name{dane_wojewodztwa}
\alias{dane_wojewodztwa}
\title{Dane województwa}
\usage{
dane_wojewodztwa(plik, kod_wojewodztwa)
}
\arguments{
  \item{plik}{ścieżka do pliku,}

  \item{kod_wojewodztwa}{ciąg znakowy używany przez
  strony RSPO do zakodowania województwa.}
}
\value{
Jeżeli parametr pliku jest określony to funkcja nic nie
zwraca. W przypadku, gdy parametr plik jest równy NULL to
funkcja zwraca połączoną tablicę.
}
\description{
Funkcja pobiera informacje o placówkach na terenie jednego
województwa. Zebrane dane zapisuje do pliku csv.
}

