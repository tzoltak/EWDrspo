\encoding{utf-8}
\name{dane_placowka_zespol}
\alias{dane_placowka_zespol}
\title{Informaje dla placowki}
\usage{
dane_placowka_zespol(placowka, tylkoNadrzedne = FALSE)
}
\arguments{
  \item{placowka}{węzeł html, zawierający wszystkie
  informacje o placówce.}

  \item{tylkoNadrzedne}{parametr logiczny określający,
  czy pobierać tylko dane jednostek nadrzędnych}
}
\value{
Funkcja zwraca listę. Pierwszy element listy to wektor z
informacjami na temat placówki. Kolejne elementy listy
zawierają dane dotyczące zespołów nadrzędnych.
}
\description{
Wynikiem działania funkcji są najważniejsze dane
dostępne na stronie RSPO dla podanej w parametrze
placówki oraz jej zespołu nadrzędnego.
}

