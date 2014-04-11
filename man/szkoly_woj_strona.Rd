\encoding{utf-8}
\name{szkoly_woj_strona}
\alias{szkoly_woj_strona}
\title{Informacje o placowkach}
\usage{
szkoly_woj_strona(kodWoj, numerStrony)
}
\arguments{
  \item{kodWoj}{kod województwa (jako wynik funkcji
  kody_wojewodztw)}

  \item{numerStrony}{numer strony wyszukiwania, z której
  informacje chcemy pobrać}
}
\value{
funkcja zwraca listę węzłów html z informacjami o
placówkach
}
\description{
Funkcja zwraca informacje o placówkach, które znajdują
się na jednej stronie z wynikami wyszukiwania dla
określonego wojewodztwa. Przez placówkę rozumie się:
szkołę podstawową, gimnazjum, technikum i leceum
ogólnokształcące.
}

