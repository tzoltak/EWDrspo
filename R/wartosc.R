#' @encoding utf-8
#' @title Pojedyncza porcja informacji o placowce
#' @description
#' Funkcja przetwarza węzeł z pojedynczą informacją (jak adres, numer telefonu, strona www,..).
#' @param nInfo węzeł html, zawierający informację o placówce.
#' @return Funkcja zwraca dwuelementowy wektor ciągów znakowych, które pierwszy element określa rodzaj informacji, a drugi zawiera informację.
wartosc <- function(nInfo)
{
  trimSemicolon <- function (x) gsub("^[ \t\r\n:]+|[ \t\r\n:]+$", "", x)
  valueName = xmlValue(getNodeSet(nInfo,"./b")[[1]])
  value = trimSemicolon(gsub(valueName,"",xmlValue(nInfo)))
  return(c( trimSemicolon(valueName) , trimSemicolon(gsub("valueName","",value))))
}