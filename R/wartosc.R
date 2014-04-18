#' @encoding utf-8
#' @title Pojedyncza porcja informacji o placowce
#' @description
#' Funkcja przetwarza węzeł z pojedynczą informacją (jak adres, numer telefonu, strona www,..).
#' @param nInfo węzeł html, zawierający informację o placówce.
#' @return Funkcja zwraca dwuelementowy wektor ciągów znakowych, które pierwszy element określa rodzaj informacji, a drugi zawiera informację.
#' @export
wartosc <- function(nInfo)
{
  trimAndClean <- function (x){
    # gsub("^[ \t\r\n:]+|\r|\n|[ \t\r\n:]+$", "", x)
    x = gsub("^[ \t\r\n:]+|[ \t\r\n:]+$", "", x)
    x = gsub("\n","",x)
    x = gsub("\r","",x)
    return(x)
  } 
  
  if(length(getNodeSet(nInfo,"./table"))!=0){
    valueName = xmlValue(getNodeSet(nInfo,"./table/tr/th")[[1]])
    value = toString( sapply(getNodeSet(nInfo,"./table/tr/td"),xmlValue)    ) 
    return(c( trimAndClean(valueName) , trimAndClean(gsub("valueName","",value))))
  }else if( length(getNodeSet(nInfo,"./b")) != 0){
    valueName = xmlValue(getNodeSet(nInfo,"./b")[[1]])
    value = trimAndClean(gsub(valueName,"",xmlValue(nInfo)))
    return(c( trimAndClean(valueName) , trimAndClean(gsub("valueName","",value))))
  }
  return(NULL)
}


