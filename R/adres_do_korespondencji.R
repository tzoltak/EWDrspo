#' @encoding utf-8
#' @title Adres korespondecyjny 
#' @description 
#' Funkcja parsuje węzeł html placówki w celu pobrania adresu korespondencji. 
#' @param  placowka węzeł html placówki
#' @return  zwraca wektor danych, który zawiera dane adresowe.
#' @export
adres_do_korespondencji <- function(placowka)
{
  trimSemicolon <- function (x) gsub("^[ \t\r\n:]+|[ \t\r\n:]+$", "", x)
  
  nodeData = getNodeSet(placowka,"./div[b='Adres do korespondencji:']")
  if( length(nodeData) == 0 ) {
    return(NULL)
  }
  
  header = sapply(getNodeSet(nodeData[[1]],"./table/tr/th"),xmlValue)
  text = sapply(getNodeSet(nodeData[[1]],"./table/tr/td"),xmlValue)
  
  ret = trimSemicolon(gsub("\r","",gsub("\n","",text)))
  names(ret) = paste(trimSemicolon(header),"_Koresp",sep="") 
  
  return(ret)
}