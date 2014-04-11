#' @encoding utf-8
#' @title Szczegółowe informacje dla placówki
#' @description
#' Funkcja przetwarza węzeł dla pojedynczej placówki.
#' @param placowka węzeł html, zawierający informacje o placówce.
#' @return 
#' Funkcja zwraca tablicę ciągów znakowych z informacjami o placówce.
szczegolowe_dane_placowka<-function(placowka)
{
  kolumny = zmienne_globalne("kolumny") 
  nie_powtarzaj = zmienne_globalne("nie_powtarzaj") 
  
  nodeData = getNodeSet(placowka,"./div")
  name = gsub("\n","",xmlValue(getNodeSet(nodeData[[1]],"./a")[[1]]))   
  
  ret = name
  names(ret)[1] <- "Nazwa"
  for(ind in 2:length(nodeData)){
    wartoscTemp = wartosc(nInfo <- nodeData[[ind]])
    
    if(!(wartoscTemp[1] %in% kolumny) | (wartoscTemp[1] %in% names(ret) & wartoscTemp[1] %in% nie_powtarzaj)  ){
      next()
    }
    
    if( length(ret)!=0 & wartoscTemp[1] %in% names(ret)  ){
      if( wartoscTemp[2] %in% ret[grepl(wartoscTemp[1],names(ret))] ){
        next()
      }
    }
    
    if(wartoscTemp[1] != 'Adres do korespondencji'){
      
      ret = c(ret,wartoscTemp[2])
      
      nameTmp = wartoscTemp[1]
      top = 1
      while( nameTmp %in% names(ret) )
      {
        nameTmp = paste(wartoscTemp[1],top,sep="")
        top = top + 1
      }
      names(ret)[length(ret)] <- nameTmp
    }
  }
  
  ret = c(ret,adres_do_korespondencji(placowka))
  return(t(ret))
}