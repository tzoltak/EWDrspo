#' @encoding utf-8
#' @title Informaje dla placowki 
#' @description
#' Wynikiem działania funkcji są najważniejsze dane dostępne na stronie RSPO dla podanej w parametrze placówki 
#' oraz zespołów i jednostek nadrzędnych.
#' @param placowka węzeł html, zawierający wszystkie informacje o placówce.
#' @return 
#' Funkcja zwraca listę. Pierwszy element listy to wektor z informacjami na temat placówki.
#' Kolejne elementy listy zawierają dane dotyczące zespołów i jednostek nadrzędnych.
#' @export
wszystkie_nadrzedne <- function(placowka){
  ret = dane_placowka_zespol(placowka) 
  
  if(length(ret) <= 1){
    return(ret)
  }
  
  rspoNadrzedneVec = sapply(ret,function(x) x[colnames(x)=="Numer RSPO"])[-1]
  
  ind=1
  while( ind <= length(rspoNadrzedneVec) ){
    print(ind)
    daneNad <- dane_nadrzedne_rspo(rspoStr <- rspoNadrzedneVec[ind])
    
    if(length(daneNad)==0){
      ind=ind+1
      next()
    }
    
    for(indDaneNad in 1:length(daneNad)){
      print(indDaneNad)
      ret[[length(ret) + 1 ]] <- daneNad[[indDaneNad]]
      names(ret)[length(ret)] <- paste("Nadrzedna",ind,indDaneNad)
    }
    
    rspoNadrzedneVec = c(rspoNadrzedneVec,sapply(daneNad,function(x) x[colnames(x)=="Numer RSPO"]))
    ind=ind+1
  }
  return(ret)
}