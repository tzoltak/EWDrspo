#' @encoding utf-8
#' @title Informaje dla placowki 
#' @description
#' Wynikiem działania funkcji są najważniejsze dane dostępne na stronie RSPO dla podanej w parametrze placówki 
#' oraz jej zespołu nadrzędnego.
#' @param placowka węzeł html, zawierający wszystkie informacje o placówce.
#' @return 
#' Funkcja zwraca listę. Pierwszy element listy to wektor z informacjami na temat placówki.
#' Kolejne elementy listy zawierają dane dotyczące zespołów nadrzędnych.
dane_placowka_zespol <- function(placowka){
  node = getNodeSet(placowka,"./div[@class='sipRowItem relative']/a[@class='moreInfo']")[[1]]
  detailSite = "http://sio.men.gov.pl/dodatki/rspo2portal/" 
  adresEnd = gsub("./","",xmlAttrs(node)["href"])
  siteAdr = paste(detailSite,adresEnd,sep="")
  
  oldWarn = options("warn")$warn
  options(warn=-1)
    site = getForm(siteAdr,.params=character(), encoding="UTF-8")
  options(warn=oldWarn)
  
  parsed = htmlParse(site, encoding="UTF-8")
  nodes = getNodeSet(parsed,"//html/body/div/div/div[contains(@class,'sipRowResultContainer ')]")
  
  retList = list(Jednostka=szczegolowe_dane_placowka(nodes[[1]]) )
  
  rspoNad = retList[[1]][colnames(retList[[1]])==zmienne_globalne("rspoNad")]
  
  if(length(nodes) >= 2){
    for(ind in 2:length(nodes))
    {
      if(wartosc(getNodeSet(nodes[[ind]],"./div[b='Typ:']")[[1]])[2]==zmienne_globalne("zespol")){
        retList[[2]] <- szczegolowe_dane_placowka(nodes[[ind]])
        names(retList)[2] <- "Zespol"
      }
    }
  }
  
  if(  length(retList)==1  ){    
    if(rspoNad!=""){
      daneRSPO <- pobierz_dane_jednego_rspo(rspoNad)
      if(!is.null(daneRSPO)){
        retList[[2]] <- daneRSPO
        names(retList)[2] <- "Nadrzedna"
      }else{
        retList[[1]] <-cbind(retList[[1]],zmienne_globalne("Brak"))
        colnames(retList[[1]])[ncol(retList[[1]]) ] <- "Uwagi"
      }
      
    }
  }else{
    rspoZesp = retList[[2]][colnames(retList[[2]])=="Numer RSPO"]
    
    if( rspoZesp != rspoNad ){
      daneRSPO <- pobierz_dane_jednego_rspo(rspoNad)
      if(!is.null(daneRSPO)){
        retList[[3]] <- daneRSPO
        names(retList)[3] <- "Nadrzedna"
      }else{
        retList[[1]] <- cbind(retList[[1]],zmienne_globalne("Brak"))
        colnames( retList[[1]] )[ ncol(retList[[1]]) ] <- "Uwagi"
      }
    }
  }
  
  return(retList)
}