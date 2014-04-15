#' @encoding utf-8
#' @title Informaje dla placowki 
#' @description
#' Wynikiem działania funkcji są najważniejsze dane dostępne na stronie RSPO dla podanej w parametrze placówki 
#' oraz jej zespołu nadrzędnego.
#' @param placowka węzeł html, zawierający wszystkie informacje o placówce.
#' @param tylkoNadrzedne parametr logiczny określający, czy pobierać tylko dane jednostek nadrzędnych 
#' @return 
#' Funkcja zwraca listę. Pierwszy element listy to wektor z informacjami na temat placówki.
#' Kolejne elementy listy zawierają dane dotyczące zespołów nadrzędnych.
#' @export
dane_placowka_zespol <- function(placowka,tylkoNadrzedne=FALSE){
  
  pierwszaStrona = szczegolowe_dane_placowka(placowka)
  rspoPierwsza = pierwszaStrona[colnames(pierwszaStrona) == "Numer RSPO" ]
  
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
  
  retList = list()
  rspoNad=""
  dane_jednostki <- szczegolowe_dane_placowka(nodes[[1]])
  if(rspoPierwsza != dane_jednostki[colnames(dane_jednostki)=="Numer RSPO"]){
    
    if(tylkoNadrzedne==FALSE){
      ret <- pierwszaStrona
      ret <- cbind(ret,Uwagi=zmienne_globalne("Brak detalow"))
      retList[[1]] <- ret
      names(retList)[1] <- "Jednostka"
      
      return(retList)
    }
    return(retList)
  }
  
  if(tylkoNadrzedne==FALSE){
    retList[[length(retList)+1]] <- dane_jednostki
    names(retList)[length(retList)] <- "Jednostka"
  }  
  
  rspoNad = dane_jednostki[colnames(dane_jednostki) == zmienne_globalne("rspoNad")]
  
  if(length(nodes) >= 2){
    for(ind in 2:length(nodes))
    {
      if(wartosc(getNodeSet(nodes[[ind]],"./div[b='Typ:']")[[1]])[2]==zmienne_globalne("zespol")){
        retList[[length(retList)+1]] <- szczegolowe_dane_placowka(nodes[[ind]])
        names(retList)[length(retList)] <- "Zespol"
      }
    }
  }
  
  if(  (length(retList)==1 & tylkoNadrzedne==FALSE) |(length(retList)==0 & tylkoNadrzedne==TRUE)   ){    
    if(rspoNad!=""){
      daneRSPO <- pobierz_dane_jednego_rspo(rspoNad)
      if(!is.null(daneRSPO)){
        retList[[length(retList) + 1]] <- daneRSPO
        names(retList)[length(retList)] <- "Nadrzedna"
      }else{
        retList[[1]] <- cbind(retList[[1]],zmienne_globalne("Brak"))
        colnames(retList[[1]])[ncol(retList[[1]]) ] <- "Uwagi"
      }
      
    }
  }else {
    rspoZesp = retList[[length(retList)]][colnames(retList[[length(retList)]])=="Numer RSPO"]
    
    if( rspoZesp != rspoNad ){
      daneRSPO <- pobierz_dane_jednego_rspo(rspoNad)
      if(!is.null(daneRSPO)){
        retList[[length(retList) + 1]] <- daneRSPO
        names(retList)[length(retList)] <- "Nadrzedna"
      }else{
        retList[[1]] <- cbind(retList[[1]],zmienne_globalne("Brak"))
        colnames( retList[[1]] )[ ncol(retList[[1]]) ] <- "Uwagi"
      }
    }
  }
  
  return(retList)
}