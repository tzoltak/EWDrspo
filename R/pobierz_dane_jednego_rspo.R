#' @encoding utf-8
#' @title Dane dla numeru rspo 
#' @description 
#' Funkcja pobiera dane dla jednego numeru RSPO. 
#' @param  rspoStr ciąg znaków zawierający numer RSPO porządanej placówki.
#' @return  Funkcja zwraca wektor z informacjami o placówce.
#' @export
pobierz_dane_jednego_rspo<-function(rspoStr){
  params <- list(param  ="Support_sipSearchResult",rspo = rspoStr)
  side1 = postForm('http://sio.men.gov.pl/dodatki/rspo2portal/index.php', .params = params, 
                   .encoding ="UTF-8",style = 'POST')
  parsed = htmlParse(side1, encoding="UTF-8")
  schools = getNodeSet(parsed,"//html/body/div/div[@class='sipContentContainer']/div[contains(@class,'sipRowResultContainer ') and @class!='sipRowResultContainer tloStron']")
  
  if(length(schools)==0){
     return(NULL)
  }
  
  placowka <- schools[[1]]
  node = getNodeSet(placowka,"./div[@class='sipRowItem relative']/a[@class='moreInfo']")[[1]]
  detailSite = "http://sio.men.gov.pl/dodatki/rspo2portal/" 
  adresEnd = gsub("./","",xmlAttrs(node)["href"])
  siteAdr = paste(detailSite,adresEnd,sep="")
  
  site = getForm(siteAdr,.params=character(), encoding="UTF-8")
  parsed = htmlParse(site, encoding="UTF-8")
  nodes = getNodeSet(parsed,"//html/body/div/div/div[contains(@class,'sipRowResultContainer ')]")
  
  szcz <- szczegolowe_dane_placowka(placowka=nodes[[1]])
  rspoSzcz <- szcz[colnames(szcz) == "Numer RSPO"]
  if(rspoStr == rspoSzcz){
    ret = szcz 
  }else{
    ret <- szczegolowe_dane_placowka(placowka)
    ret <- cbind(ret,Uwagi=zmienne_globalne("Brak detalow"))
  }
  
  return(ret)
}