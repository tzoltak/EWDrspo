#' @encoding utf-8
#' @title Numer RSPO dla placowkek nadrzędnych 
#' @description
#' Funkcja zwraca dla określonego numeru RSPO, numery RSPO placówek nadrzędnych i zespołów, do których należy.
#' @param rspoStr ciąg znakowy z numerem rspo
#' @return 
#' Funkcja zwraca wektor znakowy, który zawiera numery RSPO. 
#' @export
nadrzedne_rspo <- function(rspoStr){
  params <- list(param  ="Support_sipSearchResult",rspo = rspoStr)
  side1 = postForm('http://sio.men.gov.pl/dodatki/rspo2portal/index.php', .params = params, 
                   .encoding ="UTF-8",style = 'POST')
  parsed = htmlParse(side1, encoding="UTF-8")
  schools = getNodeSet(parsed,"//html/body/div/div[@class='sipContentContainer']/div[contains(@class,'sipRowResultContainer ') and @class!='sipRowResultContainer tloStron']")
  if(length(schools) == 0){
    return(NULL)
  }
  
  dane = wszystkie_nadrzedne(placowka=schools[[1]]) 
  ret = sapply(dane,function(x) x[colnames(x)=="Numer RSPO"])
  return(ret[-1])
}