#' @encoding utf-8
#' @title Informaje dla placowkek nadrzędnych 
#' @description
#' Funkcja zwraca dla określonego numeru rspo dane placówki nadrzędnej i zespołu, do którego (o ile) należy.
#' @param rspoStr ciąg znakowy z numerem rspo
#' @return 
#' Funkcja zwraca listę, której elementy zawierają wektory ciągów znakowych z danymi o placówce nadrzędnej i/lub zespołu.
#' @export
dane_nadrzedne_rspo <- function(rspoStr){
  params <- list(param  ="Support_sipSearchResult",rspo = rspoStr)
  side1 = postForm('http://sio.men.gov.pl/dodatki/rspo2portal/index.php', .params = params, 
                   .encoding ="UTF-8",style = 'POST')
  parsed = htmlParse(side1, encoding="UTF-8")
  schools = getNodeSet(parsed,"//html/body/div/div[@class='sipContentContainer']/div[contains(@class,'sipRowResultContainer ') and @class!='sipRowResultContainer tloStron']")
  dane = dane_placowka_zespol(placowka= schools[[1]],tylkoNadrzedne=TRUE) 
  return(dane)
}