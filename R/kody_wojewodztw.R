#' @encoding utf-8
#' @title Kody wojewodztw.
#' @description
#' Funkcja zwraca kody województw, używane przez strony RSPO w kodzie html.
#' @return funkcja zwraca wektor ciągów znakowych
#' @export
kody_wojewodztw <- function()
{
  src="http://sio.men.gov.pl/dodatki/rspo2portal/index.php?param=Support_sipSearchForm"
  site <- getURL(src, ssl.verifypeer = FALSE, encoding="UTF-8")
  parsed = htmlParse(site, encoding="UTF-8")
  aaa = getNodeSet(parsed,"//html/body/div/div/form/div[@id='mainFormId']/div[@class='fieldsSip']/select[@name='gusWoj3']/option")
  aaa[[1]]<-NULL
  return(xmlSApply(aaa,xmlAttrs))
}