#' @title Informacje o placowkach
#' @description
#' Funkcja zwraca informacje o placówkach, które znajdują się na jednej stronie
#' z wynikami wyszukiwania dla określonego wojewodztwa.
#' Przez placówkę rozumie się: szkoły podstawowe, gimnazja, technika, licea
#' ogólnokształcące i profilowane oraz szkoły artystyczne: licea plastyczne,
#' ogólnokształcące szkoły muzyczne I i II stopnia, ogólnokształcące szkoły
#' sztuk pięknych, ogólnokształcące szkoły baletowe i Poznańską Szkołę Chóralną.
#' @param kodWoj kod województwa (jako wynik funkcji kody_wojewodztw)
#' @param numerStrony numer strony wyszukiwania, z której informacje chcemy
#' pobrać
#' @return funkcja zwraca listę węzłów html z informacjami o placówkach
#' @export
#' @importFrom XML getNodeSet htmlParse
#' @importFrom RCurl postForm
szkoly_woj_strona <- function(kodWoj, numerStrony) {
  pre = "a%3A17%3A%7Bs%3A5%3A%22param%22%3Bs%3A23%3A%22Support_sipSearchResult%22%3Bs%3A5%3A%22regon%22%3Bs%3A0%3A%22%22%3Bs%3A4%3A%22rspo%22%3Bs%3A0%3A%22%22%3Bs%3A9%3A%22dotOrgPro%22%3Bs%3A1%3A%220%22%3Bs%3A7%3A%22gusWoj3%22%3Bs%3A1%3A%220%22%3Bs%3A7%3A%22gusPow3%22%3Bs%3A1%3A%220%22%3Bs%3A10%3A%22nazwOrgPro%22%3Bs%3A1%3A%220%22%3Bs%3A6%3A%22gusWoj%22%3Bs%3A2%3A%22"
  post = "%22%3Bs%3A6%3A%22gusPow%22%3Bs%3A1%3A%220%22%3Bs%3A6%3A%22gusGmi%22%3Bs%3A1%3A%220%22%3Bs%3A9%3A%22sioMiejsc%22%3Bs%3A1%3A%220%22%3Bs%3A7%3A%22typJedn%22%3Ba%3A11%3A%7Bi%3A0%3Bs%3A1%3A%224%22%3Bi%3A1%3Bs%3A2%3A%2214%22%3Bi%3A2%3Bs%3A2%3A%2227%22%3Bi%3A3%3Bs%3A2%3A%2215%22%3Bi%3A4%3Bs%3A2%3A%2229%22%3Bi%3A5%3Bs%3A2%3A%2221%22%3Bi%3A6%3Bs%3A2%3A%2224%22%3Bi%3A7%3Bs%3A2%3A%2226%22%3Bi%3A8%3Bs%3A2%3A%2282%22%3Bi%3A9%3Bs%3A1%3A%223%22%3Bi%3A10%3Bs%3A2%3A%2216%22%3B%7Ds%3A13%3A%22czyMaInternat%22%3Bs%3A1%3A%220%22%3Bs%3A11%3A%22rodzKsztalc%22%3Bs%3A1%3A%220%22%3Bs%3A19%3A%22zawodARodzajKsztalc%22%3Bs%3A1%3A%220%22%3Bs%3A11%3A%22specjalnosc%22%3Bs%3A1%3A%220%22%3Bs%3A13%3A%22specjalizacja%22%3Bs%3A1%3A%220%22%3B%7D"

  # s1 =  "a%3A17%3A%7Bs%3A5%3A%22param%22%3Bs%3A23%3A%22Support_sipSearchResult%22%3Bs%3A5%3A%22regon%22%3Bs%3A0%3A%22%22%3Bs%3A4%3A%22rspo%22%3Bs%3A0%3A%22%22%3Bs%3A9%3A%22dotOrgPro%22%3Bs%3A1%3A%220%22%3Bs%3A7%3A%22gusWoj3%22%3Bs%3A1%3A%220%22%3Bs%3A7%3A%22gusPow3%22%3Bs%3A1%3A%220%22%3Bs%3A10%3A%22nazwOrgPro%22%3Bs%3A1%3A%220%22%3Bs%3A6%3A%22gusWoj%22%3Bs%3A2%3A%2202%22%3Bs%3A6%3A%22gusPow%22%3Bs%3A1%3A%220%22%3Bs%3A6%3A%22gusGmi%22%3Bs%3A1%3A%220%22%3Bs%3A9%3A%22sioMiejsc%22%3Bs%3A1%3A%220%22%3Bs%3A7%3A%22typJedn%22%3Ba%3A11%3A%7Bi%3A0%3Bs%3A1%3A%224%22%3Bi%3A1%3Bs%3A2%3A%2214%22%3Bi%3A2%3Bs%3A2%3A%2227%22%3Bi%3A3%3Bs%3A2%3A%2215%22%3Bi%3A4%3Bs%3A2%3A%2229%22%3Bi%3A5%3Bs%3A2%3A%2221%22%3Bi%3A6%3Bs%3A2%3A%2224%22%3Bi%3A7%3Bs%3A2%3A%2226%22%3Bi%3A8%3Bs%3A2%3A%2282%22%3Bi%3A9%3Bs%3A1%3A%223%22%3Bi%3A10%3Bs%3A2%3A%2216%22%3B%7Ds%3A13%3A%22czyMaInternat%22%3Bs%3A1%3A%220%22%3Bs%3A11%3A%22rodzKsztalc%22%3Bs%3A1%3A%220%22%3Bs%3A19%3A%22zawodARodzajKsztalc%22%3Bs%3A1%3A%220%22%3Bs%3A11%3A%22specjalnosc%22%3Bs%3A1%3A%220%22%3Bs%3A13%3A%22specjalizacja%22%3Bs%3A1%3A%220%22%3B%7D"
  # s2 =  "a%3A17%3A%7Bs%3A5%3A%22param%22%3Bs%3A23%3A%22Support_sipSearchResult%22%3Bs%3A5%3A%22regon%22%3Bs%3A0%3A%22%22%3Bs%3A4%3A%22rspo%22%3Bs%3A0%3A%22%22%3Bs%3A9%3A%22dotOrgPro%22%3Bs%3A1%3A%220%22%3Bs%3A7%3A%22gusWoj3%22%3Bs%3A1%3A%220%22%3Bs%3A7%3A%22gusPow3%22%3Bs%3A1%3A%220%22%3Bs%3A10%3A%22nazwOrgPro%22%3Bs%3A1%3A%220%22%3Bs%3A6%3A%22gusWoj%22%3Bs%3A2%3A%2204%22%3Bs%3A6%3A%22gusPow%22%3Bs%3A1%3A%220%22%3Bs%3A6%3A%22gusGmi%22%3Bs%3A1%3A%220%22%3Bs%3A9%3A%22sioMiejsc%22%3Bs%3A1%3A%220%22%3Bs%3A7%3A%22typJedn%22%3Ba%3A11%3A%7Bi%3A0%3Bs%3A1%3A%224%22%3Bi%3A1%3Bs%3A2%3A%2214%22%3Bi%3A2%3Bs%3A2%3A%2227%22%3Bi%3A3%3Bs%3A2%3A%2215%22%3Bi%3A4%3Bs%3A2%3A%2229%22%3Bi%3A5%3Bs%3A2%3A%2221%22%3Bi%3A6%3Bs%3A2%3A%2224%22%3Bi%3A7%3Bs%3A2%3A%2226%22%3Bi%3A8%3Bs%3A2%3A%2282%22%3Bi%3A9%3Bs%3A1%3A%223%22%3Bi%3A10%3Bs%3A2%3A%2216%22%3B%7Ds%3A13%3A%22czyMaInternat%22%3Bs%3A1%3A%220%22%3Bs%3A11%3A%22rodzKsztalc%22%3Bs%3A1%3A%220%22%3Bs%3A19%3A%22zawodARodzajKsztalc%22%3Bs%3A1%3A%220%22%3Bs%3A11%3A%22specjalnosc%22%3Bs%3A1%3A%220%22%3Bs%3A13%3A%22specjalizacja%22%3Bs%3A1%3A%220%22%3B%7D"
  # grepl(pre,s1)
  # grepl(pre,s2)
  # grepl(post,s1)
  # grepl(post,s2)

  paramStr = paste(pre, kodWoj, post, sep="")

  params <-
    list(page = as.character(numerStrony),
         param = "Support_sipSearchResultPage",
         searchParams = paramStr)

  side1 = postForm("http://sio.men.gov.pl/dodatki/rspo2portal/index.php",
                   .params=params,
                   .encoding="UTF-8", style='POST')
  parsed = htmlParse(side1, encoding="UTF-8")
  schools = getNodeSet(parsed, "//html/body/div/div[@class='sipContentContainer']/div[contains(@class,'sipRowResultContainer ') and @class!='sipRowResultContainer tloStron']")
  return(schools)
}
