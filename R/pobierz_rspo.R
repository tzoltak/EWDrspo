#' @title Pobierz dane z RSPO
#' @description
#' Funkcja pobiera z Rejestru Szkół i Placówek Oświatowych dane:
#' \itemize{
#' \item szkół podstawowych,
#' \item gimnazjów,
#' \item liceów ogólnokształcących,
#' \item techników,
#' \item oraz zespołów, do których należą.
#' }
#' @param rownolegle wartość logiczna - czy dane z poszczególnych województw mają być zbierane równolegle (z użyciem pakietu 'snow')?
#' @return data frame z bazą RSPO
#' @export
pobierz_rspo = function(rownolegle=TRUE) {
  # weryfikacja parametrów
  stopifnot(rownolegle %in% c(TRUE, FALSE))
  # sprawdzanie, czy mamy dostęp do wielowątkowości
  if (rownolegle) {
    rownolegle = "snow"%in%.packages()
    if (!rownolegle) warning(strwrap("Pakiet 'snow' jest niedostępny. Dane dotyczące poszczególnych województw będą pobierane sekwencyjnie, a nie równolegle.", prefix=" "), immediate.=TRUE)
  }
  # odpalamy ściąganie
  kody = kody_wojewodztw()
  if (rownolegle) {  # równolegle
    cl = makeCluster(8, type="SOCK")
    wojewodztwa = parLapply(cl, as.list(kody),
                            function(kod_woj) {
                              require(EWDrspo)
                              return(dane_wojewodztwa(NULL, kod_woj))
                            }
    )
    stopCluster(cl)
  } else {  # lub sekwencyjnie
    wojewodztwa = lapply(as.list(kody), function(kod_woj) return(dane_wojewodztwa(NULL, kod_woj)))
  }
  # składanie razem
  if (length(wojewodztwa) > 1) rspo = rbind(wojewodztwa[[1]], wojewodztwa[[2]])
  if (length(wojewodztwa) > 2) {
    for (i in 3:length(wojewodztwa)) rspo = rbind(rspo, wojewodztwa[[i]])
  }
  # zwracanie wyniku
  return(rspo)
}

