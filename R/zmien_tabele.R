#' @title Zmiana formatowania tabeli z danymi RSPO
#' @description
#' Funkcja dostosowuje format braków danych oraz nazwy kolumn tabeli ze
#' ściągniętymi danymi z RSPO.
#' @param tab tabela z danymi orginalnie pobranymi z RSPO.
#' @return Funkcja zwraca obiekt typu data.frame.
#' @export
zmien_tabele <- function(tab) {
  rozbij_adres <- function(adres, postfix="") {
    miejscowosc = sub("^(|.*[ ])[[:digit:]]{2}-[[:digit:]]{3}[ ](.*)$", "\\2", adres)
    ulica       = gsub(",[ ]+$","",sub("^(|.*[ ])([[:digit:]]{2}-[[:digit:]]{3})[ ].*$", "\\1", adres))
    pna         = sub("^(|.*[ ])([[:digit:]]{2}-[[:digit:]]{3})[ ].*$", "\\2", adres)

    ret = cbind(miejscowosc, ulica, pna)
    colnames(ret) = paste(colnames(ret), postfix, sep = "")
    return(ret)
  }

  ret = cbind(tab[, !colnames(tab) %in% c("Adres","Adres_prowadzacy","Adres_przekazujacy")],
              rozbij_adres(tab[, "Adres"]),
              rozbij_adres(tab[, "Adres_prowadzacy"], "_prowadzacy"),
              rozbij_adres(tab[, "Adres_przekazujacy"], "_przekazujacy"))

  cols = gsub("[ ]", "_", tolower(colnames(ret)))
  cols = gsub("[.]", "_", cols)
  cols = gsub("numer_regon", "regon", cols)
  cols = gsub("numer_rspo", "id_rspo", cols)
  cols = gsub("ę", "e", cols)
  cols = gsub("ą", "a", cols)
  cols = gsub("ó", "o", cols)
  cols = gsub("ś", "s", cols)
  cols = gsub("ć", "c", cols)
  cols = gsub("ń", "n", cols)
  cols = gsub("ż", "z", cols)
  cols = gsub("ź", "z", cols)
  cols = gsub("ł", "l", cols)
  colnames(ret) = cols

  ret[ret[, "nip"] == "nie posiada", "nip"] = NA
  ret[ret[, "regon_jednostki_nadrzednej"] == "", "regon_jednostki_nadrzednej"] = NA
  ret[ret[, "id_rspo_jednostki_nadrzednej"] == "", "id_rspo_jednostki_nadrzednej"] = NA

  retDF = data.frame(ret, stringsAsFactors = FALSE)
  retDF[, "nip"] = as.numeric(retDF[, "nip"])
  retDF[, "id_rspo"] = as.numeric(retDF[, "id_rspo"])
  retDF[, "regon_jednostki_nadrzednej"] = as.numeric(retDF[, "regon_jednostki_nadrzednej"])
  retDF[, "id_rspo_jednostki_nadrzednej"] = as.numeric(retDF[, "id_rspo_jednostki_nadrzednej"])

  # Regon jako numer powoduje ucinanie zer z przodu numeru, przykład: 236984.
  retDF[, "regon"] = as.numeric(as.character(retDF[, "regon"]))

  telInds = which(grepl("telefon", colnames(retDF)))

  for (ind in telInds) {
    retDF[,ind] = gsub("-", "", retDF[, ind])
    toChangeInds = which(retDF[, ind] == "")
    if (length(toChangeInds) == 0) {
      retDF[, ind] = as.numeric(as.character(retDF[, ind]))
      next()
    }
    retDF[toChangeInds, ind] = NA
    retDF[, ind] = as.numeric(as.character(retDF[, ind]))
  }

  retDF[ retDF == "" & !is.na(retDF) ] = NA

  colnames(retDF) [colnames(retDF) == "typ_organu_prowadzacego_prowadzacy"] = "typ_organu_prowadzacego"
  colnames(retDF) [colnames(retDF) == "nazwa_organu_prowadzacego_prowadzacy"] = "nazwa_organu_prowadzacego"
  colnames(retDF) [colnames(retDF) == "nazwa_organu_rejestrujacego_przekazujacy"] = "nazwa_organu_rejestrujacego"

  return(retDF)
}
