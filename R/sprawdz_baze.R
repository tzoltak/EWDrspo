#' @title Sprawdź spójność pobranej bazy.
#' @description
#' Funkcja sprawdza spójność pobranej bazy pod kątem:
#' \itemize{
#' \item kompletności i unikalności id RSPO,
#' \item występowania w bazie jednostek nadrzędnych dla wszystkich jednostek podrzędnych, które są w bazie.
#' }
#' @param baza data frame zawierająca ściągniętą bazę RSPO
#' @return Funkcja nie zwraca nic jeśli nie wykryje błędów. W przeciwnym wypadku pluje komunikatami o błędach.
#' @export
sprawdz_baze = function(baza) {
  idRspo=baza[, grepl("^(numer|id).rspo$", tolower(names(baza)))]
  if (is.null(idRspo)) stop("Nie znaleziono kolumny z id RSPO.")
  if (!is.numeric(idRspo)) stop("Oczekiwano, że kolumna z id RSPO będzie typu numeric.")
  if (any(is.na(idRspo))) stop("\nBraki id_rspo w wierszach:\n", paste0("   ", rownames(baza)[is.na(idRspo)], "\n"))
  if (length(unique(idRspo)) != length(idRspo)) stop("\nZduplikowane id_rspo:\n", paste0("   ", idRspo[duplicated(idRspo)], "\n"))
  idRspoNadrz = sort(unique(na.omit(baza[, grepl("^(numer|id).rspo.(jednostki.nadrzędnej|jedn.nadrz)$", tolower(names(baza)))])))
  if (is.null(idRspoNadrz)) stop("Nie znaleziono kolumny z id RSPO jednostki nadrzędnej.")
  if (!is.numeric(idRspoNadrz)) stop("Oczekiwano, że kolumna z id RSPO jednostki nadrzędnej będzie typu numeric.")
  brakIdRspoNadrz = idRspoNadrz[!(idRspoNadrz%in%idRspo)]
  if (length(brakIdRspoNadrz) > 0) {
    liczbyBrakIdRspoNadrz = sapply(brakIdRspoNadrz, function(x, idRspoNadrz) return(sum(idRspoNadrz == x)), idRspoNadrz=idRspoNadrz)
    stop("\nBrak rekordów o jednostkach nadrzędnych o id RSPO równych:\n", paste0("   ", idRspoNadrz[ !(idRspoNadrz%in%idRspo)], " (", liczbyBrakIdRspoNadrz," odnosząca/e/ych się jednostka/i/ek)\n"))
  }
  invisible(NULL)
}