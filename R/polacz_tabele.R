#' @title Pliki wojewodztw
#' @description
#' Funkcja łączy pliki z danymi placówek w jeden data frame.
#' @param  wojList lista z tablicami danych z poszczególnych placówek
#' @return
#' Funkcja zwraca data frame.
#' @export
polacz_tabele <- function(wojList) {
  rowNum = 0
  retList = list()
  for (v in 1:(length(wojList))) {
    tab = wojList[[v]]

    for (ind in 1:ncol(tab)) {
      kolumna = tab[, ind]
      cname = colnames(tab)[ind]

      print(cname)

      if (cname %in% names(retList)) {
        retList[[cname]] = c(retList[[cname]], as.character(kolumna))
      } else {
        retList[[length(retList) + 1]] = c(rep(NA, rowNum), as.character(kolumna))
        names(retList)[length(retList)] = cname
      }
    }

    rowNum = rowNum + nrow(tab)

    for (num in 1:length(retList))     {
      if (length(retList[[num]]) == rowNum - nrow(tab) ) {
        retList[[num]] = c(retList[[num]], rep(NA, nrow(tab)))
      }
    }
  }

  ret = NULL
  for (ind in 1:length(retList))   {
    ret = cbind(ret, as.character(retList[[ind]]))
  }
  colnames(ret) = names(retList)

  ret = ret[!duplicated(ret[, 3]), ]
  return(ret)
}
