#' @title Pliki wojewodztw
#' @description
#' Funkcja łączy pliki z danymi placówek do jednego pliku csv.
#' @param  pliki wektor zawierający ścieżki do łączonych plików csv.
#' @param nazwaPliku ścieżka do pliku wynikowego
#' @return  funkcja nic nie zwraca
#' @export
#' @importFrom utils read.csv2 write.csv2
polacz_pliki <- function(pliki, nazwaPliku) {
  fileList = list()

  fileList = list()
  for (v in 1:(length(pliki))) {
    fileList[[v]] = read.csv2(pliki[v], row.names = NULL,
                              fileEncoding = "UTF-8", na = "")
  }
  ret = polacz_tabele(fileList)
  ret = zmien_tabele(tab = ret)

  write.csv2(ret, nazwaPliku, row.names = FALSE, na = '', fileEncoding = "UTF-8")
}
