#' @encoding utf-8
#' @title Pliki województw
#' @description 
#' Funkcja łączy pliki z danymi placówek do jednego pliku csv. 
#' @param  pliki wektor zawierający ścieżki do łączonych plików csv.
#' @param nazwaPliku ścieżka do pliku wynikowego
#' @return  funkcja nic nie zwraca
#' @export
polacz_pliki <- function(pliki,nazwaPliku)
{
  retList = list()
  rowNum=0;
  
  for(v in 1:(length(pliki)))
  {
    tab = read.csv2(pliki[v],row.names=NULL,fileEncoding ="UTF-8",na="" )
    
    for(ind in 1:ncol(tab))
    {
      kolumna = tab[,ind]
      cname = colnames(tab)[ind]
      
      print(pliki[v])
      print(cname)
      
      if(cname %in% names(retList))
      {
        retList[[cname]] = c(retList[[cname]],as.character(kolumna))
      }else{
        retList[[length(retList)+1]] = c(rep(NA,rowNum),as.character(kolumna))
        names(retList)[length(retList)] = cname
      }
    }
    
    rowNum = rowNum + nrow(tab)
    
    for(num in 1:length(retList))
    {
      if(length(retList[[num]]) == rowNum - nrow(tab) )
      {
        retList[[num]] = c(retList[[num]],rep(NA,nrow(tab)))
      }
    }
  }
  
  ret = NULL
  for(ind in 1: length(retList))
  {
    ret = cbind(ret , as.character(retList[[ind]]))
  }
  colnames(ret) <- names(retList)
  
  ret = ret[!duplicated(ret[,3]),]
  
  write.csv2(ret, nazwaPliku, row.names=F, na='',fileEncoding ="UTF-8" )
}