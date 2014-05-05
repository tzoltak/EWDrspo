#' @title Dane wojewodztwa
#' @description
#' Funkcja pobiera informacje o placówkach na terenie jednego województwa. 
#' Zebrane dane zapisuje do pliku csv.  
#' @param plik ścieżka do pliku, 
#' @param kod_wojewodztwa ciąg znakowy używany przez strony RSPO do zakodowania województwa.
#' @return 
#' Jeżeli parametr pliku jest określony to funkcja nic nie zwraca.
#' W przypadku, gdy parametr plik jest równy NULL to funkcja zwraca połączoną tablicę.
#' @export
dane_wojewodztwa <- function(plik, kod_wojewodztwa) {
  RSPOzespol = NULL
  
  retList = list()
  rowNum = 0;
  
  print(kod_wojewodztwa);
  pageNum = 0;
  
  while(TRUE)   {
    schools = szkoly_woj_strona(kod_wojewodztwa, pageNum)
    
    if( length(schools) == 0 ) break;
    
    print(c(kod_wojewodztwa, pageNum));
    pageNum = pageNum + 1;
    
    for(ind in 1:(length(schools))) {
      zespolVals = wszystkie_nadrzedne (placowka=schools[[ind]])
      
      for(k in 1:length(zespolVals)){
        values = zespolVals[[k]]
        
        print(names(zespolVals)[k])
        print(values[colnames(values)=="Numer RSPO"])
        
        rspo = values[colnames(values)=="Numer RSPO"]
        if(rspo %in% RSPOzespol) {
          next()
        }
        RSPOzespol = c(RSPOzespol, rspo)
        
        for(k in 1:length(values))
        {
          val = values[k]
          cname = colnames(values)[k]
          
          if(cname %in% names(retList)) {
            retList[[cname]] = c(retList[[cname]], val)
          } else {
            retList[[length(retList)+1]] = c(rep("", rowNum), val)
            names(retList)[length(retList)] = cname
          }
        }
        rowNum = rowNum + 1
        
        for(num in 1:length(retList)) {
          if( length(retList[[num]]) == (rowNum - 1) ) {
            retList[[num]] = c(retList[[num]], "")
          }
        }
      }
    }
  }
  
  ret = NULL
  for(ind in 1: length(retList)) {
    ret = cbind(ret , retList[[ind]])
  }
  colnames(ret) <- names(retList)
  
  if (!is.null(plik)) {
    write.csv2(ret, plik, row.names=FALSE, na='', fileEncoding ="UTF-8")
  } else {
    return(ret)
  }
}
