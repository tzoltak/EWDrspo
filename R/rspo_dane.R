#' @encoding utf-8
#' @title Pobieranie danych z RSPO
#' @description
#' Funkcja pobiera dane szkół podstawowych, gimnazjów, liceów ogólnokształcących oraz techników.
#' Pobrane dane zapisuje do pliku csv.
#' @param plik Ścieżka do pliku csv.
#' @return 
#' Funkcja nic nie zwraca.
#' @examples
#' #rspo_dane("rspo.csv")
#' @export
rspo_dane<-function(plik)
{
  wojewodztwa = kody_wojewodztw()
  
  retList = list()
  rowNum=0;
  
  for(v in 1:length(wojewodztwa))
  {
    print(wojewodztwa[v]);
    pageNum=0;
    
    while(TRUE)
    {
      schools = szkoly_woj_strona(wojewodztwa[v],pageNum)
      
      if(length(schools)==0)
        break;
      
      print(pageNum);
      pageNum = pageNum + 1;
      
      for(ind in 1:(length(schools)))
      {
        values = dane_placowka(placowka <- schools[[ind]])
        
        for(k in 1:length(values))
        {
          val = values[k]
          cname = colnames(values)[k]
          
          if(cname %in% names(retList))
          {
            retList[[cname]] = c(retList[[cname]],val)
            
          }else{
            retList[[length(retList)+1]] = c(rep("",rowNum),val)
            names(retList)[length(retList)] = cname
          }
        }
        
        rowNum = rowNum + 1
        
        for(num in 1:length(retList))
        {
          if(length(retList[[num]]) == rowNum - 1 )
          {
            retList[[num]] = c(retList[[num]],"")
          }
        }
      }
    }
  }
  
  ret = NULL
  for(ind in 1: length(retList))
  {
    ret = cbind(ret , retList[[ind]])
  }
  colnames(ret) <- names(retList)
  
  write.csv2(ret, plik, row.names=F, na='',fileEncoding ="UTF-8" )
  # write.table(ret,plik,sep=";",row.names = FALSE)
}