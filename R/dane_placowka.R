#' @encoding utf-8
#' @title Informaje dla pojedynczej placowki
#' @description
#' Funkcja przetwarza węzeł dla pojedynczej placówki. 
#' Wynikiem jej działania są wszystkie dane dostępne na stronie RSPO dla podanej w parametrze placówki.
#' @param placowka węzeł html, zawierający wszystkie informacje o placówce.
#' @return 
#' Funkcja zwraca wektor ciągów znakowych, którego elementy to poszczególne elementy to informacje o placowce.
#' Nazwy poszczególnych elementów tego wektora to typy informacji.
#' @export
dane_placowka<-function(placowka)
{
  nodeData = getNodeSet(placowka,"./div")
  name = gsub("\n","",xmlValue(getNodeSet(nodeData[[1]],"./a")[[1]]))   
  
  ret = name
  names(ret)[1] <- "Nazwa"
  for(ind in 2:length(nodeData)){
    # print(ind)
    wartoscTemp = wartosc(nInfo <- nodeData[[ind]])
    # print(wartoscTemp[1])
    # print(wartoscTemp[2])
    
    wartoscTemp[2] %in% ret[grepl(wartoscTemp[1],names(ret))]
    
    if( length(ret)!=0 & wartoscTemp[1] %in% names(ret)  ){
      if( wartoscTemp[2] %in% ret[grepl(wartoscTemp[1],names(ret))] ){
        # if(ind< length(nodeData)){
        # print("next")
        next()
        # }else{
        #   break() 
        # }
      }
    }
    
    ret = c(ret,wartoscTemp[2])
    # print(ind)
    
    nameTmp = wartoscTemp[1]
    top = 1
    while( nameTmp %in% names(ret) )
    {
      # print("top")
      # print(top)
      nameTmp = paste(wartoscTemp[1],top,sep="")
      top = top + 1
    }
    names(ret)[length(ret)] <- nameTmp
  }
  return(t(ret))
}