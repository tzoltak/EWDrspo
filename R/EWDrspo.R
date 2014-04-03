#' @encoding utf-8
#' @title Pobieranie danych z RSPO
#' @description
#' Funkcja pobiera dane szkół podstawowych, gimnazjów, liceów ogólnokształcących oraz techników.
#' Pobrane dane zapisuje do pliku csv.
#' @param plik Ścieżka do pliku csv.
#' @return 
#' Funkcja nic nie zwraca.
#' @examples
#' rspo_dane("rspo.csv")
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
      
      if(length(schools)==0 )
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
  
  write.table(ret,plik,sep=";",row.names = FALSE)
}

#' @encoding utf-8
#' @title Informaje dla pojedynczej placowki
#' @description
#' Funkcja przetwarza węzeł dla pojedynczej placówki. 
#' Wynikiem jej działania są wszystkie dane dostępne na stronie RSPO dla podanej w parametrze placówki.
#' @param placowka węzeł html, zawierający wszystkie informacje o placówce.
#' @return 
#' Funkcja zwraca wektor ciągów znakowych, którego elementy to poszczególne elementy to informacje o placowce.
#' Nazwy poszczególnych elementów tego wektora to typy informacji.
dane_placowka<-function(placowka)
{
  nodeData = getNodeSet(placowka,"./div")
  name = xmlValue(getNodeSet(nodeData[[1]],"./a")[[1]])
  
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
#' @encoding utf-8
#' @title Pojedyncza porcja informacji o placowce
#' @description
#' Funkcja przetwarza węzeł z pojedynczą informacją (jak adres, numer telefonu, strona www,..).
#' @param nInfo węzeł html, zawierający informację o placówce.
#' @return Funkcja zwraca dwuelementowy wektor ciągów znakowych, które pierwszy element określa rodzaj informacji, a drugi zawiera informację.
wartosc <- function(nInfo)
{
  trimSemicolon <- function (x) gsub("^[ \t\r\n:]+|[ \t\r\n:]+$", "", x)
  valueName = xmlValue(getNodeSet(nInfo,"./b")[[1]])
  value = trimSemicolon(gsub(valueName,"",xmlValue(nInfo)))
  return(c( trimSemicolon(valueName) , trimSemicolon(gsub("valueName","",value))))
}
#' @encoding utf-8
#' @title Informacje o placowkach
#' @description
#' Funkcja zwraca informacje o placówkach, które znajdują się na jednej stronie z wynikami wyszukiwania dla określonego wojewodztwa.
#' Przez placówkę rozumie się: szkołę podstawową, gimnazjum, technikum i leceum ogólnokształcące
#' @param kodWoj kod województwa (jako wynik funkcji kody_wojewodztw)
#' @param numerStrony numer strony wyszukiwania, z której informacje chcemy pobrać
#' @return funkcja zwraca listę węzłów html z informacjami o placówkach
#' sch = szkoly_woj_strona(kody_wojewodztw()[2],213)
#' xmlSApply(sch, nodeValues)
szkoly_woj_strona<- function(kodWoj,numerStrony)
{
  pre = "a%3A17%3A%7Bs%3A5%3A%22param%22%3Bs%3A23%3A%22Support_sipSearchResult%22%3Bs%3A5%3A%22regon%22%3Bs%3A0%3A%22%22%3Bs%3A4%3A%22rspo%22%3Bs%3A0%3A%22%22%3Bs%3A9%3A%22dotOrgPro%22%3Bs%3A1%3A%220%22%3Bs%3A7%3A%22gusWoj3%22%3Bs%3A1%3A%220%22%3Bs%3A7%3A%22gusPow3%22%3Bs%3A1%3A%220%22%3Bs%3A10%3A%22nazwOrgPro%22%3Bs%3A1%3A%220%22%3Bs%3A6%3A%22gusWoj%22%3Bs%3A2%3A%22"
  post = "%22%3Bs%3A6%3A%22gusPow%22%3Bs%3A1%3A%220%22%3Bs%3A6%3A%22gusGmi%22%3Bs%3A1%3A%220%22%3Bs%3A9%3A%22sioMiejsc%22%3Bs%3A1%3A%220%22%3Bs%3A7%3A%22typJedn%22%3Ba%3A4%3A%7Bi%3A0%3Bs%3A1%3A%224%22%3Bi%3A1%3Bs%3A2%3A%2214%22%3Bi%3A2%3Bs%3A1%3A%223%22%3Bi%3A3%3Bs%3A2%3A%2216%22%3B%7Ds%3A13%3A%22czyMaInternat%22%3Bs%3A1%3A%220%22%3Bs%3A11%3A%22rodzKsztalc%22%3Bs%3A1%3A%220%22%3Bs%3A19%3A%22zawodARodzajKsztalc%22%3Bs%3A1%3A%220%22%3Bs%3A11%3A%22specjalnosc%22%3Bs%3A1%3A%220%22%3Bs%3A13%3A%22specjalizacja%22%3Bs%3A1%3A%220%22%3B%7D"
  paramStr = paste(pre,kodWoj,post,sep="")
  
  params <-
    list(page  = as.character(numerStrony),
         param  ="Support_sipSearchResultPage",
         searchParams=paramStr)
  
  side1 = postForm("http://sio.men.gov.pl/dodatki/rspo2portal/index.php", .params = params, 
                   .encoding ="UTF-8",style = 'POST')
  parsed = htmlParse(side1, encoding="UTF-8")
  schools = getNodeSet(parsed,"//html/body/div/div[@class='sipContentContainer']/div[contains(@class,'sipRowResultContainer ') and @class!='sipRowResultContainer tloStron']")
  return(schools)
}
#' @encoding utf-8
#' @title Kody wojewodztw.
#' @description
#' Funkcja zwraca kody województw, używane przez strony RSPO w kodzie html.
#' @return funkcja zwraca wektor ciągów znakowych
kody_wojewodztw <- function()
{
  src="http://sio.men.gov.pl/dodatki/rspo2portal/index.php?param=Support_sipSearchForm"
  site <- getURL(src, ssl.verifypeer = FALSE, encoding="UTF-8")
  parsed = htmlParse(site, encoding="UTF-8")
  
  aaa = getNodeSet(parsed,"//html/body/div/div/form/div[@id='mainFormId']/div[@class='fieldsSip']/select[@name='gusWoj3']/option")
  aaa[[1]]<-NULL
  return(xmlSApply(aaa,xmlAttrs))
}
