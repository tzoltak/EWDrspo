#' @encoding utf-8
#' @title Zmienne globalne
#' @description Funkcja zwraca zmienne globalne urzywane w kodzie innych funkcji z pakietu EWDrspo.
#' @param  nazwa nazwa parametru.
#' @return zwraca ciąg znaków
#' @export
zmienne_globalne <- function(nazwa)
{
  ret = NULL
  if( nazwa=="kolumny" )
  {
    ret =  c("Typ","Numer RSPO","Data wpisania do RSPO","REGON",
             "NIP","Województwo","Adres","Telefon","Nazwa jednostki nadrzędnej",
             "Numer REGON jednostki nadrzędnej",
             "Numer RSPO jednostki nadrzędnej","Status publiczno-prawny",
             "Kategoria uczniów","Specyfika szkoły/przedszkola",
             "Związanie organizacyjne",
             "Data założenia",
             "Data rozpoczęcia działalności",
             "Data włączenie do zespołu jednostki złożonej",
             "Data wyłączenia z zespołu jednostki złożonej",
             "Kategoria uczniów",
             "Niepełnosprawność dominująca",
             "Obwód",
             "Czy jest internat",
             "Typ organu prowadzącego_prowadzacy",
             "Nazwa organu prowadzącego_prowadzacy",
             "Województwo_prowadzacy",
             "Adres_prowadzacy",
             "Typ_przekazujacy",
             "Nazwa organu rejestrującego_przekazujacy",
             "Województwo_przekazujacy",
             "Adres_przekazujacy")
  }else if(nazwa=="nie_powtarzaj"){
    ret = c("Adres","Typ","Województwo")
  }else if(nazwa == "zespol"){
    ret = "Zespół szkół i placówek oświatowych"
  } else if(nazwa == "rspoNad" ){
    ret = "Numer RSPO jednostki nadrzędnej"
  } else if (nazwa == "Brak"){
    ret = "Brak jednostki nadrzędnej w bazie RSPO"
  } else if (nazwa =="Brak detalow"){
    ret = "Brak szczegółówych danych"
  }
  
  return(ret)
}













