# EWDrspo

Pakiet EWDrspo służy do pobierania danych placówek oświatowych: szkół podstawowych, gimnazjów, techników i liceów ogólnokształcących oraz zespołów, do których należą.

## Instalacja / aktualizacja

Pakiet nie jest wypchnięty na CRAN-a, więc instalować trzeba ze źródeł.

Ponieważ jednak zawiera jedynie kod w R, nie ma potrzeby zaopatrywać się w kompilatory, itp.

Instalacja możliwa jest w dwóch wariantach **(aby zaktualizować pakiet do najnowszej wersji należy zrobić dokładnie to samo)**:

1) Z użyciem pakietu devtools:
```r
install.packages('devtools') # potrzbne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('tzoltak/EWDrspo')
```

**Jeśli podczas instalacji napotkasz na błąd, a używasz linuksa** sprawdź, czy nie dotyczy Cię [ten problem](https://github.com/hadley/devtools/issues/650) lub przeprowadź "uczciwą instalację ze źródeł" (patrz niżej).

2) "Uczciwa instalacja ze źródeł":

   * Pobrać z sieci i zainstalować [narzędzia GIT-a dla linii komend](http://git-scm.com/downloads) 
   
   * W konsoli wywołać:
```r
git clone https://github.com/tzoltak/EWDrspo.git
R CMD INSTALL EWDrspo
```

## Przykład użycia:

```r
library(EWDrspo)
require(snow)
# Uwaga, to będzie trwać kilka godzin!
rspo = pobierz_rspo()
# Wersja jednowoątkowa - trwa dłużej, mniej obciąża komputer:
# rspo = pobierz_rspo(rownolegle=FALSE)
str(rspo)
# Zobaczmy, czy RSPO jest spójne
sprawdz_baze(rspo)
# Zapis do pliku csv.
setwd(".")  # Ustaw katalog!
write.csv2(rspo, "RSPO.csv", row.names=FALSE, na="", fileEncoding="UTF-8")
```

## Sponsorzy

Pakiet został opracowany w ramach projektu systemowego *Rozwój metody edukacyjnej wartości dodanej na potrzeby wzmocnienia ewaluacyjnej funkcji egzaminów zewnętrznych* (UDA-POKL.03.02.00-00-001/13-00) współfinansowanego przez Unię Europejską ze środków Europejskiego Funduszu społecznego, realizowanych przez Instytut Badań Edukacyjnych.
![KL+IBE+EFS](inst/logo-IBE-EWD.png)
