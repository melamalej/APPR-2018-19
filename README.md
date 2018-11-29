# Osnovna analiza podjetij v Sloveniji

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2018/19

* [![Shiny](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/melamalej/APPR-2018-19/master?urlpath=shiny/APPR-2018-19/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/melamalej/APPR-2018-19/master?urlpath=rstudio) RStudio

## Tematika

Za analizo sem si izbrala najosnovnejše podatke o podjetjih v Sloveniji, kot so število podjetij,
število zaposlenih, prihodki in dobički. Po regijah bom primerjala število podjetij, število oseb, ki delajo v regiji in skupne prihodke v letih med 1999 in 2016, po panogah pa število podjetij in število oseb, ki delajo v panogi. Za primerjavo po panogah bom naredila dve tabeli, eno od leta 1999 do 2007 in drugo od 2008 do 2016, saj se v podatkih uporabljata dve različni standardni klasifikaciji dejavnosti (SKD). Podatke o prihodkih iz prodaje, dobičkih in številu zaposlenih bom vzela po panogah (klasifikacija dejavnosti v teh podatkih je spet malo drugačna) v letih od 2005 do 2017.

Vse zgoraj naštete podatke bom primerjala še z stopnjo rasti BDP v Sloveniji. Te podatke bom uvozila iz spletne strani Wikipedia.

Povezave do podatkovnih virov:
- SURS (https://pxweb.stat.si/pxweb/Database/Ekonomsko/Ekonomsko.asp#14) v obliki CSV
- Wikipedia (https://en.wikipedia.org/wiki/Economy_of_Slovenia) v obliki HTML

Zasnova podatkovnega modela:

- Tabela 1: leto; regija; število podjetij; število oseb, ki delajo v regiji;
- Tabela 2: leto; regija; skupni prihodek podjetij v regiji;
- Tabela 3: leto (2008-2016); panoga (SKD 2008); število podjetij; število oseb, ki delajo v panogi;
- Tabela 4: leto (1999-2007); panoga (SKD 2002); število podjetij; število oseb, ki delajo v panogi;
- Tabela 5: leto; panoga; prihodek iz prodaje; dobiček; število zaposlenih;
- Tabela 6: leto; sprememba BDP;

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `rgdal` - za uvoz zemljevidov
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `reshape2` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-201819)
omogočajo poganjanje projekta na spletu z orodjem [Binder](https://mybinder.org/).
V ta namen je bila pripravljena slika za [Docker](https://www.docker.com/),
ki vsebuje večino paketov, ki jih boste potrebovali za svoj projekt.

Če se izkaže, da katerega od paketov, ki ji potrebujete, ni v sliki,
lahko za sprotno namestitev poskrbite tako,
da jih v datoteki [`install.R`](install.R) namestite z ukazom `install.packages`.
Te datoteke (ali ukaza `install.packages`) **ne vključujte** v svoj program -
gre samo za navodilo za Binder, katere pakete naj namesti pred poganjanjem vašega projekta.

Tako nameščanje paketov se bo izvedlo pred vsakim poganjanjem v Binderju.
Če se izkaže, da je to preveč zamudno,
lahko pripravite [lastno sliko](https://github.com/jaanos/APPR-docker) z želenimi paketi.

Če želite v Binderju delati z git,
v datoteki `gitconfig` nastavite svoje ime in priimek ter e-poštni naslov
(odkomentirajte vzorec in zamenjajte s svojimi podatki) -
ob naslednjem.zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
