library(plm)
library(ggplot2)
library(readr)
library(data.table)
library(ggTimeSeries)
library(cowplot)
library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(geosphere)

# wczytywanie i wstępne przetwarzanie danych WEB API ze stacji 
places <- read_csv("places.csv")
#zmiana formaty czasu
places <- mutate(places, czas = as.POSIXct(places$timestamp, origin="1970-01-01")) %>% 
  tidyr::separate(czas, c("data", "godzina"), sep = " ", remove = FALSE) %>% 
  separate(data, c("rok", "miesiac", "dzien"), sep = "-", remove = FALSE)

# UWAGA !!! KOD JEST DLA MARCA, WIEC WPISUJEMY NUMER MIESIACA - "07"
places <- subset(places, places$miesiac == "08")

# obiekt przechowujący nazwy stacji oraz ich współrzędne geograficzne.
# DOKLADNOSC DO 1M BO MAMY 5 CYFR PO PRZECINKU
places_punkty <- unique(select(places, "coordinate", "lat", "lng", "uid", "name"))
places_punkty$lat <- round(places_punkty$lat,5)
places_punkty$lng <- round(places_punkty$lng,5)
places_punkty$coordinate <- paste(places_punkty$lat, places_punkty$lng, sep = ",")

# eliminacja punktów niebędących realnie istniejącymi stacjami
stacje <- places_punkty

# wczytywanie i wstępne przetwarzanie danych WEB API z rowerów
bikes <- read_csv("bikes.csv")
bikes <- mutate(bikes, czas = as.POSIXct(bikes$timestamp, origin="1970-01-01")) %>% 
  tidyr::separate(czas, c("data", "godzina"), sep = " ", remove = FALSE) %>% 
  separate(data, c("rok", "miesiac", "dzien"), sep = "-", remove = FALSE)


# UWAGA !!! KOD JEST DLA MARCA, WIEC WPISUJEMY NUMER MIESIACA - "07"
bikes <- subset(bikes, bikes$miesiac == "08")

# LICZĘ ILE ROWERÓW BYŁO DOSTEPNYCH DANEGO DNIA
bikes$dnum <- sprintf("%d", as.numeric(bikes$dzien ))
a <-0
c <- as.data.frame(0)
for (i in 1:31) {
    a <- subset(bikes,dnum==i) 
    c[i] <- nrow(as.data.frame(unique(a$number)))
  }
c_new<-as.data.frame(t(c))
c_new$n=c(1:31)
c_new <- c_new[,c(2,1)]
colnames(c_new) = c("day", "num")
write.csv(c_new, "number-bikes.csv", fileEncoding = "UTF-8", row.names = FALSE)


# utworzono obiekt przechowujący jedynie obserwacje rejestrujące przemieszczenie się roweru
bikes_move <-  bikes[,c(1,5,4)] %>% 
  arrange(number, timestamp)
bikes_move$trip.start <- c(diff(bikes_move$place_uid) != 0, 0)
bikes_move$trip.end <- c(0, diff(bikes_move$place_uid) != 0)
bikes_move$diff_number <- c(0, diff(bikes_move$number) != 0)

# UWAGA! KIEDY W SĄSIADUJĄCYCH OBSERWACJACH WYSTEPUJE ZMIANIA NUMERU ROWERU 
# ZMIENNE trip.start i trip.end PRZYJMUJĄ BŁEDNIE WARTOŚĆ 1
# W CELU WYELIMINOWANIA TEGO BŁEDU ZASTOSOWANO DWIE NASTĘPUJĄCE PO SOBIE PĘTLE FOR

# wstępne filtrowanie w celu przyspieszenia działania petli 
bikes_move <- subset(bikes_move, bikes_move$trip.start == 1 | bikes_move$trip.end == 1)

# 
for (i in 1:nrow(bikes_move)) {
  if (bikes_move[i,6] == 1){
    bikes_move[i,5] = 0
  }
}

for (i in 1:nrow(bikes_move)) {
  if (bikes_move[i,6] == 1){
    bikes_move[(i-1),4]= 0
  }
}
# ponowne filtrowanie
bikes_move <- subset(bikes_move, bikes_move$trip.start == 1 | bikes_move$trip.end == 1)

# likwidacja kolumny pomocniczej
bikes_move <- bikes_move[,-6]

# rozbicie obiektu bikes_move na dwa obiekty przechowujące osobno punkty początkowe oraz końcowe moves
bikes_move_starts <- subset(bikes_move, bikes_move$trip.start == 1)
bikes_move_ends <- subset(bikes_move, bikes_move$trip.end == 1)

# utworzenie obiektu przechowującego dane punktu początkowego oraz końcowego każdego moves roweru
bikes_trips <- cbind(bikes_move_starts,bikes_move_ends)

bikes_trips <- bikes_trips[, c(1,2,3,7,8)]
colnames(bikes_trips) = c("bike_number", "start_timestamp", "start_place", "end_timestamp", "end_place")

# wzbogacenie obiektu o dane pochodzące z obiektu places
moves <- bikes_trips %>% 
  mutate(czas_poczatkowy = as.POSIXct(bikes_trips$start_timestamp, origin="1970-01-01"),
         czas_koncowy = as.POSIXct(bikes_trips$end_timestamp, origin="1970-01-01"))
moves <- merge(moves, stacje, by.x = "start_place", by.y = "uid", all.x = TRUE)
moves <- merge(moves, stacje, by.x = "end_place", by.y = "uid", all.x = TRUE,
               suffixes = c("_start", "_end") ) %>% 
  arrange(start_timestamp)

# LICZYMY ODLEGLOSC POMIEDZY movesMI
moves$distance<-distHaversine(moves[,9:10], moves[,13:14])


# dodanie kolumny jednoznacznie identyfikującej obiekt liniowy przy pomocy współrzędnych początkowych i końcowych
moves$ID <- paste(moves$coordinate_start, moves$coordinate_end)

# dopisanie informacji o liczbie wystąpień danego moves w dodatkowej kolumnie
# dodanie kolumny przechowującej informację o czasie, który upłynął pomiędzy obserwacjami 

moves$duration = moves$end_timestamp - moves$start_timestamp
#PREDKOSC W KM/H
moves$speed <-(moves$distance / moves$duration) * 3.6

# PREDKOSC PRZEJAZDU POWYZEJ 0 M/S
moves <- subset(moves, moves$speed > 0)
# PREDKOSC PRZEJAZDU POZNIZEJ 30 KM/H
moves <- subset(moves, moves$speed < 30)
# CZAS PRZEJAZU MNIEJSZY NIZ 4 GODZINY
moves <- subset(moves, moves$duration < 14400)
# MINIMALNY DYSTANS TO 50M
moves <- subset(moves, moves$distance > 50)
# eksport obiektu do pliku csv
moves <- na.omit(moves)
moves$tmp <- c(0, diff(moves$bike_number) != 0)
moves <- subset(moves, moves$tmp > 0)
# usuwanie kolumny pomocniczej
moves <- moves[,-20]


moves$direct <- paste(moves$start_place, moves$end_place, sep = "_")
moves$direct_name <- paste(moves$name_start, moves$name_end, sep = " -> ")
moves_czestosc <- as.data.frame(table(moves$ID))

moves <- moves %>% 
  mutate(time_start = as.POSIXct(moves$start_timestamp, origin="1970-01-01"),
         time_end = as.POSIXct(moves$end_timestamp, origin="1970-01-01"))

moves <- moves %>% 
  separate(time_start, c("data", "godzina"), sep = " ", remove = FALSE) %>% 
  separate(data, c("rok", "miesiac", "dzien"), sep = "-", remove = FALSE) %>% 
  separate(godzina, c("h", "m", "s"), sep = ":", remove = FALSE)

moves$dzien_tygodnia <- lubridate::wday(moves$time_start, label=TRUE)
moves$wday <- wday(moves$time_start)

moves$qh <- paste(moves$h, moves$kwadrans, sep =":")
moves$qh <- as.character(moves$hq)

# okreslenie rozkładu ilości wypożyczeń w ciągu doby w interwale godzinowym.
barplot(table(moves$h), col = "black", cex.names = 0.65, las=1,
        main = "Rozkład ilości wypożyczeń w ciągu doby
        w interwale godzinowym." )

hist(moves$duration/60)

powszednie <- 2:6
weekend <- c(7,1)

trips_rano <- subset(moves, moves$wday %in% powszednie
                     & moves$h %in% c("05","06","07","08","09"))

trips_popoludnie <- subset(moves, moves$wday %in% powszednie
                           & moves$h %in% c("15","16","17","18")) 

write.csv(moves, "moves.csv", fileEncoding = "UTF-8")

jpeg('ilosc-wypozyczen-dni.jpg', width = 1400, height = 700)
barplot(table(moves$dzien), col = "black", cex.names = 1.65, las=1,
        main = "Rozkład ilości wypożyczeń w kolejnych dniach." )
dev.off()

jpeg('ilosc-wypozyczen-godziny.jpg', width = 1400, height = 700)
# okreslenie rozkładu ilości wypożyczeń w ciągu doby w interwale godzinowym.
barplot(table(moves$h), col = "black", cex.names = 0.65, las=1,
        main = "Rozkład ilości wypożyczeń w ciągu doby
        w interwale godzinowym." )
dev.off()

jpeg('ranking-stacji.jpg', width = 1400, height = 700)
par(mar = c(5, 25, 4.1, 2.1),mfrow=c(1,1))
barplot(tail(sort(table(moves$name_start)),100), horiz = TRUE, las = 1,col = "#90c6fd", cex.names = 1.5, axis.lty = 1,
        main = "Ranking stacji według łącznej liczby wypożyczeń")
dev.off()


# najczęstsze kierunki przemieszczeń
jpeg('kierunki.jpg', width = 1400, height = 700)
ranking_kierunkow <- data.frame(table(moves$direct_name)) %>% 
  arrange(-Freq) 
colnames(ranking_kierunkow) = c("name", "Freq")
par(mar = c(5.1, 48.8, 4.1, 2.1))
barplot(tail(sort(table(moves$direct_name)),40), horiz = TRUE, las = 1,col = "#66ff66",
        main = "Najczęściej występujące kierunki przemieszczeń", cex.names = 1.65,
        axis.lty = 1)
dev.off()

jpeg('wychodzace-rano.jpg', width = 1400, height = 700)
# wypozyczenia
par(mar = c(5.1, 16.8, 4.1, 2.1))
barplot(tail(sort(table(trips_rano$name_start)),40), horiz = TRUE, las = 1,col = "#ffff66",
        cex.names = 1.45, axis.lty = 1,
        main = "Ranking stacji pod względem ilości przemieszczeń wychodzących
    w dni powszednie w godzinach: 05:00 - 10:00")
dev.off()

jpeg('przychodzace-rano.jpg', width = 1400, height = 700)
par(mar = c(5.1, 16.8, 4.1, 2.1))
barplot(tail(sort(table(trips_rano$name_end)),40), horiz = TRUE, las = 1,col = "#00b300",
        cex.names = 1.45, axis.lty = 1,
        main = "Ranking stacji pod względem ilości przemieszczeń przychodzących
    w dni powszednie w godzinach: 05:00 - 10:00")
dev.off()

jpeg('kierunki-rano.jpg', width = 1400, height = 700)
par(mar = c(5.1, 40.8, 4.1, 1.1))
barplot(tail(sort(table(trips_rano$direct_name)),40), horiz = TRUE, las = 1,col = "#cccc00",
        main = "Najczęściej występujące kierunki przemieszczeń
        w dni powszednie w godzinach: 05:00 - 10:00", cex.names = 1.35,
        axis.lty = 1)
dev.off()

jpeg('wychodzace-popoludnie.jpg', width = 1400, height = 700)
par(mar = c(5.1, 26.8, 4.1, 2.1))
barplot(tail(sort(table(trips_popoludnie$name_start)),40), horiz = TRUE, las = 1,col = "#730099",
        cex.names = 1.45, axis.lty = 1,
        main = "Ranking stacji pod względem ilości przemieszczeń wychodzących
    w dni powszednie w godzinach: 15:00 - 18:00")
dev.off()

jpeg('przychodzace-popoludnie.jpg', width = 1400, height = 700)
par(mar = c(5.1, 16.8, 4.1, 2.1))
barplot(tail(sort(table(trips_popoludnie$name_end)),40), horiz = TRUE, las = 1,col = "#660033",
        cex.names = 1.45, axis.lty = 1,
        main = "Ranking stacji pod względem ilości przemieszczeń przychodzących
    w dni powszednie w godzinach: 15:00 - 18:00")
dev.off()


jpeg('kierunki-popoludnie.jpg', width = 1400, height = 700)
par(mar = c(5.1, 32.8, 4.1, 2.1))
barplot(tail(sort(table(trips_popoludnie$direct_name)),40), horiz = TRUE, las = 1,col = "#d98cb3",
        main = "Najczęściej występujące kierunki przemieszczeń
        w dni powszednie w godzinach: 15:00 - 18:00", cex.names = 1.45,
        axis.lty = 1)
dev.off()

