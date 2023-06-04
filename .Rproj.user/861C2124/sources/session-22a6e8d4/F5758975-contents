rm(list=ls()) #czyszczenie obszaru roboczego

#1. Instalacja pakietów
if (!require('zoo')) install.packages("zoo") #pakiet służący do porządkowania szeregów czasowych
if (!require('ggplot2')) install.packages("ggplot2")


#Wczytywanie danych 
setwd("C:/Users/Ilona/Downloads") # Wprowadz katalog roboczy w którym jest plik
getwd() #sprawdzanie bieżącego katalogu roboczego

data<- read.csv("lpp_d.csv", sep =",", dec = ".")
#head(data) #wyświetla kilka pierwszych obserwacji
#data = tail(data, 365) #zostawiamy 365 obserwacji z końca (tu: najnowsze dane)


y <- data[,c(1,5)] #pozostawiamy daty oraz cenę zamknięcia sesji
colnames(y) <- c("Daty","cena_zamkniecia")


#temp<-as.numeric(y[2]) 
cena_zamkniecia<-as.numeric(y$cena_zamkniecia)
daty<-as.Date(y$Daty, format="%Y-%m-%d")
lpp_sa<-zoo(cena_zamkniecia, order.by = daty) # tworzenie szeregu czasowego klucz-wartość: data-cena_zamkniecia
head(lpp_sa)



# Logarytincze stopy zwrotu

## Log-zwroty
r <- diff(log(lpp_sa))
#head(lz)


lz <- diff(log(lpp_sa))
N  <- 1250                             # ustalamy probe na podstawie ktorej liczymy statystyki
r  <- tail(r,N)                        # Zostawiamy N najnowszych obserwacji
R  <- coredata(r)                      # wariant szeregu bez dat

# Panel wykresow: pelna proba 
par(mfrow=c(2,1), cex = 0.7, bty="l") #grupowanie wykresów 2 rzedy i 1 kolumna
#plot(data, main="Poziom", xlab="", ylab="")
plot(lpp_sa, main = "Wykres data-cena zamk", xlab = "Data", ylab = "Cena zamknięcia")

plot(r, main="stopy zwrotu", xlab="", ylab="")

##########################################################
# Statystyki opisowe dla logarytmicznych stóp zwrotu WIG #
##########################################################

require(moments)
M1   = moment(R, order = 1, central = FALSE, na.rm = TRUE)
M2   = moment(R, order = 2, central = TRUE, na.rm = TRUE)
M3   = moment(R, order = 3, central = TRUE, na.rm = TRUE)
M4   = moment(R, order = 4, central = TRUE, na.rm = TRUE)

mu  = M1; mu               #średnia
sig = sqrt(M2); sig        #odchylenie std. (zmienność)
S0   = M3/(sig^3); S0      #skośność
K0   = M4/(sig^4); K0      #kurtoza

# Annualizacja
Nyear <- 250
muA   <- mu*Nyear; muA           #średnia roczna stopa zwrotu z WIG20
sigA  <- sig*sqrt(Nyear); sigA   #odchylenie std. roczne 

#######################################################
# Histogram dla logarytmicznych stóp zwrotu #
#######################################################

R0        <- (R-mu)/sig #standaryzacja zmiennej
bwdth     <- 0.1

#Porownanie z rokladem normalnym
ggplot(data.frame(R0), aes(x = R0)) +
  theme_bw() +
  geom_histogram(binwidth = bwdth, colour = "white", fill = "yellow4", size = 0.1) +
  stat_function(fun = function(x) dnorm(x)*N*bwdth, color = "red", size = 1)

ggplot(data.frame(R0), aes(x = R0)) +
  theme_bw() +
  stat_function(fun = function(x) dnorm(x), color = "red", size = 1)+
  geom_density(color="aquamarine4")  










#wygenerowana tabela z podstawowymi statystykami
df <- data.frame()  # Inicjalizacja pustej ramki danych

# Przypisanie wartości do kolumny "SPOLKA"
#df$SPOLKA <- c(min(r), max(r), mu, sig, S0, K0)

#df['SPOLKA'] <- c(min(lpp_sa), max(lpp_sa), mu, sig, S0, K0)



# Obliczanie podstawowych statystyk
statystyki <- c("Min", "Max", "Średnia", "Odchylenie std.", "Skośność", "Kurtoza")
wartosci <- c(min(r), max(r), mean(r), sd(r), skewness(r), kurtosis(r))
tabela_statystyk <- data.frame(Statystyki = statystyki, Wartości = wartosci)

# Ocena rozkładu normalnego
test_shapiro <- shapiro.test(R0)
ocena_rozkładu <- ifelse(test_shapiro$p.value < 0.05, "Nie", "Tak")

# Wyświetlanie tabeli statystyk i oceny rozkładu
print(tabela_statystyk)
print(paste("Czy stopy mają rozkład normalny?", ocena_rozkładu))

# Rysowanie wykresu
qqnorm(R0)
qqline(R0)






#model jednoczynnikowy sharpe'a

# Import danych
#rm(list=ls())
basePath <- "C:/Users/Ilona/Downloads/"
capmFilename <- "CAPM_data.csv"
ipath <- paste(basePath,capmFilename, sep = "")
dane <- read.csv(ipath, header=TRUE, sep=";", dec=",", row.names="daty")

T <- nrow(dane)     # liczba obserwacji
N <- ncol(dane)-2   # liczba akcji
ri <- dane[,1:N];   # stopy zwrotow z akcji               ri- risk investmnet
rm <- dane[,N+1];   # stopy zwrotów portfela rynkowego    rm- risk market
rf <- dane[,N+2]    # stopy wolne od ryzyka               rf- risk free
daty  <- as.Date(rownames(dane)); 
nazwy <- colnames(ri)

#Definiowanie funkcji wykorzystywanej przy szacowaniu modelu jednoczynnikowego i testowaniu CAPM
funBet <- function(ri, rm, nazwy, cons){
  N <- ncol(ri)
  if (cons==1){
    Wsp <-  matrix(NA,N,3)
    colnames(Wsp) <- c("alp","bet","Sei") 
  } else {
    Wsp <-  matrix(NA,N,2)
    colnames(Wsp) <- c("bet","Sei") 
  } 
  rownames(Wsp) <- nazwy 
  
  for (i in 1:N){
    if (cons==1){
      z <- lm(ri[,i]~rm)       
    } else {
      z <- lm(ri[,i]~rm-1)
    }
    Wsp[i,] <- c(z$coeff, sd(z$resid))
  }
  return(list(Wsp=Wsp))
}

#Szacowanie modelu jednoczynnikowego
z <- funBet(ri,rm,nazwy,cons=1)

#Wyniki dla 30 pierwszych akcji wyświetla pierwsze 30 wierszy z kolumny "Wsp" 
z$Wsp[1:1,]

#Statystyki opisowe dla wszystkich oszacowań
summary(z$Wsp)

#Testowanie modelu CAPM
# Etap 1
z <- funBet(ri-rf,rm-rf,nazwy, cons=0)
z$Wsp

#Etap 2
beta <- z$Wsp[,1]           # bety
Sei2 <- z$Wsp[,2]^2         # wariancja reszt
y    <- colMeans(ri) - mean(rf) # nadzwyczajne stopy zwrotu
x    <- cbind(beta,Sei2)

SML <- lm(y ~ x); 
summary(SML)

#Łączna weryfikacja trzech hipotez

ym   <- mean(rm)-mean(rf);          # nadzwyczajna stopa rynkowa
linearHypothesis(SML, hypothesis.matrix = diag(1,3), 
                 rhs=c(0,ym,0), test="F")







# Test Sharpe'a-Coopera (1972) modelu CAPM
Table <- matrix(NA,10,12) ## tabela 10 wierzszy i 12 kolumn 
rownames(Table) <- paste("portfel",1:10) # nazywamy wiersze portfel1, portfel2, ... itd.
colnames(Table) <- c("beta06","beta07", "beta08","beta09","beta10",as.character(2006:2010),"beta06-10","2006-2010")

p.ret     <- matrix(NA,10,5) # średnie miesięczne stopy zwrotu dla każdego z 10 portfeli w latach 2006-2010 (5 kolumn)
print(p.ret)
p.bet     <- matrix(NA,10,5)

for (p in 0:4){
  riSC   <- ri[(1+p*12):(60+p*12),] # dla p =0 wybieramy wiersze 1-60 (wiersze dla pierwszych pięciu lat 5*12=60)
  rmSC   <- rm[(1+p*12):(60+p*12)] # tempo wzrostu indeksu giełdowego
  rfSC   <- rf[(1+p*12):(60+p*12)] # stopa zwrotu instrumentów wolnych od ryzyka
  
  # wsp. beta
  z <- funBet(riSC-rfSC,rmSC-rfSC,nazwy, cons=0)
  beta <- z$Wsp[,1] # beta - pierwsza kolumna macierzy wynikowej
  ind <- sort.int(beta, decreasing = FALSE, index.return = TRUE) # sortowanie bety od najmniejszej do najw.
  BetSort   <- ind$x # posortowane wg wartści bety rosnąco
  indx      <- ind$ix # ustawiliśmy index.return = TRUE, więc zwracamy tablice indeksów np. [2, 4] - w tablicy BetaSort na 1 miejscu jest wartość o indexie 2 z beta (nieposortowanej)
  print(BetSort)
  print(indx)
  
  for (i in 0:9){
    ret   <- ri[(61+p*12):(72+p*12),indx[(i*5+1):(i*5+5)]];
    p.ret[i+1,p+1] <- mean(colMeans(ret)); 
    p.bet[i+1,p+1] <- mean(BetSort[(i*5+1):(i*5+5)])
  }
}

options(digits=3)
Table[,1:5]   <- p.bet
Table[,6:10] <- p.ret
Table[,11]   <- rowMeans(p.bet)
Table[,12]   <- rowMeans(p.ret)
Table






















#Wykres SML
SMLx  <- seq(0,2,0.01)
SMLy  <- mean(rf) + SMLx*ym
beta  <- z$Wsp[,1]

par(mfrow=c(1,1), cex = 0.7, bty="l", pin=c(3,2))
plot(SMLx,SMLy,type="l", xlim=c(0.05,2), ylim=c(-1,4), xlab="oszacowania bety", ylab="srednia stopa zwrotu")
points(0,mean(rf), pch=19, cex=2.5); text(0.1,mean(rf),"F", cex=1.5)
points(1,mean(rm), pch=19, cex=2.5); text(1.1,mean(rm),"M", cex=1.5)
points(beta,colMeans(ri),pch=18,cex=1)
abline(SML, lty=2)
legend("bottomright", c("Teoretyczna SML", "Oszacowana SML"),lty=1:2,bty="n")



