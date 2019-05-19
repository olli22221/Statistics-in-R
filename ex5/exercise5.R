#Aufgabenblatt5 "Werkzeuge der empirischen Forschung"


# Oliver Pohl, 577878, pohloliq




#Aufgabe 11

#a)

computer = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/computer.dat",
                      col.names=c("cycletime","minmem", "maxmem", "cachesize", "minchan", "maxchan", "relperf","estrelperf"),
                      colClasses=c("integer","numeric","numeric","numeric","numeric","numeric","numeric"));
#a1)

boxplot(computer["relperf"])
boxplot(computer["estrelperf"])


#a2)

#Ginis Mittelwertdifferenz
gmd <- function(x) {
  n <- length(x)
  sum(outer(x, x, function(a, b) abs(a - b))) / n / (n - 1)
}


# Statistische Kennwerte für die Variable Leistung
mean(computer$relperf)
sd(computer$relperf)
median(computer$relperf)
mean(computer$relperf[11:199])
gmd(computer$relperf)
mad(computer$relperf)

# Statistische Kennwerte für die Variable geschätzte Leistung  
mean(computer$estrelperf)
sd(computer$estrelperf)
median(computer$estrelperf)
mean(computer$estrelperf[11:199])
gmd(computer$estrelperf)
mad(computer$estrelperf)




#Aufgabe 11

#a)

banknote = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/banknote.dat",
                      col.names=c("nummer","laenge", "links", "rechts", "unten", "oben", "diagonal"),
                      colClasses=c("integer","numeric","numeric","numeric","numeric","numeric","numeric"));

mittelwert <- c("Mittelwerte",mean(banknote$laenge),mean(banknote$links),mean(banknote$rechts),mean(banknote$unten),mean(banknote$oben),mean(banknote$diagonal))
standarbweichung <- c("Standardabweichung",sd(banknote$laenge),sd(banknote$links),sd(banknote$rechts),sd(banknote$unten),sd(banknote$oben),sd(banknote$diagonal))
maximum <- c("Maximum",max(banknote$laenge),max(banknote$links),max(banknote$rechts),max(banknote$unten),max(banknote$oben),max(banknote$diagonal))
minimum <- c("Minimum",min(banknote$laenge),min(banknote$links),min(banknote$rechts),min(banknote$unten),min(banknote$oben),min(banknote$diagonal))
new <- rbind(banknote,mittelwert,standarbweichung,maximum,minimum)

View(new)


#b)

#echte Banknoten


#univariate Statistiken für die Variable laenge

mean(banknote$laenge[1:100])
sd(banknote$laenge[1:100])
quantile(banknote$laenge[1:100])
max(banknote$laenge[1:100])
min(banknote$laenge[1:100])
hist(banknote$laenge[1:100])
stem(banknote$laenge[1:100])
boxplot(banknote$laenge[1:100])
qqnorm(banknote$laenge[1:100])
qqline(banknote$laenge[1:100])
shapiro.test(banknote$laenge[1:100])
#die Variable laenge (echter Banknoten) ist normalverteilt , da p-value = 0.2567 größer als 0.05 ist

#univariate Statistiken für die Variable links

mean(banknote$links[1:100])
sd(banknote$links[1:100])
quantile(banknote$links[1:100])
max(banknote$links[1:100])
min(banknote$links[1:100])
hist(banknote$links[1:100])
stem(banknote$links[1:100])
boxplot(banknote$links[1:100])
qqnorm(banknote$links[1:100])
qqline(banknote$links[1:100])
shapiro.test(banknote$links[1:100])
#die Variable links(echter Banknoten) ist nicht normalverteilt , da p-value = 0.008254 kleiner als 0.05 ist






#unechte Banknoten


#univariate Statistiken für die Variable links

mean(banknote$links[101:200])
sd(banknote$links[101:200])
quantile(banknote$links[101:200])
max(banknote$links[101:200])
min(banknote$links[101:200])
hist(banknote$links[101:200])
stem(banknote$links[101:200])
boxplot(banknote$links[101:200])
qqnorm(banknote$links[101:200])
qqline(banknote$links[101:200])
shapiro.test(banknote$links[101:200])
#die Variable links (unechter Banknoten) ist nicht normalverteilt , da p-value = 0.02372 kleiner als 0.05 ist


#univariate Statistiken für die Variable laenge

mean(banknote$laenge[101:200])
sd(banknote$laenge[101:200])
quantile(banknote$laenge[101:200])
max(banknote$laenge[101:200])
min(banknote$laenge[101:200])
hist(banknote$laenge[101:200])
stem(banknote$laenge[101:200])
boxplot(banknote$laenge[101:200])
qqnorm(banknote$laenge[101:200])
qqline(banknote$laenge[101:200])
shapiro.test(banknote$laenge[101:200])
#die Variable laenge(unechter Banknoten) ist nicht normalverteilt , da p-value = 0.002899 kleiner als 0.05 ist



#c)

# variable laenge

#trimmed mean
mean(banknote$laenge,trim = 0.025)

#winsorized mean
lae <- banknote$laenge[6:195]
lae <- c(rep(lae[1],5),lae)
lae <- c(lae,rep(lae[195],5))
mean(lae)

#robuste Skalenschätzungen

IQR(banknote$laenge)
mad(banknote$laenge)
Sn(banknote$laenge)
Qn(banknote$laenge)
gmd(banknote$laenge) # gmd() wurde ganz am anfang dieses Skriptes definiert




# variable links

#trimmed mean
mean(banknote$links,trim = 0.025)


#winsorized mean
lin <- banknote$links[6:195]
lin1 <- c(rep(lin[1],5),lin)
lin2<- c(lin1,rep(lin[190],5))
mean(lin2)

#robuste Skalenschätzungen

IQR(banknote$links)
mad(banknote$links)
Sn(banknote$links)
Qn(banknote$links)
gmd(banknote$links) # gmd() wurde ganz am anfang dieses Skriptes definiert



# Ich empfehle als Lageschätzung nicht das arithmetische Mittel, da es Ausreißer nicht so gut kompensieren kann und dann einen verfälschtes Mittel liefert.

#Ich empfehle als Skalenschätzung nicht die empirische Varianz, da dieser Wert zur Berechnung das empirische Mittel benötigt
# und somit auch wieder nicht robust genug gegenüber Ausreißern ist.
#Außerdem ist die Skalenschätzung Range ebenfalls nicht zu empfehlen, da es keinerlei Aussagen über die Abweichungen innerhalb der Stichprobe liefert.

