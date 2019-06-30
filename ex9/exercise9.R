#Aufgabenblatt 9 "Werkzeuge der empirischen Forschung"


# Oliver Pohl, 577878, pohloliq





# Aufgabe 23

# a)

# Messungen der Stärke von Zement nach Messgerät und Brecher
# je 4 Messwiederholungen pro Faktorstufenkombination, deswegen zweifaktorielle Varianzanalyse mit Wechselwirkung

# Modell: Stärke_ijk = mu + a_i + b_j + c_ij + e_ijk

# Datenaufbereitung

measure=read.table(text=
                     "1 1 5280
                   1 1 5520
                   1 1 4760
                   1 1 5800
                   1 2 4340
                   1 2 4400
                   1 2 5020
                   1 2 6200
                   1 3 4160
                   1 3 5180
                   1 3 5320
                   1 3 4600
                   2 1 4420
                   2 1 5280
                   2 1 5580
                   2 1 4900
                   2 2 5340
                   2 2 4880
                   2 2 4960
                   2 2 6200
                   2 3 4180
                   2 3 4800
                   2 3 4600
                   2 3 4480
                   3 1 5360
                   3 1 6160
                   3 1 5680
                   3 1 5500
                   3 2 5720
                   3 2 4760
                   3 2 5620
                   3 2 5560
                   3 3 4460
                   3 3 4930
                   3 3 4680
                   3 3 5600", col.names=c("Gerät", "Brecher", "Stärke"),colClasses=c("factor","factor","integer"))


# Parameterschätzungen:

# Schätzung des Gesamtmittelwert

ges_m <- mean(measure$Stärke)

# Schätzungen der a_i (Geräte):
gr_1 <- measure[measure$Gerät == 1,]
mean_gr1 <- mean(gr_1$Stärke) # empirischer Mittelwert Gerät 1
gr_2 <- measure[measure$Gerät == 2,]
mean_gr2 <- mean(gr_2$Stärke) # empirischer Mittelwert Gerät 3
gr_3 <- measure[measure$Gerät == 3,]
mean_gr3 <- mean(gr_3$Stärke) # empirischer Mittelwert Gerät 2

# Effekte berechnen
eff_gr_1 <- mean_gr1 - ges_m
eff_gr_2 <- mean_gr2 - ges_m
eff_gr_3 <- mean_gr3 - ges_m

eff_gr_1+eff_gr_2+eff_gr_3
# Summe der Effekte ist gleich null

#####################

# Schätzungen der b_j (Brecher):
br_1 <- measure[measure$Brecher == 1,]
mean_br1 <- mean(br_1$Stärke) # empirischer Mittelwert Brecher 1
br_2 <- measure[measure$Brecher == 2,]
mean_br2 <- mean(br_2$Stärke) # empirischer Mittelwert Brecher 3
br_3 <- measure[measure$Brecher == 3,]
mean_br3 <- mean(br_3$Stärke) # empirischer Mittelwert Brecher 2

# Effekte berechnen
eff_br_1 <- mean_br1 - ges_m
eff_br_2 <- mean_br2 - ges_m
eff_br_3 <- mean_br3 - ges_m

eff_br_1+eff_br_2+eff_br_3
# Summe der Effekte ist gleich null


####################

# Schätzung der c_ij 

c_11 <- measure[measure$Gerät == 1 & measure$Brecher == 1,]
mean_c11 <- mean(c_11$Stärke)
c_21 <- measure[measure$Gerät == 2 & measure$Brecher == 1,]
mean_c21 <- mean(c_21$Stärke)
c_31 <- measure[measure$Gerät == 3 & measure$Brecher == 1,]
mean_c31 <- mean(c_31$Stärke)
c_12 <- measure[measure$Gerät == 1 & measure$Brecher == 2,]
mean_c12 <- mean(c_12$Stärke)
c_22 <- measure[measure$Gerät == 2 & measure$Brecher == 2,]
mean_c22 <- mean(c_22$Stärke)
c_32 <- measure[measure$Gerät == 3 & measure$Brecher == 2,]
mean_c32 <- mean(c_32$Stärke)
c_13 <- measure[measure$Gerät == 1 & measure$Brecher == 3,]
mean_c13 <- mean(c_13$Stärke)
c_23 <- measure[measure$Gerät == 2 & measure$Brecher == 3,]
mean_c23 <- mean(c_23$Stärke)
c_33 <- measure[measure$Gerät == 3 & measure$Brecher == 3,]
mean_c33 <- mean(c_33$Stärke)


# Effekte berechnen

eff_c11 <- mean_c11-ges_m
eff_c21 <- mean_c21-ges_m
eff_c31 <- mean_c31-ges_m
eff_c12 <- mean_c12-ges_m
eff_c22 <- mean_c22-ges_m
eff_c32 <- mean_c32-ges_m
eff_c13 <- mean_c13-ges_m
eff_c23 <- mean_c23-ges_m
eff_c33 <- mean_c33-ges_m

#Summe der Effekte ist gleich null
eff_c11+eff_c12+eff_c13+eff_c21+eff_c22+eff_c23+eff_c31+eff_c32+eff_c33

# b) 

# 2-faktorielle Varianzanalyse mit Wechselwirkungen

anova(lm(Stärke ~ Gerät + Brecher + Gerät*Brecher, data=measure))

# Die Varianzanalyse zeigte :

# Die Messgeräte unterscheiden sich nicht, da der p-Wert=0.21424 > 0.05 und damit keinerlei signifikant
# Die Brecher unterscheiden sich, da der p-Wert=0.01963 < 0.05 und damit als signifikant zu bewerten ist
# Es treten keine Wechselwirkungen auf, da die Analyse einen p-Wert=0.66290 > 0.05 zurückgab und somit nicht signifikant ist


# Wechselwirkungen Brecher-Gerät
boxplot(Stärke ~ Brecher + Gerät,data=measure, las=3)



# c)

# 2-faktorielle Varianzanalyse 

anova(lm(Stärke ~ Gerät + Brecher, data=measure))

# Ergebnisse der Varianzanalyse:

# Es besteht ein Varianzunterschied zwischen den Brechern, da der berechnete p-Wert mit 0.01518 < 0.05 ist
# und somit als signifikant gewertet wird.

# Es besteht kein Varianzunterschied zwischen den Messgeräten, da der berechnete p-Wert mit 0.19575 > 0.05 ist
# und somit keinerlei Signifikanz zeigt.




# Aufgabe 24

# a)

# Hyptothesen

# m_b ist der Erwartungswert der Gruppe der Probanden, welche Alkohol zu sich nahmen
# m_k ist der Erwartungswert der Kontrollgruppe

# H_0 : m_b >= m_k  Nullhypothese
# H_1 : m_b < m_k   Alternative Hypothese


alkohol=read.table(text=
                     "K 0.70
                   K 0.58
                   K 0.64
                   K 0.70
                   K 0.69
                   K 0.80
                   K 0.71
                   K 0.63
                   K 0.82
                   K 0.60
                   K 0.91
                   K 0.59
                   B 0.61 
                   B 0.79 
                   B 0.83 
                   B 0.66 
                   B 0.94 
                   B 0.78 
                   B 0.81 
                   B 0.60 
                   B 0.88 
                   B 0.90 
                   B 0.75 
                   B 0.86", col.names=c("Gr", "time"),colClasses=c("factor","numeric"))


# b) 

# Test auf Normalverteilung der beiden Datensätze

gr_B <- alkohol[alkohol$Gr=="B",]
gr_K <- alkohol[alkohol$Gr=="K",]

shapiro.test(gr_B$time)
# p-Wert=0.43 , daraus folgt dass Normalverteilung vorliegt.
shapiro.test(gr_K$time)
# p-Wert=0.2543 , daraus folgt dass Normalverteilung vorliegt.


require(car)
# Test auf Gleichheit der Varianzen

leveneTest(alkohol$time,alkohol$Gr,center = "mean")
# da p-Wert= 0.6792 folgt dass hier Varianzhomogenität vorliegt

# Da beide Gruppen normalverteilt und varianzhomogen sind, sollten wir hier den T-Test anwenden.

t.test(time~Gr,data = alkohol,alternative = "less",var.equal = TRUE, paired = FALSE)


# c)

# Da der p-Wert = 0.9705 ist, zeigt der Test keinerlei Signifikanz und deswegen kann man ausgehend von dieser Stichprobe
# sagen, dass Alkohol die Reaktionszeit beeinflusst.






# Aufgabe 25

tibetan = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/tibetan.dat",
                     col.names=c("maxlength","maxwidth", "height", "upface", "facewidth"),
                     colClasses=c("numeric","numeric","numeric","numeric","numeric"))

shapiro.test(tibetan$maxlength)
shapiro.test(tibetan$maxwidth)
shapiro.test(tibetan$height)
shapiro.test(tibetan$upface)
shapiro.test(tibetan$facewidth)

# Schlussfolgerung: Es sind alle Variablen normalverteilt bis auf "facewidth"


heroin1 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/heroin.dat",
                     col.names=c("id","clinic", "status", "time", "prison", "dose",rep("NULL", 6)),
                     colClasses=c("integer","integer","integer","integer","integer","integer"))
heroin1 = heroin1[,1:6]
heroin2 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/heroin.dat",
                     col.names=c(rep("NULL", 6),"id","clinic", "status", "time", "prison", "dose"),
                     colClasses=c("integer","integer","integer","integer","integer","integer"))
heroin2 = heroin2[,7:12]
heroin = rbind(heroin1,heroin2)
rm(heroin1,heroin2)

shapiro.test(heroin$time)
shapiro.test(heroin$dose)

# Schlussfolgerung: Keine der metrischen Variablen in diesem Datensatz sind normalverteilt, da die p-werte < 0.1



water1 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/water.dat",
                    col.names=c("NULL","mortal","calcium",rep("NULL", 3)),
                    colClasses=c("integer","integer"),nrows=30)
water1 = water1[,2:3]
water4 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/water.dat",
                    col.names=c(rep("NULL", 4),"mortal","calcium"),
                    colClasses=c("integer","integer"),nrows=30)
water2 = water4[,5:6]
water_lastrow = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/water.dat",
                           col.names=c("NULL","mortal","calcium"),colClasses=c("integer","integer"),skip=30)

water3 = water_lastrow[,2:3]                         
water = rbind(water1,water2)
water = rbind(water,water3)

rm(water1,water2,water3,water4,water_lastrow)

shapiro.test(water$mortal)
shapiro.test(water$calcium)

# Schlussfolgerung: Keine der metrischen Variablen in diesem Datensatz sind normalverteilt, da die p-werte < 0.1



computer = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/computer.dat",
                      col.names=c("cycletime","minmem", "maxmem", "cachesize", "minchan", "maxchan", "relperf","estrelperf"),
                      colClasses=c("integer","numeric","numeric","numeric","numeric","numeric","numeric"))

shapiro.test(computer$cycletime)
shapiro.test(computer$minmem)
shapiro.test(computer$maxmem)
shapiro.test(computer$cachesize)
shapiro.test(computer$minchan)
shapiro.test(computer$maxchan)
shapiro.test(computer$relperf)
shapiro.test(computer$estrelperf)

# Schlussfolgerung: Keine der metrischen Variablen in diesem Datensatz sind normalverteilt, da die p-werte < 0.1




ttest = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/ttest.dat",
                   col.names=c("nr","a", "b", "ba"),
                   colClasses=c("integer","numeric","numeric","numeric"),nrows=10)

ttest_stat = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/ttest.dat",
                        col.names=c("a", "b", "ba"),
                        colClasses=c("numeric","numeric","numeric"),skip=10,nrows=2)

shapiro.test(ttest$a)
shapiro.test(ttest$b)
shapiro.test(ttest$ba)

# Schlussfolgerung: Die metrischen Variablen b und a sind normalverteilt , da die p-Werte beide größer 0.1 sind 



skull = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/skull.dat",
                   col.names=c("gr","mb", "bh", "bl", "nh"),
                   colClasses=c("integer","integer","integer","integer","integer"))

shapiro.test(skull$mb)
shapiro.test(skull$bh)
shapiro.test(skull$bl)
shapiro.test(skull$nh)

# Schlussfolgerung: Es sind alle Variablen normalverteilt bis auf "nh"



synchro = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/synchro.dat",
                     col.names=c("wettkaempfer","r1", "r2", "r3", "r4", "r5"),
                     colClasses=c("integer","numeric","numeric","numeric","numeric","numeric"))

shapiro.test(synchro$r1)
shapiro.test(synchro$r2)
shapiro.test(synchro$r3)
shapiro.test(synchro$r4)
shapiro.test(synchro$r5)

# Schlussfolgerung: Es sind alle Variablen des Datensatzes synchro normalverteilt




# Aufgabe 26

# a)



moon = read.table(text=
             "m hkv 3
           nm hkv 30
           m  other 4
           nm other 269
           ", col.names=c("Gr", "hkv","anz"),colClasses=c("factor","factor","numeric"))


#b)


t.test(anz~Gr,data = moon)


