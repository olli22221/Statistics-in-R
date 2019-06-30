#Aufgabenblatt 9 "Werkzeuge der empirischen Forschung"


# Oliver Pohl, 577878, pohloliq



# Aufgabe 27

# a)

# Daten vorbereiten
mendel_theoretical <- c(312.75,104.25,104.25,34.75)
mendel_empir <- c(315,108,101,32)


ks.test(mendel_empir,mendel_theoretical,exact = NULL)
# D = 0.25 und p-Wert=0.9996 , da p-Wert > 0.05 folgt dass die Verteilungen gleich sind.



# b)

# Daten vorbereiten
mendel_matrix <- matrix(c(315,108,101,32),2,2)
dimnames(mendel_matrix) <- list(c("gelb","grün"),c("rund","kantig"))

chisq.test(mendel_matrix)
# p-Wert = 0.8208 > 0.05 , daraus folgt H_0 wird angenommen. Also es besteht keine Abhängigkeit zwischen den Merkmalen.




# Aufgabe 28

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

gr_B <- alkohol[alkohol$Gr=="B",]
gr_K <- alkohol[alkohol$Gr=="K",]

wilcox.test(gr_B$time-gr_K$time,alternative = "less")




# Aufgabe 29
# siehe PDF



# Aufgabe 30

# a)

ttest = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/ttest.dat",
                   col.names=c("nr","a", "b", "ba"),
                   colClasses=c("integer","numeric","numeric","numeric"),nrows=10);

ttest_stat = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/ttest.dat",
                        col.names=c("a", "b", "ba"),
                        colClasses=c("numeric","numeric","numeric"),skip=10,nrows=2);

ttest$ba[5] = ttest$b[5] - ttest$a[5]
ttest_stat$ba[1] = mean(ttest$ba)
ttest_stat$ba[2] = sd(ttest$ba)

# Test auf Normalverteilung der Differenz
shapiro.test(ttest$ba)
# nicht normalverteilt, da p-Wert = 0.001377 > 0.05

# Wilcoxon Test
wilcox.test(ttest$ba,alternative = "less")
# p_Wert = 0.9979, dass heißt Erwartungswert der Differenz ist größer als 0 und Medikament B ist besser.






# b)

#Daten vorbereiten
computer = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/computer.dat",
                      col.names=c("cycletime","minmem", "maxmem", "cachesize", "minchan", "maxchan", "relperf","estrelperf"),
                      colClasses=c("integer","numeric","numeric","numeric","numeric","numeric","numeric"));

#Test auf Normalverteilung beider Datensätze
shapiro.test(computer$relperf)
# da p-Wert < 2.2e-16 keine Normalverteilung
shapiro.test(computer$estrelperf)
# da p-Wert < 2.2e-16 keine Normalverteilung

#Testen ob gleicher Verteilungstyp mit dem Kolmogorov Smirnov-Test
ks.test(computer$relperf,computer$estrelperf)
# Der p-Wert beträgt hier 0.3535 also H_0 Hypothese : gleicher Verteilungstyp wir angenommen

# bei gleichen Verteilungstyp und keine Normalverteilung wird der Wilcoxon Test genommen, da robuster gegenüber T-Test
wilcox.test(computer$relperf,computer$estrelperf)
#Da der p-Wert 0.9435 beträgt, wird die Annahme, dass die beiden Merkmale gleich seien angenommen.
