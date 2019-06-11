#Aufgabenblatt 8 "Werkzeuge der empirischen Forschung"


# Oliver Pohl, 577878, pohloliq




# Aufgabe 19


water1 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/water.dat",
                    col.names=c("mortal","calcium","north",rep("NULL", 3)),
                    colClasses=c("integer","integer","integer"),nrows=30);

water1 = water1[,1:3]
water4 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/water.dat",
                    col.names=c(rep("NULL", 3),"mortal","calcium","north"),
                    colClasses=c("integer","integer","integer"),nrows=30);
water2 = water4[,4:6]
water_lastrow = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/water.dat",
                           col.names=c("mortal","calcium","north"),colClasses=c("integer","integer"),skip=30);

water3 = water_lastrow[,1:3]                         
water = rbind(water1,water2)
water = rbind(water,water3)


water_north <- water[water$north == 1,]
water_notnorth <- water[water$north == 0,]



plot(water_north$mortal,water_north$calcium)
cor(water_north$mortal,water_north$calcium,method = "pearson")
# Der Pearsonsche Korrelationskoeffizient beträgt -0.3287438 was bedeutet, dass hier eine
# eher schwache lineare Abhängigkeit zwischen der Sterblichkeitsrate der Männer und der 
# Calciumkonzentration im Trinkwasser besteht.
# Man kann nicht wirklich behaupten, dass in dieser Region(Norden) die Härte des Trinkwasser
# die Sterberate in irgendeinerweise vollständig beeinflusst. Es ist lediglich eine kleine Tendenz erkennbar



plot(water_notnorth$mortal,water_notnorth$calcium)
cor(water_notnorth$mortal,water_notnorth$calcium,method = "pearson")
# Der Pearsonsche Korrelationskoeffizient beträgt -0.5897385 was bedeutet, dass hier eine
# mittelstarke lineare abhängigkeit zwischen der Sterblichkeitsrate und Calciumkonzentration
# im Trinkwasser besteht. Man kann anhand der Grafik sehen, dass je höher die Calciumkonzentration
# im Trinkwasser ist, desto geringer die Sterberate. Lediglich ein paar Ausreißer verhindern eine hohe lineare Abhängigkeit.
# Somit kann man sagen, dass in den Städten die nicht im Norden sind, Sterblichkeit und Wasserhärte 
# eine gewisse Abhängigkeit zwischen den Variablen besteht und dies aber geographisch bedingt ist.




# Aufgabe20


# a)

# Hypothese I: 

# H_0: die Varianzen der einzelnen Gruppen 1-4 (die verschiedenen Epochen) einer Variable (z.B Maximum Breath)
# sind alle gleich (gleiche Streuung)
# also: alpha_1=alpha_2=alpha3=alpha_4

# H_1: Es existiert eine Varianz die nicht gleich der Varianz einer anderen Epoche derselben Variable(z.B Maximum Breath) ist
# also: alpha_i != alpha_l , für ein i!=l 

# Varianzanalyse (ANOVA)



# Hypothese II: Test auf Lageunterschiede innerhalb der Gruppen (Epochen)

# H_0: mu_1 = mu_2 , wobei mu_1 Erwartungswert einer Epoche ist und mu_2 Erwartungswert einer anderen Epoche ist

# H_1: mu_1 != mu_2

# unverbundener Zweistichprobentest



skull = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/skull.dat",
                   col.names=c("gr","mb", "bh", "bl", "nh"),
                   colClasses=c("integer","integer","integer","integer","integer"));

# b)

# elementare statistische Maßzahlen für Variable Maximum Breath

mean(skull$mb)
sd(skull$mb)
var(skull$mb)
median(skull$mb)
min(skull$mb)
max(skull$mb)
quantile(skull$mb)

# elementare statistische Maßzahlen für Variable Basisbregmatic Height

mean(skull$bh)
sd(skull$bh)
var(skull$bh)
median(skull$bh)
min(skull$bh)
max(skull$bh)
quantile(skull$bh)

# elementare statistische Maßzahlen für Variable Basialveolar Length

mean(skull$bl)
sd(skull$bl)
var(skull$bl)
median(skull$bl)
min(skull$bl)
max(skull$bl)
quantile(skull$bl)

# elementare statistische Maßzahlen für Variable Nasal Height

mean(skull$nh)
sd(skull$nh)
var(skull$nh)
median(skull$nh)
min(skull$nh)
max(skull$nh)
quantile(skull$nh)



# Boxplots der 4 Variablen mit den verschiedenen Epochen als Boxen
# Auf der x-Achse (Epochen) bedeutet  1: Epoche 4000 v.u.Z. , 2: Epoche 3300 v.u.Z. , 3: Epoche 1850 v.u.Z. , 4: Epoche 200 v.u.Z.  

boxplot(skull$mb~skull$gr,skull,ylab="Maximum Breath",xlab="Epochen", main = "Boxplot der Variable Maximum Breath")
boxplot(skull$bh~skull$gr,skull, ylab="Basisbregmatic Height",xlab="Epochen",main = "Boxplot der Variable Basisbregmatic Height")
boxplot(skull$bl~skull$gr,skull,ylab="Basialveolar Length",xlab="Epochen",main = "Boxplot der Variable Basialveolar Length")
boxplot(skull$nh~skull$gr,skull,ylab="Nasal Height",xlab="Epochen",main = "Boxplot der Variable Nasal Height")



# c)

ep_4000 <- skull[skull$gr == 1,]
ep_3300 <- skull[skull$gr == 2,]
ep_1850 <- skull[skull$gr == 3,]
ep_200 <- skull[skull$gr == 4,]


# Berechnung des kritischen Wertes der F-Verteilung bei k-1=3 und n-k=116 Freiheitsgraden

qf(1-0.05,3,116)
# F_krit = 2.682809

skull$gr <- as.factor(skull$gr)


# Varianzanalyse für Variable Maximum Breath

summary(aov(skull$mb ~ skull$gr, skull))
# F_Wert = 5.588 > 2.683 und P-Wert = 0.000129 < 0.05 , daraus folgt H_0 Hypothese abgelehnt.


# Varianzanalyse für Variable Basisbregmatic Height

summary(aov(skull$bh ~ skull$gr, skull))
# F_Wert = 0.664 < 2.683 und P-Wert = 0.576 > 0.05 , daraus folgt H_0 Hypothese angenommen.


# Varianzanalyse für Variable Basialveolar Length

summary(aov(skull$bl ~ skull$gr, skull))
# F_Wert = 6.64 > 2.683 und P-Wert = 0.000353 < 0.05 , daraus folgt H_0 Hypothese abgelehnt.


# Varianzanalyse für Variable Nasal Height

summary(aov(skull$nh ~ skull$gr, skull))
# F_Wert = 1.955 < 2.683 und P-Wert = 0.125 > 0.05 , daraus folgt H_0 Hypothese angenommen.



# d)

# Varianzhomogenität kann man mit dem Levene-Test überprüfen.
# Er testet die Gruppen auf Gleichheit der Varianzen (Homoskedastizität) 
# Auch bei diesem Test haben wie wieder ein Signifikanzniveau mit dem wir am Ende vergleichen und entscheiden
# können, ob wir die Nullhypothese ablehnen oder annehmen
# Varianzhomogenität ist zum Beispiel Voraussetzung für den t-Test für unabhängige

library(car)

leveneTest(skull$mb ~ skull$gr,data = skull)
leveneTest(skull$bh ~ skull$gr,data = skull)
leveneTest(skull$bl ~ skull$gr,data = skull)
leveneTest(skull$nh ~ skull$gr,data = skull)

# Alle vier LeveneTest erwiesen sich als nicht signifikant, da die p-Werte > 0.05 waren.
# Somit kann man den ANOVA (Analysis of Variance) durchführen.





# Aufgabe 21

# a)

# Für y = 2

# Da wir es hier mit sehr kleinen Stichprobe zu tun haben (erst bei N > 50 kann Normalverteilung angenommen werden ),
# sowohl für x_0 als auch y_0, müssen wir erst einmal auf Normalverteilung testen.
# Der Shapiro-Wilk Test testet einen Datensatz auf Normalverteilung, beide Datensätze sind nicht normalverteilt 
shapiro.test(y_0)
# p-Wert liegt bei 0.000131 und somit bestätigt dass meine Aussage (Zeile 177) 

x_0 <- c(1,1,1,1,1)
y_0 <- c(1,1,1,1,2)
t.test(x_0,y_0)
# p-Wert liegt bei 0.3739


# Für y = 5

x_1 <- c(1,1,1,1,1)
y_1 <- c(1,1,1,1,5)

shapiro.test(y_1)
# p-Wert liegt bei 0.000131 und somit liegt keine Normalverteilung vor

t.test(x_1,y_1)
# p-Wert liegt bei 0.3739

# Für y = 10


x_2 <- c(1,1,1,1,1)
y_2 <- c(1,1,1,1,10)

shapiro.test(y_2)
# p-Wert liegt bei 0.000131 und somit liegt keine Normalverteilung vor

t.test(x_2,y_2)
# p-Wert liegt bei 0.3739


# Feststellung : Bei allen drei unverbundenen Zwei-Stichproben  T-Tests war der p-Wert gleich, obwohl sich das 
# empirische Mittel der zweiten unabhängigen Stichprobe Y mit steigenden y-Wert erhöhte.
# Somit kann man sagen, dass der T-Test bei Stichproben, welche nicht normalverteilt sind und die zwei Stichproben ziemlich große Unterschiede
# in der Varianz aufweisen keine aussagekräftigen Ergebnisse (p-Wert) liefert.


# b)

x_3 <- c(1,2,3)
y_3 <- c(0,0,0)

shapiro.test(x_3)
# p-Wert liegt bei 1 und somit liegt Normalverteilung vor, allerdings ist y_3 nicht normalverteilt und somit hat
# der t-Test hier wenig Aussagekraft


t.test(x_3,y_3)
# Der p-Wert liegt bei 0.07418


x_4 <- c(1,2,3,10)
y_4 <- c(0,0,0)

shapiro.test(x_4)
# p-Wert liegt bei 0.1152 und somit liegt keine Normalverteilung vor , allerdings ist y_4 nicht normalverteilt und somit hat
# der t-Test hier wenig Aussagekraft

t.test(x_4,y_4)
# Der p-Wert liegt bei 0.1697

# Feststellung : Man würde eher erwarten, dass der p-Wert von t.test(x_3,y_3) größer als t.test(x_4,y_4) ist,
# da durch das Hinzufügen der 10 in x_4 die Varianz und der Mittelwert ansteigen und somit weniger nahe an dem Mittelwert von y_4 liegen
# als es bei x_3 und y_3 der Fall war, wo der Unterschied lediglich 2 betrug.
# Somit ist auch hier festzustellen dass der T-Test nur bei normalverteilten Stichproben (was für beide Stichproben gilt) funktioniert bzw.
# aussagekräftige Ergebnisse liefert.


# Aufgabe 22


# a)

# Beispiel für unverbundene Zweistichprobenproblem:

# Wir haben zwei Geräte(mit unterschiedlichen Antennen) welche GPS-Signal empfangen können. Beide Geräte sind zusammen mit ihren 
# Antennen an unterschiedlichen Orten in einem Auto verbaut. Die GPS-Signale beider Geräte verarbeiten wir als Daten(Latitude,Longitude).
# Nun fahren beide Geräte (in Autos natürlich) die selbe Strecke. Wir geben für die gefahrene Strecke eine Orientierungslinie an der  
# beide Geräte getestet werden vor. Nun nehmen wir als eigentliche Daten auf denen wir Statistik betreiben, den Abstand der Koordinaten
# beider Geräte zur Orientierungslinie . Anschließend machen wir einen Zweistichproben Lagetest


# Beispiel für verbundene Zweistichprobenproblem:

# Wir haben 1000 Menschen als Probanden und wollen als Signal(Daten) Pulsschläge pro Minute messen.
# Wir messen jeweils vor einem Bungeesprung aus 100 Metern und nach diesem Sprung.
# Da dies zwei Messungen an denselben Personen gelten diese Stichproben als verbunden


# Aufgabe 22+

n <- 100 # Anzahl der pseudozufälligen Vektoren

vector_xi = c()
set.seed(1991)
for(i in 1:n-1){

  
  vector_xi <- c(vector_xi,mean(rnorm(100,0,1)))
  
}

vector_yi = c()
set.seed(1992)
for(i in 1:n-1){
  
  
  vector_yi <- c(vector_yi,mean(rnorm(100,0.2,1)))
  
}

# Tests von Y

t.test(vector_yi)
# p-Wert ist < 2.2e-16, was bedeutet H_0 : das der Erwartungswert gleich null ist wird abgelehnt

wilcox.test(vector_yi)
# p-Wert ist < 2.2e-16, was bedeutet H_0 : das der Erwartungswert gleich null ist wird abgelehnt


# Test von X

t.test(vector_xi)
# p-Wert ist 0.8514, was bedeutet H_0 : das der Erwartungswert gleich null ist wird angenommen

wilcox.test(vector_xi)
# p-Wert ist 0.9904, was bedeutet H_0 : das der Erwartungswert gleich null ist wird angenommen




