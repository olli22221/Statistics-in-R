#Aufgabenblatt7 "Werkzeuge der empirischen Forschung"


# Oliver Pohl, 577878, pohloliq


# Aufgabe 15

water = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/water.dat",
                    col.names=c("mortal","calcium","north",rep("NULL", 3)),
                    colClasses=c("integer","integer","integer","integer","integer","integer"),nrows=30);
water_lastrow = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/water.dat",
                           col.names=c("mortal","calcium","north"),colClasses=c("integer","integer","integer"),skip=30);
water1 = water[,1:3]
water2 = water[,4:6]
names(water2)[1]<-paste("mortal")
names(water2)[2]<-paste("calcium")
names(water2)[3]<-paste("north")
water_end = rbind(water1,water2)
water_result = rbind(water_end,water_lastrow)

var_mortal <- water_result$mortal
var_calcium <- water_result$calcium

fit_lin = lm(mortal~calcium,water_result)
# Regressionsgleichung für die lineare Regression ist -3.226x + 1676.356

fit_cubic = lm(mortal~calcium + I(calcium^2) + I(calcium^3) , water_result)
# Regressionsgleichung für die kubische Regression ist -0.0002281x^3 + 0.04140x^2 - 5.096x + 1690


fit_spl = smooth.spline(var_calcium,var_mortal,spar = 0.1,all.knots = TRUE)


plot(var_calcium,var_mortal)
abline(fit_lin,col="red")
curve(-0.0002281*x^3 + 0.04140*x^2 - 5.096*x + 1690,0,150,add = TRUE,col="green")
lines(fit_spl,col="purple")
legend(60,2000,pch=15,col=c("red","green","purple"),legend=c("lin.Regression","kubische Regression","Spline"))


# Aufgabe 16

heroin1 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/heroin.dat",
                     col.names=c("id","clinic", "status", "time", "prison", "dose",rep("NULL", 6)),
                     colClasses=c("integer","integer","integer","integer","integer","integer"));
heroin1 = heroin1[,1:6]
heroin2 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/heroin.dat",
                     col.names=c(rep("NULL", 6),"id","clinic", "status", "time", "prison", "dose"),
                     colClasses=c("integer","integer","integer","integer","integer","integer"));
heroin2 = heroin2[,7:12]
heroin = rbind(heroin1,heroin2)

# a)

# Klinik 1
clinic1 <- heroin[heroin$clinic == 1,]
clinic1_data <- clinic1$time
t.test(clinic1_data, mu= 500,alternative = "less")


# hier nochmal ohne R t.test Funktion
n <- length(clinic1_data) #Anzahl der Daten
mu <- mean(clinic1_data) # empirischer Mittelwert der Stichprobe
m <- 500 # mu_0
s <- sd(clinic1_data) 
t_hat <- -abs(mu-m)/(s/sqrt(n)) # Teststatistik berechnen
t_hat
t_krit <- qt(0.05,n-1) # kritischer Wert
t_krit
pt(t_hat,n-1) # Berechnung des p-Wertes für berechnete Teststatistik
pt(t_krit,n-1) # Berechnung des p-Wertes für kritischen Wert

# Da unsere Teststatistik kleiner als der kritische Wert muss die Nullhypothese abgelehnt werden
# Der p-Wert von t_hat ist ebenfalls kleiner als 0.05 was für eine Ablehnung von H_0 spricht


# Klinik 2
clinic2 <- heroin[heroin$clinic == 2,]
clinic2_data <- clinic2$time
t.test(clinic2_data, mu= 500,alternative = "less")

# hier nochmal ohne R t.test Funktion
n <- length(clinic2_data) #Anzahl der Daten
mu <- mean(clinic2_data) # empirischer Mittelwert der Stichprobe
m <- 500 # mu_0
s <- sd(clinic2_data) 
t_hat <- -abs(mu-m)/(s/sqrt(n)) # Teststatistik berechnen
t_hat
t_krit <- qt(0.05,n-1) # kritischer Wert
t_krit
pt(t_hat,n-1) # Berechnung des p-Wertes für berechnete Teststatistik
pt(t_krit,n-1) # Berechnung des p-Wertes für kritischen Wert

# Da unsere Teststatistik größer als der kritische Wert ist, muss die Nullhypothese angenommen werden
# Der p-Wert von t_hat ist mit 0.314 ebenfalls größer als 0.05 und befindet sich somit nicht im Ablehnungsbereich

# b)

dose <- heroin$dose
t.test(dose,mu=55)
a <- ttest(x=dose,mu=55,alpha = 0.05)

#hier nochmal ohne R t.test Funktion
n <- length(dose)
m <- 55
mu <- mean(dose)
s <- sd(dose)
t_krit_left <- qt(0.025,n-1)
t_krit_right <- qt(1-0.025,n-1)
t_hat_right <- abs(mu-m)/(s/sqrt(n))
t_hat_left <- -t_hat_right
t_hat_left
t_krit_left
t_krit_right
pt(t_hat_left,n-1,lower.tail = TRUE)+pt(t_hat_right,n-1,lower.tail = FALSE) #Berechnung des p-Wertes

# Da die Teststatistik t_hat_right mit 5.764 > 1.970 und t_hat_left mit -5.764 < -1.970 ist, ist dementsprechend
# auch der p-Wert mit 0.00000002541 oder 2.541033e-08 < 0.05
# Somit muss die Nullhypothese H_0 abgelehnt werden.



# c)

cor(heroin$dose,heroin$time,method = "spearman")



# Aufgabe 17

banknote = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/banknote.dat",
                      col.names=c("nummer","laenge", "links", "rechts", "unten", "oben", "diagonal"),
                      colClasses=c("integer","numeric","numeric","numeric","numeric","numeric","numeric"));

# g)
# echte Banknoten
plot(banknote$links[1:100],banknote$laenge[1:100],xlim=c(128,132),ylim=c(213.5,217),xlab = "links",ylab = "laenge")
fit_echte = lm(banknote$laenge[1:100]~banknote$links[1:100]) 
abline(lm(banknote$laenge[1:100]~banknote$links[1:100]), col="red")

#Regressionsgleichung: 0.438*x  + 158.108 , rote Gerade im Plot und Punkte sind schwarz 

# gefaelschte Bankoten
points(banknote$links[101:200],banknote$laenge[101:200],xlab = "links",ylab = "laenge",col="blue")
fit_unechte = lm(banknote$laenge[101:200]~banknote$links[101:200]) 
abline(lm(banknote$laenge[101:200]~banknote$links[101:200]), col="yellow")

#Regressionsgleichung: 0.484*x  + 151.696 , gelbe Gerade im Plot und Punkte sind blau 



# h)

# echte Banknoten

t.test(banknote$oben[1:100]-banknote$unten[1:100])

# da t = 15.967 > tkrit = 1.970  , -t = -15.967 < -tkrit = -1.970 und p-Wert < 2.2e-16 < 0.05 muss  die
# Nullhyptothese (die besagt, dass die beiden verbundenen Stichproben gleiche Erwartungswerte haben) abgelehnt werden.

# gefaelschte banknoten

t.test(banknote$oben[101:200]-banknote$unten[101:200])

# da t = 3.693 > tkrit = 1.970  , -t = -3.693 < -tkrit = -1.970 und p-Wert < 0.0003631 < 0.05 muss  die
# Nullhyptothese (die besagt, dass die beiden verbundenen Stichproben gleiche Erwartungswerte haben) abgelehnt werden.




# Aufgabe 18

ttest = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/ttest.dat",
                   col.names=c("nr","a", "b", "ba"),
                   colClasses=c("integer","numeric","numeric","numeric"),nrows=10);

ttest_stat = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/ttest.dat",
                        col.names=c("a", "b", "ba"),
                        colClasses=c("numeric","numeric","numeric"),skip=10,nrows=2);

ttest$ba[5] = ttest$b - ttest$a
ttest_stat$ba[1] = mean(ttest$ba)
ttest_stat$ba[2] = sd(ttest$ba)

# a)



# Testproblem 1: H_0: mu_1 = mu_2  gegen H_1: mu_1 != mu_2 , wobei mu_1 der Erwartungswert von Medikament a ist und 
# mu_2 der Erwartungswert von Medikament b ist ist

t.test(ttest$a-ttest$b)
t_krit <- qt(0.025,length(ttest$a)-1)
t_krit
# H_0 muss abgelehnt werden, da der p-Wert = 0.001166 < 0.05 ist und t = -4.6716 < -2.2621 = t_krit
# Bedeutet die Erwartungswerte beider verbundener Stichproben sind nicht gleich.



# Testproblem 2: Test der Variable ba auf bestimmten Mittelwert
# H_0: mu >= mu_0 = 2  gegen H_1: mu < mu_0 
t_krit <- qt(1-0.05,length(ttest$ba)-1)
t_krit
t.test(ttest$ba,mu=2,alternative = "less")

# H_0 wird angenommen, da der p-Wert=0.7922 > 0.05   und t = -0.85347 > t_krit = -1.83311


# Testproblem 3: Test der Variable a auf mittelwert mu_0=1
# H_0:  mu <= mu_0 = 0.5  gegen H_1: mu > mu_0
t_krit <- qt(0.05, length(ttest$a)-1)
t_krit
t.test(ttest$a,mu=-2,alternative = "greater")

# H_0 wird angenommen, da der p-Wert=0.2885 > 0.05   und t = 0.27234 > t_krit = -1.83311

# b)

#Teststatistik von Testproblem 1

data <- ttest$a-ttest$b
n <- length(data)
m <- 0
mu <- mean(data)
s <- sd(data)
t_krit_left <- qt(0.025,n-1)
t_krit_right <- qt(1-0.025,n-1)
t_hat_right <- abs(mu-m)/(s/sqrt(n)) #Berechnung der Teststatistik
t_hat_left <- -t_hat_right
t_hat_left
t_krit_left
t_krit_right
pt(t_hat_left,n-1,lower.tail = TRUE)+pt(t_hat_right,n-1,lower.tail = FALSE) #Berechnung des p-Wertes


# c)

# Diese Funktion nimmt als Parameter eine Stichprobe , einen zu testenden Erwartungswert und einen Wert Alpha 
# Als Rückgabe erhät man eine Liste mit folgenden Werten [p-Wert,untere Grenze des Konfidenzintervalls, obere Grenze des Konfidenzintervalls]
ttest <- function(x,mu,alpha){
      
    x_mean <- mean(x)
    x_sd <- sd(x)
    x_len <- length(x)
    left <- qt(alpha/2,x_len-1)
    right <- qt(alpha/2,x_len-1)
    conf_left <- x_mean-abs((x_sd/sqrt(x_len))*left)
    conf_right <- x_mean+abs((x_sd/sqrt(x_len))*right)
    
    t_hat_right <- abs(x_mean-mu)/(x_sd/sqrt(x_len))
    t_hat_left <- -t_hat_right
    p_value = pt(t_hat_left,x_len-1,lower.tail = TRUE)+pt(t_hat_right,x_len-1,lower.tail = FALSE) 
    
    results <- c(p_value,conf_left,conf_right)
    return(results)
  
    
}


# d)



# Medikament A

med_A <- ttest$a

med_A_m <- mean(med_A)
med_A_sd <- sd(med_A)
med_len <- length(med_A)

error <- qt(0.975,df=med_len-1)*med_A_sd/sqrt(med_len)
left <- med_A_m - error
right <- med_A_m + error

left # untere Grenze des Konfidenzintervalls
right # obere Grenze des Konfidenzintervalls


# Medikament B

med_B <- ttest$b

med_B_m <- mean(med_B)
med_B_sd <- sd(med_B)
med_len <- length(med_B)

error <- qt(0.975,df=med_len-1)*med_B_sd/sqrt(med_len)
left <- med_B_m - error
right <- med_B_m + error

left # untere Grenze des Konfidenzintervalls
right # obere Grenze des Konfidenzintervalls



# e)
#Interpretation:

# zu Medikament A:

# Medikament hat ein Konfidenzintervall von -0.669016 bis 1.989016, was bedeutet, dass mit 95-prozentiger Wahrscheinlichkeit 
# eine Stichprobe mit dem Umfang 10 einen mittleren Zugewinn an Schlafdauer in diesem Bereich hat. 
# Da auch Mittelwerte im negativen Bereich möglich sind (siehe untere Grenze), würde ich dieses Medikament als nicht
# sehr zuverlässig einschätzen.


# zu Medikament B

# Medikament B hat ein Konfidenzintervall von 0.8976775 bis 3.762322, was bedeutet, dass mit 95-prozentiger Wahrscheinlichkeit
# eine Stichprobe mit dem Umfang 10 einen mittleren Zugewinn an Schlafdauer in diesem Bereich hat.
# Da das Konfidenzintervall nur positive Werte hat , würde ich dieses Medikament als zuverlässig einschätzen.
# Somit kann man auf Basis dieses statistischen Verfahren sagen, dass Medikament B sehr wohl Einfluss auf den Schlaf hat.
# Außerdem ist Medikament B besser als Medikament A.

