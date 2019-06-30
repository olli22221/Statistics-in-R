#Aufgabenblatt 11 "Werkzeuge der empirischen Forschung"


# Oliver Pohl, 577878, pohloliq



# Aufgabe 33



heroin1 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/heroin.dat",
                     col.names=c("id","clinic", "status", "time", "prison", "dose",rep("NULL", 6)),
                     colClasses=c("integer","integer","integer","integer","integer","integer"));
heroin1 = heroin1[,1:6]
heroin2 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/heroin.dat",
                     col.names=c(rep("NULL", 6),"id","clinic", "status", "time", "prison", "dose"),
                     colClasses=c("integer","integer","integer","integer","integer","integer"));
heroin2 = heroin2[,7:12]
heroin = rbind(heroin1,heroin2)


sign.test = function(data,mu=0,...){
  
  sig = sign(data-mu);
  vplus = length(sig[sig==1]);
  ties = length(sig[sig==0]);
  n = length(data) - ties;
  binom.test(vplus,n,p=0.5,...);
  
}

# a)

# Vorzeichentest Klinik 1

clinic_1 <- heroin[heroin$clinic == 1,]
sign.test(data = clinic_1$time,mu=500)

# Der p-Wert beträgt 9.466e-06 und ist kleiner als 0.05. Daher wird die Nullhypothese abgelehnt.

# Vorzeichentest Klinik 2

clinic_2 <- heroin[heroin$clinic == 2,]
sign.test(data = clinic_2$time,mu=500)

# Der p-Wert beträgt 0.8176 und ist größer als 0.05. Daher wird die Nullhypothese angenommen.

# Vorzeichen-Rangtest Klinik 1

wilcox.test(clinic_1$time,mu=500)

# Der p-Wert beträgt 0.0000.. und ist kleiner als 0.05. Daher wird die Nullhypothese abgelehnt.

# Vorzeichen-Rangtest Klinik 2

wilcox.test(clinic_2$time,mu=500)

# Der p-Wert beträgt 0.6196 und ist größer als 0.05. Daher wird die Nullhypothese angenommen.


# b)

# VorzeichenTest

sign.test(heroin$dose,mu=55)

# Der p-Wert beträgt 0.000.. und ist kleiner als 0.05. Daher wird die Nullhypothese abgelehnt.


# Vorzeichen-Rangtest

wilcox.test(heroin$dose,mu=55,alternative = "two.sided")

# Der p-Wert beträgt 0.000.. und ist kleiner als 0.05. Daher wird die Nullhypothese abgelehnt.




# Aufgabe 34

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

plot(var_calcium,var_mortal)
lm(var_mortal~var_calcium)
curve(-3.226*x+1676.356,add = TRUE,col="red")

cor.test(var_calcium,var_mortal)
# Der Korrelationskoeffizient beträgt -0.6548 und somit besteht eine mittelstarke lineare Abhängigkeit zwischen
# den Variablen Calcium und Mortal




# Aufgabe 35

tibetan = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/tibetan.dat",
                     col.names=c("maxlength","maxwidth", "height", "upface", "facewidth"),
                     colClasses=c("numeric","numeric","numeric","numeric","numeric"));

plot(tibetan$maxlength,tibetan$maxwidth)
lm(tibetan$maxwidth~tibetan$maxlength)
curve(0.07958*x+124.74262,add = TRUE,col="red")

a <- predict(lm(tibetan$maxwidth~tibetan$maxlength),interval = "confidence")
