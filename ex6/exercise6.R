#Aufgabenblatt6 "Werkzeuge der empirischen Forschung"


# Oliver Pohl, 577878, pohloliq


#Aufgabe 12

# d)

banknote = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/banknote.dat",
                      col.names=c("nummer","laenge", "links", "rechts", "unten", "oben", "diagonal"),
                      colClasses=c("integer","numeric","numeric","numeric","numeric","numeric","numeric"));

#echte Banknoten

hist(banknote$laenge[1:100],col = "blue", density = 10,breaks = 40,xlim = c(213.5,216),
     ylim = c(0,20),main = "Häufigkeitsverteilung der Variable laenge unechter Banknoten",xlab = "laenge der Banknote",ylab = "Anzahl");


#unechte Banknoten

hist(banknote$laenge[101:200],col = "blue", density = 10,breaks = 40,xlim = c(213.5,216.5),
     ylim = c(0,20),main = "Häufigkeitsverteilung der Variable laenge unechter Banknoten",xlab = "laenge der Banknote",ylab = "Anzahl");


# e1)

#variable oben
oldpar = par(mfrow=c(1,2))

hist(banknote$oben[1:100],col = "blue", xlab = "Variable oben",sub="echte Banknoten",main = "Histogramm",density = 10,xlim = c(7,13),ylim = c(0,35),ylab = "Anzahl")
hist(banknote$oben[101:200],col = "red",sub="unechte Banknoten",main = "Histogramm",density = 5,add=TRUE)

par(oldpar)


#variable unten
oldpar = par(mfrow=c(1,2))

hist(banknote$unten[1:100],col = "blue", xlab = "Variable unten",main = "Histogramm",density = 10,xlim = c(7,13),ylim = c(0,30),ylab = "Anzahl")
hist(banknote$unten[101:200],col = "red",main = "Histogramm",density = 5,add=TRUE)

par(oldpar)


# e2)

#variable oben

oldpar = par(mfrow=c(1,2))

hist(banknote$oben[1:100],col = "blue", xlab = "Variable oben",sub="echte Banknoten",main = "Histogramm",density = 10,xlim = c(7,13),ylim = c(0,35),ylab = "Häufigkeiten")
hist(banknote$oben[101:200],col = "red",xlab = "Variable oben", sub="unechte Banknoten",main = "Histogramm",density = 5, ylab = "Häufigkeiten",xlim = c(9,13),ylim = c(0,35))

par(oldpar)


#variable unten

oldpar = par(mfrow=c(1,2))

hist(banknote$unten[1:100],col = "blue", xlab = "Variable unten",sub="echte Banknoten",main = "Histogramm",density = 10,xlim = c(7,11),ylim = c(0,30),ylab = "Häufigkeiten")
hist(banknote$unten[101:200],col = "red",xlab = "Variable unten", sub="unechte Banknoten",main = "Histogramm",density = 5, ylab = "Häufigkeiten",xlim = c(7,13),ylim = c(0,20))




#e3)

#variable oben, echte Banknoten
oldpar = par(mfrow=c(1))


hist(banknote$oben[1:100],col = "blue", density = 10,angle = 45,xlim = c(7,13),
     ylim = c(0,35),main = "Histogramm",xlab = "variable oben",ylab = "Anzahl");

par(oldpar)
#variable oben, unechte Banknoten
hist(banknote$oben[101:200],col = "blue", density = 10,angle = -45,xlim = c(9,13),
     ylim = c(0,35),main = "Histogramm",xlab = "variable oben",ylab = "Anzahl");




#variable unten, echte Banknoten
par(oldpar)
hist(banknote$unten[1:100],col = "blue", density = 10,angle = 45,xlim = c(7,11),
     ylim = c(0,35),main = "Histogramm",xlab = "variable unten",ylab = "Anzahl");


#variable unten, unechte Banknoten
par(oldpar)
hist(banknote$unten[101:200],col = "blue", density = 10,angle = -45,xlim = c(7,13),
     ylim = c(0,20),main = "Histogramm",xlab = "variable unten",ylab = "Anzahl");



#f)

#variable oben

#echte Banknoten
hist(banknote$oben[1:100],col = "yellow",freq = F,xlim = c(7,13),ylim = c(0,0.7),ylab = "rel. Häufigkeiten")
lines(density(banknote$oben[1:100]),col="red")
curve(dnorm(x=x,mean(banknote$oben[1:100]),sd(banknote$oben[1:100])),col="blue",add = T)
legend("topright",pch=15,col = c("yellow","red","blue"),
       legend = c("Histogramm","nichtparam. DS", "parametrische DS"))

#unechte Banknoten
hist(banknote$oben[101:200],col = "yellow",freq = F,xlim = c(8,14),ylim = c(0,0.7),ylab = "rel. Häufigkeiten")
lines(density(banknote$oben[101:200]),col="red")
curve(dnorm(x=x,mean(banknote$oben[101:200]),sd(banknote$oben[101:200])),col="blue",add = T)
legend("topright",pch=15,col = c("yellow","red","blue"),
       legend = c("Histogramm","nichtparam. DS", "parametrische DS"))


#variable unten

#echte Banknoten
hist(banknote$unten[1:100],col = "yellow",freq = F,xlim = c(6,12),ylim = c(0,0.7),ylab = "rel. Häufigkeiten")
lines(density(banknote$unten[1:100]),col="red")
curve(dnorm(x=x,mean(banknote$unten[1:100]),sd(banknote$unten[1:100])),col="blue",add = T)
legend("topright",pch=15,col = c("yellow","red","blue"),
       legend = c("Histogramm","nichtparam. DS", "parametrische DS"))

#unechte Banknoten
hist(banknote$unten[101:200],col = "yellow",freq = F,xlim = c(6,14),ylim = c(0,0.5),ylab = "rel. Häufigkeiten")
lines(density(banknote$unten[101:200]),col="red")
curve(dnorm(x=x,mean(banknote$unten[101:200]),sd(banknote$unten[101:200])),col="blue",add = T)
legend("topright",pch=15,col = c("yellow","red","blue"),
       legend = c("Histogramm","nichtparam. DS", "parametrische DS"))




# Aufgabe 13

toxaemia = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/toxaemia.dat",
                      col.names=c("social","smoking", "hyperten", "proteinu"),
                      colClasses=c("integer","integer","integer","integer"));

# a)

tab_hyperten <- table(toxaemia$hyperten)
tab_proteinu <- table(toxaemia$proteinu)



# b)

tab_common <- table(toxaemia$hyperten,toxaemia$proteinu)

chisq.test(tab_common)

# die Variablen hyperten und proteinu sind abhängig,  da p-value < 2.2e-16  


