#Aufgabenblatt4 "Werkzeuge der empirischen Forschung"


# Oliver Pohl, 577878, pohloliq



# Aufgabe 9

# a)

x <- seq(0,20,1)
y <- dbinom(x,20,0.1)
plot(x,y)


y <- dpois(x,lambda = 2)
points(x,y,col="red")

# Die Grafiken sehen ähnlich aus. Man kann für kleine Wahrscheinlichkeiten und große Stichproben n die Binomialverteilung durch die Poissonverteilung annähern.

#b)

#P(X=4), wenn X~Bi(20,1/6)

dbinom(4,20,1/6)


#c)
x <- seq(0,20,0.001)
plot(x,dexp(x,2),type = "l")
lines(x,dexp(x,1),col="red")
lines(x,dexp(x,0.5),col="blue")
legend(10,0.3,legend = c("lambda=2","lambda=1","lambda=0.5"),col = c("black","red","blue"),pch=c("-","-","-"),lty=c(1,2,3), ncol=1)



# d)

plot(x,dchisq(x, 9))
plot(x,dchisq(x, 1))


# e)

dnorm(-1.645,mean = 0, sd = 1)
dnorm(1.645,mean = 0, sd = 1)
dnorm(-1.96,mean = 0, sd = 1)
dnorm(1.96,mean = 0, sd = 1)
dnorm(2.33,mean = 0, sd = 1)
dnorm(-2.33,mean = 0, sd = 1)

# f)
violett <- rgb(red = 0.7,blue = 0.9, green = 0,alpha = 1,maxColorValue = 1)

x <- seq(-10,10,0.001)
plot(x,dt(x,1),type = "l",ylim = c(0,0.4))
lines(x,dt(x,2),col="yellow")
lines(x,dt(x,4),col="orange")
lines(x,dt(x,9),col=violett)
lines(x,dt(x,18),col="blue")
lines(x,dt(x,98),col="red")

# g)

qt(0.95,9)
qt(0.95,30)
qt(0.95,198)

qt(0.99,9)
qt(0.99,30)
qt(0.99,198)



#Aufgabe 10

# b)

#Daten vorbereiten

anz <- c(0,1,2,3,4,5)
ha <- c(144,91,32,11,2,0)
pferdetritte <- data.frame(anz,ha)


# Berechnung der ML-Schätzung und der Momenten-Schätzung
(1/sum(pferdetritte["ha"]))*sum(pferdetritte["ha"]*pferdetritte["anz"])


