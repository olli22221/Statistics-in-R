#Aufgabe 4

#a)

tibetan <- read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/tibetan.dat",
                     col.names=c("maxlength","maxwidth", "height", "upface", "facewidth"),
                     colClasses=c("numeric","numeric","numeric","numeric","numeric"));
tibetan$Typ <-c(rep("A",17),rep("B",15))
save(tibetan,file = "C:\\Users\\olli\\Documents\\GitHub\\Statistics-in-R\\ex2\\tibetan.RData")


#b)

tibetanA <- subset(tibetan, Typ=="A")
tibetanB <- subset(tibetan, Typ=="B")
a<-tibetanA
b<-capture.output(a)
c<-paste(b)
cat(c )


#c)
tibetan123 <- tibetan[,c("maxlength","maxwidth","height")]
tibetan45 <- tibetan[,c("upface","facewidth")]
tibetanNeu <- cbind(tibetan123,tibetan45)
print(tibetan123)
print(tibetan45)
print(tibetanNeu)

#d)

mittelwerte <-c(mean(tibetan[,"maxlength"]),mean(tibetan[,"maxwidth"]),mean(tibetan[,"height"]),mean(tibetan[,"upface"]),mean(tibetan[,"facewidth"]))
standardabweichungen <- c(sd(tibetan[,"maxlength"]),sd(tibetan[,"maxwidth"]),sd(tibetan[,"height"]),sd(tibetan[,"upface"]),sd(tibetan[,"facewidth"]))
mediane <- c(median(tibetan[,"maxlength"]),median(tibetan[,"maxwidth"]),median(tibetan[,"height"]),median(tibetan[,"upface"]),median(tibetan[,"facewidth"]))
statistiken <- rbind(mittelwerte,standardabweichungen,mediane)
colnames(statistiken) <- c("maxlength","maxwidth","height","upface","facewidth")


#Aufgabe 5a

square50 <- c(1:50)^2

print(square50)

sumsquare50 <- c(rep(0,50))
for (i in 1:length(square50)){

  sumsquare50[i] <- sum(square50[1:i])
  
}
print(sumsquare50)

#Aufgabe 5b

#P(X>6), wenn X~Bi(10,5/6)

1-pbinom(6,10,5/6,lower.tail = TRUE)

#P(X>6), wenn X~Poi(50/6)

ppois(6,50/6,lower=FALSE)

#P(X>6), wenn X~Geo(5/6)

1-pgeom(6,5/6,lower.tail = TRUE)

#P(X>6), wenn X~N(6,3^2)

1-pnorm(6,mean=6,sd=9,lower.tail = TRUE)
