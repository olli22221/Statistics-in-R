
#Aufgabenblatt3 "Werkzeuge der empirischen Forschung"


# Oliver Pohl, 577878, pohloliq




#Aufgabe 6

name_stud1 <- c("Theodor", "Sieglinde", "Josefine", "Engelbert","Egbert")
geschl <- c("m","w","w","m",NA)
Student1 <- data.frame(name_stud1,geschl)
Student1 <- Student1[order(Student1$name_stud1),]
colnames(Student1) <- c("Name", "Geschlecht")


name_stud2 <- c("Theodor","Josefine", "Sieglinde","Engelbert","Egbert")
fach <- c("Inf","Geo","Med",NA,NA)
Student2 <- data.frame(name_stud2,fach)
Student2 <- Student2[order(Student2$name_stud2),]
colnames(Student2) <- c("Name", "Fach")

name_stud3 <- c("Engelbert", "Sieglinde", "Josefine", "Egbert","Theodor")
alter <- c(22,27,19,23,NA)
Student3 <- data.frame(name_stud3,alter)
Student3 <- Student3[order(Student3$name_stud3),]
colnames(Student3) <- c("Name", "Alter")


student <- cbind(Student1,Student2,Student3)
student <- student[, -c(3,5)]

#Aufgabe 7

# b)

#P(X>6), wenn X~Poi(50/6)

dpois(1,0.01)


# c)

#P(X>6), wenn X~Poi(50/6)

1-ppois(2,0.01,lower=TRUE)



#Aufgabe 8

#a)

phyper(2,16,384,25,lower.tail=TRUE)


#b)

pbinom(2,25,16/400,lower.tail = TRUE)



