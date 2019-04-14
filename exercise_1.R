banknote = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/banknote.dat",
                      col.names=c("nummer","laenge", "links", "rechts", "unten", "oben", "diagonal"),
                      colClasses=c("integer","numeric","numeric","numeric","numeric","numeric","numeric"));
save(banknote,file="D:/olli/workspace/R_Stuff/banknote.Rda")
load.Rdata(file="D:/olli/workspace/R_Stuff/banknote.Rda",objname="dat.s3")
a <- "halrs"

