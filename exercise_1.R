
#Aufgabenblatt1 "Werkzeuge der empirischen Forschung"


# Oliver Pohl, 577878, pohloliq




#Aufgabe1

tibetan = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/tibetan.dat",
                      col.names=c("maxlength","maxwidth", "height", "upface", "facewidth"),
                      colClasses=c("numeric","numeric","numeric","numeric","numeric"));
computer = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/computer.dat",
                      col.names=c("cycletime","minmem", "maxmem", "cachesize", "minchan", "maxchan", "relperf","estrelperf"),
                      colClasses=c("integer","numeric","numeric","numeric","numeric","numeric","numeric"));
darwin = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/darwin.dat",
                      col.names=c("nummer", "heightk", "heights"),
                      colClasses=c("integer","numeric","numeric"));
ttest = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/ttest.dat",
                      col.names=c("nr","a", "b", "ba"),
                      colClasses=c("integer","numeric","numeric","numeric"),nrows=10);

ttest_stat = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/ttest.dat",
                        col.names=c("a", "b", "ba"),
                        colClasses=c("numeric","numeric","numeric"),skip=10,nrows=2);
banknote = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/banknote.dat",
                      col.names=c("nummer","laenge", "links", "rechts", "unten", "oben", "diagonal"),
                      colClasses=c("integer","numeric","numeric","numeric","numeric","numeric","numeric"));
toxaemia = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/toxaemia.dat",
                      col.names=c("social","smoking", "hyperten", "proteinu"),
                      colClasses=c("integer","integer","integer","integer"));
skull = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/skull.dat",
                      col.names=c("gr","mb", "bh", "bl", "nh"),
                      colClasses=c("integer","integer","integer","integer","integer"));
synchro = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/synchro.dat",
                      col.names=c("wettkaempfer","r1", "r2", "r3", "r4", "r5"),
                      colClasses=c("integer","numeric","numeric","numeric","numeric","numeric"));
water1 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/water.dat",
                      col.names=c("NULL","mortal","calcium",rep("NULL", 3)),
                      colClasses=c("integer","integer"),nrows=30);
water1 = water1[,2:3]
water4 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/water.dat",
                    col.names=c(rep("NULL", 4),"mortal","calcium"),
                    colClasses=c("integer","integer"),nrows=30);
water2 = water4[,5:6]
water_lastrow = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/water.dat",
                           col.names=c("NULL","mortal","calcium"),colClasses=c("integer","integer"),skip=30);

water3 = water_lastrow[,2:3]                         
water = rbind(water1,water2)
water = rbind(water,water3)
heroin1 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/heroin.dat",
                      col.names=c("id","clinic", "status", "time", "prison", "dose",rep("NULL", 6)),
                      colClasses=c("integer","integer","integer","integer","integer","integer"));
heroin1 = heroin1[,1:6]
heroin2 = read.table(file="http://www2.informatik.hu-berlin.de/~koessler/SAS_Kurs/Kursdaten/heroin.dat",
                     col.names=c(rep("NULL", 6),"id","clinic", "status", "time", "prison", "dose"),
                     colClasses=c("integer","integer","integer","integer","integer","integer"));
heroin2 = heroin2[,7:12]
heroin = rbind(heroin1,heroin2)
rm(heroin1,heroin2,water1,water2,water3,water4,water_lastrow)

save(banknote,file="D:/olli/workspace/R_Stuff/banknote.Rda")




#Aufgabe2

vectorlength <- function(vec){
  
  return(sqrt(sum(vec^2)))
}



#Aufgabe3

x<-c(-1,2,-1,3,4,-8)
y<-c(5,0,3,-2,-8,-1)
q<-x/y
quadrant<-c(0,0,0,0,0,0)

for (i in 1:length(x)){
  
    if (x[i]>0 & y[i]>0){
      quadrant[i]=1;
    }
  else if(x[i]<0 & y[i]>0){
    quadrant[i]=2;
  }
  else if(x[i]<0 & y[i]<0){
    quadrant[i]=3;
  }
  else if(x[i]>0 & y[i]<0){
    quadrant[i]=4;
  }
  else{
    quadrant[i]="NA";
  }
  
}

dataframe <- data.frame(x,y,q,quadrant)
colnames(dataframe) <- c("x","y","x/y","Quadrant")
dataframe <- dataframe[order(dataframe$Quadrant),]

