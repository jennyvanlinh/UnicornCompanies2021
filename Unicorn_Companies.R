# remove setwd for privacy reason
Unicorn_Companies = read.csv("Unicorn_Companies.csv")
library(tidyverse)

summary(Unicorn_Companies)

#only consider entry without None
Unicorn_Companies[c(1:9, 11:12)]<-na_if(Unicorn_Companies[c(1:9, 11:12)], "None")
Unicorn_Companies<-drop_na(Unicorn_Companies)

summary(Unicorn_Companies)

Unicorn_Companies$Founded.Year<-as.numeric(Unicorn_Companies$Founded.Year)
Unicorn_Companies$Deal.Terms<-as.numeric(Unicorn_Companies$Deal.Terms)
Unicorn_Companies$Investors.Count<-as.numeric(Unicorn_Companies$Investors.Count)

len<-nchar(Unicorn_Companies$Total.Raised)
Unicorn_Companies$Letter<-substring(Unicorn_Companies$Total.Raised, len, len)

#replace $ in string
Unicorn_Companies$Total.Raised<-
  gsub("[^[:alnum:].]", "", Unicorn_Companies$Total.Raised)
#replace M, B and K in string
Unicorn_Companies$Total.Raised<-
  gsub("[MBK]", "", Unicorn_Companies$Total.Raised)
#turn total raise to double
Unicorn_Companies$Total.Raised<-as.double(Unicorn_Companies$Total.Raised)

count<-0
for (val in Unicorn_Companies$Letter) {
  count=count+1
  if(val=="B"){
    Unicorn_Companies[count,9]<-Unicorn_Companies[count,9]*1000000000
  } else if(val=="M") {
    Unicorn_Companies[count,9]<-Unicorn_Companies[count,9]*100000
  } else if(val=="K") {
    Unicorn_Companies[count,9]<-Unicorn_Companies[count,9]*1000
  }
}

Unicorn_Companies %>%
  filter(Financial.Stage!="None") 
#Replace Acq with Acquired
Unicorn_Companies$Financial.Stage[Unicorn_Companies$Financial.Stage=="Acq"]<-"Acquired"

Unicorn_Companies$Letter<-NULL  
  
summary(Unicorn_Companies)

#format to 2022-M-D (because US datetime)
Unicorn_Companies$Date.Joined<-as.Date(Unicorn_Companies$Date.Joined, "%m/%d/%Y") 
#format to d/m/Y
Unicorn_Companies$Date.Joined<-format(as.Date(Unicorn_Companies$Date.Joined), "%d/%m/%Y")   


write.csv(Unicorn_Companies, file="List of Unicorn Companies.csv", row.names=TRUE)



