if (require(reshape2)==FALSE){
  install.packages("reshape2") }
if (require(ggplot2)==FALSE){
  install.packages("ggplot2") }
if (require(readxl)==FALSE){
  install.packages("readxl") }
library(reshape2)
library(ggplot2)
library(readxl)

# define age ranges for children, active population and retirees
child <- c("0-4","5-9","10-14")
active <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
retired <- c("65-69","70-74","75-79","80-84","85-89","90-94","95-99","100-")


# Load data downloaded from UN website.
download.file("https://population.un.org/wpp/Download/Files/2_Indicators%20(Probabilistic%20Projections)/UN_PPP2019_Output_PopulationByAge_Male.xlsx",destfile = "MPop.xlsx",mode = "wb")
download.file("https://population.un.org/wpp/Download/Files/2_Indicators%20(Probabilistic%20Projections)/UN_PPP2019_Output_PopulationByAge_Female.xlsx",destfile = "FPop.xlsx", mode = "wb")
MPop <- read_xlsx(path = "MPop.xlsx",range = "Median!A17:AC4352",col_names = TRUE,na = "...")
FPop <- read_xlsx(path = "FPop.xlsx",range = "Median!A17:AC4352",col_names = TRUE,na = "...")
MPop$GND <- "M"
FPop$GND <- "F"
TPop <- rbind(FPop,MPop)
TPop[,c(1:2,4:5,7)] <- NULL
TPop <- TPop[!is.na(TPop$`0-4`),]
TPop <- TPop[TPop$Type != "SDG region" & TPop$Type != "SDG subregion" & TPop$Type != "Development Group",]
TPop$Type <- factor(TPop$Type,levels = c("World","Income Group","Region","Subregion","Country/Area","Special other"))
names(TPop)[c(1,3)] <- c("Country","Year")
TPop <- TPop[order(TPop$Type,TPop$Country,TPop$Year),]

# Change country names of Bolivia, Brasil and Venezuela
TPop$Country[TPop$Country=="Bolivia (Plurinational State of)"] <- "Bolivia"
TPop$Country[TPop$Country=="Venezuela (Bolivarian Republic of)"] <- "Venezuela"


TAgg <- cbind(TPop[,c(1:3,25)],
              Pact = rowSums(TPop[,colnames(TPop)[colnames(TPop)%in%active]]),
              Pchi = rowSums(TPop[,colnames(TPop)[colnames(TPop)%in%child]]),
              Pret = rowSums(TPop[,colnames(TPop)[colnames(TPop)%in%retired]]))
TAgg$TOT <- rowSums(TAgg[,c("Pact","Pchi","Pret")])
TAgg <- aggregate(cbind(Pact=TAgg$Pact,Pchi=TAgg$Pchi,Pret=TAgg$Pret,Tot=TAgg$TOT),
                  by = list(Country=TAgg$Country,Year=TAgg$Year),FUN=sum)
TAgg$DR <- (TAgg$Pchi+TAgg$Pret)/TAgg$Pact*100

save(TPop,file = "TPop.R")
save(TAgg,file = "TAgg.R")
rm(MPop,FPop,TPop,TAgg,active,child,retired)
