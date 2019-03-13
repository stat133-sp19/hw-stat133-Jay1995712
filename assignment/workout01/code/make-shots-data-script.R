#title:data-cleaning/recoding
#description:import data and recode data;finally combined 5 datasets into one
#inputs:5 datasets of NBA players in golden state warriors
#outputs: adding name and minute columns in each datasets;the combined dataset with 5 players info;summaries of each individual datasets and summary of the combined dataset
  
  
#2)Data
iguodala <- read.csv("../data/andre-iguodala.csv",stringsAsFactors = F)
green <- read.csv("../data/draymond-green.csv",stringsAsFactors = F)
durant <- read.csv("../data/kevin-durant.csv",stringsAsFactors = F)
thompson <- read.csv("../data/klay-thompson.csv",stringsAsFactors = F)
curry <- read.csv("../data/stephen-curry.csv",stringsAsFactors = F)

#3)Data Preparation
iguodala$name <- "Andre Iguodala"
iguodala <- iguodala[,c(14,1:13)]
green$name <- "Draymond Green"
green <- green[,c(14,1:13)]
durant$name <- "Kevin Durant"
durant <- durant[,c(14,1:13)]
thompson$name <- "Klay Thompson"
thompson <- thompson[,c(14,1:13)]
curry$name <- "Stephen Curry"
curry <- curry[,c(14,1:13)]

iguodala$shot_made_flag[iguodala$shot_made_flag=='n']='shot_no'
iguodala$shot_made_flag[iguodala$shot_made_flag=='y']='shot_yes'
green$shot_made_flag[green$shot_made_flag=='n']='shot_no'
green$shot_made_flag[green$shot_made_flag=='y']='shot_yes'
durant$shot_made_flag[durant$shot_made_flag=='n']='shot_no'
durant$shot_made_flag[durant$shot_made_flag=='y']='shot_yes'
thompson$shot_made_flag[thompson$shot_made_flag=='n']='shot_no'
thompson$shot_made_flag[thompson$shot_made_flag=='y']='shot_yes'
curry$shot_made_flag[curry$shot_made_flag=='n']='shot_no'
curry$shot_made_flag[curry$shot_made_flag=='y']='shot_yes'

iguodala$minute=12*iguodala$period-iguodala$minutes_remaining
green$minute=12*green$period-green$minutes_remaining
durant$minute=12*durant$period-durant$minutes_remaining
thompson$minute=12*thompson$period-thompson$minutes_remaining
curry$minute=12*curry$period-curry$minutes_remaining

sink(file = '../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()
sink(file = '../output/draymond-green-summary.txt')
summary(green)
sink()
sink(file = '../output/kevin-durant-summary.txt')
summary(durant)
sink()
sink(file = '../output/klay-thompson-summary.txt')
summary(thompson)
sink()
sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
sink()

combinded <- rbind(iguodala,green,durant,thompson,curry,make.row.names=F,stringAsFactors=F)
write.csv(x=combinded, 
file = '../data/shots-data.csv'          
)
sink(file = '../output/shots-data-summary.txt')
summary(combinded)
sink()

