#title:Shot Charts
#description: generate the shot chart using data
#inputs:dataset for each player and cobmined dataset with 5 players
#outputs:the shot charts for each player;the shot charts containing 5 players
library(ggplot2)
library(jpeg)
library(grid)
court_file <- "../images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1,"npc"),
  height = unit(1,"npc")
)
#shot charts of each player
iguodala_shot_chart <- ggplot(data = iguodala)+annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+
  ggtitle('Shot Chart:Andre Iguodala(2016 season)')+
  theme_minimal()
ggsave(filename = '../images/andre-iguodala-shot-chart.pdf',width = 6.5,height = 5)
unlink(iguodala_shot_chart)

durant_shot_chart <- ggplot(data = durant)+annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+
  ggtitle('Shot Chart:Kevin Durant(2016 season)')+
  theme_minimal()
ggsave(filename = '../images/kevin-durant-shot-chart.pdf',width = 6.5,height = 5)
unlink(durant_shot_chart)

green_shot_chart <- ggplot(data = green)+annotation_custom(court_image,-250,250,-50,420)+
geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+
ggtitle('Shot Chart:Draymond Green(2016 season)')+
theme_minimal()
ggsave(filename = '../images/draymond-green-shot-chart.pdf',width = 6.5,height = 5)
unlink(green_shot_chart)

klay_shot_chart <- ggplot(data = thompson)+annotation_custom(court_image,-250,250,-50,420)+
geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+
ggtitle('Shot Chart:Klay Thompson(2016 season)')+
theme_minimal()
ggsave(filename = '../images/klay-thompson-shot-chart.pdf',width = 6.5,height = 5)
unlink(klay_shot_chart)

curry_shot_chart <- ggplot(data = curry)+annotation_custom(court_image,-250,250,-50,420)+
geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+
ggtitle('Shot Chart:Stehpen Curry(2016 season)')+
theme_minimal()
ggsave(filename = '../images/stehpen-curry-shot-chart.pdf',width = 6.5,height = 5)
unlink(curry_shot_chart)

#facetted shot chart

dat_all <- read.csv("../data/shots-data.csv",stringsAsFactors = F)
facetted_shot_chart <- ggplot(data = dat_all)+annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+
  ggtitle('Shot Chart:GSW (2016 season)')+
  theme_minimal()+
  facet_wrap(~name)
ggsave(filename = '../images/gsw-shot-charts.pdf',width = 8,height = 7)
ggsave(filename = '../images/gsw-shot-charts.png',width = 8,height = 7)
unlink(facetted_shot_chart)

