install.packages("ggplot2")

#problem 1(Implemented with interpretation A)
library(ggplot2)
data(midwest)
midwest_data<-as.data.frame(midwest)
midwest_data$popprof<-with(midwest_data,percprof*popadults/100)
midwest_prof<-midwest_data[c("state", "popadults","percprof","popprof")]
midwest_prof_by_state<-aggregate(. ~ state, midwest_prof, sum)
#calculate the percentatge
midwest_prof_by_state$percprof<-with(midwest_prof_by_state,100*popprof/popadults )
#plot the percentage of professionals by state with bar chart
ggplot(data=midwest_prof_by_state, aes(x=state, y=percprof, fill=state)) +geom_bar(stat="identity")

#problem 2(Implemented with interpretation A)
library(reshape2)
midwest_data<-as.data.frame(midwest)
midwest_data$pophsd<-with(midwest_data,perchsd*popadults/100)
midwest_data$popcollege<-with(midwest_data,percollege*popadults/100)
midwest_hsd_college<-midwest_data[c("state", "popadults","pophsd","popcollege")]
midwest_hsd_college_by_state<-aggregate(. ~ state, midwest_hsd_college, sum)
midwest_hsd_college_by_state$perchsd<-with(midwest_hsd_college_by_state,100*pophsd/popadults )
midwest_hsd_college_by_state$perccollege<-with(midwest_hsd_college_by_state,100*popcollege/popadults )
midwest_hsd_college_by_state<-midwest_hsd_college_by_state[c("state", "perchsd","perccollege")]
df <- melt(midwest_hsd_college_by_state, id.vars='state')
#plot the percentage of high school degree by state with barchart
ggplot(data=midwest_hsd_college_by_state, aes(x=state, y=perchsd, fill=state)) +geom_bar(stat="identity")
#plot the percentage of college degree by state with barchart
ggplot(data=midwest_hsd_college_by_state, aes(x=state, y=perccollege, fill=state)) +geom_bar(stat="identity")
#plot the percentage of high school and percentage of college at same time
ggplot(data=df, aes(x=state, y=value, fill=variable)) +geom_bar(stat="identity", position=position_dodge())

#problem 4(Implemented with interpretation A)
N<-4000
n<-c()
pdf<-c()
jpeg<-c()
png<-c()
eps<-c()
for(i in c(1:N)){
x<-runif(i,0,1)
y<-runif(i,0,1)
runif_df<- data.frame(x,y)
n[i]<-i
p <- ggplot(runif_df, aes(x, y))
p + geom_point()
#save file and read file size
ggsave("length-hist.pdf", plot = last_plot(), device = "pdf")
pdf[i]<-file.size("length-hist.pdf")
ggsave("length-hist.jpeg", plot = last_plot(), device ="jpeg")
jpeg[i]<-file.size("length-hist.jpeg")
ggsave("length-hist.png", plot = last_plot(), device ="png")
png[i]<-file.size("length-hist.png")
ggsave("length-hist.eps", plot = last_plot(), device = "eps")
eps[i]<-file.size("length-hist.eps")
}
size_df<-data.frame(n,pdf,png,eps,jpeg)
size_df_melt<- melt(size_df, id.vars="n", value.name="value", variable.name="Year")
ggplot(size_df, aes(x = n)) + 
  geom_line(aes(y = pdf), colour="blue") + 
  geom_line(aes(y = png), colour = "grey") + 
  geom_line(aes(y = eps), colour="red") + 
  geom_line(aes(y = jpeg), colour = "yellow") + 
  ylab(label="file size") + 
  xlab("N")



#problem 5
library(ggplot2)
data(diamonds)
diamonds_data<-as.data.frame(diamonds)
ggplot(diamonds_data, aes(color))+geom_bar()
qplot(price, data=diamonds_data, geom="histogram", binwidth=40)
qplot(carat, data=diamonds_data, geom="histogram", binwidth=0.02)
#plot the three way relationship between carat, color and price
ggplot(data=diamonds_data, aes(x=carat, y=price, group=color, colour=color)) +geom_line() +geom_point()
#plot the relationshio between color and price
ggplot(diamonds_data, aes(x=color, y=price)) + stat_summary(fun.y="mean", geom="bar")
#plot the relationshio between carat and price
ggplot(diamonds_data, aes(x=carat, y=price)) + stat_summary(fun.y="mean", geom="bar")
#plot the relationshio between carat and color
ggplot(diamonds_data, aes(x=color, y=carat)) + stat_summary(fun.y="mean", geom="bar")




