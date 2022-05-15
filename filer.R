#load persian data
install.packages("dplyr")
install.packages("data.table")
Sys.setlocale(locale = "persian")
basic2 <- read.table("basic2.txt", sep="\t", header = TRUE, fileEncoding = "UCS-2LE")
View(basic2)
library(data.table)
#merge data
total1<-merge(x=grades11.UID, y = basicinfo, all.x = TRUE)
View(total1)
Sys.setlocale(locale = "persian")
basic3 <- read.table("basic3.txt", sep="\t", header = T, fileEncoding = "UCS-2LE")
View(basic3)
total1<-merge(x=grades11.UID, y = basic3, all.x = TRUE)
View(total1)
#sort data
ord<-order(total1$Term)
ord
total1.term<-total1[ord,]
View(total1.term)
######IMPORT FINAL#####
Sys.setlocale(locale = "persian")
Final<- read.table("Final.txt", sep="\t", header = TRUE, fileEncoding = "UCS-2LE")
View(Final)
######################import basic information 94-1-98-1
Sys.setlocale(locale = "persian")
basic2new<- read.table("basic2new.txt", sep="\t", header = TRUE, fileEncoding = "UCS-2LE")
View(basic2new)
#delete duplicate row name
library(dplyr)
Final %>% 
  distinct(DARS, .keep_all = TRUE)
View(Final)
dim(Final)
Final = Final[!duplicated(Final$group),]
###Dunmmy#######
install.packages("fastDummies")
library(fastDummies)
Final$Gender<- ifelse(Final$Gender== "F", 0,1)
Final$Boomi<-ifelse(Final$Boomi=="YES", 0,1)
Finacl$Married<- ifelse(Finalc$Married== "YES", 0,1)
Final$Gender=as.factor(Final$Gender)
Final$Gender=relevel(Final$Gender,ref = "0")
Final$Boomi=as.factor(Final$Boomi)
Final$Boomi=relevel(Final$Boomi,ref = "1")
#changing score class##############
class(Final$Score)
Final$Score <-gsub('/','.',Final$Score)
Final$Score <-as.numeric(Final$Score)
#group id & dars###
library(dplyr)
Final$group1 <- group_by(Final,semesterID,UID)
Final$group1 <- group_indices(Final, semesterID, UID)
class(Final$group)
Final$group <-as.character(Final$group)
View(Final$group)

##########writexlsx#######
library("writexl")
write_xlsx(Final,"C:\\users\\sajedeh\\Desktop\\Final.xlsx")
#########boxplot###??????????########
library(ggplot2)
boxplot(Score~as.factor(Credit),
        data=Final,
        main="?????????? ???? ???????? ?????? ?????????? ?????????? ?????? ????????????",
        xlab="?????????? ????????????",
        ylab="??????????",
        col="orange",
        border="brown"
)
######boxplot####????????######
boxplot(Score~as.factor(Boomi),
        data=Final,
        main="?????????? ???? ???????? ?????? ?????????? ???????? ?? ?????? ????????",
        xlab="????????",
        ylab="??????????",
        col="green",
        border="brown"
)
#########boxplot#########??????????###############
boxplot(Score~as.factor(Gender),
        data=Final,
        main="?????????? ???? ???????? ?????? ?????????? ??????????",
        xlab="??????????",
        ylab="??????????",
        col="yellow",
        border="brown"
)
################pie chart#########
# Basic piechart???????? ?????? ????????????######col=rainbow(length(lbls))
mytable <- table(basic$??????.????????.????????????)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
library(RColorBrewer)
myPalette <- brewer.pal(3, "Set1") 
pie(mytable, labels = lbls, col=myPalette, main="???????? ?????? ????????????") 
#############pie chart ????????#####
mytable <- table(basic$????????)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pct <- round(mytable/sum(mytable)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
library(RColorBrewer)
myPalette <- brewer.pal(4, "Set4") 
pie(mytable, labels = lbls, col=myPalette, main="???????? ?????? ????????????") 

############????????####pie
mytable <- table(basic$gender)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pct <- round(mytable/sum(mytable)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
library(RColorBrewer)
myPalette <- brewer.pal(3, "Set3") 
pie(mytable, labels = lbls, col=myPalette, main="??????????", border="green" ) 
###########plot3d#############
library(plotrix)
mytable <- table(basic$??????.??????????)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pct <- round(mytable/sum(mytable)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie3D(mytable,labels=lbls,explode=0.1,main="?????????? ????",radius = 0.8)
##################################
mytable <- table(Final$classType)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pct <- round(mytable/sum(mytable)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
library(RColorBrewer)
myPalette <- brewer.pal(3, "Set2") 
pie(mytable, labels = lbls, col=myPalette, main="????????", border="white" ) 
###########ggplot2########
Give the chart file a name.
png(file = "boxplot_with_notch.png")

# Plot the chart.
Give the chart file a name.
png(file = "boxplot_with_notch.png")

# Plot the chart.
boxplot(Score ~Credit, data = Final, 
        xlab = "?????????? ????",
        ylab = "??????????", 
        pars = list(boxwex = 0.8, staplewex = 1,outwex = 0.5),
        range=1,
        main = "?????????? ???? ?????????? ?????? ????????????",
        notch = TRUE, 
        varwidth = TRUE,
        col = c("green","yellow","purple")
)

######3box plot#########geom_bar
library(ggplot2)
library(plotly)
library(plotly)
set.seed(051020)
Final <- data.frame(cond = factor(rep(c("Gender","Score"), each=10)),rating = c(rnorm(20),rnorm(20, mean=5)))
+coord_cartesian(ylim=c(0,20))
p <- ggplot(Final, aes(x=Gender, y=Score, fill=connd)) + geom_boxplot()
fig <- ggplotly(p)
fig
##############boxplot###########
str(Final)#look at data
dim(Final) #number of participants number of variables
nrow(Final)#number of participant
ncol(Final) #number of variables
Final$Gender<-as.factor(Final$Gender)
Finalc$Credit<as.factor(Final$Credit)

Final$Boomi<as.factor(Final$Boomi)

Final$Gender<-factor(Final$Gender,
                         levels=c(0,1),
                         labels=c("????","??????"))

Finalc$Credit<-factor(Finalc$Credit,
                          levels=c(2,1,0),
                          labels=c("????????","???????????????????? ????????????","??????????????????"))

Final$Boomi<-factor(Final$Boomi,
                          levels=c(0,1),
                          labels=c("????????" ,"?????? ????????"))
str(Final)
with(Final, summary(Gender))
GenderPlot1 = ggplot(Final, aes(x = Gender, y = Score,fill=Gender)) + geom_boxplot()+ scale_fill_brewer(palette = "Set1")
GenderPlot1
CollegePlot1 = ggplot(Final, aes(x = Boomi, y = Score,fill=Boomi)) + geom_boxplot()+ scale_fill_brewer(palette = "Set1")
CollegePlot1
CollegePlot1 = ggplot(Finalc, aes(x = Credit, y = Score,fill=Credit)) + geom_boxplot()+ scale_fill_brewer(palette = "Set1")
CollegePlot1
#################rename credit########
Final$Credit[Final$Credit=="Isargaran"]<-"0"
Final$Credit[Final$Credit=="Estedadderakhshan"]<-"1"
Final$Credit[Final$Credit=="Azad"]<-"2"
View(Finalc)
##############TIME#######
pie + scale_fill_brewer("Blues") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=5)
###########UID to int#########33
library(dplyr)
basic2$UID<- 1:nrow(basic2) 
##################???????? ?????? ???????????? BARchart
library(ggplot2)
library(dplyr)
basic2new%>%
  ggplot(aes(x=??????.????????.????????????))+

  geom_bar(fill="#66C9CB",width=.5)+
  
  #theme_bw()
labs(x="???????? ?????? ????????????",y=NULL,title = "???????? ?????? ????????????")
########################???????? ?????????? ????????????##############
ggplot(data=Rotbe, aes(x=??????????, y=????????, fill=????????)) +
  geom_bar(stat="identity",width = .5)+theme_bw()
####################gg
ggplot(data=basic, aes(x=??????????, y=????????, fill=????????)) +
  geom_bar(stat="identity",width = .5)+theme_bw()
  ###??????????
library(ggplot2)
library(dplyr)
basic2new%>%
  ggplot(aes(x=????????))+
  geom_bar(fill="#071150",width = .5)+
  labs(x="??????????",y=NULL,title = "?????????? ????????????")
####
set.seed(5250)

Final <- data.frame(NUM= rep(seq(from = 1,
                                   to = 5, by = 1), 10933),
                     Score = sample(x = 0:20,
                                     size = 10933,
                                     replace = TRUE),
                     Gender = sample(c("0", "1"),
                                    size = 10933,
                                    replace = TRUE),
                     Boomi = sample(c("1", "0"),
                                    size = 10933,
                                    replace = TRUE)
)

Finalframe <- within(Final, {
  NUM<-factor(NUM)
  Gender<- factor(Gender)
  Boomi <- factor(Boomi)
})

Finalframe <- Final[order(Final$NUM), ]
head(Finalframe)

aov2 <- with(Finalframe,
                   aov(Score ~Gender * Boomi+
                         Error(Score / (Gender * Boomi)))
)
summary(aov2)
#############aov############
install.packages("car")
library(car)
Finalf <- within(Finalc, {
  NUM<-factor(NUM)
  Gender<- factor(Gender)
  Credit<factor(Credit)
  Boomi <- factor(Boomi)
})

my_anova <- aov(Score ~ Gender +Boomi, data = Finalf)
Anova(my_anova, type = "III")
summary(my_anova)
summary(aov1)
################Average score per term for each UID##########
Final$group1 <- group_indices(Final,semesterID,UID)
Final$group2<-group_indices(Final,semesterID,NUM)
Final$Average <- with(Final, new/unit)
library(dplyr)
Finals <- Final%>% mutate(Multiply = unitNumber * Score)
Final<- transform(Final, new = unitNumber * Score)
aggregate(x = Final$multiply,           
          by = list(Final$group1),            
          FUN = mean)                           
Final<-aggregate(Final$unitNumber, by=list(N=Final$group2), FUN=sum)
View(Final)
Finalag<-aggregate(Final$unitNumber, by=list(unum=Final$group2), FUN=sum)
################################################################
library(dplyr)
Final[!duplicated(Final$semesterID,Final$UID)
Final[!duplicated(Final[c("UID", "semesterID")]),]
View(Final)     
Final <- distinct(Final, semesterID, UID, .keep_all = TRUE)
View(Final)
###############government expenditure#############
Data1$Year <- as.character(Data1$Year)
Data1$Year <-as.Date(Data1$Year,format("%Y"))
typeof(Data1$Year)# Plot
Data1%>%
  tail(10) %>%
  ggplot( aes(x=Year, y=Value)) +
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +                                           
  theme() +
  ggtitle("Government Expenditure On Education,Total (% Of GDP)")
##########################
library(plot3D)
library(plotrix)
pie3D(basic2$??????????.????????, labels = lbls,explode=0.1,main="????????")                                       
mytable <- table(basic2$??????????.????????)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pct <- round(mytable/sum(mytable)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="")
pie3D(mytable, labels = lbls,explode=0.5,main="????????")
##################
mytable <- table(basic$??????????.????????)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
library(RColorBrewer)
myPalette <- brewer.pal(3, "Set2") 
pie3D(mytable,labels = lbls,explode = 0.1, col= rainbow(length(lbls)),radius = 0.9, main="?????????? ????????")
######################
mytable <- table(basic$??????.????????.????????????)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
library(RColorBrewer)
myPalette <- brewer.pal(3, "Set1") 
pie(mytable, labels = lbls, col=myPalette, main=
####################################
library(plotrix)
mytable <- table(basic$??????????.????????)
slices <- mytable
lbls<-mytable
lbls <- paste(names(mytable), "\n", mytable, sep="")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct) # add percents to labels
#lbls <- paste(lbls,"%",sep="") # ad % to label
lbls = paste(lbls,pct, "%")
library(RColorBrewer)
myPalette <- brewer.pal(3, "Set3") 
pie3D(mytable,labels = lbls,explode = 0., col= myPalette,radius =0.9, main="?????????? ????????")
###############################
#pie chart 
library(ggplot2)
pie_chart_df_ex <- data.frame(Category = c("????", "??????"), "freq" = c(189,302))
ggplot(pie_chart_df_ex, aes (x="", y = freq, fill = factor(Category))) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = paste(round(freq / sum(freq) * 100, 1), "%"), x = 1.3),
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "??????????",
       x = NULL,
       y = NULL,
       title = "??????????") + 
  coord_polar("y")
#####################################
library(ggplot2)

pie_chart_df_ex <- data.frame(Category = c("?????????????? ?????? ????????????", "?????????????????? ??...","????????"), "freq" = c(24,56,435))

ggplot(pie_chart_df_ex, aes (x="", y = freq, fill = factor(Category))) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = paste(round(freq / sum(freq) * 100, 1), "%"), x = 1.3),
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "??????????",
       x = NULL,
       y = NULL,
       title = "?????????? ?????? ????????????") + 
  coord_polar("y")
############################
library(ggplot2)
pie_chart_df_ex <- data.frame(Category = c("????????","?????? ????????"), "freq" = c(287,204))
ggplot(pie_chart_df_ex, aes (x="", y = freq, fill = factor(Category))) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = paste(round(freq / sum(freq) * 100, 1), "%"), x = 1.3),
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "??????????",
       x = NULL,
       y = NULL,
       title = "???????? ?? ?????? ????????") + 
  coord_polar("y")
#######################pie 3d tahahol
library(plotrix)  
x <- c(434,9,48)  
labels <- c("???????? ??????????????","????????","??????????")  
pie_percent<- round(100*x/sum(x), 1)  
pie3D(x, labels = pie_percent, main = "?????????? ????????",border="blue", col=hcl.colors(length(x),"Berlin"))
legend("topright", c("????????","???????? ??????????????","??????????"), cex = 0.8,  fill =  hcl.colors(length(x),"Berlin"))#rainbow(length(x)))  
###################3333
library(plotrix)  
x <- c(384,107,9,6)  
labels <- c("????????","????????????-?????????? ??????????","??????????","????????????")
pie_percent<- round(100*x/sum(x), 1)  
pie3D(x, labels = pie_percent, main = "???????? ?????? ????????????",border="white", col=hcl.colors(length(x),"blue-red"))
legend("topright", c("???????????? ","??????????","????????????-?????????? ??????????","????????"), cex = 0.8,  fill =  hcl.colors(length(x),"blue-red"))#rainbow(length(x)))  
###################################################
Final %>%
  mutate(name = fct_reorder(name, val)) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# Reverse side
data %>%
  mutate(name = fct_reorder(name, desc(val))) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
########################################33
ggplot(tg, aes(x = dose, y = length, colour = supp)) +
  geom_line()
##########################?????? ???????? ???????? ???????? ???? ???? ?????????? ???? ???? ???????? ??????????.
datanew<-data2[!(data2$Year >="2012"),]
########################33
library(ggplot2)
Final <- ggplot() + geom_line(aes(y = Score, x =semesterID, colour = Score),
                           data=Final, stat="identity")
Final
################################
library(dplyr)
data2 %>% 
  mutate(Year = na_if(Year,2013))
##########################
install.packages("naniar")
library(naniar)
replace_with_na_all(data = data2,
                    condition = ~Year >= 2014)
############################################
library(ggplot2)
ggplot(df, aes(x=??????.??????, y=??????????, fill=??????.??????)) + 
  geom_bar(stat="identity") +
  ggtitle(" ?????????? ????????") +
  xlab("?????? ??????") +
  ylab("??????????")
coord_flip()
###########################
library(dplyr)
library(forcats)
library(ggplot2)
df <- Noedars[order(Noedars$??????????,decreasing = TRUE),]
########################################################33
library(dplyr)
library(ggplot2)
Final %>%
  ggplot(aes(x = Score, y = Gender)) +
  geom_point(colour = "red")
cor(Final$Score, Final$Gender)
####################Autocorolation

#############################auto rem
library(dplyr)
library(sandwich)
ols %>% 
  vcovHC() %>% 
  diag() %>% 
  sqrt()
coeftest(ols3, vcov = vcovHC(ols3))
#############################
library(MASS)
install.packages("corTest")
library(corTest)
cor.test(Finall$Boomi, ols$residuals)
##################################
library("car")
qqPlot(ols)
ks.test(ols,residuals)
ks.test(x=rnorm(ols$residuals),y='pnorm',alternative='two.sided')
#################import finall############
Sys.setlocale(locale = "persian")
Finall0<- read.table("Finall0.txt", sep="\t", header = TRUE, fileEncoding = "UCS-2LE")
View(Finall0)
###################################
library("writexl")
write_xlsx(Finall,"C:\\users\\sajedeh\\Desktop\\Finall.xlsx")
#############dummy##################
Finall0$Gender<- ifelse(Finall0$Gender== "F", 0,1)
Finall0$Boomi<-ifelse(Finall0$Boomi=="YES", 0,1)
Finall0$Married<- ifelse(Finall0$Married== "YES", 0,1)
Finall0$Married<-as.factor(Finall0$Married)
Finall0$Married<-relevel(Finall0$Married,ref="1")
Finall0$Gender=as.factor(Finall0$Gender)
Finall0$Gender=relevel(Finall0$Gender,ref = "0")
Finall0$Boomi=as.factor(Finall0$Boomi)
Finall0$Boomi=relevel(Finall0$Boomi,ref = "1")
Finall0$CreditID=as.factor(Finall0$CreditID)
Finall0$CreditID=relevel(Finall0$CreditID,ref ="0")
class(Finall$Score)
Finall0$Score <-gsub('/','.',Finall0$Score)
Finall0$Score <-as.numeric(Finall0$Score)
Finall0$degreeID<-as.factor(Finall0$degreeID)
Finall0$degreeID<-relevel(Finall0$degreeID, ref="10")                           
Finall0$EducationalgroupID<-as.factor(Finall0$EducationalgroupID)
Finall0$EducationalgroupID<-relevel(Finall0$EducationalgroupID,ref="1")
Finall0$classTypeID<-as.factor(Finall0$classTypeID)
Finall0$classTypeID<-relevel(Finall0$classTypeID,ref="3")
Finall0$unitNumber<-as.factor(Finall0$unitNumber)
Finall0$unitNumber<-relevel(Finall0$unitNumber,ref = "3")
Finall0$CourseID<-as.factor(Finall0$CourseID)
Finall0$CourseID<-relevel(Finall0$CourseID,ref="4")
Finall0$classID<-as.factor(Finall0$classID)
Finall0$semesterID<-as.factor(Finall$semesterID)
###Regression#####################################
ols3<-lm(Score~Gender+Boomi+CreditID+degreeID+Married+??????????.????????????+classID+total.gpa+semesterID,data = Finall0)
summary(ols3)
par(mfrow=c(2,2))
plot()
par(mfrow=c(1,1))
hist(pooled$residuals, breaks = 20,main = "Histogram of Residuals")
hist(pooled$residuals, col="bisque", freq=FALSE,main="Density of Residuals")
lines(density(pooled$residuals), col="blue")
mean(ols3$residuals)
library(gvlma)
gvlma(ols3)
library(car)
vif(ols3)
durbinWatsonTest(pooled)
acf(pooled$residuals)
summary(ols)
install.packages("lawstat")
library(lawstat)
runs.test(pooled$residuals)
library(lmtest)
#######regression model against indep variable###############
library(ggplot2)
ggplot(data=Finall0, mapping=aes(Finall0,x=EducationalgroupID, y=resid(ols3)))+
  theme_bw()+geom_point()+
  geom_hline(yintercept = 0,col='red')+
  labs(y='residuals',x='square feet','Gender')
####################residuals against fittes############
library(ggplot2)
ggplot(data=Finall0, mapping=aes(Finall0,x=fitted(ols3), y=resid(ols3)))+
  theme_bw()+geom_point()+
  geom_hline(yintercept = 0,col='red')+
  labs(y='residuals',x='Fitted value')
###################################
library(lmtest)
library(sandwich)
install.packages("stargazer")
library(stargazer)
library(tidyverse)
library(magrittr)
Finall0%<>%mutate(uhat=resid(ols3))
Finall0%<>%mutate(yhat=fitted(ols3))
Finall0%<>%mutate(uhatsq=uhat^2,yhatsq=yhat^2)
model_BP<-lm(uhatsq~Gender+CreditID+Boomi,Finall0)
summary(model_BP)
library(car)
coeftest(ols3,vcov=hccm(ols3,type="hc0"))
############panel data###############
Sys.setlocale(locale = "persian")
panel1<- read.table("panel1.txt", sep="\t", header = TRUE, fileEncoding = "UCS-2LE")
View(panel1)
############################
panel1$Gender<- ifelse(panel1$Gender== "F", 0,1)
panel1$Boomi<-ifelse(panel1$Boomi=="YES", 0,1)
panel1$Married<- ifelse(panel1$Married== "YES", 0,1)
panel1$Married<-as.factor(panel1$Married)
panel1$Married<-relevel(panel1$Married,ref="1")
panel1$Gender=as.factor(panel1$Gender)
panel1$Gender=relevel(panel1$Gender,ref = "0")
panel1$Boomi=as.factor(panel1$Boomi)
panel1$Boomi=relevel(panel1$Boomi,ref = "1")
panel1$CreditID=as.factor(panel1$CreditID)
panel1$CreditID=relevel(panel1$CreditID,ref ="0")
class(Finall$Score)
panel1$Score <-gsub('/','.',panel1$Averagescore)
panel1$Score <-as.numeric(panel1$Averagescore)
panel1$degreeID<-as.factor(panel1$degreeID)
panel1$degreeID<-relevel(panel1$degreeID, ref="10")                           
panel1$Educationalgroup.1<-as.factor(panel1$Educationalgroup.1)
panel1$Educationalgroup.1<-relevel(panel1$Educationalgroup.1,ref="1")
panel1$CourseID<-as.factor(panel1$CourseID)
panel1$CourseID<-relevel(panel1$CourseID,ref="2")
panel1$total.gpa<-as.factor(panel1$total.gpa)
########PLM########
library(plm)
pooling<-plm(formula =Averagescore~Gender+Boomi+CreditID+degreeID+CourseID+Educationalgroup.1+Married+factor(SemesterID),data = panel1,index=c("UID","SemesterID"),model = "pooling")
summary(pooling)
pooled<-plm(Averagescore~Gender+Boomi+CreditID+SemesterID+??????????.????????????,data=panel1,model = "pooling")
summary(pooled)
########################0LS WITH OANNEL DATA############3
olspanel<- lm(Averagescore~Gender+Boomi+CreditID, data=panel1)
summary(olspanel)
plot(ols)


remotes::install_github("R-CoderDotCom/ggcats@main")
library(ggcats)
bptest(ols3,~fitted(ols3)+I(fitted(ols3)^2))
library(gvlma)
gvlma(ols3)
########################################
panel1$SemesterID<-as.character.Date(panel1$SemesterID)
##########
library(dplyr)
set.seed(123)
panel1%>%
  arrange(UID, SemesterID) %>%
  group_by(UID,SemesterID) %>%
  mutate(o=panel1,COUNTER = 1:n(1605))
panel1<-data.frame(UID,semesterID)
aggregate(row.names(panel1) ~ UID + semesterID, panel1, length)
count(panel1, c('UID','SemesterID'))
library("writexl")
write_xlsx(paneld,"C:\\users\\sajedeh\\Desktop\\paneld.xlsx")
library(data.table)
panel1[,Term := order(SemesterID), by  = UID]
Finall0[,term:= order(semesterID), by = UID]
library(ggplot2)
ggplot(data=scorestatus, aes(x=??????????, y=??????????, fill=??????????)) +
  geom_bar(stat="identity",width = .7)+scale_fill_brewer(palette = "Set2")
# Note we convert the cyl variable to a factor here in order to fill by cylinder
ggplot(mpg) + 
  geom_bar(aes(x =????????, fill = factor(cyl)), position = position_dodge(preserve = 'single'))
##
library(ggplot2)
theme_set(theme_bw())
# Draw plot
ggplot(cty_mpg, aes(x=make, y=mileage)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
###################################
ggplot(datavis) + 
  geom_bar(aes(x = year, fill = ??????), position = position_dodge(preserve = 'single'))#+ggtitle('?????????? ???? ???? ??????')
ggplot(datavis) + 
  geom_bar(aes(x = year, fill =????????.???????? ), position = position_dodge(preserve = 'single'))#+ggtitle('?????????? ???? ???? ??????')
Sys.setlocale(locale = "persian")
datavis<- read.table("datavis.txt", sep="\t", header = TRUE, fileEncoding = "UCS-2LE")
View(datavis)
class(datavis$semesterID)
datavis$year<-as.factor(datavis$year)
library(ggplot2)
BarChart(BasicInfo$??????, rotate_x=45, offset=1, sort="-")
######################################
ggplot(data=scorestatus, aes(x=??????????, y=??????????, fill=??????????,y = (..count..)/sum(..count..))) ) +
  geom_bar(stat="identity",width = .7)+scale_fill_brewer(palette = "Set2")+stat_bin(geom = "text",
                                                                                    aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
                                                                                    vjust = 5) 
  scale_y_continuous(labels = percent)
#################
  ggplot(karshenasiboomi, aes(x = factor(??????), y = ????????, colour = `?????????? ?????? ??????????`, group =`?????????? ?????? ??????????`)) +
      geom_line()+geom_point(size = 4, shape = 21)+scale_fill_brewer(palette = "set2")
#
  ggplot(karshenasigender, aes(x = factor(??????), y = ????????, colour = ??????????, group =??????????)) +
     geom_line()+geom_point(size = 4, shape = 21)+scale_fill_brewer(palette = "set2")+
    geom_line(aes(color =?????????? ), size = 2) +
    geom_point(aes(color = country), size = 5)
  #
  
ggplot(karshenasisahmie, aes(x = factor(??????), y = ????????, colour = `??????????`, group =`??????????`)) +
  geom_line()+geom_point(size = 4, shape = 21)+scale_fill_brewer(palette = "set2")
####################333
ggplot(boomi, aes(x=`?????????? ?????? ??????????`, y=??????????, fill=??????????)) + 
  geom_bar(stat="identity",width = .5,fill="pink")+geom_text(aes(label = Freq), vjust = 0)
##################
library(lessR)
BarChart(??????,data=basic3,theme=getOption("theme"))
#
library(ggplot2)
library(dplyr)
basic3%>%
  ggplot(aes(x=??????))+
  geom_bar(fill="#071150",width = .4)+
  labs(x="??????????",y=NULL)
#
ggplot(sahm, aes(x=??????????, y=??????????, fill=??????????)) + 
  geom_bar(stat="identity",width = .5) +
  geom_text(aes(label=??????????), vjust=1.5, colour="white", size=3.5)
#
library(ggplot2)
ggplot(data=doore, aes(x=`???????? ?????? ????????????`, y=??????????, fill=`???????? ?????? ????????????`)) +
  geom_bar(stat="identity",width = .8)+geom_text(aes(label=??????????), vjust=1.7, colour="black", size=4)

ggplot(data=phdsahmie, aes(x=`?????????? ??????????`, y=??????????, fill=`?????????? ??????????`)) +xlab("") + # Set axis labels
  ylab("") +guides(fill=FALSE) +
  geom_bar(stat="identity",width = .7)+geom_text(aes(label=??????????), vjust=.95, colour="black", size=4)+
  theme_minimal()+theme_gray()

library(ggplot2)
theme_set(theme_bw())

# Draw plot
ggplot(BAsahmie, aes(x=`?????????? ????????????????`, y=??????????)) + 
  geom_bar(stat="identity", width=.5,fill="red") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.4))
###

ggplot(karshenasigender, aes(x = factor(??????), y = ????????, group = ??????????)) +
  geom_line(aes(color = ??????????), size = 1.2) +
  geom_point(aes(color = ??????????), size = 3)
###
ggplot(karshenasiboomi, aes(x = factor(??????), y = ????????, group = `?????????? ?????? ??????????`)) +
  geom_line(aes(color =  `?????????? ?????? ??????????`), size = 1.2) +
  geom_point(aes(color =  `?????????? ?????? ??????????`), size = 3)
###
ggplot(karshenasisahmie, aes(x = factor(??????), y = ????????, group = ??????????)) +
  geom_line(aes(color =  ??????????), size = 1.2) +
  geom_point(aes(color =  ??????????), size = 3)
###
###
ggplot(arshadtermgender, aes(x = factor(??????), y = ????????, group = ??????????)) +
  geom_line(aes(color =  ??????????), size = 1.2) +
  geom_point(aes(color =  ??????????), size = 3)
###
ggplot(arshadtermboomi, aes(x = factor(??????), y = ????????, group = ??????????)) +
  geom_line(aes(color =  ??????????), size = 1.2) +
  geom_point(aes(color = ??????????), size = 3)
###
##
ggplot(arshadtermsahmie, aes(x = factor(??????), y = ????????, group = ??????????)) +
  geom_line(aes(color =  ??????????), size = 1.2) +
  geom_point(aes(color =  ??????????), size = 3)
ghotbi$????????<-as.factor(ghotbi$????????)

library(dplyr)
paste0(data=pastee, collapse = "f","id")

Total$X = paste0(Total$??????????.????????????,Total$??????????????)
pastee$id=as.vector(pastee$id)
pastee$f=as.vector(pastee$f)
class()
library(tidyr)
library(magrittr)
pastee %<>%
  unite(col, id, f,remove = FALSE)
pastee<-gsub("-", "", pastee$col)



remotes::install_github("R-CoderDotCom/ggcats@main")
library(ggcats)
library(CRAN)


library(ggplot2)
ggplot()+geom_cat(aes(1, 2,cat ="grumpy"), size = 15) 
ggplot()+geom_cat(aes(1, 2,cat ="colonel"), size = 15) 
ggplot()+geom_cat(aes(1, 2,cat ="bongo"), size = 15) 

ggplot()+geom_cat(aes(1, 2,cat ="pusheen"), size = 15) 

devtools::install_github("keithmcnulty/Rmusic", build_vignettes = TRUE)
library(Rmusic)

library(ggplot2)
ggplot(karshenasitermgender, aes(x = factor(??????), y = `?????????????? ????????`, group = ??????????)) +
  geom_line(aes(color = ??????????), size = 1.2) +
  geom_point(aes(color =??????????), size = 3)
##
library(ggplot2)
ggplot(karshenasitermboomi, aes(x = factor(??????), y = `?????????????? ????????`, group =`?????????? ?????? ??????????`)) +
  geom_line(aes(color = `?????????? ?????? ??????????`), size = 1.2) +
  geom_point(aes(color =`?????????? ?????? ??????????`), size = 3)
#
library(ggplot2)
ggplot(karshenasitermsahmie, aes(x = factor(??????), y = `?????????????? ????????`, group =`??????????`)) +
  geom_line(aes(color = `??????????`), size = 1.2) +
  geom_point(aes(color =`??????????`), size = 3)
#
library(ggplot2)
ggplot(arshadtermboomi, aes(x = factor(??????), y = `?????????????? ????????`, group =`?????????? ?????? ??????????`)) +
  geom_line(aes(color = `?????????? ?????? ??????????`), size = 1.2) +
  geom_point(aes(color =`?????????? ?????? ??????????`), size = 3)

#
library(ggplot2)
ggplot(arshadtermsahmie, aes(x = factor(??????), y = `?????????????? ????????`, group =`??????????`)) +
  geom_line(aes(color = `??????????`), size = 1.2) +
  geom_point(aes(color =`??????????`), size = 3)
#
Sys.which("make")
writeLines('PATH="C:\\rtools40\\usr\\bin;${PATH}"', con = "~/.Renviron")
.libPaths("C:/software/Rpackages")
getwd()
.libPaths("C:/software/Rpackages")
.libPaths()
#

library(emmeans)
emmip(OLSBA, Gender~Native,CIs=TRUE)
summary(OLSBA)
library(ggplot2)
TotalBA <- emmip(OLSBA, Gender ~ Credit, CIs=TRUE, plotit=FALSE)
ggplot(data=TotalBA, aes(x=Credit,y=Score, fill=Gender)) + geom_bar(stat="identity",position="dodge")
install.packages("vtable")
library()
sumtable(TotalBA, col.breaks = c(4,8))
st(TotalBA$Score, TotalBA$Gender)
by(TotalBA$Score, TotalBA$Gender, summary)
mean(TotalBA$Score[TotalBA$ClassType=="Asli"])
sd(TotalBA$Score)
sd(Ftotalba$Score)