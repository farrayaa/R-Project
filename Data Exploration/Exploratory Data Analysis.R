
#RANGKUMAN AED

# Week 2 ------------------------------------------------------------------------------------------------------------
#Pie chart, Histogram, Bar Plot

getwd()
setwd('D:/[KULIAH]/[SEMESTER 2]/ANALISIS EKSPLORASI DATA/PRAKTIKUM/Week 2/Tugas Week 2')

dfAktivitas <- read.csv('Data Tugas Pie Chart R.csv', header = TRUE, sep = ';')
dfAktivitas


pie(dfAktivitas$Frekuensi,
    main = '2D Pie Chart [Farah Syahfira]',
    labels = paste(percentage, '%', sep=';'),
    radius = 0.8,
    clockwise = TRUE,
    col = c('coral', 'aquamarine3', 'darkolivegreen3', 'lightsalmon', 'darkgoldenrod1', 'darkgrey'))

percentage <- round(dfAktivitas$Frekuensi/sum(dfAktivitas$Frekuensi)*100,2)

pie(dfAktivitas$Frekuensi,
    main = '2D Pie Chart [Farah Syahfira]',
    labels = paste(percentage, '%', sep=';'),
    radius = 0.8,
    clockwise = TRUE,
    col = c('coral', 'aquamarine3', 'darkolivegreen3', 'lightsalmon', 'darkgoldenrod1', 'darkgrey'))
legend('bottomleft',
       legend = c(dfAktivitas$Aktivitas),
       fill = c('coral', 'aquamarine3', 'darkolivegreen3', 'lightsalmon', 'darkgoldenrod1', 'darkgrey'),
       title = 'Aktivitas',
       box.lty = 0,
       cex = 0.6)

library('plotrix')

pie3D(dfAktivitas$Frekuensi, 
      main = '3D Pie Chart [Farah Syahfira]',
      labels = paste(percentage, '%', sep=','),
      radius = 0.8,
      col=c('coral', 'aquamarine3', 'darkolivegreen3', 'lightsalmon', 'darkgoldenrod1', 'darkgrey'),
      explode = 0.1)
legend('bottomleft',
       legend = c(dfAktivitas$Aktivitas),
       fill = c('coral', 'aquamarine3', 'darkolivegreen3', 'lightsalmon', 'darkgoldenrod1', 'darkgrey'),
       title = 'Aktivitas',
       box.lty = 0,
       cex = 0.5)

dfIRIS <- read.csv('IRIS.csv', header = TRUE, sep = ',')  
head(dfIRIS)

Sepal <- dfIRIS$sepal_length
hist(Sepal,
     main = 'Histogramku [Farah Syahfira]',
     xlab = 'Sepal Length',
     ylab = 'Frequency',
     ylim = c(0,35),
     xlim = c(3,9),
     col = c('lightgoldenrod', 'lightsteelblue', 'darkkhaki', 'sandybrown', 'plum3', 'lightsalmon3', 'slategray', 'mistyrose2'),
     freq = TRUE)

barplot(dfAktivitas$Frekuensi,
        main = 'Bar Plot Ku [Farah Syahfira]',
        names = dfAktivitas$Aktivitas,
        col= c('olivedrab', 'navajowhite4', 'yellow', 'orange2', 'mediumpurple', 'lightblue'),
        border = FALSE,
        ylim= range(0,60),
        xlab = 'Aktivitas',
        ylab = 'Frequency')

par(mar=c(1,1,1,1))


# Week 3 ------------------------------------------------------------------------------------------------------------
#Boxplot, Line Plot, Scatter, statistika deskriptif
getwd()
setwd('D:/[KULIAH]/[SEMESTER 2]/ANALISIS EKSPLORASI DATA/PRAKTIKUM/Week 3/Tugas Week 3')

library(moments)
options(scipen=999)

#BOXPLOT
dfbaseball = read.table('Baseball Dataset Salary.csv', sep = ';', 
                        fill = TRUE, 
                        header = TRUE,
                        quote = "", 
                        stringsAsFactors = TRUE,
                        fileEncoding="UTF-16LE")
head(dfbaseball)

baseball_2018 = dfbaseball[dfbaseball$year == 2018, ]
head(baseball_2018)

baseball_2018$team = factor(baseball_2018$team , 
                            levels=c("ARI", "DET", "MIL", "SEA","TEX"))

boxplot(baseball_2018$exp ~ baseball_2018$team,
        xlab = 'Year of Experience',
        ylab = 'Team',
        col = c('#FFFE9E','#F7A72B','#EA5A4E','#BF2D72','#7B106D'),
        horizontal = TRUE,
        col.axis = '#F26839',
        col.lab = '#7B106D',
        las = 1,
        ylim = c(0,25),
        frame.plot = F,
        cex.axis = 0.8,)
title("      Box Plot Distribution Year of Experience (Farah Syahfira)",
      adj  = 0.5, line = 1.5,
      col.main = '#7B106D')
mtext(" 5 Major League Baseball Teams, 2018 ", adj = 0.5, line = 0,
      col = '#F26839')

axis(1, col.axis = '#F26839', col = '#7B106D', cex.axis = 0.8, labels =F)
axis(2, col.axis = '#F26839', col = '#7B106D', cex.axis = 0.8, labels = F)

#outlier
mtext("Miguel Cabrera & 
      Victor Martinez : 16", adj = 0.7, line = -13.5, cex = 0.6, col = 'red')
mtext("Francisco Liriano : 13", adj = 0.55, line = -15, cex = 0.6, col = 'red')
mtext("Adrian Beltre & 
      Bartolo Colon : 21", adj = 0.9, line = -4.3, cex = 0.6, col = 'red')

#interpretasi
#subset
ARI18 = subset(baseball_2018, team == 'ARI')
DET18 = subset(baseball_2018, team == 'DET')
MIL18 = subset(baseball_2018, team == 'MIL')
SEA18 = subset(baseball_2018, team == 'SEA')
TEX18 = subset(baseball_2018, team == 'TEX')

#statistika deskriptif
#ARI
mean(ARI18$exp)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(ARI18$exp)
print(result)
quantile(ARI18$exp)
max(ARI18$exp)-min(ARI18$exp)

#DET
mean(DET18$exp)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(DET18$exp)
print(result)
quantile(DET18$exp)
max(DET18$exp)-min(DET18$exp)

#MIL
mean(MIL18$exp)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(MIL18$exp)
print(result)
quantile(MIL18$exp)
max(MIL18$exp)-min(MIL18$exp)

#SEA
mean(SEA18$exp)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(SEA18$exp)
print(result)
quantile(SEA18$exp)
max(SEA18$exp)-min(SEA18$exp)

#TEX
mean(TEX18$exp)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(TEX18$exp)
print(result)
quantile(TEX18$exp)
max(TEX18$exp)-min(TEX18$exp)

#mean & median
mtext("       mean: 6,7
      median: 5", adj = 0.23, line = -3, cex = 0.6, col = 'white')
mtext("       mean: 6,6
      median: 7", adj = 0.2, line = -7, cex = 0.6, col = 'white')
mtext("       mean: 6,28
      median: 5", adj = 0.23, line = -11, cex = 0.6, col = 'black')
mtext("       mean: 5,5
      median: 4", adj = 0.2, line = -14.5, cex = 0.6, col = 'black')
mtext("       mean: 6,7
      median: 6", adj = 0.28, line = -18, cex = 0.6, col = 'black')


#LINE PLOT MULTI
library(ggplot2)

baseball_5year = dfbaseball[dfbaseball$year %in% c(2015,2016,2017,2018,2019), ]
head(baseball_5year)

baseballteam_5year = baseball_5year[baseball_5year$team %in% c("ARI", "CHC", "COL", "OAK","SFG"), ]

baseballteam_scatter <- aggregate(baseballteam_5year$sal, by=list(baseballteam_5year$team, baseballteam_5year$year), 
                                  function(x)mean(x, na.rm=TRUE))

names(baseballteam_scatter) <- c("Team", "Year", "Salary")

baseballteam_scatter$Team <- factor(baseballteam_scatter$Team)
ggplot(data = baseballteam_scatter[!is.na(baseballteam_scatter$Salary), ], 
       aes(x = Year, y = Salary, group=Team, color=Team)) + 
  labs(x="Year", y="Mean of Salary",
       title="Line Plot Average Salary (Farah Syahfira)",
       subtitle="5 Major League Baseball Teams in 2015 to 2019") +
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F")) +
  geom_point()+geom_line()+ scale_colour_manual(values = c('#E1BD6D','#EABE94','#0B775E','#35274A','#F2300F'))

+
  geom_text(data = baseballteam_scatter, aes(y = Salary, label = sprintf("%0.0f", round((Salary), digits = 2)), vjust=1.2, hjust=0.5))

#SCATTER PLOT
#scatterplot Basic

baseball_2019 = dfbaseball[dfbaseball$year == 2019, ]
head(baseball_2019)

baseball_2019 = baseball_2019[baseball_2019$team %in% c("BAL", "COL", "NYM", "SEA","TEX"), ]

plot(baseball_2019$exp, baseball_2019$sal,
     xlab="Year of Experience",ylab="Salary",
     xlim = c(0,20),ylim =c(0,30000000),
     xaxt='n',yaxt='n',
     col=ifelse(baseball_2019$team == 'BAL', '#9C6755',
                ifelse(baseball_2019$team == 'COL', '#DFA398',
                       ifelse(baseball_2019$team == 'NYM','#F5C98E',
                              ifelse(baseball_2019$team == 'SEA','#80CDC1','#D65B5A')))),
     pch=16,col.lab='#5C6BC0', frame.plot = F)

BAL = subset(baseball_2019, team == 'BAL')
abline(lm(sal~exp, data = BAL), col = '#9C6755')
COL = subset(baseball_2019, team == 'COL')
abline(lm(sal~exp, data = COL), col = '#DFA398')
NYM = subset(baseball_2019, team == 'NYM')
abline(lm(sal~exp, data = NYM), col = '#F5C98E')
SEA = subset(baseball_2019, team == 'SEA')
abline(lm(sal~exp, data = SEA), col = '#80CDC1', lty = 2)
TEX = subset(baseball_2019, team == 'TEX')
abline(lm(sal~exp, data = TEX), col = '#D65B5A')

title('Scatter Plot Distribution of Salaries
      Based on Years of Experience (Farah Syahfira)', adj  = 0.6, line = 1.5, col.main = '#5C6BC0')
mtext('5 Major League Baseball Team, 2019', side=3, adj = 0.6, cex=0.75, line = 0,
      font = 2, col = '#689F38')
legend('right', legend = c("BAL", "COL", "NYM", 'SEA',"TEX"), 
       col = c('#9C6755', '#DFA398', '#F5C98E', 
               '#80CDC1', '#D65B5A'),
       title = 'Team',
       bty = 'o', pch = 16,
       cex = 0.6, box.col = '#5C6BC0',
       text.col = '#689F38')
axis(1, col = '#5C6BC0',col.axis= '#689F38', cex.axis = 1)

axis(2, col = '#5C6BC0', col.axis= '#689F38', cex.axis = 1)

#LINE PLOT SINGLE
baseball_player = dfbaseball[dfbaseball$name == 'Jarrod Dyson',]

plot(x=baseball_player$year,y=baseball_player$sal,
     xlab="Year",
     ylab="Salary",
     col="#9B262B",
     col.axis= '#595800',
     col.lab = '#CD6D37',
     pch=16, type = "b")

title("      Line Plot Major League Player 
      'Jarrod Dyson' KCR Team (Farah Syahfira)", 
      adj  = 0.5, line = 1.5, col.main = '#2D3606')
mtext("Salary 2012 to 2019", adj = 0.5, line = 0, col = '#CD6D37')

#nilai max min
range(baseball_player$sal)

# Week 4 ------------------------------------------------------------------------------------------------------------
# Jitter, Marginal, violin

getwd()
setwd("/D:/[KULIAH]/[SEMESTER 2]/ANALISIS EKSPLORASI DATA")

#Jitter Plot
library(ggplot2)
library(ggExtra)
dfbaseball = read.table('Baseball Dataset Salary.csv', sep = ';', 
                        fill = TRUE, 
                        header = TRUE,
                        quote = "", 
                        stringsAsFactors = TRUE,
                        fileEncoding="UTF-16LE")
head(dfbaseball)

baseball_2019 = dfbaseball[dfbaseball$year == 2019, ]
head(baseball_2019)

baseball_2019 = baseball_2019[baseball_2019$team %in% c("BAL", "COL", "SEA", "NYM","TEX"), ]

g = ggplot(baseball_2019, aes(exp, sal, color=team)) +
  geom_jitter() +
  geom_smooth(formula = y ~ x, method="lm", se=F) + 
  labs(x="Year of Experience", y="Salary",
       title="Jitter Plot Distribution of Salaries Based on Year of Experience (Farah & Dzakia)",
       subtitle="5 Major League Baseball Team, 2019") +
  theme(axis.title = element_text(colour = '#449559'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#BE2A3E')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#BE2A3E')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#22763F", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#E36148"))
g

#Violin
library(vioplot)
baseball_2018 = dfbaseball[dfbaseball$year == 2018, ]
head(baseball_2018)

baseball_2018$team = factor(baseball_2018$team , 
                            levels=c("ARI", "DET", "MIL", "SEA","TEX"))

vioplot(baseball_2018$exp ~ baseball_2018$team,
        xlab = 'Year of Experience',
        ylab = 'Team',
        col = c('#843400','#E47400','#F6BB22','#FADB88','#FEFCDE'),
        col.axis = '#FA9F48',
        col.lab = "#8C2F00",
        ylim = c(0,25),
        frame.plot = F,
        cex.axis = 0.8,
        rectCol = "#C01F00", 
        lineCol = "white",
        colMed = "black", 
        border = "black",
        pchMed = 16,
        plotCentre = "point")
title("      Violin Plot Distribution Year of Experience (Farah & Dzakia)",
      adj  = 0.5, line = 1.5,
      col.main = '#8C2F00')
mtext(" 5 Major League Baseball Teams, 2018 ", adj = 0.5, line = 0,
      col = '#FA9F48')
axis(1, col.axis = '#FA9F48', col = '#843400', cex.axis = 0.8, labels =F)
axis(2, col.axis = '#FA9F48', col = '#843400', cex.axis = 0.8, labels = F)

#Marginal
library(ggplot2)
library(ggExtra)

baseball_team = dfbaseball[dfbaseball$year == 2017, ]
baseball_team = baseball_team[baseball_team$team %in% "CHC", ]
g = ggplot(baseball_team, aes(exp, sal, color=team)) +
  geom_jitter() +
  geom_smooth(formula = y ~ x, method="lm", se=F)+ 
  labs(x="Year of Experience", y="Salary",
       title="Marginal Plot Distribution of Salaries Based on Year of Experience (Farah & Dzakia)",
       subtitle="Major League Baseball Team CHC 2017") +
  theme(axis.title = element_text(colour = '#E14BA8'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#7898C9')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#7898C9')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#3D1778", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#E65BA8"))+
  theme(legend.position = "none")

ggMarginal(g, type = "histogram", fill="#FFEBE3")
ggMarginal(g, type = "boxplot", fill="#C5C1E0")

# Week 5 ------------------------------------------------------------------------------------------------------------
# Korelasi

#using multi packages
#install.packages("reshape2")
#install.packages("maditr")
#install.packages("dplyr")
packages = c('rcompanion', 'vcd', 'psych', 'DescTools', 'epitools', 'ggplot2', 'reshape2', 'dplyr', 'maditr')
lapply(packages, library, character.only = TRUE)

#import data
data(mtcars)
head(mtcars)

#desc data
str(mtcars)

#edit datatype
#library(dplyr)
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("auto", "manual"))
  cyl  <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})
head(mtcars2)

#summary data
summary(mtcars2) #statistik deskriptif

#membuat tabel kontingensi

data = mtcars2 %>% #sama aja kayak koma
  group_by(am, vs) %>% #cuman ambil kolom am sama vs
  summarise(jumlah = n())
data


data = dcast(data, am ~ vs, value.var='jumlah')
data

data_matrix = data.matrix(data[ ,2:3])
data_matrix 

#menggunakan xtabs
data_xtabs = xtabs(~mtcars2$am + mtcars2$vs)
data_xtabs

data_xtabs1 = xtabs(~mtcars2$cyl + mtcars2$vs)
data_xtabs1

#menggunakan table()
data_table = table(mtcars2$cyl,mtcars2$vs)
data_table

#Phi Coef.
# dari library psych
#Dari sini, mau menggunakan data_matrix atau data_xtabs sama saja
phi(data_xtabs, digits = 4) #buat nentuin berapa angka belakang koma

# dari library DescTools
# DescTools always produces a positive value.
Phi(data_matrix)

#Crammer's V

#create 2x3 table
data = matrix(c(6, 9, 8, 5, 12, 10), nrow=2)

#view table
data

#calculate Cramer's V
cramerV(data)

#df = min(#rows-1, #columns-1)
df = min(1, 2)
df

#create 3x3 table
data = matrix(c(8, 2, 4, 5, 8, 6, 6, 3, 8), nrow=3)

#view table
data

#calculate Cramer's V
cramerV(data)

#Calculate degree of freedom
#df = min(#rows-1, #columns-1)
df = min(2, 2)
df

#Contingency Coef.
#library(vcd)
assocstats(data_xtabs)
#library(DescTools)
Assocs(data_xtabs)

# Create stack barchart vs and am
ggplot(mtcars2, aes(x = am, fill = vs)) + 
  geom_bar(position = "dodge")  +  #position = "dodge" to have a side-by-side (i.e. not stacked) barchart
  theme_bw()

# Create stack barchart vs and am
ggplot(mtcars2, aes(x = am, fill = vs)) + 
  geom_bar(position = "stack")  +  #position = "stack" to have a stacked barchart
  theme_bw()

#uji korelasi
cor.test(x=mtcars2$drat, y=mtcars2$wt, method = 'pearson')

#Scatter Plot
plot(mtcars2$mpg, mtcars2$hp, col='#184E77', pch=19,
     main = "Scatterplot of mpg vs hp",
     xlab = "Miles/(US) gallon",
     ylab = "Gross horsepower",
     cex.axis = 1)
mtext(paste0('Korelasi = ', round(cor(mtcars2$mpg, mtcars2$hp), digits = 3)), adj = .75, line = -7)

#Pair Plot
#library(psych)
pairs.panels(mtcars2[,c(3:7)], 
             method = 'pearson', # correlation method
             hist.col = '#34A0A4',
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             main = 'Pair plot of disp, hp, drat, wt, qsec')

#uji korelasi spearman
cor.test(x=cars$speed, y=cars$dist, method = 'spearman')
#uji korelasi kendall
cor.ken<-cor.test(x=cars$speed, y=cars$dist, method="kendall")
cor.ken

# Week 6 ------------------------------------------------------------------------------------------------------------
# Cleaning Data

packages = c('gapminder', 'MASS', 'dplyr', 'ggplot2', 'outliers', 'EnvStats', 'nortest', 'normtest', 'moments', 'ggpubr', 'car', 'qqplotr', 'e1071')
lapply(packages, library, character.only = TRUE)

# Karena data default dari R sehingga data ChickWeight dapat langsung digunakan.

data("ChickWeight")
head(ChickWeight)


summary(ChickWeight)

#Manual
index12 <- which(ChickWeight$Time == 12)
wts12 <- ChickWeight$weight[index12]

#Using Dplyr Package
databaru <- filter(ChickWeight, Time == 12)
weightbaru <- databaru$weight

weightbaru

#Check Modus 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(wts12)
print(result)

# getmode <- function(v) {
#    uniqv <- unique(v)
#    uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# result <- getmode(weightbaru)
# print(result)

print(skewness(wts12))
print(skewness(weightbaru))

#histogram hist or truehist
par(mfrow=c(1,2)) #mfrow berguna untuk membagi panel ke berapa (kolom,baris)
index12 <- which(ChickWeight$Time == 12)
wts12 <- ChickWeight$weight[index12]
hist(wts12, main = "From hist()", 
     xlab="Chick weight at 12 days")
truehist(wts12, main = "From truehist()", 
         xlab="Chick weight at 12 days")


## Density plot
d <- density(wts12) # menghitung densitas data
d
plot(d) # plot hasil

#density plot in truehist
truehist(wts12, xlab="Chickweight at 12 days")
mu <- mean(wts12)
sigma <- sd(wts12)
x <- seq(0,250,2)
px <- dnorm(x, mean = mu, sd = sigma)
lines(density(wts12))
lines(x, px)

dev.off()

#qqplot
qqPlot(wts12)  #library(car)

ggqqplot(wts12) #library(ggpubr)

probplot(wts12) #library(e1071)

#Uji Shapiro-Wilk
shapiro.test(wts12)

#Uji Chi-Square
res <- chisq.test(wts12, p = c(rep(1/49,49)))
res

#Anderson-Darling Test
ad.test(wts12)

#ks test
lillie.test(wts12)

#Dealing with Outliers

#import dataframe
setwd("D:/[KULIAH]/[SEMESTER 2]/ANALISIS EKSPLORASI DATA/PRAKTIKUM/Week 6/Materi Week 6")
library(gridExtra)
library(Hmisc)

df=read.csv("missing_value.csv", header=TRUE, sep=",")
head(df)
str(df)

df_clean = within(df, {
  Embarked <- factor(df$Embarked)
  Survived <- factor(df$Survived)
  Pclass <- factor(df$Pclass,
                   order=TRUE,
                   levels = c(3,2,1))
})
str(df_clean)

#removing outlier with 3 sigma rules
#detecting it using jitter plot
mean_fare=mean(df_clean$Fare,na.rm = TRUE)
sd_fare=sd(df_clean$Fare,na.rm = TRUE)

upper_bound_fare=mean_fare+3*sd_fare
lower_bound_fare=mean_fare-3*sd_fare

abline(h=upper_bound_fare,col='red')
upper_bound_fare

ggplot(df_clean, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = TRUE) +
  geom_hline(yintercept = upper_bound_fare ) +
  geom_hline(yintercept = lower_bound_fare ) 

#removing outlier
df_no_3sigma<- subset(df_clean, df_clean$Fare > lower_bound_fare & df_clean$Fare < upper_bound_fare)
mean_fare_no=mean(df_no_3sigma$Fare,na.rm = TRUE)
sd_fare_no=sd(df_no_3sigma$Fare,na.rm = TRUE)
upper_bound_fare_no=mean_fare+3*sd_fare
lower_bound_fare_no=mean_fare-3*sd_fare


plot1 = ggplot(df_clean, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = TRUE) +
  geom_hline(yintercept = upper_bound_fare ) +
  geom_hline(yintercept = lower_bound_fare ) 

plot2 = ggplot(df_no_3sigma, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = TRUE) +
  geom_hline(yintercept = upper_bound_fare_no ) +
  geom_hline(yintercept = lower_bound_fare_no ) 
grid.arrange(plot1, plot2, ncol=2)
dev.off()

#removing outlier with hampel identifier
madm=1.4826*mad(df_clean$Fare,na,rm=TRUE)
median_fare=median(df_clean$Fare,na.rm = TRUE)
upper_bound_fare=median_fare+3*madm
lower_bound_fare=median_fare-3*madm

df_no_hampel<- subset(df_clean, df_clean$Fare > lower_bound_fare & df_clean$Fare < upper_bound_fare)
madm_no=1.4826*mad(df_no_hampel$Fare,na.rm = TRUE)
median_fare_no=median(df_no_hampel$Fare,na.rm = TRUE)
upper_bound_fare_no=median_fare+3*madm
lower_bound_fare_no=median_fare-3*madm




plot1 = ggplot(df_clean, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = TRUE) +
  geom_hline(yintercept =upper_bound_fare ) +
  geom_hline(yintercept = median_fare,color='red') +
  geom_hline(yintercept =lower_bound_fare )

plot2 = ggplot(df_no_hampel, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = TRUE) +
  geom_hline(yintercept = upper_bound_fare_no ) +
  geom_hline(yintercept = median_fare_no,color='red' ) +
  geom_hline(yintercept = lower_bound_fare_no )
grid.arrange(plot1, plot2, ncol=2)

dev.off()
#removing outlier with boxplot
Q <- quantile(df_clean$Fare, probs=c(.25,0.5,.75), na.rm = TRUE)
iqr <- IQR(df_clean$Fare)
upper_bound_boxplot <-  Q[3]+1.5*iqr # Upper Range  
lower_bound_boxplot <- Q[1]-1.5*iqr # Lower Range???
median_boxplot <- Q[2]

df_boxplot_no<- subset(df_clean, df_clean$Fare > (Q[1] - 1.5*iqr) & df_clean$Fare < (Q[3]+1.5*iqr))
Q_no <- quantile(df_boxplot_no$Fare, probs=c(.25,0.5,.75), na.rm = TRUE)
iqr_no <- IQR(df_boxplot_no$Fare)
upper_bound_boxplot_no <-  Q_no[3]+1.5*iqr_no # Upper Range  
lower_bound_boxplot_no <- Q_no[1]-1.5*iqr_no # Lower Range???
median_boxplot_no <- Q_no[2]



plot1 = ggplot(df_clean, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = TRUE) +
  geom_hline(yintercept =upper_bound_boxplot ) +
  geom_hline(yintercept = median_boxplot,color='red') +
  geom_hline(yintercept =lower_bound_boxplot )

plot2 = ggplot(df_boxplot_no, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = TRUE) +
  geom_hline(yintercept = upper_bound_boxplot_no ) +
  geom_hline(yintercept = median_boxplot_no,color='red' ) +
  geom_hline(yintercept = lower_bound_boxplot_no )
grid.arrange(plot1, plot2, ncol=2)

dev.off()

#imputation missing value
df_imputation_0=df
df_imputation_median=df
df_imputation_average=df
df_imputation_0 = impute(df_imputation_0$Age,0)
df_imputation_median$Age = impute(df_imputation_median$Age,median(df_imputation_median$Age,na.rm=TRUE)) #median kalo dia skewness
df_imputation_average$Age = impute(df_imputation_average$Age,mean(df_imputation_average$Age,na.rm=TRUE)) #rata-rata kalo data distribusi normal
#impute data text with random value
df$Survived=impute(df$Survived,'random')
#impute data text with mode
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
result <- getmode(df$Pclass)
print(result)
df$Pclass=impute(df$Pclass,'3')

df$Pclass

# Week 10 ------------------------------------------------------------------------------------------------------------
# Resistant Line

data(mtcars)
head(mtcars)

library(dplyr)

mtcars %>%
  filter(mpg > 21)

head(mtcars[c('mpg', 'disp')])

x <- mtcars$mpg
y <- mtcars$disp

class(R)

R <- line(x, y, iter = 4)
R

residuals(R)
coef(R)
fitted(R)

intersep <- R$coefficients[1]
slope <- R$coefficients[2]

plot(x, y)
abline(a = intersep, b = slope)

R1 <- line(x, y, iter = 1)
R2 <- line(x, y, iter = 2)
R3 <- line(x, y, iter = 3)
R4 <- line(x, y, iter = 4)

plot(x, y)
abline(a = R1$coefficients[1], b = R1$coefficients[2], col = "red")
abline(a = R2$coefficients[1], b = R2$coefficients[2], col = "green")
abline(a = R3$coefficients[1], b = R3$coefficients[2], col = "blue")
abline(a = R4$coefficients[1], b = R4$coefficients[2], col = "black")


# Week 11 ------------------------------------------------------------------------------------------------------------
# Regresi Linear, Robust Regresi

library(MASS)
library(foreign)
library(car)

data("Cars93")
head(Cars93)

#cek linear atau tidak
#scatterplot horse power vs weight
modelHW <- lm(Horsepower ~ Weight, data = Cars93)
summary(modelHW)
plot(Cars93$Weight, Cars93$Horsepower)
abline(modelHW, lty = 2, lwd = 2)

#scatterplot horse power vs price
modelHP <- lm(Horsepower ~ Price, data = Cars93)
summary(modelHP)
plot(Cars93$Price, Cars93$Horsepower)
abline(modelHP, lty = 2, lwd = 2)

#scatterplot weight vs price
modelWP <- lm(Weight ~ Price, data = Cars93)
cor(Cars93$Weight, Cars93$Price)
plot(Cars93$Price, Cars93$Weight)
abline(modelWP, lty = 2, lwd = 2)

#model regresi horsepower dengan price dan weight
modelHWP <- lm(Horsepower ~ Price + Weight, data = Cars93)
summary(modelHWP)
vif(modelHWP) #Tes multicollinearity  (antar variabel bebas tdk boleh saling berkorelasi) <=4 tdk muktico/ tdk korelasi

#regresi model HP
predHP <- predict(modelHP)
resHP <- resid(modelHP)
sresHP <- rstandard(modelHP)
#regresi model HW
predHW <- predict(modelHW)
resHW <- resid(modelHW)
sresHW <- rstandard(modelHW)
#regresi model HWP
predHWP <- predict(modelHWP)
resHWP <- resid(modelHWP)
sresHWP <-rstandard(modelHWP)

#autocorellation
par(mfrow=c(3,1))
#residual model HP
plot(predHP, sresHP)
abline(0,0)
#residual model HW
plot(predHW, sresHW)
abline(0,0)
#residual model HWP
plot(predHWP, sresHWP)
abline(0,0)
dev.off()


#Uji Homoscedasticity - varians residual sama
library(lmtest)
bptest(modelHP)
bptest(modelHW)
bptest(modelHWP)


#model regresi horsepower dengan price, weight, min price, max price
modelHWPPP <- lm(Horsepower ~ Price + Weight + Min.Price + Max.Price, data = Cars93)
summary(modelHWPPP)
vif(modelHWPPP)


#-----------------------------------------------------------------------------------------------#


#Library yg dipake "foreign"
#import data no outlier

data("whiteside")
head(whiteside)

#make outlier
df_outlier = whiteside
df_outlier$Temp[40] <- 99
df_outlier$Gas[40] <- 99

#regresi ols no outlier
plot(whiteside$Gas~whiteside$Temp)
olslm_1<-lm(whiteside$Gas~whiteside$Temp)
summary(olslm_1)
abline(olslm_1, col='red')

#regresi ols with outlier
plot(df_outlier$Gas~df_outlier$Temp)
olslm_2<-lm(df_outlier$Gas~df_outlier$Temp)
summary(olslm_2)
abline(olslm_2, col='red')

#checking outlier
library(olsrr)
ols_plot_resid_lev(olslm_2)

#checking outlier 2
d1 <- cooks.distance(olslm_2)
r <- stdres(olslm_2)
rabs <- abs(r)
a <- cbind(df, d1, r, rabs)
a[d1 > 4/56, ]#56 is number of observations

#regresi robust (M, Huber) no outlier
rr.huber <- rlm(whiteside$Gas~whiteside$Temp, method = "M",
                psi = psi.huber, maxit=50)
summary(rr.huber)
plot(whiteside$Gas~whiteside$Temp)
abline(rr.huber)
abline(olslm_1, col='red')

#regresi robust (M, Huber) with outlier
rr.huber_outlier <- rlm(df_outlier$Gas~df_outlier$Temp,
                        method = "M", psi = psi.huber, maxit=50)
summary(rr.huber_outlier)
plot(df_outlier$Gas~df_outlier$Temp)
abline(rr.huber_outlier)
abline(olslm_2, col='red')

# Week 13 ------------------------------------------------------------------------------------------------------------
# Rootgram, Median Polish

# getwd()
setwd("C:/Users/Asus/Downloads/KUIS")

df <- read.csv("datamedpol.csv", header = TRUE, sep = ";")
head(df)

rownames(df) <- c("Northeast", "North Center", "South", "West")
head(df)

df.med <- medpolish(df , maxiter = 5)

df.med

#!Rootgram in R
install.packages("countreg", repos="http://R-Forge.R-project.org")

library(countreg)

Chest <- rep(33:38, c(3,18,81,185,420,749))
Chest

#Cara 1
#!Standing
m <- glm(Chest ~ 1, family = poisson)
rootogram(m, style = "standing")
#!Hanging
m <- glm(Chest ~ 1, family = poisson)
rootogram(m, style = "hanging")
#!Suspend
m <- glm(Chest ~ 1, family = poisson)
rootogram(m, style = "suspend")

#Cara 2
#!Standing
rootogram(Chest, fitted = "poisson", style = "standing")
#!Hanging
rootogram(Chest, fitted = "poisson", style = "hanging")
#!Suspend
rootogram(Chest, fitted = "poisson", style = "suspend")


# Post Test ------------------------------------------------------------------------------------------------------------

library(foreign)
library(MASS)

setwd("C:/Users/Asus/Downloads/Kuis AED")
getwd()

df <- read.csv("world_happy.csv", header = TRUE, sep = ",")
head(df)

#no 1
#1a
#scatterplot happiness score vs GDP
modelHG <- lm(df$Happiness.Score ~ df$Economy..GDP.per.Capita.)
summary(modelHG)

plot(df$Happiness.Score ~ df$Economy..GDP.per.Capita.,
     main = " Regression Line GDP per Capita vs Happiness Score",
     xlab = "GDP per Capita",
     ylab = "Happiness Score")
#1b
#cek distribusi residual
library(foreign)
predHG <- predict(modelHG)
resHG <- resid(modelHG)
sresHG <- rstandard(modelHG)

plot(predHG, sresHG)
abline(0,0)

#cek Multicollinearity
#model regresi horsepower dengan price dan weight
modelHGL <- lm(Happiness.Score ~ Economy..GDP.per.Capita.+ 
                 Health..Life.Expectancy., data = df)
summary(modelHGL)
vif(modelHGL)

#autocorellation
#regresi model HG
modelHG <- lm(df$Happiness.Score ~ df$Economy..GDP.per.Capita.)
predHG <- predict(modelHG)
resHG <- resid(modelHG)
sresHG <- rstandard(modelHG)
#regresi model HL
modelHL <- lm(df$Happiness.Score ~ df$Health..Life.Expectancy.)
predHL <- predict(modelHL)
resHL <- resid(modelHL)
sresHL <- rstandard(modelHL)
#regresi model HGL
modelHGL <- lm(Happiness.Score ~ Economy..GDP.per.Capita.+ 
                 Health..Life.Expectancy., data = df)
predHGL <- predict(modelHGL)
resHGL <- resid(modelHGL)
sresHGL <-rstandard(modelHGL)

#plot
par(mfrow=c(3,1))
#residual model HP
plot(predHG, sresHG)
abline(0,0)
#residual model HW
plot(predHL, sresHL)
abline(0,0)
#residual model HWP
plot(predHGL, sresHGL)
abline(0,0)
dev.off()

#Uji Homoscedasticity - varians residual sama
library(lmtest)
bptest(modelHG)
bptest(modelHL)
bptest(modelHGL)

#1c
#Regression Line
plot(df$Happiness.Score ~ df$Economy..GDP.per.Capita.,
     main = " Regression Line GDP per Capita vs Happiness Score",
     xlab = "GDP per Capita ",
     ylab = "Happiness Score")
abline(modelHG, lty = 2, lwd = 2, col = "red")


#-----------------------------------------------------------------------------------------------#
#No 2

#simple regression
modelHG <- lm(df$Happiness.Score ~ df$Economy..GDP.per.Capita.)
summary(modelHG)

plot(df$Happiness.Score ~ df$Economy..GDP.per.Capita.,
     main = " Regression Line GDP per Capita  vs Happiness Score",
     xlab = "GDP per Capita ",
     ylab = "Happiness Score")
abline(modelHG, lty = 2, lwd = 2, col = "green")


#regresi robust

plot(df$Happiness.Score ~ df$Economy..GDP.per.Capita)
olslm_1<-lm(df$Happiness.Score ~ df$Economy..GDP.per.Capita)
summary(olslm_1)
abline(olslm_1, col="red")

#cek regresi robust 
ols <- lm(df$Happiness.Score ~ df$Economy..GDP.per.Capita.)
plot(df$Happiness.Score, 
     rstandard(ols), 
     ylab='Standardized Residuals', 
     xlab='Happiness Score') 
abline(h = 0, 
       col = "red")

library(MASS)
#fit robust regression model
robust <- rlm(df$Happiness.Score ~ df$Economy..GDP.per.Capita.)

#find residual standard error of ols model
summary(ols)$sigma

#find residual standard error of ols model
summary(robust)$sigma


#cek outlier
library(olsrr)
ols_plot_resid_lev(olslm_1)

#plot regresi robust
rr.huber <- rlm(df$Happiness.Score ~ df$Economy..GDP.per.Capita, 
                method = "M",
                psi = psi.huber, maxit=50) 
summary(rr.huber)
plot(df$Happiness.Score ~ df$Economy..GDP.per.Capita,
     main = " Robust Regression Line GDP per Capita vs Happiness Score",
     xlab = "GDP per Capita",
     ylab = "Happiness Score") 
abline(rr.huber, col = "blue")
abline(olslm_1, col='red')


#resistant line
x = df$Economy..GDP.per.Capita.
y = df$Happiness.Score

R <- line(x, y, iter = 4)
R

residuals(R)
coef(R)
fitted(R)

intersep <- R$coefficients[1]
slope <- R$coefficients[2]

plot(x, y)
abline(a = intersep, b = slope)

R1 <- line(x, y, iter = 1)
R2 <- line(x, y, iter = 2)
R3 <- line(x, y, iter = 3)
R4 <- line(x, y, iter = 4)

plot(x, y,  
     main = "Resistant Line GDP per Capita vs Happiness Score",
     xlab = "GDP per Capita ",
     ylab = "Happiness Score")
abline(a = R1$coefficients[1], b = R1$coefficients[2], col = "red")
abline(a = R2$coefficients[1], b = R2$coefficients[2], col = "green")
abline(a = R3$coefficients[1], b = R3$coefficients[2], col = "blue")
abline(a = R4$coefficients[1], b = R4$coefficients[2], col = "black")

#-----------------------------------------------------------------------------------------------#
#No 3
#Multiple regresi Happiness score dengan GDP, life expectancy, freedom, trust government corruption
modelHGLFTC <- lm(Happiness.Score ~ Economy..GDP.per.Capita.+ Health..Life.Expectancy. + 
                    Freedom + Trust..Government.Corruption. , data = df)
summary(modelHGLFTC)
vif(modelHGLFTC)

#plot
par(mfrow=c(2,2))

#GDP vs Happiness Score
modelGH <- lm(Happiness.Score ~ Economy..GDP.per.Capita., data = df)
summary(modelGH)
plot(df$Economy..GDP.per.Capita., df$Happiness.Score,
     main = "Regression Line GDP per Capita vs Happiness Score",
     xlab = "GDP per Capita ",
     ylab = "Happiness Score")
abline(modelGH, lty = 2, lwd = 2)

#Life Expectancy vs Happiness Score
modelLH <- lm(Happiness.Score ~ Health..Life.Expectancy., data = df)
summary(modelLH)
plot(df$Health..Life.Expectancy., df$Happiness.Score,
     main = "Regression Line  Life Expectancy vs Happiness Score",
     xlab = "Life Expectancy",
     ylab = "Happiness Score")
abline(modelLH, lty = 2, lwd = 2)

#Freedom vs Happiness Score
modelFH <- lm(Happiness.Score ~ Freedom, data = df)
summary(modelFH)
plot(df$Freedom, df$Happiness.Score,
     main = "Regression Line Freedom vs Happiness Score",
     xlab = "Freedom",
     ylab = "Happiness Score")
abline(modelFH, lty = 2, lwd = 2)

#Trust Government Corruption vs Happiness Score
modelTH <- lm(Happiness.Score ~ Trust..Government.Corruption., data = df)
summary(modelTH)
plot(df$Trust..Government.Corruption., df$Happiness.Score,
     main = "Regression Line Trust Government Corruption vs Happiness Score",
     xlab = "Trust Government Corruption",
     ylab = "Happiness Score")
abline(modelTH, lty = 2, lwd = 2)
dev.off()

#-----------------------------------------------------------------------------------------------#
#No 4
#Median polish
df2 <- read.csv("medpol.csv", header = TRUE, sep = ";")
head(df2)

df2 = df2 [,-1]
df2

rownames(df2) <- c("Kanker", "Diabetes", "Hepaitis", "Tumor")
head(df2)

df.med <- medpolish(df2 , maxiter = 5)

df.med



