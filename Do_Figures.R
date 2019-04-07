############# THESE CODES ARE FOR JOURNAL OF MACROECONOMICS PAPER & ONLINE APPENDIX

# Figure 1: Economic significance on betaGS_PW, sample period 1960-2016
library(readxl)
fig1 <- read_excel("Figures.xlsx", sheet = "fig1")
View(fig1)
names(fig1)[1]<-"Variable"
fig1<-fig1[order(fig1$esGS_PW),]
attach(fig1)
fig1$starGS_PW<- ifelse(pvalGS_PW <= 0.05, "***", ifelse(pvalGS_PW <= 0.1, "**", ifelse(pvalGS_PW<=0.2, "*", "")))
attach(fig1)
fig1$varname1<-paste(Variable,starGS_PW)
fig1$angle<-ifelse(fig1$Group=="Political component risk index",0,ifelse(fig1$Group=="Composite risk index", 45,ifelse(fig1$Group=="Export structure",90, 150)))
fig1$density<-ifelse(fig1$Group=="Political component risk index",0,ifelse(fig1$Group=="Composite risk index", 10,ifelse(fig1$Group=="Export structure",20, 30)))
library(graphics)
setEPS()
postscript("fig1.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 6, width = 15)
par(mar = c(9,4,4,2))
plot<-barplot(fig1$esGS_PW,names.arg = fig1$varname1,ylim=c(-0.3,0.2),beside = TRUE,horiz = FALSE,density = fig1$density,angle = fig1$angle,col="Black",cex.names=1,las=2)
text(plot,y=ifelse(fig1$esGS_PW>=0,fig1$esGS_PW+0.01,fig1$esGS_PW-0.01),labels = round(fig1$esGS_PW,digits=3),cex=1)
legend("topleft", legend=c("Composite risk index","Debt","Export structure","Political component risk index"),col="Black",density=c(10,30,20,0),angle=c(45,150,90,0),bty = "n")
dev.off()
#--------------------------------#


# Figure 2: Economic Significance of Public Debt-Tax Base Ratio to Government-Spending Cyclicality by Region
library(readxl)
fig2 <- read_excel("Figures.xlsx",sheet = "fig2")
View(fig2)
fig2<-subset(fig2,select=c("regioncode","rfiscapm1016","esfiscapr6016"))
fig2<-fig2[!duplicated(fig2), ]
library(tidyr)
fig2<-fig2%>% drop_na()
fig2<-fig2[order(fig2$regioncode),]
attach(fig2)
library(graphics)
fig2$angle<-ifelse(fig2$regioncode=="EAS",0,ifelse(fig2$regioncode=="ECS",15,ifelse(fig2$regioncode=="LCN",60, ifelse(fig2$regioncode=="MEA",90,150))))
fig2$density<-ifelse(fig2$regioncode=="EAS",0,ifelse(fig2$regioncode=="ECS",10,ifelse(fig2$regioncode=="LCN",20,ifelse(fig2$regioncode=="MEA",30,40))))
par(mfrow=c(2,1)) 
plot1<-barplot(fig2$rfiscapm1016,names.arg = fig2$regioncode,ylim=c(-0.5,4.5),beside = TRUE,horiz = FALSE,density = fig2$density,angle = fig2$angle,col="Black",cex.names=1,las=1,main="Actual average public debt/tax base ratio (2010-2016)")
text(plot1,y=fig2$rfiscapm1016+0.5,labels = round(fig2$rfiscapm1016,digits=3),cex=1)
plot2<-barplot(fig2$esfiscapr6016,names.arg = fig2$regioncode,ylim=c(-0.03,0.07),beside = TRUE,horiz = FALSE,density = fig2$density,angle = fig2$angle,col="Black",cex.names=1,las=1,main="Predicted economic significance of public debt/tax base ratio")
text(plot2,y=ifelse(fig2$esfiscapr6016>=0,fig2$esfiscapr6016+0.01,fig2$esfiscapr6016-0.01),labels = round(fig2$esfiscapr6016,digits=3),cex=1)
#--------------------------------#


# Figure 3: Economic significance by region
##### East Asia & Pacific
library(readxl)
fig3 <- read_excel("Figures.xlsx",sheet = "fig3")
View(fig3)
names(fig3)[1]<-"Variable"
fig3<-fig3[order(fig3$esEAS),]
attach(fig3)
fig3$starEAS<- ifelse(pvalEAS <= 0.05, "***", ifelse(pvalEAS <= 0.1, "**", ifelse(pvalEAS<=0.2, "*", "")))
attach(fig3)
fig3$varname1<-paste(Variable,starEAS)
fig3$angle<-ifelse(fig3$Group=="Political component risk index",0,ifelse(fig3$Group=="Composite risk index", 45,ifelse(fig3$Group=="Export structure",90, 150)))
fig3$density<-ifelse(fig3$Group=="Political component risk index",0,ifelse(fig3$Group=="Composite risk index", 10,ifelse(fig3$Group=="Export structure",20, 30)))
library(graphics)
par(mar = c(9,4,4,2))
plot<-barplot(fig3$esEAS,names.arg = fig3$varname1,ylim=c(-0.65,0.4),beside = TRUE,horiz = FALSE,density = fig3$density,angle = fig3$angle,col="Black",cex.names=1,las=2)
text(plot,y=ifelse(fig3$esEAS>=0,fig3$esEAS+0.02,fig3$esEAS-0.015),labels = round(fig3$esEAS,digits=3),cex=1)
legend("topleft", legend=c("Composite risk index","Debt","Export structure","Political component risk index"),col="Black",density=c(10,30,20,0),angle=c(45,150,90,0),bty = "n")
#--------------------------------#
##### Europe & Central Asia
library(readxl)
fig3 <- read_excel("Figure.xlsx",sheet = "fig3")
View(fig3)
names(fig3)[1]<-"Variable"
fig3<-fig3[order(fig3$esECS),]
attach(fig3)
fig3$starECS<- ifelse(pvalECS <= 0.05, "***", ifelse(pvalECS <= 0.1, "**", ifelse(pvalECS<=0.2, "*", "")))
attach(fig3)
fig3$varname1<-paste(Variable,starECS)
fig3$angle<-ifelse(fig3$Group=="Political component risk index",0,ifelse(fig3$Group=="Composite risk index", 45,ifelse(fig3$Group=="Export structure",90, 150)))
fig3$density<-ifelse(fig3$Group=="Political component risk index",0,ifelse(fig3$Group=="Composite risk index", 10,ifelse(fig3$Group=="Export structure",20, 30)))
library(graphics)
par(mar = c(9,4,4,2))
plot<-barplot(fig3$esECS,names.arg = fig3$varname1,ylim=c(-0.3,0.2),beside = TRUE,horiz = FALSE,density = fig3$density,angle = fig3$angle,col="Black",cex.names=1,las=2)
text(plot,y=ifelse(fig3$esECS>=0,fig3$esECS+0.01,fig3$esECS-0.01),labels = round(fig3$esECS,digits=3),cex=1)
legend("topleft", legend=c("Composite risk index","Debt","Export structure","Political component risk index"),col="Black",density=c(10,30,20,0),angle=c(45,150,90,0),bty = "n")
#--------------------------------#
##### Latin America & Caribbean
library(readxl)
fig3 <- read_excel("Figures.xlsx",sheet = "fig3")
View(fig3)
names(fig3)[1]<-"Variable"
fig3<-fig3[order(fig3$esLCN),]
attach(fig3)
fig3$starLCN<- ifelse(pvalLCN <= 0.05, "***", ifelse(pvalLCN <= 0.1, "**", ifelse(pvalLCN<=0.2, "*", "")))
attach(fig3)
fig3$varname1<-paste(Variable,starLCN)
fig3$angle<-ifelse(fig3$Group=="Political component risk index",0,ifelse(fig3$Group=="Composite risk index", 45,ifelse(fig3$Group=="Export structure",90, 150)))
fig3$density<-ifelse(fig3$Group=="Political component risk index",0,ifelse(fig3$Group=="Composite risk index", 10,ifelse(fig3$Group=="Export structure",20, 30)))
library(graphics)
par(mar = c(9,4,4,2))
plot<-barplot(fig3$esLCN,names.arg = fig3$varname1,ylim=c(-0.35,0.35),beside = TRUE,horiz = FALSE,density = fig3$density,angle = fig3$angle,col="Black",cex.names=1,las=2)
text(plot,y=ifelse(fig3$esLCN>=0,fig3$esLCN+0.015,fig3$esLCN-0.01),labels = round(fig3$esLCN,digits=3),cex=1)
legend("topleft", legend=c("Composite risk index","Debt","Export structure","Political component risk index"),col="Black",density=c(10,30,20,0),angle=c(45,150,90,0),bty = "n")
#--------------------------------#
##### Middle East & North Africa
library(readxl)
fig3 <- read_excel("Figures.xlsx",sheet = "fig3")
View(fig3)
names(fig3)[1]<-"Variable"
fig3<-fig3[order(fig3$esMEA),]
attach(fig3)
fig3$starMEA<- ifelse(pvalMEA <= 0.05, "***", ifelse(pvalMEA <= 0.1, "**", ifelse(pvalMEA<=0.2, "*", "")))
attach(fig3)
fig3$varname1<-paste(Variable,starMEA)
fig3$angle<-ifelse(fig3$Group=="Political component risk index",0,ifelse(fig3$Group=="Composite risk index", 45,ifelse(fig3$Group=="Export structure",90, 150)))
fig3$density<-ifelse(fig3$Group=="Political component risk index",0,ifelse(fig3$Group=="Composite risk index", 10,ifelse(fig3$Group=="Export structure",20, 30)))
library(graphics)
par(mar = c(9,4,4,2))
plot<-barplot(fig3$esMEA,names.arg = fig3$varname1,ylim=c(-0.3,0.15),beside = TRUE,horiz = FALSE,density = fig3$density,angle = fig3$angle,col="Black",cex.names=1,las=2)
text(plot,y=ifelse(fig3$esMEA>=0,fig3$esMEA+0.01,fig3$esMEA-0.01),labels = round(fig3$esMEA,digits=3),cex=1)
legend("topleft", legend=c("Composite risk index","Debt","Export structure","Political component risk index"),col="Black",density=c(10,30,20,0),angle=c(45,150,90,0),bty = "n")
#--------------------------------#
##### Sub-Saharan Africa
library(readxl)
fig3 <- read_excel("Figures.xlsx",sheet = "fig3")
View(fig3)
names(fig3)[1]<-"Variable"
fig3<-fig3[order(fig3$esSSF),]
attach(fig3)
fig3$starSSF<- ifelse(pvalSSF <= 0.05, "***", ifelse(pvalSSF <= 0.1, "**", ifelse(pvalSSF<=0.2, "*", "")))
attach(fig3)
fig3$varname1<-paste(Variable,starSSF)
fig3$angle<-ifelse(fig3$Group=="Political component risk index",0,ifelse(fig3$Group=="Composite risk index", 45,ifelse(fig3$Group=="Export structure",90, 150)))
fig3$density<-ifelse(fig3$Group=="Political component risk index",0,ifelse(fig3$Group=="Composite risk index", 10,ifelse(fig3$Group=="Export structure",20, 30)))
library(graphics)
par(mar = c(9,4,4,2))
plot<-barplot(fig3$esSSF,names.arg = fig3$varname1,ylim=c(-0.25,0.6),beside = TRUE,horiz = FALSE,density = fig3$density,angle = fig3$angle,col="Black",cex.names=1,las=2)
text(plot,y=ifelse(fig3$esSSF>=0,fig3$esSSF+0.015,fig3$esSSF-0.015),labels = round(fig3$esSSF,digits=3),cex=1)
legend("topleft", legend=c("Composite risk index","Debt","Export structure","Political component risk index"),col="Black",density=c(10,30,20,0),angle=c(45,150,90,0),bty = "n")
#--------------------------------#


# Figure 4: Public debt/3-year MA tax base (lfiscap) by region
library(readxl)
fig4 <- read_excel("Figures.xlsx",sheet = "fig2")
View(fig4)
fig4<-subset(fig4,select=c("regioncode","rlfiscapm1016","eslfiscapr6016"))
fig4<-fig4[!duplicated(fig4), ]
library(tidyr)
fig4<-fig4%>% drop_na()
fig4<-fig4[order(fig4$regioncode),]
attach(fig4)
library(graphics)
fig4$angle<-ifelse(fig4$regioncode=="EAS",0,ifelse(fig4$regioncode=="ECS",15,ifelse(fig4$regioncode=="LCN",60, ifelse(fig4$regioncode=="MEA",90,150))))
fig4$density<-ifelse(fig4$regioncode=="EAS",0,ifelse(fig4$regioncode=="ECS",10,ifelse(fig4$regioncode=="LCN",20,ifelse(fig4$regioncode=="MEA",30,40))))
par(mfrow=c(2,1)) 
plot1<-barplot(fig4$rlfiscapm1016,names.arg = fig4$regioncode,ylim=c(-0.5,6),beside = TRUE,horiz = FALSE,density = fig4$density,angle = fig4$angle,col="Black",cex.names=1,las=1,main="Actual average public debt/3-years moving-average tax base ratio (2010-2016)")
text(plot1,y=fig4$rlfiscapm1016+0.6,labels = round(fig4$rlfiscapm1016,digits=3),cex=1)
plot2<-barplot(fig4$eslfiscapr6016,names.arg = fig4$regioncode,ylim=c(-0.02,0.08),beside = TRUE,horiz = FALSE,density = fig4$density,angle = fig4$angle,col="Black",cex.names=1,las=1,main="Predicted economic significance of public debt/3-years moving-average tax base ratio")
text(plot2,y=ifelse(fig4$eslfiscapr6016>=0,fig4$eslfiscapr6016+0.01,fig4$eslfiscapr6016-0.01),labels = round(fig4$eslfiscapr6016,digits=3),cex=1)
#--------------------------------#


# Figure 5: Public debt/3-year MA tax base (lfiscap) by country
library(readxl)
fig5 <- read_excel("Figures.xlsx",sheet = "fig2")
View(fig5)
fig5<-subset(fig5,select=c("iso","regioncode","clfiscapm1016","eslfiscapc6016"))
View(fig5)
fig5<-fig5[!is.na(fig5$clfiscapm1016)|!is.na(fig5$eslfiscapc6016),]
library(tidyr)
fig5<-gather(fig5,variable,value,-c("iso","regioncode"))
fig5$variable[fig5$variable=="clfiscapm1016"]<-"Actual average public debt/3-years MA tax base (2010-2016)"
fig5$variable[fig5$variable=="eslfiscapc6016"]<-"Predicted economic significance of public debt/3-years MA tax base"
fig5<-fig5[!(fig5$regioncode=="NAC"|fig5$regioncode=="SAS"),]
names(fig5)[2]<-"Region"
library(ggplot2)
setEPS()
postscript("fig5.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 15)
ggplot(fig5,aes(x=iso,y=value,shape=Region))+geom_jitter(aes(width=3, height=3))+facet_grid(variable~.,scales="free", shrink = TRUE)+labs(x="Countries",y=NULL)+geom_text(aes(label=iso),color= "black",hjust=0.5, vjust=2, size = 2.5)+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),panel.border = element_rect(colour = "black"))+theme(strip.text = element_text(size=7),axis.text.x = element_blank(),axis.ticks.x = element_blank())+theme(legend.position="top")+theme(legend.text=element_text(size=12))+guides(shape = guide_legend(override.aes = list(size = 4)))
dev.off()
#--------------------------------#


########### ONLINE APPENDIX
# Figure A1: Public debt/GDP of AEs and EEs
library(readxl)
figA1 <- read_excel("Figures.xlsx", sheet = "figA1")
View(figA1)
names(figA1)[2]<-"Advanced economies"
names(figA1)[3]<-"Emerging economies"
library(tidyr)
figA1<-gather(figA1,Group,Debt,-year)
library(ggplot2)
setEPS()
postscript("figA1.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 6, width = 15)
ggplot(figA1,aes(x=year,y=Debt,shape=Group,color=Group))+geom_line()+scale_x_continuous(breaks=seq(2001, 2015, 3))+labs(x="Year",y="Public Debt/GDP (%)")+theme(legend.text=element_text(size=12))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+theme(legend.position="top")+theme(legend.title=element_blank())+theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
dev.off()
#--------------------------------#


# Figure A2: betaGS for 170 countries by region/income/significance level, sample period 1960-2016
library(ggExtra)
library(readxl)
figA2 <- read_excel("Figures.xlsx", sheet = "figA2")
View(figA2)
figA2$iso1<-1:170
figA2$Region[figA2$Region=="East Asia & Pacific"]<-"East Asia and Pacific"
figA2$Region[figA2$Region=="Europe & Central Asia"]<-"Europe and Central Asia"
figA2$Region[figA2$Region=="Latin America & Caribbean"]<-"Latin America and Caribbean"
figA2$Region[figA2$Region=="Middle East & North Africa"]<-"Middle East and North Africa"
library(plyr)
byregion<-ddply(figA2,.(Region),summarize,beta_mean=mean(Coefficient),iso1_mean=mean(iso1))
library(ggplot2)
setEPS()
postscript("figA2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 6, width = 15)
ggplot(figA2,aes(x=iso1,y=Coefficient,col=Level,shape=Significance))+geom_jitter(aes(),size=2.5)+facet_grid(.~Region)+
  geom_text(aes(label=iso),color= "black",position = "jitter", hjust=1, vjust=1, size = 2)+labs(x=NULL,y="Government spending cyclicality")+
  geom_point(data=byregion,aes(x=iso1_mean,y=beta_mean,fill="red"),shape=21,size=3,inherit.aes = F)+facet_grid(.~Region)+
  scale_fill_identity(name ="Mean", guide ="legend",labels = c("Mean coefficient by region"))+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(strip.text = element_text(size=11),axis.text.x = element_blank(),axis.ticks.x = element_blank())+theme(legend.text=element_text(size=11))+ theme(legend.position="top")
dev.off()
#--------------------------------#


# Figure A3: Public debt/3-year MA tax base (fiscap) by country
library(readxl)
figA3 <- read_excel("Figures.xlsx",sheet = "fig2")
View(figA3)
figA3<-subset(figA3,select=c("iso","regioncode","cfiscapm1016","esfiscapc6016"))
View(figA3)
figA3<-figA3[!is.na(figA3$cfiscapm1016)|!is.na(figA3$esfiscapc6016),]
library(tidyr)
figA3<-gather(figA3,variable,value,-c("iso","regioncode"))
figA3$variable[figA3$variable=="cfiscapm1016"]<-"Actual average public debt/tax base (2010-2016)"
figA3$variable[figA3$variable=="esfiscapc6016"]<-"Predicted economic significance of public debt/tax base"
figA3<-figA3[!(figA3$regioncode=="NAC"|figA3$regioncode=="SAS"),]
names(figA3)[2]<-"Region"
library(ggplot2)
setEPS()
postscript("figA3.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 15)
ggplot(figA3,aes(x=iso,y=value,shape=Region))+geom_jitter(aes(width=3, height=3))+facet_grid(variable~.,scales="free", shrink = TRUE)+labs(x="Countries",y=NULL)+geom_text(aes(label=iso),color= "black",hjust=0.5, vjust=2, size = 2.5)+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),panel.border = element_rect(colour = "black"))+theme(strip.text = element_text(size=7),axis.text.x = element_blank(),axis.ticks.x = element_blank())+theme(legend.position="top")+theme(legend.text=element_text(size=12))+guides(shape = guide_legend(override.aes = list(size = 4)))
dev.off()
#--------------------------------#


# Figure A4: Economic significance of variables to VAT cyclicality
library(readxl)
figA4 <- read_excel("Figures.xlsx", sheet = "fig1")
View(figA4)
names(figA4)[1]<-"Variable"
figA4<-figA4[order(figA4$esVAT_PW),]
attach(figA4)
figA4$starVAT_PW<- ifelse(pvalVAT_PW <= 0.05, "***", ifelse(pvalVAT_PW <= 0.1, "**", ifelse(pvalVAT_PW<=0.2, "*", "")))
attach(figA4)
figA4$varname1<-paste(Variable,starVAT_PW)
figA4$angle<-ifelse(figA4$Group=="Political component risk index",0,ifelse(figA4$Group=="Composite risk index", 45,ifelse(figA4$Group=="Export structure",90, 150)))
figA4$density<-ifelse(figA4$Group=="Political component risk index",0,ifelse(figA4$Group=="Composite risk index", 10,ifelse(figA4$Group=="Export structure",20, 30)))
library(graphics)
setEPS()
postscript("figA4.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 6, width = 15)
par(mar = c(9,4,4,2))
plot<-barplot(figA4$esVAT_PW,names.arg = figA4$varname1,ylim=c(-0.04,0.025),beside = TRUE,horiz = FALSE,density = figA4$density,angle = figA4$angle,col="Black",cex.names=1,las=2)
text(plot,y=ifelse(figA4$esVAT_PW>=0,figA4$esVAT_PW+0.002,figA4$esVAT_PW-0.002),labels = round(figA4$esVAT_PW,digits=4),cex=1)
legend("topleft", legend=c("Composite risk index","Debt","Export structure","Political component risk index"),col="Black",density=c(10,30,20,0),angle=c(45,150,90,0),bty = "n")
dev.off()
#--------------------------------#


# Figure A5: Economic significance of variables to PIT cyclicality
library(readxl)
figA5 <- read_excel("Figures.xlsx", sheet = "fig1")
View(figA5)
names(figA5)[1]<-"Variable"
figA5<-figA5[order(figA5$esPIT_PW),]
attach(figA5)
figA5$starPIT_PW<- ifelse(pvalPIT_PW <= 0.05, "***", ifelse(pvalPIT_PW <= 0.1, "**", ifelse(pvalPIT_PW<=0.2, "*", "")))
attach(figA5)
figA5$varname1<-paste(Variable,starPIT_PW)
figA5$angle<-ifelse(figA5$Group=="Political component risk index",0,ifelse(figA5$Group=="Composite risk index", 45,ifelse(figA5$Group=="Export structure",90, 150)))
figA5$density<-ifelse(figA5$Group=="Political component risk index",0,ifelse(figA5$Group=="Composite risk index", 10,ifelse(figA5$Group=="Export structure",20, 30)))
library(graphics)
setEPS()
postscript("figA5.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 6, width = 15)
par(mar = c(9,4,4,2))
plot<-barplot(figA5$esPIT_PW,names.arg = figA5$varname1,ylim=c(-0.15,0.15),beside = TRUE,horiz = FALSE,density = figA5$density,angle = figA5$angle,col="Black",cex.names=1,las=2)
text(plot,y=ifelse(figA5$esPIT_PW>=0,figA5$esPIT_PW+0.005,figA5$esPIT_PW-0.005),labels = round(figA5$esPIT_PW,digits=3),cex=1)
legend("topleft", legend=c("Composite risk index","Debt","Export structure","Political component risk index"),col="Black",density=c(10,30,20,0),angle=c(45,150,90,0),bty = "n")
dev.off()
#--------------------------------#


# Figure A6: Economic significance of variables to CIT cyclicality
library(readxl)
figA6 <- read_excel("Figures.xlsx", sheet = "fig1")
View(figA6)
names(figA6)[1]<-"Variable"
figA6<-figA6[order(figA6$esCIT_PW),]
attach(figA6)
figA6$starCIT_PW<- ifelse(pvalCIT_PW <= 0.05, "***", ifelse(pvalCIT_PW <= 0.1, "**", ifelse(pvalCIT_PW<=0.2, "*", "")))
attach(figA6)
figA6$varname1<-paste(Variable,starCIT_PW)
figA6$angle<-ifelse(figA6$Group=="Political component risk index",0,ifelse(figA6$Group=="Composite risk index", 45,ifelse(figA6$Group=="Export structure",90, 150)))
figA6$density<-ifelse(figA6$Group=="Political component risk index",0,ifelse(figA6$Group=="Composite risk index", 10,ifelse(figA6$Group=="Export structure",20, 30)))
library(graphics)
setEPS()
postscript("figA6.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 6, width = 15)
par(mar = c(9,4,4,2))
plot<-barplot(figA6$esCIT_PW,names.arg = figA6$varname1,ylim=c(-0.06,0.04),beside = TRUE,horiz = FALSE,density = figA6$density,angle = figA6$angle,col="Black",cex.names=1,las=2)
text(plot,y=ifelse(figA6$esCIT_PW>=0,figA6$esCIT_PW+0.005,figA6$esCIT_PW-0.005),labels = round(figA6$esCIT_PW,digits=4),cex=1)
legend("topleft", legend=c("Composite risk index","Debt","Export structure","Political component risk index"),col="Black",density=c(10,30,20,0),angle=c(45,150,90,0),bty = "n")
dev.off()
#--------------------------------#EndOfCodes#--------------------------------#