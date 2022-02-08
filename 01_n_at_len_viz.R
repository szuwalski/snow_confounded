#==numbers at length visualizations
# Bring in observed survey numbers
library(ggplot2)
library(dplyr)
library(reshape)
library(ggridges)
library(gghighlight)
library(gridExtra)

library(gapminder)
library(gganimate)
library(transformr)

# plot(snowad.rep[[1]]$"estimated number of recruits female"[5:38]/1000000~
#        snowad.rep[[1]]$"Predicted Female survey mature Biomass"[1:34],
#      las=1,pch=16,col='grey',ylab='Recruits (1000000)',
#      xlab='Mature biomass (1000t)',xlim=c(0,240),ylim=c(0,2.6))

DATfile <-readLines("20_sq/2016sc.DAT")
surv_yr<-38
tmp<-grep("males new shell immature survey",DATfile)
SurvImmMalNew<-matrix(as.numeric(unlist(strsplit(DATfile[(tmp+3):(tmp+surv_yr+2)],split=" "))),nrow=surv_yr,byrow=T)
tmp<-grep("survey males new shell mature",DATfile)
SurvMatMalNew<-matrix(as.numeric(unlist(strsplit(DATfile[(tmp+3):(tmp+surv_yr+2)],split=" "))),nrow=surv_yr,byrow=T)
tmp<-grep("survey males old shell mature",DATfile)
SurvMatMalOld<-matrix(as.numeric(unlist(strsplit(DATfile[(tmp+2):(tmp+surv_yr+1)],split=" "))),nrow=surv_yr,byrow=T)

tmp<-grep("survey males old shell immature",DATfile)
temp<-unlist(strsplit(DATfile[(tmp+2):(tmp+surv_yr+1)],split=" "))
temp<-unlist(strsplit(temp,split="\t"))
SurvImmMalOld<-matrix(as.numeric(temp),nrow=surv_yr,byrow=T)

totMales<-SurvImmMalNew+SurvMatMalNew+SurvMatMalOld+SurvImmMalOld
colnames(totMales)<-seq(27.5,132.5,5)
rownames(totMales)<-seq(1982,2019)
melted<-melt(totMales)
colnames(melted)<-c("Year","Size","value")
##==ggridges
xlab <- paste0("\n", xlab)
p <- ggplot(data=melted) 
p_natl <- p + geom_density_ridges(aes(x=Size, y=Year, height = value, group = Year, 
                                 fill=stat(y),alpha=.9999),stat = "identity",scale=5) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0)) +
        #text = element_text(size=20)) +
  labs(x="Carapace width (mm)")
print(p_natl)

png("plots/n_at_len.png",height=9,width=6,res=400,units='in')
print(p_natl)
dev.off()

#==pull out 2018 numbers at length
natl_2018<-filter(melted,Year==2018)
#==multiply by size transition matrix
size_trans<-as.matrix(read.csv("data/size_trans.csv",header=F))
#==remove catches
growed<-natl_2018$value %*% size_trans
growed_m<-growed*exp(-0.3)
grow_m<-data.frame(Size=seq(27.5,132.5,5),
                   value=c(growed_m))
#==remove natural mortality
#==plot line
library(RColorBrewer)

stack_p<-ggplot(melted) +
  geom_line(aes(x=Size,y=value,group=as.factor(Year),col=as.factor(Year)),lwd=1.5) +
  gghighlight(Year > 2014,Year<2019, use_direct_label = FALSE)+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position=c(.7,.7)) +
  labs(y="Numbers (1000000s)",x="Carapace width (mm)") +
  geom_line(data=filter(melted,Year==2019),
            aes(x=Size,y=value),col='red',lwd=1.5) +
  geom_line(data=grow_m,aes(x=Size,y=value),col='green',lwd=1.5,lty=2)+ 
  scale_color_manual(values = colorRampPalette(brewer.pal(9, "Blues"))(9)[5:9])+
  labs(col="Year")
print(stack_p)


png("plots/figure_2.png",height=8,width=4,res=400,units='in')
#grid.arrange(p_natl,stack_p,layout_matrix=matrix(c(1,1,2,2),nrow=2))
grid.arrange(p_natl,stack_p)
dev.off()


