diet<- read.csv("birdantdiet.csv")
head(diet)
oecodiet<- diet[diet$oeco == 1,]
chabdiet<- diet[diet$birdlow == 1,]
bhodiet<- diet[diet$birdbho ==1,]

library(gplots)
library(gridExtra)
library(VennDiagram)
venndat1<- list(unique(chabdiet$Order),unique(bhodiet$Order),unique(oecodiet$Order))
venndat2<- list(unique(chabdiet$Family),unique(bhodiet$Family),unique(oecodiet$Family))
venndat3<- list(unique(chabdiet$Genus),unique(bhodiet$Genus),unique(oecodiet$Genus))
venndat4<- list(unique(chabdiet$Species),unique(bhodiet$Species),unique(oecodiet$Species))


venn.plot1 <- venn.diagram(venndat1, NULL,fill= c("steelblue4","khaki1","gray50"),
                           alpha= 0.5, cex = 0.6, cat.fontface= 1, cat.pos = c(300,60,0),
                           cat.dist =c(-0.14,-0.14, -0.08),category.names=c("low", "mid", ""),
                           scaled= TRUE, col= "transparent", fontfamily = "serif",
                           fontface = 1, euler.d= TRUE, cat.cex= 0.6, main= "a. Order",  
                           main.cex = 0.7, main.fontface = "bold")
venn.plot2 <- venn.diagram(venndat2, NULL,fill= c("steelblue4","khaki1","gray50"),
                           alpha= 0.5, cex = 0.6, cat.fontface= 1, cat.pos = c(320,40,0),
                           cat.dist =c(-0.065,-0.065, -0.28),category.names=c("low", "mid", ""),
                           scaled= TRUE, col= "transparent", fontfamily = "serif",
                           fontface = 1, euler.d= TRUE, cat.cex= 0.6, main= "b. Family",  
                           main.cex = 0.7, main.fontface = "bold")
venn.plot3 <- venn.diagram(venndat3, NULL,fill= c("steelblue4","khaki1","gray50"),
                           alpha= 0.5, cex = 0.6, cat.fontface= 1, cat.pos = c(320,40,0),
                           cat.dist =c(-0.065,-0.065, -0.28),category.names=c("low", "mid", ""),
                           scaled= TRUE, col= "transparent", fontfamily = "serif",
                           fontface = 1, euler.d= TRUE, cat.cex= 0.6, main= "c. Genus", 
                           main.fontface = "bold", main.cex = 0.7)
venn.plot4 <- venn.diagram(venndat4, NULL,fill= c("steelblue4","khaki1","gray50"),
                           alpha= 0.5, cex = 0.6, cat.fontface= 1, cat.pos = c(320,40,0),
                           cat.dist =c(-0.065,-0.065, -0.28),category.names=c("low", "mid", ""),
                           scaled= TRUE, col= "transparent", fontfamily = "serif",
                           fontface = 1, euler.d= TRUE, cat.cex= 0.6, main= "d. Species",
                           main.fontface = "bold", main.cex = 0.7)

tiff("dietoverlap.tiff", width = 1100, height = 1100, units = "px", res= 300)
grid.arrange(grobTree(venn.plot1), grobTree(venn.plot2),grobTree(venn.plot3),grobTree(venn.plot4),
             nrow= 2)
dev.off()


#frequency tables
indch<- read.csv("chapdietind.csv")
count<- function(x) tapply(x, indch$Order,sum)
indchdat<- indch[,3:20]
res1<- apply(indchdat,2, count)
res1<- as.data.frame(res1)
res1t<- t(res1)
write.csv(res1t, "tabch.csv") 

indant<- read.csv("weaverantind.csv")
count<- function(x) tapply(x, indant$Order,sum)
indantdat<- indant[,3:25]
res2<- apply(indantdat,2, count)
res2<- as.data.frame(res2)
res2t<- t(res2)
write.csv(res2t, "tabants.csv") 

indbho<- read.csv("bhodietind.csv")
count<- function(x) tapply(x, indbho$Order,sum)
indbhodat<- indbho[,3:18]
res3<- apply(indbhodat,2, count)
res3<- as.data.frame(res3)
res3t<- t(res3)
write.csv(res3t, "tabbho.csv") 


indall<- read.csv("alldietindtaxid.csv")
countf<- function(x) tapply(x, indall$Family,sum)
indalldat<- indall[,6:103]
resf<- apply(indalldat,2, countf)
resf<- as.data.frame(resf)
write.csv(resf, "tabindfam.csv")

counto<- function(x) tapply(x, indall$Order,sum)
reso<- apply(indalldat,2, counto)
reso<- as.data.frame(reso)
write.csv(reso, "tabindorder.csv")

countg<- function(x) tapply(x, indall$Genus,sum)
resg<- apply(indalldat,2, countg)
resg<- as.data.frame(resg)
write.csv(resg, "tabindgenus.csv")

countsp<- function(x) tapply(x, indall$Species,sum)
ressp<- apply(indalldat,2, countsp)
ressp<- as.data.frame(ressp)
write.csv(ressp, "tabindspecies.csv")


#Species accumulation and rarefaction curves
library(vegan)

#Order level
chapv<- read.table("tabch.csv", header= T, sep=",")
sp1 <- specaccum(chapv[,-1])

bhov<- read.csv("tabbho.csv")
sp2<- specaccum(bhov[,-1])

antv<- read.csv("tabants.csv")
sp3<- specaccum(antv[,-1])

#Family level
family<- read.csv("familyaccum.csv")
spf1<- specaccum(family[1:18,-c(1,2)])
spf2<- specaccum(family[19:34, -c(1,2)])
spf3<- specaccum(family[35:57, -c(1,2)])

#Genus level
genus<- read.csv("genusaccum.csv")
spg1<- specaccum(genus[1:18,-c(1,2)])
spg2<- specaccum(genus[19:34, -c(1,2)])
spg3<- specaccum(genus[35:57, -c(1,2)])

#Species level
species<- read.csv("speciesaccum.csv")
sps1<- specaccum(species[1:18,-c(1,2)])
sps2<- specaccum(species[19:34, -c(1,2)])
sps3<- specaccum(species[35:57, -c(1,2)])

tiff("FigS5.tiff",width = 1200, height = 1600, units = "px", res= 300)
par(mfrow=c(4,3), mar= c(3,3,2,1), mgp = c(1.5, 0.3, 0))
plot(sp1,ci.type="poly", ci.lty=0, ci.col="grey", xlab= "Samples", ylab= "Order",
     las= 1, main= "birds, low elevation", cex.main = 0.8, cex.axis= 0.8, tck= -0.04,
     cex.lab= 0.8)
plot(sp2,ci.type="poly", ci.lty=0, ci.col ="grey", xlab= "Samples", ylab= "Order",
     las= 1, main= "birds, mid-elevation", cex.main = 0.8, cex.axis= 0.8, tck= -0.04,
     cex.lab= 0.8)
plot(sp3,ci.type="poly", ci.lty=0, ci.col="grey", xlab= "Samples", ylab= "Order",
     las= 1, main= "ants, low-elevation", cex.main = 0.8, cex.axis= 0.8, tck= -0.04,
     cex.lab= 0.8)
plot(spf1,ci.type="poly", ci.lty=0, ci.col="grey", xlab= "Samples", ylab= "Family",
     las= 1, main= "birds, low elevation", cex.main = 0.8, cex.axis= 0.8, tck= -0.04,
     cex.lab= 0.8)
plot(spf2,ci.type="poly", ci.lty=0, ci.col ="grey", xlab= "Samples", ylab= "Family",
     las= 1, main= "birds, mid-elevation", cex.main = 0.8, cex.axis= 0.8, tck= -0.04,
     cex.lab= 0.8)
plot(spf3,ci.type="poly", ci.lty=0, ci.col="grey", xlab= "Samples", ylab= "Family",
     las= 1, main= "ants, low-elevation", cex.main = 0.8, cex.axis= 0.8, tck= -0.04,
     cex.lab= 0.8)
plot(spg1,ci.type="poly", ci.lty=0, ci.col="grey", xlab= "Samples", ylab= "Genus",
     las= 1, main= "birds, low elevation", cex.main = 0.8, cex.axis= 0.8, tck= -0.04,
     cex.lab= 0.8)
plot(spg2,ci.type="poly", ci.lty=0, ci.col ="grey", xlab= "Samples", ylab= "Genus",
     las= 1, main= "birds, mid-elevation", cex.main = 0.8, cex.axis= 0.8, tck= -0.04,
     cex.lab= 0.8)
plot(spg3,ci.type="poly", ci.lty=0, ci.col="grey", xlab= "Samples", ylab= "Genus",
     las= 1, main= "ants, low-elevation", cex.main = 0.8, cex.axis= 0.8, tck= -0.04,
     cex.lab= 0.8)
plot(sps1,ci.type="poly", ci.lty=0, ci.col="grey", xlab= "Samples", ylab= "Species",
     las= 1, main= "birds, low elevation", cex.main = 0.8, cex.axis= 0.8, tck= -0.04,
     cex.lab= 0.8)
plot(sps2,ci.type="poly", ci.lty=0, ci.col ="grey", xlab= "Samples", ylab= "Species",
     las= 1, main= "birds, mid-elevation", cex.main = 0.8, cex.axis= 0.8, tck= -0.04,
     cex.lab= 0.8)
plot(sps3,ci.type="poly", ci.lty=0, ci.col="grey", xlab= "Samples", ylab= "Species",
     las= 1, main= "ants, low-elevation", cex.main = 0.8, cex.axis= 0.8, tck= -0.04,
     cex.lab= 0.8)
dev.off()


#Code for figure 5
dietexcl3<- read.csv("exclusiondietsum.csv")
library(ggplot2)
library(gridExtra)
dietexcl3$Order <- factor(dietexcl3$Order, levels = c("Lepidoptera","Coleoptera",
                                                      "Hemiptera",
                                                      "Araneae","Diptera","Hymenoptera","Entomobryomorpha",
                                                      "Thysanoptera"))
dietexcl4<- dietexcl3[1:8,]

q<- ggplot(dietexcl3, aes(x=Order, y=Mean_diff)) + 
  geom_bar(position=position_dodge(), stat="identity", width = 0.4) +
  theme_classic() + 
  geom_errorbar(aes(ymin= Mean_diff-SE_diff, ymax= Mean_diff+SE_diff), width=.2,
                position=position_dodge(.9))

p3<- q + theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(), legend.position = c(0.9, 0.9),
               legend.title=element_blank(), 
               plot.margin=unit(c(9.5,5.5,5.5,28),"points")) +
  labs(y = "Difference in 
       mean abundance")

p4<- ggplot(dietexcl4, aes(x= Order, y=Freq_birddiet)) +
  geom_bar(position=position_dodge(), stat="identity",  
           fill= "steelblue", width = 0.4) +
  theme_classic() +
  labs(y = "Frequency in bird diet")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin=unit(c(9.5,5.5,5.5,28),"points"))

library(ggpubr)
tiff("Fig5.tiff",width = 1200, height = 1600, units = "px", res= 300)
ggarrange(p3, p4, labels = c("a.", "b."), nrow = 2,  align = "v",
          heights = c(0.8, 2))
dev.off()

#nmds at the order level
nmdsdat<- read.csv("nmds2.csv")
library(vegan)

nmdsdat4<- nmdsdat[1:57,]
nmdsdat3<- nmdsdat4[,-c(1,2)]
example_NMDS2=metaMDS(nmdsdat3,k=2, trymax=1000) 
stressplot(example_NMDS2)
plot(example_NMDS2)

ordiplot(example_NMDS2,type="n")
ordiellipse(example_NMDS2,groups=nmdsdat4$Type,draw="polygon",col="grey90", label=T)
#the whole ASV table
nmdsasv<- read.csv("nmdsasv.csv")
nmdsasv3<- nmdsasv[1:57,]
nmdsasv2<- nmdsasv3[,-c(1,2)]
asv_NMDS=metaMDS(nmdsasv2,k=2, trymax=1000) 
stressplot(asv_NMDS)
plot(asv_NMDS)
ordiplot(asv_NMDS,type="n")
ordihull(asv_NMDS,groups=nmdsasv3$Type,draw="polygon",col="grey90", label=T)

#family level
nmdsfam<- read.csv("nmdsfam.csv")
nmdsfam3<- nmdsfam[1:57,]
nmdsfam2<- nmdsfam3[,-c(1,2)]
fam_NMDS=metaMDS(nmdsfam2,k=2, trymax=10000) 
stressplot(fam_NMDS)
plot(fam_NMDS)
ordiplot(fam_NMDS,type="n")
ordiellipse(fam_NMDS,groups=nmdsfam3$Type,draw="polygon",col="grey90", label=T)

#genus level
nmdsgenus<- read.csv("nmdsgenus.csv")
nmdsgenus3<- nmdsgenus[1:57,]
nmdsgenus2<- nmdsgenus3[,-c(1,2)]
genus_NMDS=metaMDS(nmdsgenus2,k=2, trymax=10000) 
stressplot(genus_NMDS)
plot(genus_NMDS)
ordiplot(genus_NMDS,type="n")
ordihull(genus_NMDS,groups=nmdsgenus3$Type,draw="polygon",col="grey90", label=T)

#species level
nmdsspecies<- read.csv("nmdsspecies.csv")
nmdsspecies3<- nmdsspecies[1:57,]
nmdsspecies2<- nmdsspecies3[,-c(1,2)]
species_NMDS=metaMDS(nmdsspecies2,k=2, trymax=10000) 
stressplot(species_NMDS)
plot(species_NMDS)
ordiplot(species_NMDS,type="n")
ordihull(species_NMDS,groups=nmdsspecies3$Type,draw="polygon",col="grey90", label=T)

#plot all four together
tiff("nmdsall.tiff",width = 1600, height = 1600, units = "px", res= 300)
par(mfrow=c(2,2),mar= c(3,3,2,1), mgp = c(1.5, 0.3, 0))
ordiplot(example_NMDS2,type="n", main= "(a) Order")
ordiellipse(example_NMDS2,groups=nmdsdat4$Type,draw="polygon",label=F,
            col=c("khaki1","steelblue4","grey50"))
legend("topleft",legend= c("Mid-elevation birds","Low-elevation birds","Weaver ants"), 
       col=c("khaki1","steelblue4","grey80"), horiz=F, cex= 0.7, 
       pt.cex=2, pch= 15, inset=0.02)
ordiplot(fam_NMDS,type="n", main= "(b) Family")
ordiellipse(fam_NMDS,groups=nmdsfam3$Type,draw="polygon",label=F,
            col=c("grey80","steelblue4","khaki1"))
ordiplot(genus_NMDS,type="n", main= "(c) Genus")
ordiellipse(genus_NMDS,groups=nmdsgenus3$Type,draw="polygon",
            col=c("grey80","khaki1","steelblue4"))
ordiplot(species_NMDS,type="n", main= "(d) Species")
ordiellipse(species_NMDS,groups=nmdsspecies3$Type,draw="polygon",
            label=F,col=c("grey80","khaki1","steelblue4"))
dev.off()

#EcoSimR analyses

library(EcoSimR)


Data.File <- read.csv("forecosim.csv")

#Oecophylla, Chapramari and Bhote
bengal<- Data.File[1:3,]
Mod6<- niche_null_model(bengal)
summary(Mod6)
plot(Mod6, type="hist")
plot(Mod6, type="niche")

#Only for Oecophylla & Chapramari birds
loweelev<- Data.File[1:2,]
Mod2 <- niche_null_model(loweelev)
summary(Mod2)
plot(Mod2, type="hist")
plot(Mod2, type="niche")

#Only for Oecophylla & Bhote birds
oecobho<- Data.File[c(1,3),]
Mod3 <- niche_null_model(oecobho)
summary(Mod3)
plot(Mod3, type="hist")
plot(Mod3, type="niche")

#Only for Chapramari & Bhote birds
chabho<- Data.File[c(2,3),]
Mod8 <- niche_null_model(chabho)
summary(Mod8)
plot(Mod8, type="hist")
plot(Mod8, type="niche")

#Only Chapramari and Sachen birds
chasach<- Data.File[c(2,4),]
Mod9<- niche_null_model(chasach)
summary(Mod9)

#Family level analysis
datfam<- read.csv("forecosimfam.csv")
datfam<- datfam[, colSums(datfam != 0) > 0]
Mod4 <- niche_null_model(datfam)
summary(Mod4)
plot(Mod4, type="hist")
plot(Mod4, type="niche")

#only for oecophylla & chapramari
datfam2<- datfam[c(1,3),]
Mod5 <- niche_null_model(datfam2)
summary(Mod5)
plot(Mod5, type="hist")
plot(Mod5, type="niche")

#only for chapramari & bhote
datfam3<- datfam[c(1,2),]
Mod10 <- niche_null_model(datfam3)
summary(Mod10)
plot(Mod10, type="hist")
plot(Mod10, type="niche")

#only for bhote & oecophylla
datfam4<- datfam[c(2,3),]
Mod11 <- niche_null_model(datfam4)
summary(Mod11)
plot(Mod11, type="hist")
plot(Mod11, type="niche")

#Genus level analysis
datgenus<- read.csv("forecosimgenus.csv")
datgenus<- datgenus[, colSums(datgenus != 0) > 0]
Mod12 <- niche_null_model(datgenus)
summary(Mod12)
plot(Mod12, type="hist")
plot(Mod12, type="niche")

#only for oecophylla & chapramari
datgenus2<- datgenus[c(1,3),]
Mod13 <- niche_null_model(datgenus2)
summary(Mod13)
plot(Mod13, type="hist")
plot(Mod13, type="niche")

#Only for chapramari & bhote
datgenus3<- datgenus[c(1,2),]
Mod14 <- niche_null_model(datgenus3)
summary(Mod14)
plot(Mod14, type="hist")
plot(Mod14, type="niche")

#Only for bhote & oecophylla
datgenus4<- datgenus[c(2,3),]
Mod15 <- niche_null_model(datgenus4)
summary(Mod15)
plot(Mod15, type="hist")
plot(Mod15, type="niche")

#Species level analysis
datsp<- read.csv("forecosimspecies.csv")
datsp<- datsp[, colSums(datsp != 0) > 0]
Mod16 <- niche_null_model(datsp)
summary(Mod16)
plot(Mod16, type="hist")
plot(Mod16, type="niche")

#Only for chapramari & bhote
datsp2<- datsp[c(1,2),]
Mod17 <- niche_null_model(datsp2)
summary(Mod17)
plot(Mod17, type="hist")
plot(Mod17, type="niche")

#Only for chapramari & weaver ants
datsp3<- datsp[c(1,3),]
Mod18 <- niche_null_model(datsp3)
summary(Mod18)
plot(Mod18, type="hist")
plot(Mod18, type="niche")

#For Bhote & weaver ants
datsp4<- datsp[c(2,3),]
Mod19 <- niche_null_model(datsp4)
summary(Mod19)
plot(Mod19, type="hist")
plot(Mod19, type="niche")

#ASV level analysis
datasv<- read.csv("ecosimasv.csv")
datasv<- datasv[, colSums(datasv != 0) > 0]
Mod20 <- niche_null_model(datasv)
summary(Mod20)
plot(Mod20, type="hist")
plot(Mod20, type="niche")

#Only for chapramari & bhote
datasv2<- datasv[c(1,2),]
Mod21 <- niche_null_model(datasv2)
summary(Mod21)
plot(Mod21, type="hist")
plot(Mod21, type="niche")

#Only for chapramari & weaver ants
datasv3<- datasv[c(1,3),]
Mod22 <- niche_null_model(datasv3)
summary(Mod22)
plot(Mod22, type="hist")
plot(Mod22, type="niche")

#For Bhote & weaver ants
datasv4<- datasv[c(2,3),]
Mod23 <- niche_null_model(datasv4)
summary(Mod23)
plot(Mod23, type="hist")
plot(Mod23, type="niche")

#Fig. S3
dietelev<- read.csv("orderfreqbirddiet_forfigure3.csv")
dietant<- read.csv("orderfreqantdiet_forfigure3.csv")

base<- ggplot(dietelev, aes(x=Elevation, y=Frequency, color=Order)) + geom_point() + geom_line() + theme_classic() 

tiff("FigS3_Nov2019.tiff",width = 1400, height = 800, units = "px", res= 300)
base+geom_point(data= dietant)
dev.off()



