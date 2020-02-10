#Summary Figure 1 with data from Price et al 2014 and Ghosh-Harihar 2012
datf1<- read.csv("figure1data.csv")

#Code for Figure 1
modf11<- lm(datf1$SpOscines ~ datf1$indoscines)
modf12<- lm(datf1$SpOscines ~ datf1$arthropod.abundance)

cor.test(datf1$indoscines,datf1$SpOscines)
cor.test(datf1$arthropod.abundance,datf1$SpOscines)

tiff("Fig1.tiff",  width = 1400, height = 800, units = "px", res= 300)
par(mfrow=c(1,2), mar= c(4,3,2,1), mgp = c(1.7, 0.5, 0))
plot(datf1$SpOscines ~ datf1$indoscines, ylab= "No. of songbird species", 
     xlab= "No. of songbird individuals", las= 1, cex.lab= 0.8, cex.axis = 0.75, cex= 0.8, tck= -0.04, pch= 16)
abline(modf11)
points(datf1$indoscines[c(4,8)], datf1$SpOscines[c(4,8)], pch= 16, 
       col= c("steelblue","#d8b365"), cex= 1)
text(70,40.5,"a.", cex= 0.75, font= 2)
text(135,40.4, "r = 0.89, p < 0.001", cex= 0.65)

plot(datf1$SpOscines ~ datf1$arthropod.abundance, tck= -0.04,
     ylab= "No. of songbird species", xlab= "No. of arthropods/bag",las= 1, cex.lab= 0.8, 
     cex.axis = 0.75, cex= 0.8, pch=16)
abline(modf12)
points(datf1$arthropod.abundance[c(4,8)], datf1$SpOscines[c(4,8)], pch= 16, 
       col= c("steelblue","#d8b365"), cex= 1)
text(6.5,40.5,"b.", cex= 0.75, font= 2)
text(12.5,40.4, "r = 0.92, p < 0.001", cex= 0.65)
dev.off()

#analyses on non-experimental data on arthropod abundance with or without weaver ants
dat10<- read.csv("insectsOeco2014-15.csv")
dat11<- read.csv("leafdamageOeco2014-15 .csv")
dat12<- read.csv("leafdamagefig.csv")
library(Rmisc)
t.test(log(dat10$totalOeco_homoant +1),log(dat10$totalWithout_homoant +1), paired=TRUE)
t.test(log(dat10$collepOeco +1),log(dat10$collepWithout +1), paired=TRUE)

summarySE(dat11, measurevar="branch_damage", groupvars=c("oecophylla.presence"))
summarySE(dat11, measurevar="random_damage", groupvars=c("oecophylla.presence"))

summarySE(dat10, measurevar= "collepOeco")
summarySE(dat10, measurevar= "collepWithout")
summarySE(dat10, measurevar= "totalOeco_homoant")
summarySE(dat10, measurevar= "totalWithout_homoant")

t.test(dat11$branch_damage ~ dat11$oecophylla.presence,  
       paired= TRUE)
t.test(dat11$random_damage ~ dat11$oecophylla.presence, 
       paired= TRUE)

#Analyses on leaf damage data from ant exclusion experiment
dat4<- read.csv("AEComb_diffAug1.csv")

#remove AE 58 and AE59 from dat4 for the one year after analyses
dat5<- dat4[c(1:6,8:21,23:30),]

dat4$Rlddiff<- dat4$Rldafter - dat4$Rldbefore
dat4$Rlddiffyear<- dat4$Rldafteryear- dat4$Rldbefore
dat5<- dat4[c(1:6,8:21,23:30),]
t.test(dat4$Rlddiff ~ dat4$Treatment, var.equal=FALSE, 
       paired= TRUE)
summarySE(dat4, measurevar="Rlddiff", groupvars=c("Treatment"))
t.test(dat5$Rlddiffyear ~ dat5$Treatment, var.equal=FALSE, 
       paired= TRUE)
summarySE(dat5, measurevar="Rlddiffyear", groupvars=c("Treatment"))

dat4$Brlddiff<- dat4$Brldafter - dat4$Brldbefore
dat4$Brlddiffyear<- dat4$Brldafteryear- dat4$Brldbefore
dat5<- dat4[c(1:6,8:21,23:30),]
t.test(dat4$Brlddiff ~ dat4$Treatment, var.equal=FALSE, 
       paired= TRUE)
summarySE(dat5, measurevar="Brlddiff", groupvars=c("Treatment"))
t.test(dat5$Brlddiffyear ~ dat5$Treatment, var.equal=FALSE, 
       paired= TRUE)
summarySE(dat5, measurevar="Brlddiffyear", groupvars=c("Treatment"))

#arthropod data from ant exclusion experiment
dat7<- read.csv("AEcombdiff_wohomop.csv")
dat8<- dat7[c(1:6,8:21,23:30),]
t.test(dat7$Totalh_diff ~ dat7$Treatment, var.equal=FALSE, 
       paired= TRUE)
t.test(dat8$Totalhyr_diff ~ dat8$Treatment, var.equal=FALSE, 
       paired= TRUE)
summarySE(dat7, measurevar="Totalh_diff", groupvars=c("Treatment"))
summarySE(dat7, measurevar="Totalh_before", groupvars=c("Treatment"))
summarySE(dat7, measurevar="Totalh_after", groupvars=c("Treatment"))


t.test(dat7$collep_diff ~ dat7$Treatment, var.equal=FALSE, 
       paired= TRUE)
summarySE(dat7, measurevar="collep_diff", groupvars=c("Treatment"))

#Getting silhouettes for use in the figures
library(rphylopic)
large<- image_data("956150a7-8660-4fe5-8626-ed3172a657c7", size = 128)[[1]]
leaf<- image_data("f0df9279-c2bf-4ddc-b88b-4610c0c44b5f", size = 128)[[1]]
branch<- image_data("be5ff6f2-c790-4e12-ad01-06a3a357835a", size = 128)[[1]]

#Code for figure 3
tiff("Fig3Nov2019.tiff",  width = 1400, height = 700, units = "px", res= 300)
par(mfrow=c(1,2), mar= c(4,3,2,1), mgp = c(1.2, 0.5, 0))
plot(jitter(dat10$collepOeco),jitter(dat10$collepWithout), xlab= "weaver ants present",
     ylab= "no weaver ants", las= 1,
     xlim= c(0,8), ylim= c(0,8), tck= -0.04, pch= 16,
     cex.lab= 0.7, cex.axis = 0.6, cex= 0.6)
abline(a= 0, b= 1, col= "grey")
text(5,7.6,"a. Coleoptera + Lepidoptera", cex= 0.5, font= 2)
text(5.5,6.65,expression ('t'[16]*' = 2.11, P = 0.05'),  cex= 0.4)
add_phylopic_base(large, 0.09, 0.92, 0.13, alpha= 1)

plot(jitter(dat12$randomOeco),jitter(dat12$randomWithout), xlab= "weaver ants present",
     ylab= "no weaver ants", las= 1,
     xlim= c(0,40), ylim= c(0,40), tck= -0.04, pch= 16,
     cex.lab= 0.7, cex.axis = 0.6, cex= 0.6)
abline(a= 0, b= 1, col= "grey")
text(29,38.5,"b. Random leaves", cex= 0.5, font= 2)
text(29,34,expression ('t'[16]*' =2.39, P= 0.03'),  cex= 0.4)
add_phylopic_base(leaf, 0.14, 0.9, 0.25, alpha= 1)
dev.off()

#Code for Figure 4
tiff("Fig4AENov2019.tiff",  width = 800, height = 800, units = "px", res= 300)
par(mar= c(4,3,2,2), mgp = c(1.2, 0.5, 0))
matplot(t(data.frame(dat7$Totalh_before[dat7$Treatment==0],dat7$Totalh_after[dat7$Treatment==0])), type="b", pch=19, col= adjustcolor("black", alpha=0.3),
        lty=1,xaxt="n",ylab="Number of arthropods", las= 1, 
        tck= -0.02, cex.lab= 0.6, cex.axis= 0.5, ylim= c(0,25), cex= 0.5)
axis(1, at= c(1,2),labels= c("before","one month after"), cex.axis= 0.6)
matpoints(t(data.frame(dat7$Totalh_before[dat7$Treatment==1],dat7$Totalh_after[dat7$Treatment==1])), 
          pch=19, col= adjustcolor("purple", alpha=0.3), cex= 0.5)
matlines(t(data.frame(dat7$Totalh_before[dat7$Treatment==1],dat7$Totalh_after[dat7$Treatment==1])), 
         col= adjustcolor("purple", alpha=0.3), lty= 1)
legend("topleft", inset=0.01, legend= c("Control","Ant exclusion"), 
       col=c("black","purple"), horiz=F, cex= 0.4, lty= 1, lwd= 2, bty= "n")
text(1.2,21,expression ('t'[14]*' = 3.7, P= 0.003'),cex= 0.4)
dev.off()

#Code for figure S2
library(rphylopic)
small <- image_data("956150a7-8660-4fe5-8626-ed3172a657c7", size = 64)[[1]]
bird <- image_data("ec11f62e-ed3e-49bb-bcff-4e47b281d378", size = 128)[[1]]
#credit to Anthony Caravaggi for the bird silhouette(https://creativecommons.org/licenses/by-nc-sa/3.0/)
ant <- image_data("45ccedd3-d5cb-42e8-b158-7e027ab1ff22", size = 128)[[1]]

tiff("FigS2.tiff",  width = 1400, height = 1400, units = "px", res= 300)
par(mfrow=c(2,2), mar= c(4,3,2,1), mgp = c(1.7, 0.5, 0))

plot(datf1$SpOscines ~ datf1$Elevation, ylim= c(8,42), ylab= "Number of songbird species", 
     xlab= "Elevation", las= 1, cex.lab= 0.9, cex.axis = 0.8, cex= 0.8, tck= -0.04)
points(datf1$SpInsectivores ~ datf1$Elevation, pch=16)
text(200,40,"a.", cex= 0.75, font= 2)
add_phylopic_base(bird, 0.14, 0.9, 0.13, alpha= 1)

plot(datf1$indoscines ~ datf1$Elevation, ylim= c(0,250), tck= -0.04,
     ylab= "Number of songbird individuals", xlab= "Elevation",las= 1, cex.lab= 0.9, 
     cex.axis = 0.8, cex= 0.8)
points(datf1$indinsectivores ~ datf1$Elevation, pch=16)
text(200,240,"b.", cex= 0.75, font= 2)
add_phylopic_base(bird, 0.14, 0.9, 0.13, alpha= 1)

plot(datf1$arthropod.abundance ~ datf1$Elevation, ylim= c(0,25), pch=16, tck= -0.04,
     ylab= "Number of arthropods/bag", xlab= "Elevation",las= 1, cex.lab= 0.9, 
     cex.axis = 0.8, cex= 0.8)
text(200,24,"c.", cex= 0.75, font= 2)
add_phylopic_base(small, 0.14, 0.92, 0.13, alpha= 1)

plot(datf1$ant.abundance ~ datf1$Elevation, pch=16, ylab= "Number of ants in a bag", 
     xlab= "Elevation",las= 1, cex.lab= 0.9, cex.axis = 0.8, cex= 0.8, tck= -0.04)
text(200,1.5,"d.", cex= 0.75, font= 2)
add_phylopic_base(ant, 0.13, 0.91, 0.1, alpha= 1)
dev.off()

#Code for Fig S6Nov2019
tiff("FigS6Nov2019.tiff",  width = 1500, height = 800, units = "px", res= 300)
par(mfrow=c(1,2), mar= c(4,3,2,1), mgp = c(1.7, 0.5, 0))
plot(dat10$totalOeco_homoant,dat10$totalWithout_homoant, xlab= "weaver ants present",
     ylab= "no weaver ants", las= 1,
     xlim= c(0,20), ylim= c(0,20), tck= -0.04, pch= 16,
     cex.lab= 0.8, cex.axis = 0.8, cex= 0.8)
abline(a= 0, b= 1, col= "grey")
text(13.5,19,"a. No. of arthropods", cex= 0.7, font= 2)
text(13.5,16.82, expression('t'[16]*' = 0.69, P = 0.49'), cex= 0.7)
add_phylopic_base(small, 0.1, 0.9, 0.08, alpha= 1)

plot(dat12$branchOeco,dat12$branchWithout, xlab= "weaver ants present",
     ylab= "no weaver ants", las= 1,
     xlim= c(0,40), ylim= c(0,40), tck= -0.04, pch= 16,
     cex.lab= 0.8, cex.axis = 0.8, cex= 0.8)
abline(a= 0, b= 1, col= "grey")
text(28,38.5,"b. Clipped branch", cex= 0.7, font= 2)
text(28,34, expression ('t'[16]*' =1.45, P= 0.16'),  cex= 0.7)
add_phylopic_base(branch, 0.1, 0.9, 0.12, alpha= 1)
dev.off()

#Code for Fig S7
tiff("FigS7Nov2019.tiff",  width = 800, height = 800, units = "px", res= 300)
par(mar= c(4,3,2,2), mgp = c(1.2, 0.5, 0))
matplot(t(data.frame(dat8$Totalh_before[dat8$Treatment==0],dat8$Totalh_year[dat8$Treatment==0])), type="b", pch=19, col= adjustcolor("black", alpha=0.3),
        lty=1,xaxt="n",ylab="Number of arthropods", las= 1, 
        tck= -0.02, cex.lab= 0.6, cex.axis= 0.5, ylim= c(0,30), cex= 0.5)
axis(1, at= c(1,2),labels= c("before","one year after"), cex.axis= 0.6)
matpoints(t(data.frame(dat8$Totalh_before[dat8$Treatment==1],dat8$Totalh_year[dat8$Treatment==1])), 
          pch=19, col= adjustcolor("purple", alpha=0.3), cex= 0.5)
matlines(t(data.frame(dat8$Totalh_before[dat8$Treatment==1],dat8$Totalh_year[dat8$Treatment==1])), 
         col= adjustcolor("purple", alpha=0.3), lty= 1)
legend("topleft", inset=0.01, legend= c("Control","Ant exclusion"), 
       col=c("black","purple"), horiz=F, cex= 0.4, lty= 1, lwd= 2, bty= "n")
text(1.2,25,expression ('t'[13]*' = 1.8, P= 0.09'),cex= 0.4)
dev.off()

#Code for Figure S8

tiff("FigS8.tiff", width = 1800, height = 1400, units = "px", res= 300)
par(mfrow=c(2,3), mar= c(4,3,2,1), mgp = c(1.7, 0.5, 0))
plot(jitter(dat4$Rldbefore[dat4$Treatment==0]),jitter(dat4$Rldbefore[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,41), ylim= c(0,41), tck= -0.04, pch= 16)
abline(a= 0, b= 1, col= "grey")
text(35,39,"a. before", font= 2)
par(mar= c(4,3,2,2))
plot(jitter(dat4$Rldafter[dat4$Treatment==0]),jitter(dat4$Rldafter[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,41), ylim= c(0,41), tck= -0.04, pch= 16, 
     main= "leaf damage on random leaves", cex.main= 0.96)
abline(a= 0, b= 1, col= "grey")
text(27,39,"b. after one month", font= 2)
par(mar= c(4,3,2,2))
plot(jitter(dat4$Rldafteryear[dat4$Treatment==0]),jitter(dat4$Rldafteryear[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,41), ylim= c(0,41), tck= -0.04, pch= 16)
abline(a= 0, b= 1, col= "grey")
text(28,39,"c. after one year",font= 2)

plot(jitter(dat4$Brldbefore[dat4$Treatment==0]),jitter(dat4$Brldbefore[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,54), ylim= c(0,54), tck= -0.04, pch= 16)
abline(a= 0, b= 1, col= "grey")
text(45,52,"d. before",font= 2)
par(mar= c(4,3,2,2))
plot(jitter(dat4$Brldafter[dat4$Treatment==0]),jitter(dat4$Brldafter[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,54), ylim= c(0,54), tck= -0.04, pch= 16, 
     main= "leaf damage on clipped branch", cex.main= 0.96)
abline(a= 0, b= 1, col= "grey")
text(36,52,"e. after one month",font= 2)
par(mar= c(4,3,2,2))
plot(jitter(dat4$Brldafteryear[dat4$Treatment==0]),jitter(dat4$Brldafteryear[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,54), ylim= c(0,54), tck= -0.04, pch= 16)
abline(a= 0, b= 1, col= "grey")
text(38,52,"f. after one year",font= 2)
dev.off()

