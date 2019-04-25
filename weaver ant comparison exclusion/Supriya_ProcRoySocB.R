#analyses on non-experimental data on arthropod abundance with or without weaver ants
dat10<- read.csv("insectsOeco2014-15.csv")
dat11<- read.csv("leafdamageOeco2014-15 .csv")
dat12<- read.csv("leafdamagefig.csv")
library(Rmisc)
t.test(log(dat10$smallOeco +1),log(dat10$smallWithout +1), paired=TRUE)
t.test(log(dat10$largeOeco +1),log(dat10$largeWithout +1), paired=TRUE)

summarySE(dat11, measurevar="branch_damage", groupvars=c("oecophylla.presence"))
summarySE(dat11, measurevar="random_damage", groupvars=c("oecophylla.presence"))

summarySE(dat10, measurevar="largeOeco")
summarySE(dat10, measurevar="largeWithout")

t.test(dat11$branch_damage ~ dat11$oecophylla.presence,  
       paired= TRUE)
t.test(dat11$random_damage ~ dat11$oecophylla.presence, 
       paired= TRUE)

#Analyses on data from ant exclusion experiment
dat4<- read.csv("AEComb_diffAug1.csv")

#remove AE 58 and AE59 from dat4 for the one year after analyses
dat5<- dat4[c(1:6,8:21,23:30),]

t.test(dat4$Smalldiff ~ dat4$Treatment, var.equal=FALSE, 
       paired= TRUE)
summarySE(dat4, measurevar="Smalldiff", groupvars=c("Treatment"))
t.test(dat5$Smalldiffyear ~ dat5$Treatment, var.equal=FALSE, 
       paired= TRUE)
summarySE(dat5, measurevar="Smalldiffyear", groupvars=c("Treatment"))

t.test(dat4$Largediff ~ dat4$Treatment, var.equal=FALSE, 
       paired= TRUE)
summarySE(dat4, measurevar="Largediff", groupvars=c("Treatment"))
t.test(dat5$Largediffyear ~ dat5$Treatment, var.equal=FALSE, 
       paired= TRUE)
summarySE(dat5, measurevar="Largediffyear", groupvars=c("Treatment"))

summarySE(dat4, measurevar="Lbefore", groupvars=c("Treatment"))
summarySE(dat4, measurevar="Lafter", groupvars=c("Treatment"))
#Same for leaf damage
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

#Getting silhouettes for use in the figures
library(rphylopic)
small <- image_data("956150a7-8660-4fe5-8626-ed3172a657c7", size = 64)[[1]]
large<- image_data("956150a7-8660-4fe5-8626-ed3172a657c7", size = 128)[[1]]
leaf<- image_data("f0df9279-c2bf-4ddc-b88b-4610c0c44b5f", size = 128)[[1]]
branch<- image_data("be5ff6f2-c790-4e12-ad01-06a3a357835a", size = 128)[[1]]

#Code for Fig1 ProcB
tiff("Fig1AEProcBApr22_2019.tiff",  width = 1400, height = 1400, units = "px", res= 300)
par(mfrow=c(2,2), mar= c(4,3,2,1), mgp = c(1.7, 0.5, 0))
plot(jitter(dat10$largeOeco),jitter(dat10$largeWithout), xlab= "weaver ants present",
     ylab= "no weaver ants", las= 1,
     xlim= c(0,8), ylim= c(0,8), tck= -0.04, pch= 16,
     cex.lab= 0.9, cex.axis = 0.8, cex= 0.8)
abline(a= 0, b= 1, col= "grey")
text(5.5,7.6,"a. Large arthropods", cex= 0.75, font= 2)
text(5.5,6.65,expression ('t'[16]*' = 2.85, P = 0.01'),  cex= 0.75)
points(1.29,2.59, pch = 17, col= "blue")
add_phylopic_base(large, 0.09, 0.92, 0.13, alpha= 1)

plot(jitter(dat12$randomOeco),jitter(dat12$randomWithout), xlab= "weaver ants present",
     ylab= "no weaver ants", las= 1,
     xlim= c(0,40), ylim= c(0,40), tck= -0.04, pch= 16,
     cex.lab= 0.9, cex.axis = 0.8, cex= 0.8)
abline(a= 0, b= 1, col= "grey")
text(29,38.5,"b. Random leaves", cex= 0.75, font= 2)
text(29,34,expression ('t'[16]*' =2.39, P= 0.03'),  cex= 0.75)
points(12.22,17.29, pch = 17, col= "blue")
add_phylopic_base(leaf, 0.14, 0.9, 0.25, alpha= 1)

plot(jitter(dat4$Lbefore[dat4$Treatment==0]),jitter(dat4$Lbefore[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,17), ylim= c(0,17), tck= -0.04, pch= 16,
     cex.lab= 0.9, cex.axis = 0.8, cex= 0.8)
abline(a= 0, b= 1, col= "grey")
text(14,16,"c. Before", font= 2, cex= 0.75)
points(2.73,1.47, pch = 17, col= "blue")
add_phylopic_base(large, 0.09, 0.92, 0.13, alpha= 1)


plot(jitter(dat4$Lafter[dat4$Treatment==0]),jitter(dat4$Lafter[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,17), ylim= c(0,17), tck= -0.04, pch= 16,
     cex.lab= 0.9, cex.axis = 0.8, cex= 0.8)
abline(a= 0, b= 1, col= "grey")
text(11,16,"d. After one month", font= 2, cex= 0.75)
#text(11,14, expression ('t'[14]*' = 3.7, P= 0.003'),cex= 0.75)
mtext(expression ('t'[14]*' = 3.7, P= 0.003'),cex= 0.65, side= 3, line= -16, outer= TRUE)
points(1.8,4.73, pch = 17, col= "blue")
add_phylopic_base(large, 0.09, 0.92, 0.13, alpha= 1)
dev.off()


#Code for Fig S2
tiff("FigS2.tiff",  width = 1500, height = 800, units = "px", res= 300)
par(mfrow=c(1,2), mar= c(4,3,2,1), mgp = c(1.7, 0.5, 0))
plot(dat10$smallOeco,dat10$smallWithout, xlab= "weaver ants present",
     ylab= "no weaver ants", las= 1,
     xlim= c(0,20), ylim= c(0,20), tck= -0.04, pch= 16,
     cex.lab= 0.8, cex.axis = 0.8, cex= 0.8)
abline(a= 0, b= 1, col= "grey")
text(13.5,19,"a. Small arthropods", cex= 0.7, font= 2)
text(13.5,16.82, expression('t'[16]*' = 0.65, P = 0.52'), cex= 0.7)
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

#Code for Figure S3

tiff("FigS3.tiff",  width = 1800, height = 1400, units = "px", res= 300)
par(mfrow=c(2,3), mar= c(4,3,3,1), mgp = c(1.7, 0.5, 0))
plot(jitter(dat4$Sbefore[dat4$Treatment==0]),jitter(dat4$Sbefore[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,50), ylim= c(0,50), tck= -0.04, pch= 16)
abline(a= 0, b= 1, col= "grey")
text(42,47,"a. before", font= 2)
add_phylopic_base(small, 0.1, 0.9, 0.08, alpha= 1)

par(mar= c(4,3,3,2))
plot(jitter(dat4$Safter[dat4$Treatment==0]),jitter(dat4$Safter[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,50), ylim= c(0,50), tck= -0.04, pch= 16)
abline(a= 0, b= 1, col= "grey")
text(32,47,"b. after one month", font= 2)
#text(33,41, expression ('t'[14]*' = 1.79, P= 0.09'))
mtext(expression ('t'[14]*' = 1.79, P= 0.09'),cex= 0.6, side= 3, line= -2, outer= TRUE, adj=0.35)
add_phylopic_base(small, 0.1, 0.9, 0.08, alpha= 1)

par(mar= c(4,3,3,2))
plot(jitter(dat4$Safteryear[dat4$Treatment==0]),jitter(dat4$Safteryear[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,50), ylim= c(0,50), tck= -0.04, pch= 16)
abline(a= 0, b= 1, col= "grey")
text(34,47,"c. after one year", font= 2)
#text(33,41, expression ('t'[13]*' = 0.71, P= 0.49'))
mtext(expression ('t'[13]*' = 0.71, P= 0.49'),cex= 0.6, side= 3, line= -2, outer= TRUE, adj= 0.7)
add_phylopic_base(small, 0.1, 0.9, 0.08, alpha= 1)

par(mar= c(4,3,3,1))
plot(jitter(dat4$Lbefore[dat4$Treatment==0]),jitter(dat4$Lbefore[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,17), ylim= c(0,17), tck= -0.04, pch= 16)
abline(a= 0, b= 1, col= "grey")
text(14,16,"d. before", font= 2)
add_phylopic_base(large, 0.1, 0.9, 0.13, alpha= 1)

par(mar= c(4,3,3,2))
plot(jitter(dat4$Lafter[dat4$Treatment==0]),jitter(dat4$Lafter[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,17), ylim= c(0,17), tck= -0.04, pch= 16)
abline(a= 0, b= 1, col= "grey")
text(11,16,"e. after one month", font= 2)
#text(11,14, expression ('t'[14]*' = 3.7, P= 0.003'))
mtext(expression ('t'[14]*' = 3.7, P= 0.003') ,cex= 0.6, side= 3, line= -20, outer= TRUE, adj= 0.35)
add_phylopic_base(large, 0.1, 0.9, 0.13, alpha= 1)

par(mar= c(4,3,3,2))
plot(jitter(dat4$Lafteryear[dat4$Treatment==0]),jitter(dat4$Lafteryear[dat4$Treatment==1]), xlab= "weaver ants not removed",
     ylab= "weaver ants removed", las= 1,
     xlim= c(0,17), ylim= c(0,17), tck= -0.04, pch= 16)
abline(a= 0, b= 1, col= "grey")
text(11.5,16,"f. after one year", font= 2)
#text(11,14, expression ('t'[13]*' = 1.4, P= 0.19'))
mtext(expression ('t'[13]*' = 1.4, P= 0.19') ,cex= 0.6, side= 3, line= -20, outer= TRUE, adj= 0.7)
add_phylopic_base(large, 0.1, 0.9, 0.13, alpha= 1)
dev.off()

#Code for Figure S4

tiff("FigS4.tiff", width = 1800, height = 1400, units = "px", res= 300)
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

#order-level t-tests
t.test(dat4$coleodiff ~ dat4$Treatment, var.equal=FALSE, 
       paired= TRUE)
summarySE(dat4, measurevar="coleodiff", groupvars=c("Treatment"))

t.test(dat4$lepiddiff ~ dat4$Treatment, var.equal=FALSE, 
       paired= TRUE)
summarySE(dat4, measurevar="lepiddiff", groupvars=c("Treatment"))

t.test(dat4$hemidiff ~ dat4$Treatment, var.equal=FALSE, 
       paired= TRUE)
summarySE(dat4, measurevar="hemidiff", groupvars=c("Treatment"))
