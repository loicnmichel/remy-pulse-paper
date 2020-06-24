#Load necessary packages
library(SIBER)
library(ggplot2)
library(RColorBrewer)
library(officer)
library(rvg)
library(magrittr)
library(devEMF)
library(cowplot)



################################################################################
# Part 1 - Community niches (Figure 3)
################################################################################

#Read in the data
CommsImp <- read.csv("community.csv", header=T, sep=",")

#Extract data for each season
SplitComm <- split(CommsImp, CommsImp$community)
august <- SplitComm$`1`
november <-SplitComm$`2`
march <- SplitComm$`3`
may <- SplitComm$`4`

#Compute species means for each species in each season
means.iso1.august <- aggregate(august$iso1,list(august$group),mean)$x
means.iso2.august <- aggregate(august$iso2,list(august$group),mean)$x
group.august <- aggregate(august$community,list(august$group),mean)$x
community.august <- aggregate(august$community,list(august$group),mean)$x
CommaugustRaw <- cbind(means.iso1.august,means.iso2.august,group.august,community.august)

means.iso1.november <- aggregate(november$iso1,list(november$group),mean)$x
means.iso2.november <- aggregate(november$iso2,list(november$group),mean)$x
group.november <- aggregate(november$community,list(november$group),mean)$x
community.november <- aggregate(november$community,list(november$group),mean)$x
CommnovemberRaw <- cbind(means.iso1.november,means.iso2.november,group.november,community.november)

means.iso1.march <- aggregate(march$iso1,list(march$group),mean)$x
means.iso2.march <- aggregate(march$iso2,list(march$group),mean)$x
group.march <- aggregate(march$community,list(march$group),mean)$x
community.march <- aggregate(march$community,list(march$group),mean)$x
CommmarchRaw <- cbind(means.iso1.march,means.iso2.march,group.march,community.march)

means.iso1.may <- aggregate(may$iso1,list(may$group),mean)$x
means.iso2.may <- aggregate(may$iso2,list(may$group),mean)$x
group.may <- aggregate(may$community,list(may$group),mean)$x
community.may <- aggregate(may$community,list(may$group),mean)$x
CommmayRaw <- cbind(means.iso1.may,means.iso2.may,group.may,community.may)

#Put all species mean in a data frame and format it for SIBER
CommRaw <- rbind (CommaugustRaw, CommnovemberRaw, CommmarchRaw, CommmayRaw)
colnames(CommRaw)[1] <- "iso1"
colnames(CommRaw)[2] <- "iso2"
colnames(CommRaw)[3] <- "group"
colnames(CommRaw)[4] <- "community"
CommClean <- as.data.frame(CommRaw)

#Turn the data in a SIBER object
CommSIBER <- createSiberObject(CommClean)

#Compute ellipse metrics
group.ML.Comms <- groupMetricsML(CommSIBER)

#Organize them in a table and export it
Metrcomm<-cbind(c("August","November","March","May"),
                c("Whole Community","Whole Community","Whole Community","Whole Community"),
                round(t(group.ML.Comms),2))
write.csv(Metrcomm,"nichemetrics.csv", row.names = FALSE)

#Get ellipse coordinates for communities in each season
SEComms1 <- addEllipse(CommSIBER$ML.mu[[1]][ , , 1],
                       CommSIBER$ML.cov[[1]][ , , 1],
                       m = NULL,
                       n = 100,
                       p.interval = NULL,
                       ci.mean = FALSE)

SEComms2 <- addEllipse(CommSIBER$ML.mu[[2]][ , , 1],
                       CommSIBER$ML.cov[[2]][ , , 1],
                       m = NULL,
                       n = 100,
                       p.interval = NULL,
                       ci.mean = FALSE)

SEComms3 <- addEllipse(CommSIBER$ML.mu[[3]][ , , 1],
                       CommSIBER$ML.cov[[3]][ , , 1],
                       m = NULL,
                       n = 100,
                       p.interval = NULL,
                       ci.mean = FALSE)

SEComms4 <- addEllipse(CommSIBER$ML.mu[[4]][ , , 1],
                       CommSIBER$ML.cov[[4]][ , , 1],
                       m = NULL,
                       n = 100,
                       p.interval = NULL,
                       ci.mean = FALSE)

#Organize them in a data frame usable by ggplot
Comms1_ <- rep(1, length(SEComms1[,1]))
Comms2_ <- rep(2, length(SEComms2[,1]))
Comms3_ <- rep(3, length(SEComms3[,1]))
Comms4_ <- rep(4, length(SEComms4[,1]))
Comms_ <- c(Comms1_, Comms2_, Comms3_, Comms4_)
xComms <- c(SEComms1[,1],SEComms2[,1],SEComms3[,1],SEComms4[,1])
yComms <- c(SEComms1[,2],SEComms2[,2],SEComms3[,2],SEComms4[,2])
df_SEComms <- data.frame(xComms,yComms,Comms_)

#Change color palette
Commscol <- brewer.pal(4, "Set2")

#Plot the niches in ggplot
plotComms <- ggplot(CommClean, aes(x=iso1, y=iso2, group=factor(group))) +
  theme_bw() +
  geom_point(aes(shape=factor(group), color=factor(group)), size=2) +
  scale_shape_manual(values=c(16, 15, 17, 1))+
  scale_color_manual(labels = c("August","November", "March","May"), 
                     values= Commscol) +
  scale_x_continuous(name="d13C", limits=c(-22.1, -13.5), breaks=c(-12,-14,-16,-18,-20,-22), minor_breaks = c(-13,-15,-17,-19,-21)) +
  scale_y_continuous(name="d15N", limits=c(0.9,8.2), breaks=c(2,4,6,8)) +
  geom_path(data=df_SEComms, aes(x=xComms, y=yComms, group=factor(Comms_), color=factor(Comms_), linetype=factor(Comms_), size=factor(Comms_))) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid")) +
  scale_size_manual(values=c(1.1, 1.1, 1.1, 1.1)) +
  guides(size=FALSE, linetype=FALSE, shape=FALSE)+
  guides(color=guide_legend(title="Seasons",
                            keywidth=1.3,
                            title.hjust=0.5,
                            override.aes = list(
                              shape= c(16, 15, 17, 1),
                              linetype=c("solid","solid","solid","solid")
                            ))) +
  ggtitle("Community niches") +
  theme(plot.title = element_text(hjust = 0))
plotComms

#Export the graph to powerpoint for easy tweaking of small details and integration into manuscript document
#Please note that some characters are not supported (hence the lack of delta and per mille in axis titles)
PlotComms_forexport <- dml(ggobj = plotComms)

read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(value=PlotComms_forexport, location = ph_location(width=9, height=7, type = "body") ) %>% 
  print(target = "PlotComms.pptx") %>% 
  invisible()



################################################################################
# Part 2 - Population niches for the 5 dominant species (Figure 4)
################################################################################

#Read in the data and select adequate columns
FiveSpImp <- read.csv("5species.csv", header=T, sep=",")
FiveSpClean <- FiveSpImp[1:4]

#Split the data by season
SplitFiveSp <- split(FiveSpClean, FiveSpClean$community)
FiveSpAug <- SplitFiveSp$`1`
FiveSpNov <- SplitFiveSp$`2`
FiveSpMar <- SplitFiveSp$`3`
FiveSpMay <- SplitFiveSp$`4`

#Set the color palette
Col5Sp <- brewer.pal(5, "Set1")

#Plot for August
#Create SIBER object
SIBER5spAug <- createSiberObject(FiveSpAug)

#Compute niche metrics
group.ML.Aug <- groupMetricsML(SIBER5spAug)

#Extract ellipse coordinates for each species
SEAug1 <- addEllipse(SIBER5spAug$ML.mu[[1]][ , , 1],
                     SIBER5spAug$ML.cov[[1]][ , , 1],
                       m = NULL,
                       n = 100,
                       p.interval = NULL,
                       ci.mean = FALSE)

SEAug2 <- addEllipse(SIBER5spAug$ML.mu[[1]][ , , 2],
                     SIBER5spAug$ML.cov[[1]][ , , 2],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)

SEAug3 <- addEllipse(SIBER5spAug$ML.mu[[1]][ , , 3],
                     SIBER5spAug$ML.cov[[1]][ , , 3],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)

SEAug4 <- addEllipse(SIBER5spAug$ML.mu[[1]][ , , 4],
                     SIBER5spAug$ML.cov[[1]][ , , 4],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)

SEAug5 <- addEllipse(SIBER5spAug$ML.mu[[1]][ , , 5],
                     SIBER5spAug$ML.cov[[1]][ , , 5],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)

#Put ellipse coordinates in a data frame that ggplot can use
Aug1_ <- rep(1, length(SEAug1[,1]))
Aug2_ <- rep(2, length(SEAug2[,1]))
Aug3_ <- rep(3, length(SEAug3[,1]))
Aug4_ <- rep(4, length(SEAug4[,1]))
Aug5_ <- rep(5, length(SEAug5[,1]))
Aug_ <- c(Aug1_, Aug2_, Aug3_, Aug4_, Aug5_)
xAug <- c(SEAug1[,1],SEAug2[,1],SEAug3[,1],SEAug4[,1],SEAug5[,1])
yAug <- c(SEAug1[,2],SEAug2[,2],SEAug3[,2],SEAug4[,2],SEAug5[,2])
df_SEAug <- data.frame(xAug,yAug,Aug_)

#Plot ellipses for the 5 species in August
#Please note the typo in the graph title - some letters would cause graphs to be misaligned when piecing the figure together
plotAug <- ggplot(FiveSpAug, aes(x=iso1, y=iso2, group=factor(group))) +
  theme_bw() +
  geom_point(aes(shape=factor(group), color=factor(group)), size=2) +
  scale_shape_manual(values=c(16, 15, 17, 1, 0))+
  scale_color_manual(labels = c("A. nitescens","G. aequicauda", "G. fucicola","M. hergensis","P. xiphias"), 
                     values= Col5Sp) +
  scale_x_continuous(name="d13C", limits=c(-21.5, -12.8), breaks=c(-12,-14,-16,-18,-20), minor_breaks = c(-13,-15,-17,-19,-21)) +
  scale_y_continuous(name="d15N", limits=c(-1,7.5), breaks=c(0,2,4,6)) +
  geom_path(data=df_SEAug, aes(x=xAug, y=yAug, group=factor(Aug_), color=factor(Aug_), linetype=factor(Aug_), size=factor(Aug_))) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid")) +
  scale_size_manual(values=c(1.1, 1.1, 1.1, 1.1, 1.1)) +
  #theme(panel.grid.minor = element_blank()) +
  guides(size=FALSE, linetype=FALSE, shape=FALSE, color=FALSE)+
  ggtitle("Auhust") +
  theme(plot.title = element_text(hjust = 0))
plotAug

#Plot for November
#It's the same code as above but for a different season, so it won't be commented
SIBER5spNov <- createSiberObject(FiveSpNov)
group.ML.Nov <- groupMetricsML(SIBER5spNov)
SENov1 <- addEllipse(SIBER5spNov$ML.mu[[1]][ , , 1],
                     SIBER5spNov$ML.cov[[1]][ , , 1],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
SENov2 <- addEllipse(SIBER5spNov$ML.mu[[1]][ , , 2],
                     SIBER5spNov$ML.cov[[1]][ , , 2],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
SENov3 <- addEllipse(SIBER5spNov$ML.mu[[1]][ , , 3],
                     SIBER5spNov$ML.cov[[1]][ , , 3],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)

SENov4 <- addEllipse(SIBER5spNov$ML.mu[[1]][ , , 4],
                     SIBER5spNov$ML.cov[[1]][ , , 4],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
SENov5 <- addEllipse(SIBER5spNov$ML.mu[[1]][ , , 5],
                     SIBER5spNov$ML.cov[[1]][ , , 5],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
Nov1_ <- rep(1, length(SENov1[,1]))
Nov2_ <- rep(2, length(SENov2[,1]))
Nov3_ <- rep(3, length(SENov3[,1]))
Nov4_ <- rep(4, length(SENov4[,1]))
Nov5_ <- rep(5, length(SENov5[,1]))
Nov_ <- c(Nov1_, Nov2_, Nov3_, Nov4_, Nov5_)
xNov <- c(SENov1[,1],SENov2[,1],SENov3[,1],SENov4[,1],SENov5[,1])
yNov <- c(SENov1[,2],SENov2[,2],SENov3[,2],SENov4[,2],SENov5[,2])
df_SENov <- data.frame(xNov,yNov,Nov_)
plotNov <- ggplot(FiveSpNov, aes(x=iso1, y=iso2, group=factor(group))) +
  theme_bw() +
  geom_point(aes(shape=factor(group), color=factor(group)), size=2) +
  scale_shape_manual(values=c(16, 15, 17, 1, 0))+
  scale_color_manual(labels = c("A. nitescens","G. aequicauda", "G. fucicola","M. hergensis","P. xiphias"), 
                     values= Col5Sp) +
  scale_x_continuous(name="d13C", limits=c(-21.5, -12.8), breaks=c(-12,-14,-16,-18,-20), minor_breaks = c(-13,-15,-17,-19,-21)) +
  scale_y_continuous(name="d15N", limits=c(-1,7.5), breaks=c(0,2,4,6)) +
  geom_path(data=df_SENov, aes(x=xNov, y=yNov, group=factor(Nov_), color=factor(Nov_), linetype=factor(Nov_), size=factor(Nov_))) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid")) +
  scale_size_manual(values=c(1.1, 1.1, 1.1, 1.1, 1.1)) +
  #theme(panel.grid.minor = element_blank()) +
  guides(size=FALSE, linetype=FALSE, shape=FALSE, color=FALSE)+
  ggtitle("November") +
  theme(plot.title = element_text(hjust = 0))
plotNov

#Plot for March
#It's the same code as above but for a different season, so it won't be commented
SIBER5spMar <- createSiberObject(FiveSpMar)
group.ML.Mar <- groupMetricsML(SIBER5spMar)
SEMar1 <- addEllipse(SIBER5spMar$ML.mu[[1]][ , , 1],
                     SIBER5spMar$ML.cov[[1]][ , , 1],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
SEMar2 <- addEllipse(SIBER5spMar$ML.mu[[1]][ , , 2],
                     SIBER5spMar$ML.cov[[1]][ , , 2],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
SEMar3 <- addEllipse(SIBER5spMar$ML.mu[[1]][ , , 3],
                     SIBER5spMar$ML.cov[[1]][ , , 3],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
SEMar4 <- addEllipse(SIBER5spMar$ML.mu[[1]][ , , 4],
                     SIBER5spMar$ML.cov[[1]][ , , 4],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
SEMar5 <- addEllipse(SIBER5spMar$ML.mu[[1]][ , , 5],
                     SIBER5spMar$ML.cov[[1]][ , , 5],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
Mar1_ <- rep(1, length(SEMar1[,1]))
Mar2_ <- rep(2, length(SEMar2[,1]))
Mar3_ <- rep(3, length(SEMar3[,1]))
Mar4_ <- rep(4, length(SEMar4[,1]))
Mar5_ <- rep(5, length(SEMar5[,1]))
Mar_ <- c(Mar1_, Mar2_, Mar3_, Mar4_, Mar5_)
xMar <- c(SEMar1[,1],SEMar2[,1],SEMar3[,1],SEMar4[,1],SEMar5[,1])
yMar <- c(SEMar1[,2],SEMar2[,2],SEMar3[,2],SEMar4[,2],SEMar5[,2])
df_SEMar <- data.frame(xMar,yMar,Mar_)
plotMar <- ggplot(FiveSpMar, aes(x=iso1, y=iso2, group=factor(group))) +
  theme_bw() +
  geom_point(aes(shape=factor(group), color=factor(group)), size=2) +
  scale_shape_manual(values=c(16, 15, 17, 1, 0))+
  scale_color_manual(labels = c("A. nitescens","G. aequicauda", "G. fucicola","M. hergensis","P. xiphias"), 
                     values= Col5Sp) +
  scale_x_continuous(name="d13C", limits=c(-21.5, -12.8), breaks=c(-12,-14,-16,-18,-20), minor_breaks = c(-13,-15,-17,-19,-21)) +
  scale_y_continuous(name="d15N", limits=c(-1,7.5), breaks=c(0,2,4,6)) +
  geom_path(data=df_SEMar, aes(x=xMar, y=yMar, group=factor(Mar_), color=factor(Mar_), linetype=factor(Mar_), size=factor(Mar_))) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid")) +
  scale_size_manual(values=c(1.1,1.1,1.1,1.1,1.1)) +
  guides(size=FALSE, linetype=FALSE, shape=FALSE, color=FALSE)+
  ggtitle("March") +
  theme(plot.title = element_text(hjust = 0))
plotMar

#Plot for May
#It's the same code as above but for a different season, so it won't be commented
SIBER5spMay <- createSiberObject(FiveSpMay)
group.ML.May <- groupMetricsML(SIBER5spMay)
SEMay1 <- addEllipse(SIBER5spMay$ML.mu[[1]][ , , 1],
                     SIBER5spMay$ML.cov[[1]][ , , 1],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
SEMay2 <- addEllipse(SIBER5spMay$ML.mu[[1]][ , , 2],
                     SIBER5spMay$ML.cov[[1]][ , , 2],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
SEMay3 <- addEllipse(SIBER5spMay$ML.mu[[1]][ , , 3],
                     SIBER5spMay$ML.cov[[1]][ , , 3],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
SEMay4 <- addEllipse(SIBER5spMay$ML.mu[[1]][ , , 4],
                     SIBER5spMay$ML.cov[[1]][ , , 4],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
SEMay5 <- addEllipse(SIBER5spMay$ML.mu[[1]][ , , 5],
                     SIBER5spMay$ML.cov[[1]][ , , 5],
                     m = NULL,
                     n = 100,
                     p.interval = NULL,
                     ci.mean = FALSE)
May1_ <- rep(1, length(SEMay1[,1]))
May2_ <- rep(2, length(SEMay2[,1]))
May3_ <- rep(3, length(SEMay3[,1]))
May4_ <- rep(4, length(SEMay4[,1]))
May5_ <- rep(5, length(SEMay5[,1]))
May_ <- c(May1_, May2_, May3_, May4_, May5_)
xMay <- c(SEMay1[,1],SEMay2[,1],SEMay3[,1],SEMay4[,1],SEMay5[,1])
yMay <- c(SEMay1[,2],SEMay2[,2],SEMay3[,2],SEMay4[,2],SEMay5[,2])
df_SEMay <- data.frame(xMay,yMay,May_)
plot(df_SEMay$xMay, df_SEMay$yMay)
plotMay <- ggplot(FiveSpMay, aes(x=iso1, y=iso2, group=factor(group))) +
  theme_bw() +
  geom_point(aes(shape=factor(group), color=factor(group)), size=2) +
  scale_shape_manual(values=c(16, 15, 17, 1, 0))+
  scale_color_manual(labels = c("A. nitescens","G. aequicauda", "G. fucicola","M. hergensis","P. xiphias"), 
                     values= Col5Sp) +
  scale_x_continuous(name="d13C", limits=c(-21.5, -12.8), breaks=c(-12,-14,-16,-18,-20), minor_breaks = c(-13,-15,-17,-19,-21)) +
  scale_y_continuous(name="d15N", limits=c(-1,7.5), breaks=c(0,2,4,6)) +
  geom_path(data=df_SEMay, aes(x=xMay, y=yMay, group=factor(May_), color=factor(May_), linetype=factor(May_), size=factor(May_))) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid")) +
  scale_size_manual(values=c(1.1, 1.1, 1.1, 1.1, 1.1)) +
  #theme(panel.grid.minor = element_blank()) +
  guides(size=FALSE, linetype=FALSE, shape=FALSE, color=FALSE)+
  ggtitle("May") +
  theme(plot.title = element_text(hjust = 0))
plotMay


#Assemble the 4 parts of the figure
plot5sp <- plot_grid(plotAug, plotNov, plotMar, plotMay,  ncol=2)
plot5sp

#Export the graph to powerpoint for easy tweaking of small details and integration into manuscript document
#Please note that some characters are not supported (hence the lack of delta and per mille in axis titles)
Plot5sp_forexport <- dml(ggobj = plot5sp)

read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(value=Plot5sp_forexport, location = ph_location(width=11.5, height=7.5, type = "body") ) %>% 
  print(target = "Plot5sp.pptx") %>% 
  invisible()

#This is a random plot just to get a legend. I then used powerpoint to piece it back on the main figure.
#There's probably a much more elegant way to do that. Drop me a line if you know one.
plotLeg <- ggplot(FiveSpAug, aes(x=iso1, y=iso2, group=factor(group))) +
  theme_bw() +
  geom_point(aes(shape=factor(group), color=factor(group)), size=2) +
  scale_shape_manual(values=c(16, 15, 17, 1, 0))+
  scale_color_manual(labels = c("Athanas nitescens","Gammarus aequicauda", "Gammarella fucicola","Melita hergensis","Palaemon xiphias"), 
                     values= Col5Sp) +
  scale_x_continuous(name="d13C", limits=c(-21.5, -12.8), breaks=c(-12,-14,-16,-18,-20), minor_breaks = c(-13,-15,-17,-19,-21)) +
  scale_y_continuous(name="d15N", limits=c(-1,7.5), breaks=c(0,2,4,6)) +
  geom_path(data=df_SEAug, aes(x=xAug, y=yAug, group=factor(Aug_), color=factor(Aug_), linetype=factor(Aug_), size=factor(Aug_))) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid")) +
  scale_size_manual(values=c(1.1, 1.1, 1.1, 1.1, 1.1)) +
  #theme(panel.grid.minor = element_blank()) +
  guides(size=FALSE, linetype=FALSE, shape=FALSE)+
  guides(color=guide_legend(title="Species",
                            keywidth=1.3,
                            title.hjust=0.5,
                            override.aes = list(
                              shape= c(16, 15, 17, 1, 0),
                              linetype=c("solid","solid","solid","solid", "solid")
                            ))) +
  theme(legend.position="bottom") +
  ggtitle("Auhust") +
  theme(plot.title = element_text(hjust = 0))
plotLeg
PlotLeg_forexport <- dml(ggobj = plotLeg)
read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(value=PlotLeg_forexport, location = ph_location(width=11.5, height=7.5, type = "body") ) %>% 
  print(target = "PlotLeg.pptx") %>% 
  invisible()

#Finally, organize niche metrics in a table and export it
transaug<-cbind(c("August","August","August","August","August"),
                c("Athanas nitescens","Gammarus aequicauda","Gammarella fucicola","Melita hergensis","Palaemon xiphias"),
                round(t(group.ML.Aug),2))
transnov<-cbind(c("November","November","November","November","November"),
                c("Athanas nitescens","Gammarus aequicauda","Gammarella fucicola","Melita hergensis","Palaemon xiphias"),
                round(t(group.ML.Nov),2))
transmar<-cbind(c("March","March","March","March","March"),
                c("Athanas nitescens","Gammarus aequicauda","Gammarella fucicola","Melita hergensis","Palaemon xiphias"),
                round(t(group.ML.Mar),2))
transmay<-cbind(c("May","May","May","May","May"),
                c("Athanas nitescens","Gammarus aequicauda","Gammarella fucicola","Melita hergensis","Palaemon xiphias"),
                round(t(group.ML.May),2))
all_sea <- rbind(transaug,transnov,transmar,transmay)
write.csv(all_sea,"nichemetrics5sp.csv", row.names = FALSE)

#End of script