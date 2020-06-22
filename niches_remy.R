library(SIBER)
library(ggplot2)
library(RColorBrewer)
library(officer)
library(rvg)
library(magrittr)
library(devEMF)
library(cowplot)

group.ellipses.args  <- list(n = 100, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")

#COMMUNITY NICHES#

CommsImp <- read.csv("community.csv", header=T, sep=",")

SplitComm <- split(CommsImp, CommsImp$community)

august <- SplitComm$`1`
november <-SplitComm$`2`
march <- SplitComm$`3`
may <- SplitComm$`4`

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

CommRaw <- rbind (CommaugustRaw, CommnovemberRaw, CommmarchRaw, CommmayRaw)
colnames(CommRaw)[1] <- "iso1"
colnames(CommRaw)[2] <- "iso2"
colnames(CommRaw)[3] <- "group"
colnames(CommRaw)[4] <- "community"
CommClean <- as.data.frame(CommRaw)
CommSIBER <- createSiberObject(CommClean)

plotSiberObject(CommSIBER,
                ax.pad = 0.1, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~('\u2030')),
                ylab = expression({delta}^15*N~('\u2030')))

group.ML.Comms <- groupMetricsML(CommSIBER)


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

Comms1_ <- rep(1, length(SEComms1[,1]))
Comms2_ <- rep(2, length(SEComms2[,1]))
Comms3_ <- rep(3, length(SEComms3[,1]))
Comms4_ <- rep(4, length(SEComms4[,1]))

Comms_ <- c(Comms1_, Comms2_, Comms3_, Comms4_)
xComms <- c(SEComms1[,1],SEComms2[,1],SEComms3[,1],SEComms4[,1])
yComms <- c(SEComms1[,2],SEComms2[,2],SEComms3[,2],SEComms4[,2])

df_SEComms <- data.frame(xComms,yComms,Comms_)
plot(df_SEComms$xComms, df_SEComms$yComms)

Commscol <- brewer.pal(4, "Set2")

plotComms <- ggplot(CommClean, aes(x=iso1, y=iso2, group=factor(group))) +
  theme_bw() +
  geom_point(aes(shape=factor(group), color=factor(group)), size=2) +
  scale_shape_manual(values=c(16, 15, 17, 1))+
  scale_color_manual(labels = c("August","November", "March","May"), 
                     values= Commscol) +
  scale_x_continuous(name="d13C", limits=c(-22.1, -13.5), breaks=c(-12,-14,-16,-18,-20,-22), minor_breaks = c(-13,-15,-17,-19,-21)) +
  scale_y_continuous(name="d15N", limits=c(0.9,8.2), breaks=c(2,4,6,8)) +
  geom_path(data=df_SEComms, aes(x=xComms, y=yComms, group=factor(df_SEComms$Comms_), color=factor(df_SEComms$Comms_), linetype=factor(df_SEComms$Comms_), size=factor(df_SEComms$Comms_))) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid")) +
  scale_size_manual(values=c(1.1, 1.1, 1.1, 1.1)) +
  #theme(panel.grid.minor = element_blank()) +
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

read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with_vg(code = print(plotComms), width=9, height=7,type = "body") %>% 
  print(target = "PlotComms.pptx") %>% 
  invisible()


#SPECIES NICHES FOR EACH SEASON#

FiveSpImp <- read.csv("5species.csv", header=T, sep=",")
FiveSpClean <- FiveSpImp[1:4]

SplitFiveSp <- split(FiveSpClean, FiveSpClean$community)

FiveSpAug <- SplitFiveSp$`1`
FiveSpNov <- SplitFiveSp$`2`
FiveSpMar <- SplitFiveSp$`3`
FiveSpMay <- SplitFiveSp$`4`

Col5Sp <- brewer.pal(5, "Set1")


#Plot for August
SIBER5spAug <- createSiberObject(FiveSpAug)

plotSiberObject(SIBER5spAug,
                ax.pad = 0.1, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~('\u2030')),
                ylab = expression({delta}^15*N~('\u2030')))

group.ML.Aug <- groupMetricsML(SIBER5spAug)


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

Aug1_ <- rep(1, length(SEAug1[,1]))
Aug2_ <- rep(2, length(SEAug2[,1]))
Aug3_ <- rep(3, length(SEAug3[,1]))
Aug4_ <- rep(4, length(SEAug4[,1]))
Aug5_ <- rep(5, length(SEAug5[,1]))

Aug_ <- c(Aug1_, Aug2_, Aug3_, Aug4_, Aug5_)
xAug <- c(SEAug1[,1],SEAug2[,1],SEAug3[,1],SEAug4[,1],SEAug5[,1])
yAug <- c(SEAug1[,2],SEAug2[,2],SEAug3[,2],SEAug4[,2],SEAug5[,2])

df_SEAug <- data.frame(xAug,yAug,Aug_)
plot(df_SEAug$xAug, df_SEAug$yAug)

plotAug <- ggplot(FiveSpAug, aes(x=iso1, y=iso2, group=factor(group))) +
  theme_bw() +
  geom_point(aes(shape=factor(group), color=factor(group)), size=2) +
  scale_shape_manual(values=c(16, 15, 17, 1, 0))+
  scale_color_manual(labels = c("A. nitescens","G. aequicauda", "G. fucicola","M. hergensis","P. xiphias"), 
                     values= Col5Sp) +
  scale_x_continuous(name="d13C", limits=c(-21.5, -12.8), breaks=c(-12,-14,-16,-18,-20), minor_breaks = c(-13,-15,-17,-19,-21)) +
  scale_y_continuous(name="d15N", limits=c(-1,7.5), breaks=c(0,2,4,6)) +
  geom_path(data=df_SEAug, aes(x=xAug, y=yAug, group=factor(df_SEAug$Aug_), color=factor(df_SEAug$Aug_), linetype=factor(df_SEAug$Aug_), size=factor(df_SEAug$Aug_))) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid")) +
  scale_size_manual(values=c(1.1, 1.1, 1.1, 1.1, 1.1)) +
  #theme(panel.grid.minor = element_blank()) +
  guides(size=FALSE, linetype=FALSE, shape=FALSE, color=FALSE)+
  ggtitle("Auhust") +
  theme(plot.title = element_text(hjust = 0))

plotAug


#Plot for November
SIBER5spNov <- createSiberObject(FiveSpNov)

plotSiberObject(SIBER5spNov,
                ax.pad = 0.1, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~('\u2030')),
                ylab = expression({delta}^15*N~('\u2030')))

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
plot(df_SENov$xNov, df_SENov$yNov)

plotNov <- ggplot(FiveSpNov, aes(x=iso1, y=iso2, group=factor(group))) +
  theme_bw() +
  geom_point(aes(shape=factor(group), color=factor(group)), size=2) +
  scale_shape_manual(values=c(16, 15, 17, 1, 0))+
  scale_color_manual(labels = c("A. nitescens","G. aequicauda", "G. fucicola","M. hergensis","P. xiphias"), 
                     values= Col5Sp) +
  scale_x_continuous(name="d13C", limits=c(-21.5, -12.8), breaks=c(-12,-14,-16,-18,-20), minor_breaks = c(-13,-15,-17,-19,-21)) +
  scale_y_continuous(name="d15N", limits=c(-1,7.5), breaks=c(0,2,4,6)) +
  geom_path(data=df_SENov, aes(x=xNov, y=yNov, group=factor(df_SENov$Nov_), color=factor(df_SENov$Nov_), linetype=factor(df_SENov$Nov_), size=factor(df_SENov$Nov_))) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid")) +
  scale_size_manual(values=c(1.1, 1.1, 1.1, 1.1, 1.1)) +
  #theme(panel.grid.minor = element_blank()) +
  guides(size=FALSE, linetype=FALSE, shape=FALSE, color=FALSE)+
  ggtitle("November") +
  theme(plot.title = element_text(hjust = 0))

plotNov


#Plot for March
SIBER5spMar <- createSiberObject(FiveSpMar)

plotSiberObject(SIBER5spMar,
                ax.pad = 0.1, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~('\u2030')),
                ylab = expression({delta}^15*N~('\u2030')))

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
plot(df_SEMar$xMar, df_SEMar$yMar)

plotMar <- ggplot(FiveSpMar, aes(x=iso1, y=iso2, group=factor(group))) +
  theme_bw() +
  geom_point(aes(shape=factor(group), color=factor(group)), size=2) +
  scale_shape_manual(values=c(16, 15, 17, 1, 0))+
  scale_color_manual(labels = c("A. nitescens","G. aequicauda", "G. fucicola","M. hergensis","P. xiphias"), 
                     values= Col5Sp) +
  scale_x_continuous(name="d13C", limits=c(-21.5, -12.8), breaks=c(-12,-14,-16,-18,-20), minor_breaks = c(-13,-15,-17,-19,-21)) +
  scale_y_continuous(name="d15N", limits=c(-1,7.5), breaks=c(0,2,4,6)) +
  geom_path(data=df_SEMar, aes(x=xMar, y=yMar, group=factor(df_SEMar$Mar_), color=factor(df_SEMar$Mar_), linetype=factor(df_SEMar$Mar_), size=factor(df_SEMar$Mar_))) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid")) +
  scale_size_manual(values=c(1.1,1.1,1.1,1.1,1.1)) +
  #theme(panel.grid.minor = element_blank()) +
  guides(size=FALSE, linetype=FALSE, shape=FALSE, color=FALSE)+
  ggtitle("March") +
  theme(plot.title = element_text(hjust = 0))

plotMar


#Plot for May
SIBER5spMay <- createSiberObject(FiveSpMay)

plotSiberObject(SIBER5spMay,
                ax.pad = 0.1, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~('\u2030')),
                ylab = expression({delta}^15*N~('\u2030')))

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
  geom_path(data=df_SEMay, aes(x=xMay, y=yMay, group=factor(df_SEMay$May_), color=factor(df_SEMay$May_), linetype=factor(df_SEMay$May_), size=factor(df_SEMay$May_))) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid")) +
  scale_size_manual(values=c(1.1, 1.1, 1.1, 1.1, 1.1)) +
  #theme(panel.grid.minor = element_blank()) +
  guides(size=FALSE, linetype=FALSE, shape=FALSE, color=FALSE)+
  ggtitle("May") +
  theme(plot.title = element_text(hjust = 0))

plotMay


#plot with all 4 seasons

plot5sp <- plot_grid(plotAug, plotNov, plotMar, plotMay,  ncol=2)

plot5sp

read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with_vg(code = print(plot5sp), width=11.5, height=7.5,type = "body") %>% 
  print(target = "Plot5sp.pptx") %>% 
  invisible()




#Random plot just to have a legend
plotLeg <- ggplot(FiveSpAug, aes(x=iso1, y=iso2, group=factor(group))) +
  theme_bw() +
  geom_point(aes(shape=factor(group), color=factor(group)), size=2) +
  scale_shape_manual(values=c(16, 15, 17, 1, 0))+
  scale_color_manual(labels = c("Athanas nitescens","Gammarus aequicauda", "Gammarella fucicola","Melita hergensis","Palaemon xiphias"), 
                     values= Col5Sp) +
  scale_x_continuous(name="d13C", limits=c(-21.5, -12.8), breaks=c(-12,-14,-16,-18,-20), minor_breaks = c(-13,-15,-17,-19,-21)) +
  scale_y_continuous(name="d15N", limits=c(-1,7.5), breaks=c(0,2,4,6)) +
  geom_path(data=df_SEAug, aes(x=xAug, y=yAug, group=factor(df_SEAug$Aug_), color=factor(df_SEAug$Aug_), linetype=factor(df_SEAug$Aug_), size=factor(df_SEAug$Aug_))) +
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

read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with_vg(code = print(plotLeg), width=11.5, height=7.5,type = "body") %>% 
  print(target = "PlotLeg.pptx") %>% 
  invisible()




#SEA & TA tables for Gilles
transcomm<-t(group.ML.Comms)
transcomm<-cbind(c("August","November","March","May"),
                 c("Whole Community","Whole Community","Whole Community","Whole Community"),
                 transcomm)
transaug<-t(group.ML.Aug)
transaug<-cbind(c("August","August","August","August","August"),
                c("Athanas nitescens","Gammarus aequicauda","Gammarella fucicola","Melita hergensis","Palaemon xiphias"),
                transaug)
transnov<-t(group.ML.Nov)
transnov<-cbind(c("November","November","November","November","November"),
                c("Athanas nitescens","Gammarus aequicauda","Gammarella fucicola","Melita hergensis","Palaemon xiphias"),
                transnov)
transmar<-t(group.ML.Mar)
transmar<-cbind(c("March","March","March","March","March"),
                c("Athanas nitescens","Gammarus aequicauda","Gammarella fucicola","Melita hergensis","Palaemon xiphias"),
                transmar)
transmay<-t(group.ML.May)
transmay<-cbind(c("May","May","May","May","May"),
                c("Athanas nitescens","Gammarus aequicauda","Gammarella fucicola","Melita hergensis","Palaemon xiphias"),
                transmay)
all_sea <- rbind(transcomm,transaug,transnov,transmar,transmay)
write.csv(all_sea,"nichemetrics.csv", row.names = FALSE)
