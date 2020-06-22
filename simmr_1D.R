library(simmr)
library(ggplot2)
library(gdata)
library(officer)
library(rvg)
library(magrittr)
library(devEMF)
library(cowplot)
library(hdrcde)


#1D Model (d13C only) for primary consumers & omnivores
#Read in the data
consumers_1D <- read.csv("input_consumers.csv",header=TRUE)
consumers_1D_01Aug <- consumers_1D[consumers_1D$Season=="01_august",]
consumers_1D_02Nov <- consumers_1D[consumers_1D$Season=="02_November",]
consumers_1D_03Mar <- consumers_1D[consumers_1D$Season=="03_march",]
consumers_1D_04May <- consumers_1D[consumers_1D$Season=="04_May",]
sources_1D <- read.csv("sources_primary.csv", header=TRUE)
sources_1D_01Aug <- sources_1D[sources_1D$Season=="01_august",]
sources_1D_02Nov <- sources_1D[sources_1D$Season=="02_november",]
sources_1D_03Mar <- sources_1D[sources_1D$Season=="03_march",]
sources_1D_04May <- sources_1D[sources_1D$Season=="04_may",]
TEFs_1D <- read.csv("TEF_primary.csv",header=TRUE)

#Model for August
simmr1D_input_01Aug = simmr_load(mixtures=as.matrix(consumers_1D_01Aug[,4]),
                        group=as.character(consumers_1D_01Aug$Species),
                        source_names=as.character(sources_1D_01Aug$Source),
                        source_means=as.matrix(sources_1D_01Aug$meand13C),
                        source_sds=as.matrix(sources_1D_01Aug$SDd13C),
                        correction_means=as.matrix(TEFs_1D$meand13C),
                        correction_sds=as.matrix(TEFs_1D$SDd13C),
                        concentration_means = NULL)
plot(simmr1D_input_01Aug, group=1:5, xlab=expression(paste(delta^13, "C (\u2030)",sep=""))) 
model1D_01Aug = simmr_mcmc(simmr1D_input_01Aug,  mcmc_control = list(iter = 100000, burn = 10000, thin = 100, n.chain = 4))
save(model1D_01Aug, file="model1D_01Aug.rdata")

#Model for November
simmr1D_input_02Nov = simmr_load(mixtures=as.matrix(consumers_1D_02Nov[,4]),
                                 group=as.character(consumers_1D_02Nov$Species),
                                 source_names=as.character(sources_1D_02Nov$Source),
                                 source_means=as.matrix(sources_1D_02Nov$meand13C),
                                 source_sds=as.matrix(sources_1D_02Nov$SDd13C),
                                 correction_means=as.matrix(TEFs_1D$meand13C),
                                 correction_sds=as.matrix(TEFs_1D$SDd13C),
                                 concentration_means = NULL)
plot(simmr1D_input_02Nov, group=1:5, xlab=expression(paste(delta^13, "C (\u2030)",sep=""))) 
model1D_02Nov = simmr_mcmc(simmr1D_input_02Nov,  mcmc_control = list(iter = 100000, burn = 10000, thin = 100, n.chain = 4))
save(model1D_02Nov, file="model1D_02Nov.rdata")

#Model for March
simmr1D_input_03Mar = simmr_load(mixtures=as.matrix(consumers_1D_03Mar[,4]),
                                 group=as.character(consumers_1D_03Mar$Species),
                                 source_names=as.character(sources_1D_03Mar$Source),
                                 source_means=as.matrix(sources_1D_03Mar$meand13C),
                                 source_sds=as.matrix(sources_1D_03Mar$SDd13C),
                                 correction_means=as.matrix(TEFs_1D$meand13C),
                                 correction_sds=as.matrix(TEFs_1D$SDd13C),
                                 concentration_means = NULL)
plot(simmr1D_input_03Mar, group=1:5, xlab=expression(paste(delta^13, "C (\u2030)",sep=""))) 
model1D_03Mar = simmr_mcmc(simmr1D_input_03Mar,  mcmc_control = list(iter = 100000, burn = 10000, thin = 100, n.chain = 4))
save(model1D_03Mar, file="model1D_03Mar.rdata")

#Model for May
simmr1D_input_04May = simmr_load(mixtures=as.matrix(consumers_1D_04May[,4]),
                                 group=as.character(consumers_1D_04May$Species),
                                 source_names=as.character(sources_1D_04May$Source),
                                 source_means=as.matrix(sources_1D_04May$meand13C),
                                 source_sds=as.matrix(sources_1D_04May$SDd13C),
                                 correction_means=as.matrix(TEFs_1D$meand13C),
                                 correction_sds=as.matrix(TEFs_1D$SDd13C),
                                 concentration_means = NULL)
plot(simmr1D_input_04May, group=1:5, xlab=expression(paste(delta^13, "C (\u2030)",sep=""))) 
model1D_04May = simmr_mcmc(simmr1D_input_04May,  mcmc_control = list(iter = 100000, burn = 10000, thin = 100, n.chain = 4))
save(model1D_04May, file="model1D_04May.rdata")

#Mega plot with the diet of all species - detailed density plot
#Extract & plot data for Melita hergensis
Melitaoutput <- c(model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'DL'],
                  model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'Epi'],
                  model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'RAM'],
                  model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'SOM'],
                  model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'DL'],
                  model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'Epi'],
                  model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'RAM'],
                  model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'SOM'],
                  model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'DL'],
                  model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'Epi'],
                  model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'RAM'],
                  model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'SOM'],
                  model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'DL'],
                  model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'Epi'],
                  model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'RAM'],
                  model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'SOM'])
DetailedSeasonsNames <- c(rep("2011/08", length(model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'DL'])),
                          rep("2011/08", length(model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'Epi'])),
                          rep("2011/08", length(model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'RAM'])),
                          rep("2011/08", length(model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'SOM'])),
                          rep("2011/11", length(model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'DL'])),
                          rep("2011/11", length(model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'Epi'])),
                          rep("2011/11", length(model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'RAM'])),
                          rep("2011/11", length(model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'SOM'])),
                          rep("2012/03", length(model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'DL'])),
                          rep("2012/03", length(model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'Epi'])),
                          rep("2012/03", length(model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'RAM'])),
                          rep("2012/03", length(model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'SOM'])),
                          rep("2012/05", length(model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'DL'])),
                          rep("2012/05", length(model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'Epi'])),
                          rep("2012/05", length(model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'RAM'])),
                          rep("2012/05", length(model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'SOM'])))
PrimarySourcesNames <- c(rep("DL", length(model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'DL'])),
                         rep("Epi", length(model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'Epi'])),
                         rep("RAM", length(model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'RAM'])),
                         rep("SOM", length(model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'SOM'])),
                         rep("DL", length(model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'DL'])),
                         rep("Epi", length(model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'Epi'])),
                         rep("RAM", length(model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'RAM'])),
                         rep("SOM", length(model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'SOM'])),
                         rep("DL", length(model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'DL'])),
                         rep("Epi", length(model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'Epi'])),
                         rep("RAM", length(model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'RAM'])),
                         rep("SOM", length(model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'SOM'])),
                         rep("DL", length(model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'DL'])),
                         rep("Epi", length(model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'Epi'])),
                         rep("RAM", length(model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'RAM'])),
                         rep("SOM", length(model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'SOM'])))
Melita <- data.frame(Melitaoutput, DetailedSeasonsNames, PrimarySourcesNames)
MelitaPlot <- ggplot(Melita, aes(x = Melitaoutput, colour = PrimarySourcesNames, fill = PrimarySourcesNames)) + 
  geom_density(alpha=0.6) + 
  theme_bw() +
  scale_x_continuous(name=NULL, limits= c(0,1)) +
  scale_y_continuous(name=NULL, breaks=NULL, expand = c(0.01, 0)) +
  scale_color_manual(values=c("#A65628","#4DAF4A","#E41A1C", "#377EB8")) +
  scale_fill_manual(values=c("#A65628","#4DAF4A", "#E41A1C","#377EB8")) +
  ggtitle(expression(italic("Melita hergensis"))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.background = element_rect(fill=NULL, size=NULL, linetype=NULL)) +
  guides(fill=guide_legend(title="Source"), colour=guide_legend(title="Source")) +
  facet_grid(. ~ DetailedSeasonsNames) +
  theme(strip.background =element_rect(fill="white")) +
  coord_cartesian(clip = "off")
MelitaPlot

#Extract & plot data for Gammarella fucicola
Gammarellaoutput <- c(model1D_01Aug$output$`GF`$BUGSoutput$sims.list$p[,'DL'],
                  model1D_01Aug$output$`GF`$BUGSoutput$sims.list$p[,'Epi'],
                  model1D_01Aug$output$`GF`$BUGSoutput$sims.list$p[,'RAM'],
                  model1D_01Aug$output$`GF`$BUGSoutput$sims.list$p[,'SOM'],
                  model1D_02Nov$output$`GF`$BUGSoutput$sims.list$p[,'DL'],
                  model1D_02Nov$output$`GF`$BUGSoutput$sims.list$p[,'Epi'],
                  model1D_02Nov$output$`GF`$BUGSoutput$sims.list$p[,'RAM'],
                  model1D_02Nov$output$`GF`$BUGSoutput$sims.list$p[,'SOM'],
                  model1D_03Mar$output$`GF`$BUGSoutput$sims.list$p[,'DL'],
                  model1D_03Mar$output$`GF`$BUGSoutput$sims.list$p[,'Epi'],
                  model1D_03Mar$output$`GF`$BUGSoutput$sims.list$p[,'RAM'],
                  model1D_03Mar$output$`GF`$BUGSoutput$sims.list$p[,'SOM'],
                  model1D_04May$output$`GF`$BUGSoutput$sims.list$p[,'DL'],
                  model1D_04May$output$`GF`$BUGSoutput$sims.list$p[,'Epi'],
                  model1D_04May$output$`GF`$BUGSoutput$sims.list$p[,'RAM'],
                  model1D_04May$output$`GF`$BUGSoutput$sims.list$p[,'SOM'])
Gammarella <- data.frame(Gammarellaoutput, DetailedSeasonsNames, PrimarySourcesNames)
GammarellaPlot <-ggplot(Gammarella, aes(x = Gammarellaoutput, colour = PrimarySourcesNames, fill = PrimarySourcesNames)) + 
  geom_density(alpha=0.6) + 
  theme_bw() +
  scale_x_continuous(name=NULL, limits= c(0,1)) +
  scale_y_continuous(name=NULL, breaks=NULL, expand = c(0.01, 0)) +
  scale_color_manual(values=c("#A65628","#4DAF4A","#E41A1C", "#377EB8")) +
  scale_fill_manual(values=c("#A65628","#4DAF4A", "#E41A1C","#377EB8")) +
  ggtitle(expression(italic("Gammarella fucicola"))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.background = element_rect(fill=NULL, size=NULL, linetype=NULL)) +
  guides(fill=guide_legend(title="Source"), colour=guide_legend(title="Source")) +
  facet_grid(. ~ DetailedSeasonsNames) +
  theme(strip.background =element_rect(fill="white")) +
  coord_cartesian(clip = "off")
GammarellaPlot

#Extract & plot data for Gammarus aequicauda
Gammarusoutput <- c(model1D_01Aug$output$`GA`$BUGSoutput$sims.list$p[,'DL'],
                      model1D_01Aug$output$`GA`$BUGSoutput$sims.list$p[,'Epi'],
                      model1D_01Aug$output$`GA`$BUGSoutput$sims.list$p[,'RAM'],
                      model1D_01Aug$output$`GA`$BUGSoutput$sims.list$p[,'SOM'],
                      model1D_02Nov$output$`GA`$BUGSoutput$sims.list$p[,'DL'],
                      model1D_02Nov$output$`GA`$BUGSoutput$sims.list$p[,'Epi'],
                      model1D_02Nov$output$`GA`$BUGSoutput$sims.list$p[,'RAM'],
                      model1D_02Nov$output$`GA`$BUGSoutput$sims.list$p[,'SOM'],
                      model1D_03Mar$output$`GA`$BUGSoutput$sims.list$p[,'DL'],
                      model1D_03Mar$output$`GA`$BUGSoutput$sims.list$p[,'Epi'],
                      model1D_03Mar$output$`GA`$BUGSoutput$sims.list$p[,'RAM'],
                      model1D_03Mar$output$`GA`$BUGSoutput$sims.list$p[,'SOM'],
                      model1D_04May$output$`GA`$BUGSoutput$sims.list$p[,'DL'],
                      model1D_04May$output$`GA`$BUGSoutput$sims.list$p[,'Epi'],
                      model1D_04May$output$`GA`$BUGSoutput$sims.list$p[,'RAM'],
                      model1D_04May$output$`GA`$BUGSoutput$sims.list$p[,'SOM'])
Gammarus <- data.frame(Gammarusoutput, DetailedSeasonsNames, PrimarySourcesNames)
GammarusPlot <-ggplot(Gammarus, aes(x = Gammarusoutput, colour = PrimarySourcesNames, fill = PrimarySourcesNames)) + 
  geom_density(alpha=0.6) + 
  theme_bw() +
  scale_x_continuous(name=NULL, limits= c(0,1)) +
  scale_y_continuous(name=NULL, breaks=NULL, expand = c(0.01, 0)) +
  scale_color_manual(values=c("#A65628","#4DAF4A","#E41A1C", "#377EB8")) +
  scale_fill_manual(values=c("#A65628","#4DAF4A", "#E41A1C","#377EB8")) +
  ggtitle(expression(italic("Gammarus aequicauda"))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.background = element_rect(fill=NULL, size=NULL, linetype=NULL)) +
  guides(fill=guide_legend(title="Source"), colour=guide_legend(title="Source")) +
  facet_grid(. ~ DetailedSeasonsNames) +
  theme(strip.background =element_rect(fill="white")) +
  coord_cartesian(clip = "off")
GammarusPlot

#Extract data for Athanas nitescens
Athanasoutput <- c(model1D_01Aug$output$`AN`$BUGSoutput$sims.list$p[,'DL'],
                    model1D_01Aug$output$`AN`$BUGSoutput$sims.list$p[,'Epi'],
                    model1D_01Aug$output$`AN`$BUGSoutput$sims.list$p[,'RAM'],
                    model1D_01Aug$output$`AN`$BUGSoutput$sims.list$p[,'SOM'],
                    model1D_02Nov$output$`AN`$BUGSoutput$sims.list$p[,'DL'],
                    model1D_02Nov$output$`AN`$BUGSoutput$sims.list$p[,'Epi'],
                    model1D_02Nov$output$`AN`$BUGSoutput$sims.list$p[,'RAM'],
                    model1D_02Nov$output$`AN`$BUGSoutput$sims.list$p[,'SOM'],
                    model1D_03Mar$output$`AN`$BUGSoutput$sims.list$p[,'DL'],
                    model1D_03Mar$output$`AN`$BUGSoutput$sims.list$p[,'Epi'],
                    model1D_03Mar$output$`AN`$BUGSoutput$sims.list$p[,'RAM'],
                    model1D_03Mar$output$`AN`$BUGSoutput$sims.list$p[,'SOM'],
                    model1D_04May$output$`AN`$BUGSoutput$sims.list$p[,'DL'],
                    model1D_04May$output$`AN`$BUGSoutput$sims.list$p[,'Epi'],
                    model1D_04May$output$`AN`$BUGSoutput$sims.list$p[,'RAM'],
                    model1D_04May$output$`AN`$BUGSoutput$sims.list$p[,'SOM'])
Athanas <- data.frame(Athanasoutput, DetailedSeasonsNames, PrimarySourcesNames)
AthanasPlot <-ggplot(Athanas, aes(x = Athanasoutput, colour = PrimarySourcesNames, fill = PrimarySourcesNames)) + 
  geom_density(alpha=0.6) + 
  theme_bw() +
  scale_x_continuous(name=NULL, limits= c(0,1)) +
  scale_y_continuous(name=NULL, breaks=NULL, expand = c(0.01, 0)) +
  scale_color_manual(values=c("#A65628","#4DAF4A","#E41A1C", "#377EB8")) +
  scale_fill_manual(values=c("#A65628","#4DAF4A", "#E41A1C","#377EB8")) +
  ggtitle(expression(italic("Athanas nitescens"))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.background = element_rect(fill=NULL, size=NULL, linetype=NULL)) +
  guides(fill=guide_legend(title="Source"), colour=guide_legend(title="Source")) +
  facet_grid(. ~ DetailedSeasonsNames) +
  theme(strip.background =element_rect(fill="white")) +
  coord_cartesian(clip = "off")
AthanasPlot

#Extract data for Palaemon xiphias
Palaemonoutput <- c(model1D_01Aug$output$`PX`$BUGSoutput$sims.list$p[,'DL'],
                   model1D_01Aug$output$`PX`$BUGSoutput$sims.list$p[,'Epi'],
                   model1D_01Aug$output$`PX`$BUGSoutput$sims.list$p[,'RAM'],
                   model1D_01Aug$output$`PX`$BUGSoutput$sims.list$p[,'SOM'],
                   model1D_02Nov$output$`PX`$BUGSoutput$sims.list$p[,'DL'],
                   model1D_02Nov$output$`PX`$BUGSoutput$sims.list$p[,'Epi'],
                   model1D_02Nov$output$`PX`$BUGSoutput$sims.list$p[,'RAM'],
                   model1D_02Nov$output$`PX`$BUGSoutput$sims.list$p[,'SOM'],
                   model1D_03Mar$output$`PX`$BUGSoutput$sims.list$p[,'DL'],
                   model1D_03Mar$output$`PX`$BUGSoutput$sims.list$p[,'Epi'],
                   model1D_03Mar$output$`PX`$BUGSoutput$sims.list$p[,'RAM'],
                   model1D_03Mar$output$`PX`$BUGSoutput$sims.list$p[,'SOM'],
                   model1D_04May$output$`PX`$BUGSoutput$sims.list$p[,'DL'],
                   model1D_04May$output$`PX`$BUGSoutput$sims.list$p[,'Epi'],
                   model1D_04May$output$`PX`$BUGSoutput$sims.list$p[,'RAM'],
                   model1D_04May$output$`PX`$BUGSoutput$sims.list$p[,'SOM'])
Palaemon <- data.frame(Palaemonoutput, DetailedSeasonsNames, PrimarySourcesNames)
PalaemonPlot <-ggplot(Palaemon, aes(x = Palaemonoutput, colour = PrimarySourcesNames, fill = PrimarySourcesNames)) + 
  geom_density(alpha=0.6) + 
  theme_bw() +
  scale_x_continuous(name=NULL, limits= c(0,1)) +
  scale_y_continuous(name=NULL, breaks=NULL, expand = c(0.01, 0)) +
  scale_color_manual(values=c("#A65628","#4DAF4A","#E41A1C", "#377EB8")) +
  scale_fill_manual(values=c("#A65628","#4DAF4A", "#E41A1C","#377EB8")) +
  ggtitle(expression(italic("Palaemon xiphias"))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.background = element_rect(fill=NULL, size=NULL, linetype=NULL)) +
  guides(fill=guide_legend(title="Source"), colour=guide_legend(title="Source")) +
  facet_grid(. ~ DetailedSeasonsNames) +
  theme(strip.background =element_rect(fill="white")) +
  coord_cartesian(clip = "off")
PalaemonPlot


#Combine in a big graph and export to ppt
Megaplot <- plot_grid(MelitaPlot + theme(legend.position="none"),
                      GammarellaPlot + theme(legend.position="none"),
                      GammarusPlot + theme(legend.position="none"), 
                      AthanasPlot + theme(legend.position="none"), 
                      PalaemonPlot + theme(legend.position="none"), 
                      ncol=1)
Megaplot

legend_b <- get_legend(
  PalaemonPlot + 
    theme(legend.position = "bottom"))
Megaplot_legend <- plot_grid(Megaplot, legend_b, ncol = 1, rel_heights = c(1, .1))

read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with_vg(code = print(Megaplot_legend), width=10, height=12,type = "body") %>% 
  print(target = "Megaplot.pptx") %>% 
  invisible()


#Extract confidence interval limits
summary(model1D_01Aug,type="quantiles", group=c(1:5))
summary(model1D_02Nov,type="quantiles", group=c(1:5))
summary(model1D_03Mar,type="quantiles", group=c(1:5))
summary(model1D_04May,type="quantiles", group=c(1:5))

mode <- function(x) {density(x)$x[which.max(density(x)$y)]}

modelmodes <- rbind(mode(model1D_01Aug$output$`AN`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_01Aug$output$`AN`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_01Aug$output$`AN`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_01Aug$output$`AN`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_01Aug$output$`GA`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_01Aug$output$`GA`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_01Aug$output$`GA`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_01Aug$output$`GA`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_01Aug$output$`GF`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_01Aug$output$`GF`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_01Aug$output$`GF`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_01Aug$output$`GF`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_01Aug$output$`MH`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_01Aug$output$`PX`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_01Aug$output$`PX`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_01Aug$output$`PX`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_01Aug$output$`PX`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_02Nov$output$`AN`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_02Nov$output$`AN`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_02Nov$output$`AN`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_02Nov$output$`AN`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_02Nov$output$`GA`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_02Nov$output$`GA`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_02Nov$output$`GA`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_02Nov$output$`GA`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_02Nov$output$`GF`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_02Nov$output$`GF`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_02Nov$output$`GF`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_02Nov$output$`GF`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_02Nov$output$`MH`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_02Nov$output$`PX`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_02Nov$output$`PX`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_02Nov$output$`PX`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_02Nov$output$`PX`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_03Mar$output$`AN`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_03Mar$output$`AN`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_03Mar$output$`AN`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_03Mar$output$`AN`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_03Mar$output$`GA`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_03Mar$output$`GA`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_03Mar$output$`GA`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_03Mar$output$`GA`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_03Mar$output$`GF`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_03Mar$output$`GF`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_03Mar$output$`GF`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_03Mar$output$`GF`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_03Mar$output$`MH`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_03Mar$output$`PX`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_03Mar$output$`PX`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_03Mar$output$`PX`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_03Mar$output$`PX`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_04May$output$`AN`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_04May$output$`AN`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_04May$output$`AN`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_04May$output$`AN`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_04May$output$`GA`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_04May$output$`GA`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_04May$output$`GA`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_04May$output$`GA`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_04May$output$`GF`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_04May$output$`GF`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_04May$output$`GF`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_04May$output$`GF`$BUGSoutput$sims.list$p[,'RAM']),
                    mode(model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_04May$output$`MH`$BUGSoutput$sims.list$p[,'RAM']),                    
                    mode(model1D_04May$output$`PX`$BUGSoutput$sims.list$p[,'DL']),
                    mode(model1D_04May$output$`PX`$BUGSoutput$sims.list$p[,'Epi']),
                    mode(model1D_04May$output$`PX`$BUGSoutput$sims.list$p[,'SOM']),
                    mode(model1D_04May$output$`PX`$BUGSoutput$sims.list$p[,'RAM'])
                    )
print(round(modelmodes, 4))
