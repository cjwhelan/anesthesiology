###script to produce figures for PONE-D-23_19934 anesthesiology paper with 
###Aaron et al.
###taking script from rmflux_consolidated_logTotalFlux and exvivo_consolidated
###data files here:
### D:\\cancer flashdrive\\anesthesiology and cancer\\experimental data_arig\\longitudinal data analysis\\consolidated\\Consolidated3-10-14.csv
### and here:
### D:\\cancer flashdrive\\anesthesiology and cancer\\experimental data_arig\\longitudinal data analysis\\consolidated\\mets1-4.csv



rm(list=ls())
library(dplyr)
library(ggplot2)
library(gcookbook)
library(afex)
library(emmeans)
library(psych)
library(ggpubr)
library(rstatix)  #this provides "get_anova_table() command
library(colortools)

#make an object:
rmFlux<-read.csv( "D:\\cancer flashdrive\\anesthesiology and cancer\\experimental data_arig\\longitudinal data analysis\\consolidated\\Consolidated3-10-14.csv")

#check:
glimpse(rmFlux)

# Convert the variable experiment from a numeric to a factor variable
rmFlux$Expt <- as.factor(rmFlux$Expt)
glimpse(rmFlux)

# Convert the variable day from a numeric to a factor variable
rmFlux$Day <- as.factor(rmFlux$Day)
glimpse(rmFlux)

##GGplot command is placed into an object "rmFluxMeans_boxplot" so we can view the graph after the command runs.

rmFluxMeans_boxplot<-ggplot(rmFlux, aes(Expt, log(TotalFlux), fill=Expt)) +
  geom_boxplot() +
  #  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  #  stat_summary(geom = "errorbar", fun.data = mean_se, 
  #               linewidth = 0.72, position = "dodge", width = .2) + 
  theme_bw() +
  xlab("Experiment") + ylab("log(Total Flux)") +
  scale_x_discrete(labels=c("1" = "One", "2" = "Two",
                            "4" = "Three")) +
  theme(axis.line=element_line(size=1.25),
        axis.text.x = element_text(face="bold", color="Black", 
                                   size=30),
        axis.text.y = element_text(face="bold", color="Black", 
                                   size=30),
        axis.title.x = element_text(face = "bold", color = "black", size = 30),
        axis.title.y = element_text(face = "bold", color = "black", size = 30),
        legend.position="none"
  )

rmFluxMeans_boxplot ### call for object to see plot.Table #call the object to see the means table

###now for flux x mouse strain

rmFluxMeans_boxplot_MouseStrain<-ggplot(rmFlux, 
                                         aes(MouseStrain, log(TotalFlux), fill=MouseStrain)) +
  geom_boxplot() +
  #  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  #  stat_summary(geom = "errorbar", fun.data = mean_se, 
  #               linewidth = 0.72, position = "dodge", width = .2) + 
  theme_bw() +
  xlab("Mouse Strain") + ylab("log(Total Flux)")  +
  theme(
    axis.line=element_line(size=1.25),
    axis.text.x = element_text(face="bold", color="Black", 
                               size=30),
    axis.text.y = element_text(face="bold", color="Black", 
                               size=30),
    axis.title.x = element_text(face = "bold", color = "black", size = 30),
    axis.title.y = element_text(face = "bold", color = "black", size = 30),
    legend.position="none"
  )

rmFluxMeans_boxplot_MouseStrain ### call for object to see plot.Table #call the object to see the means table


###Day on x-axis; flux contingent upon anesthetic and mouse strain

rmFluxMeans_boxplot_Anesthetic<-ggplot(rmFlux, 
                                        aes(Day, log(TotalFlux), fill=Anesthetic)) +
  geom_boxplot() +
  #  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  #  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9), width = .2) + 
  theme_bw() +
  facet_grid(.~MouseStrain) + xlab("Day") + ylab("log(Total Flux)") +
  theme(
    strip.text.x = element_text(
      size = 30, color = "black", face = "bold"),
    strip.background = element_rect(fill = NA, linetype = "solid", #can also set fill to "transparent"
                                    color = "black", linewidth = 1),
    axis.text.x = element_text(face="bold", color="Black", 
                               size=30),
    axis.text.y = element_text(face="bold", color="Black", 
                               size=30),
    axis.title.x = element_text(face = "bold", color = "black", size = 30),
    axis.title.y = element_text(face = "bold", color = "black", size = 30), 
    legend.title = element_text(face = "bold", color = "black", size = 30),
    legend.text = element_text(face = "bold", color = "black", 
                               size = 30)
  ) +
  guides(fill = guide_legend(# title.hjust = 1, # adjust title if needed
    label.position = "left",
    label.hjust = 1))

rmFluxMeans_boxplot_Anesthetic ### call for object to see plot.Table #call for object to see the means table

########################  ex vivo experiment results  #####################

rm(list=ls())

#make an object:
exvivo_all<-read.csv( "D:\\cancer flashdrive\\anesthesiology and cancer\\experimental data_arig\\longitudinal data analysis\\consolidated\\mets1-4.csv")

#check:
glimpse(exvivo_all)

# Convert the variable experiment from a numeric to a factor variable
exvivo_all$Expt <- as.factor(exvivo_all$Expt)
head(exvivo_all)


#Plot boxplot
exvivo_allMeans_boxplot_OrganMouseStrain<-ggplot(exvivo_all, aes(MouseStrain, 
                                                                  log(TotalFlux), fill=Organ)) +
  geom_boxplot() +
  #  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  #  stat_summary(geom = "errorbar", fun.data = mean_se, 
  #               linewidth = 0.72, position = position_dodge(.9), width = .2) +
  theme_bw() +
  ylab("log(Total Flux)") +
  theme(axis.line=element_line(size=1.25),
        axis.text.x = element_text(face="bold", color="Black", 
                                   size=30),
        axis.text.y = element_text(face="bold", color="Black", 
                                   size=30),
        axis.title.x = element_text(face = "bold", color = "black", size = 30),
        axis.title.y = element_text(face = "bold", color = "black", size = 30),
        legend.title = element_text(color = "black", size = 30),
        legend.text = element_text(size = 30)
  ) +
  guides(fill = guide_legend(# title.hjust = 1, # adjust title if needed
    label.hjust = 1))

exvivo_allMeans_boxplot_OrganMouseStrain ### call for object to see plot.

