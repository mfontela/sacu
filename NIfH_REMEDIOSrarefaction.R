#NIfH_rarefaction try
library(readxl)
library(tidyverse)
library(vegan)


nice_colors = c("#999999", "#E69F00", "#56B4E9","#e98756","#c08160","#5800e6", "#CDDC49", "#C475D3", 
                "#E94B30", "#233F57", "#FEE659", "#A1CFDD", "#F4755E", "#D6F6F7","#EB6D58", "#6898BF")
Ds <- read_excel("C:/Users/MFontela/OneDrive - Universidade de Vigo/NifH/data/Remedios_ASVs_nifH_seasonal_cycle.xlsx", 
                 sheet = "RAW_DATA", col_names = FALSE, 
                 col_types = c("text", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "text", "text", 
                               "numeric", "text", "text", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric"),
                 range = "A6:BP5472") #con el rango elimino las sumas de columnas del final y demás
#colnames (en vez de leerlos hice un datapasta como vectors desde Addins)
Ds_colnames<-c("Nomenclature", "broza", "Phylum", "Class", "Order", "Family", "broza1", "Genus_species", "clusterFAIL", "broza2", "broza3", "seq_name", "sequence", "cluster")
#van 14 columnas, faltan las 54 de los sampling","ID/time, otro datapasta
Ds_colnames<-c(Ds_colnames, 
               c("ID24","ID6","ID19","ID33","ID49","ID34","ID7","ID20","ID35","ID50","ID37","ID8","ID21","ID36","ID51","ID41","ID9","ID22","ID38","ID52","ID45","ID10","ID23","ID39","ID53","ID1","ID11","ID25","ID40","ID54","ID4","ID12","ID26","ID42","ID17","ID13","ID27","ID43","ID28","ID14","ID29","ID44","ID2","ID15","ID30","ID46","ID3","ID16","ID31","ID47","ID5","ID18","ID32","ID48")
               )
colnames(Ds)<-Ds_colnames #asocio los nombres de las columnas
#TTD: agregar los tiempos del muestreo

#elimina columnas broza
Ds<-Ds%>%select(!(c("broza", "broza1", "broza2", "broza3")))


# switch obs and rows -----------------------------------------------------
obsDs<-Ds[,11:64]%>%
  t %>%as.data.frame()
colnames(obsDs)<-Ds$seq_name

#slighltly adapted from rarefy {vegan} help example 

S <- specnumber(obsDs) # observed number of species
(raremax <- min(rowSums(obsDs)))
Srare <- rarefy(obsDs, raremax)
selected_step=20
#plot something
par(mfrow=c(1,2))
# plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
# abline(0, 1)
out=rarecurve(obsDs, step = selected_step, col = nice_colors, cex = 0.75, 
              main=paste("Done with step sample:", selected_step, "and with subsample size for rarefying community", raremax))
# selected_step=100
out=rarecurve(obsDs, step = selected_step, col = nice_colors, cex = 0.6, main=paste("Done with the interval of step sample:", selected_step))

Sdf<-data.frame(S, Srare)
ggplot(Sdf, aes(S, Srare))+
  geom_point(size=4, aes(colour=rownames(Sdf)))+
  geom_abline(intercept = 0, slope = 1, color="red")

#candidatas ID54, 39 y 24


par(mfrow=c(1,3))
plot(out[[which(rownames(obsDs)=="ID24")]], main="ID24", xlab = "Sample Size", ylab = "Species")
plot(out[[which(rownames(obsDs)=="ID39")]], main="ID39", xlab = "Sample Size", ylab = "Species")
plot(out[[which(rownames(obsDs)=="ID54")]], main="ID54", xlab = "Sample Size", ylab = "Species")
