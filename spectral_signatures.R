#Spectral separability of species in each community

#PACKAGES----

#data general
library(readxl)
library(tidyr)
library(dplyr)
library(tibble)

#spectral data import and visualization
# install.packages("pavo") #not working
install.packages("photobiologyInOut") #https://cran.r-project.org/web/packages/photobiologyInOut/photobiologyInOut.pdf
library(photobiologyInOut)
install.packages("ggspectra") #to visualize
library(ggspectra)

#IMPORT DATA------
#spectral signatures

#try with one spectra
prova<-read_oo_ssirrad("/Users/laiouisa/OneDrive - ČZU v Praze/SPECTRAL DIVERSITY Project/Data/Spectrometer measurements 2019/Ach_mil_1A.txt")
#irradiance is not reflectance..?
#negative values ?
class(prova)
str(prova)
names(prova)
attr(prova, "file.header")

autoplot(prova) #quick visualization
getWhatMeasured(prova)
getWhenMeasured(prova)
cat(comment(prova))

autoplot(prova[prova$w.length>400&prova$w.length<900]) #best values - check with others if it is ok

#visualize example
ggplot(prova[prova$w.length>300&prova$w.length<650,]) +
  geom_spct() +
  scale_y_s.q.irrad_continuous() +
  scale_x_wl_continuous(sec.axis = sec_axis_w_number()) +
  theme_bw()

#/Loop to import all spectra into a list----

#define the folder where the plot data are:
fol<-"/Users/laiouisa/OneDrive - ČZU v Praze/SPECTRAL DIVERSITY Project/Data/Spectrometer measurements 2019/Data txt files"
list1<-dir(path=fol) #make a list of all the plot tables in the folder specified
list1

spectra<-list()
for (i in 1:length(list1)){
  # setwd("C:/Users/User/Magistrale/TESI/Analisi/v-score da excel/plot") #the directory with plot tables
  sp<-read_oo_ssirrad(paste(fol, list1[i], sep='/')) #read in the spectra files one by one
  # sp<-sp[sp$w.length>400&sp$w.length<900]#eliminate the unwanted range
  autoplot(sp)
  sample.id<-unlist(strsplit(list1[i], "..x"))[1] #substitute sample id from the file name
  spectra[[i]]<-sp
  names(spectra)[i]<-sample.id
}

head(spectra)
summary(spectra)
names(spectra)

autoplot(spectra[[9]]) #leaf
autoplot(spectra[[11]]) #flower
autoplot(spectra[[81]]) #flower

#CLEAN DATA---------------
#separate leaves from flowers
flowers<-unlist(lapply(strsplit(list1[grep("flower", list1)], "..x"), function(x){x[[1]]}))

spectra_leaves<-spectra[-c(match(flowers, names(spectra)))]
names(spectra_leaves)
head(spectra_leaves)

spectra_flowers<-spectra[c(match(flowers, names(spectra)))]
names(spectra_flowers)
head(spectra_flowers)

#Clean leaves spectra to 400-900 nm range
spectra_leaves_clean<- lapply(spectra_leaves, function(x){x[x$w.length>450&x$w.length<850]})#eliminate the unwanted range
names(spectra_leaves_clean)
head(spectra_leaves_clean)

summary(unlist(lapply(spectra_leaves_clean, function(x){x[,2]}))<0) #check if there are still negative values
#spectra with negative values:
names(spectra_leaves_clean)[which(unlist(lapply(lapply(spectra_leaves_clean, function(x){x[,2]}), function(x){sum(which(x<0))>0})))]
autoplot(spectra_leaves_clean[["Lat_pra_13B"]]) #they all seem to have a problem from 750 nm and some also around 400
#Lat_pra_13B has a proble around 690 nm
#Maybe it would be better to standardize all values?
#The issue with excluding values after 750 (near infrared) is that we are excluding information on leaf cell structure
#also, we exclude the range corresponding to the range that the drone samples
#One solution woulld be to smooth the curve values
#another solution is to exclude samples that are not correct
#how big is the pixel?? size of the sample?

names(spectra_leaves)
a<-spectra_leaves[["Tri_arv_7F"]]
autoplot(a)
autoplot(a[a$w.length>500&a$w.length<600,]) #leaf

autoplot(spectra_leaves_clean[["Ach_mil_11H"]])
autoplot(spectra_leaves_clean[["Ach_mil_14C"]])
