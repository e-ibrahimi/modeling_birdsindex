#Load libraries and data

library(feather)
library(data.table)
library(mgcv)
library(car)
library(ggplot2)
library(grid)
library(animation)
library(readr)


birds <- read_csv("C:/Users/user/Desktop/birds.csv", show_col_types = FALSE)

birds$Family <- factor(birds$Family)
birds$habitat <- factor(birds$habitat)
birds$species <- factor(birds$species)
```
All birds by habitat

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=habitat ), method="REML", data = birds, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)



#All birds by family

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=Family), method="REML", data = birds, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)


#Family Fringillidae 

#data
fringi=birds[Family=="Fringillidae",]

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = fringi, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)

#Family Accipitridae

#data
accip=birds[Family=="Accipitridae",]

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = accip, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)


#Family Acrididae 

#data
acrid=birds[Family=="Acrididae",]

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = acrid, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)


#Family Alaudidae 

#data
alaud=birds[Family=="Alaudidae",]

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = alaud, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)

#Family Alcedinidae 

#data
alced=birds[Family=="Alcedinidae",]

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = alced, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)

#Family Bombycillidae

#data
bomby=birds[Family=="Bombycillidae",] 

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = bomby, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)


#Family Gruidae

#data
grui=birds[Family=="Gruidae",] 

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = grui, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)

#Family Laridae

#data
Larid=birds[Family=="Laridae",] 

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = Larid, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)


#Family Meropidae

#data
merop=birds[Family=="Meropidae",] 

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = merop, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)


#Family Motacillidae

#data
#mota=birds[Family=="Motacillidae",] 
anthus=birds[birds$species=="Anthus campestris",]
#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = anthus, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)


#Family Otididae

#data
otid=birds[Family=="Otididae",] 

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = otid, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)


#Family Phasianidae

#data
phasia=birds[Family=="Phasianidae",] 

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = phasia, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)


#Family Podicipedidae

#data
podi=birds[birds$Family=="Podicipedidae",] 
cristatus=birds[birds$species=="Podiceps cristatus",]
#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = podi, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)


#Family Scolopacidae

#data
scolo=birds[birds$Family=="Scolopacidae",] 
tringat=birds[birds$species=="Tringa totanus",] 
#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species), method="REML", data = tringat, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)


#Family Sylviidae

#data
sylvi=birds[Family=="Sylviidae",] 

#GAM
mod_gam1 = gam(log(Index) ~ s(Year, bs = "cr", by=species ), method="REML", data = sylvi, random=list(code = ~1))
summary(mod_gam1)
gam.check(mod_gam1)

plot(mod_gam1)
plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
gratia::draw(mod_gam1)





