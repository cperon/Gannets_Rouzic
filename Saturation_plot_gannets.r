##--------------------------------------------------------------------------------------------------------
## SCRIPT : Saturation plot - minimum tracks
##
## Authors : Clara Peron
## Last update : 2017-09-12
##
## R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
## Copyright (C) 2016 The R Foundation for Statistical Computing
## Platform: x86_64-w64-mingw32/x64 (64-bit)
##--------------------------------------------------------------------------------------------------------

rm(list=ls())

# Packages ? charger
library(maps)
library(trip)
library(adehabitatLT)
library(gdata)

load('C:/Users/Clara PERON/Documents/Collaborations/2017-Rouzic-TangiLeBot/gps_tracks/GPS_Gannets_cleaned.Rdata')
summary(gannets)

# remove locs on land
gannets <- gannets[gannets$trip_id != 0,]
gannets$ID <- paste(gannets$id, gannets$trip_id, sep='-')
gannets$ID <- as.factor(gannets$ID)
gannets <- gannets[gannets$trip_id == 1,]
gannets <- drop.levels(gannets)

# r?solution de la grille
res=10000
ylim_min=5000000
ylim_max=6000000
xlim_min=-240000
xlim_max=400000

gt <- GridTopology(cellcentre.offset=c(xlim_min,ylim_min), cellsize = c(res, res), cells.dim=c((xlim_max-xlim_min)/res,(ylim_max-ylim_min)/res))

# Initialisation
NB=NULL

for (i in levels(gannets$ID)){
    print(i)
    X <- gannets[gannets$ID==i,]

    if(nrow(X) !=0){
    X$Num<- seq(1,nrow(X),1)
    
    # suppression des duplicas
    X <- X[duplicated(X$Date_Time_GMT)==FALSE,]

       # 1) temps pass? par secteur :
           trajet <- data.frame(ID=X$ID,long=X$X_UTM,lat=X$Y_UTM,date=X$Date_Time_GMT)
         #  trajet2 <- trajet[order(trajet$ID, trajet$date),]

        		# Conversion en classe 'trip'
              coordinates(trajet) <- ~long + lat
              trajett <- trip(trajet, c("date", "ID"))
              sp::proj4string(trajett) <- CRS("+init=epsg:3857") # def du systeme de coord

            # Create a grid of time spent by approximating the time between locations for separate trip events.
              trg <- tripGrid(trajett, grid = gt, method="pixellate")     # d?coupage, 1 point = 60 sec

              IndexTrg <- getGridIndex(coordinates(trg), gt, all.inside = TRUE)

              trg1 <- data.frame(trg)
              trg1$bloc <- IndexTrg
              trg1$ID <- i
              trg1 <- subset(trg1,trg1$z !=0)

              nb <- data.frame(id=i, num.cell=trg1$bloc)
              NB <- rbind(NB, nb)}
                }


NB$Year <- substr(NB$id, 1, 4)
NB$Year <- as.factor(NB$Year)


# Ranking of the tracks depending on the surface covered
TOT=NULL

# Loop over years
for(Year in levels(NB$Year)){
    print(Year)
    nb <- NB[NB$Year==Year,]
    nb <- drop.levels(nb)
    nb$id <- as.character(nb$id)
    idd <- as.character(unique(nb$id))
    seq <- as.character(rev(order(table(nb$id))))
    SEQ <- data.frame(rank=seq(1,length(seq),1), id=seq)
    for(j in 1:nrow(SEQ)){
      SEQ$ID[j] <- attr( table(nb$id), 'names')[as.numeric(as.character(SEQ$id[j]))]
    }

    # Initialisation
    # Loop over id by rank
    tot = NULL
    for (i in 1:nrow(SEQ)){
        print(i)     
          sub <- nb[nb$id==SEQ$ID[i],]
          sub$rank1 <- SEQ$rank[i]
          tot <- rbind(tot, sub)
          }
    
    tot <- tot[order(tot$rank1),]
    
  # Loop over indivuals, by rank  
    
    B <- tot[tot$rank1==1,]
    Sum <- data.frame(id=B$id[1], rank1=1, nb=nrow(B))
    
    for (j in 2:length(seq)){
        print(j)
        Index.B <- unique(B$num.cell)
        Xp <- tot[tot$rank1==j,]
        Xp.unique <- Xp[!Xp$num.cell %in% Index.B,]
        B <- rbind(B, Xp)
        Sum <- rbind(Sum, data.frame(id=unique(Xp$id), rank1= unique(Xp$rank1), nb=length(Index.B)+nrow(Xp.unique)))
        }
TOT <- rbind(TOT, Sum)
rm(Sum)
}
    

str(TOT)
TOT$Year <- substr(TOT$id, 1, 4)

p <- ggplot(data=TOT, aes(x=rank1, y=nb, group=Year, colour=Year)) +  geom_line() +  geom_point() + theme_bw() + xlab('Number of birds tracked with GPS')
p <- p +  ylab(expression(paste('Surface incrementation (', km^{2}, ")", sep="")))
ggsave("C:/Users/Clara PERON/Documents/Collaborations/2017-Rouzic-TangiLeBot/gps_tracks/Fig_saturation_NGannets.png", p, width=25, height=14, units = 'cm', dpi=600)  

