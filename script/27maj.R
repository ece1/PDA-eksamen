

save.image(file='27maj.R')
load('27maj.R')


library(ggfortify)
library(olsrr)
library(sjPlot)
library(interplot)
library(jtools)
library(devtools)
library(mapDK)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(geosphere)
library(sp)
library(data.table)
library(margins)


# Asylcentre og mapDK -------------------------------------------------------------------





#Konstater, at vores kort det virker, nu skal vi bare have plottet asyl koordinaterne ind.
mapDK(detail = "polling")

#Henter vores koordinater fra exceldokument.
asylcentre <- read_xlsx("\\Users\\chemn\\OneDrive\\8. semester\\politcal data science\\Eksamensopgave\\Koordinater asylcentre .xlsx")

#Inspicerer asyldataframe
head(asylcentre)
tail(asylcentre)
View(asylcentre)


# Laver om til numeric
asyl1 <- transform(asylcentre, Long = as.numeric(Long),
                   Lat = as.numeric(Lat))

#Fjener "navne kolonnen" og runder ned til kun 5 decimaler
asyl2<- asyl1[,c(2,3)] %>% round(digits = 5)



#Laver to ny matrixes bem�rk det lat og lon st�r med sm�t ellers vil det ikke virke
lat <- asyl2$Lat
lon <- asyl2$Long


#Vi laver et data.frame der hedder asyl4 og kalder det pr�cis det samme, som de vektorer, der
#er skabt ovenover. Dette skal g�res for at "pointDK" nedenunder virker.

asyl4 <- data.frame(lon = lon , lat = lat)



#plotter ind p� kort, tager 5-10 sek.
pointDK(asyl4 , detail = "polling", point.colour = "blue")



#Kort over Bornholm med Asylcentre.
pointDK(asyl4[c(20,21),], detail = "polling", sub.plot = "bornholm", point.colour = "blue")


# Afstand mellem asylcentre og konstiturende valgdistriktspunkter ---------------------------------------------------

#Laver et data.frame der indeholder, lon, lat , group, id, komunenummer, kummunenavn.
#Data bliver hentet fra mapDK-pakken. 

data <- mapDK(detail = "polling")[["data"]] ##NB uden valgresultater

#Laver en ren variabel med lon og lat.
#S�ledes f�r vi koordinaterne for hvert valgdistriktspunkt i hele DK
data.ren <- data %>% 
  transmute(long, lat )

View(data.ren)


#Vi har nu 155.072 r�kker med valgdistriktspunkt (lon, lat) i hver r�kke
#Dette datas�t skal vi ikke bruge endnu, men senere.



# Vi laver nu en variabel ud fra mapDK, hvor vi samler "KommuneNum" og "Afstemkod", s�ledes at 
#det bliver til en variabel "pr�ve". Dern�st laver vi en ny variabel "valgID" ud fra
#"pr�ve", hvor vi erstatter _ med 0. I dette datas�t har vi ogs� "lon" og "lat", som i data.ren


samlet <- polling %>% 
  unite(pr�ve, c("KommuneNum","AfstemKod"), remove = F) %>% 
  mutate(valgID = str_replace(pr�ve, "_", "0"))


View(samlet)



# Vi laver nu en ny variabel, hvor vi fjerner kolonner, som er overfl�dige fra "samlet".
# s� det bliver lettere at overskue.
str(samlet)

drop.kol <- c("group", "pr�ve","AfstemKod", "id", "KommuneNum", 
              "KommuneNav", "OpstilNum", "OpstilNav")

stor_data <- samlet %>% select(-one_of(drop.kol))
View(stor_data)






# Laver data om til numeric. 
stordata1 <- transform(stor_data, valgID = as.numeric(valgID))
head(stordata1)

#Det smarte er nu, at nu har vi valgID koblet sammen med "lon" og "lat" kan joines med 
# valgstatistik fra "Danmarks Statistik, der ogs� har lon, lat og valgID

#Inden vi joiner, kobler vi alt vores valgdata p� "stordata1". Valgdata fra 2015 og kan 
#nemlig blive joinet med vores valgID i stordata1

#Henter vores valgdata 2015 fra exceldokument.


# Joine data --------------------------------------------------------------
valgdata2015 <- read_xlsx("\\Users\\chemn\\OneDrive\\8. semester\\politcal data science\\Eksamensopgave\\Valgdata_15 .xlsx")
View(valgdata2015)

#Vi ser, at i "Gruppe" er et valgid, nummer, som passer, sammen med "valgID fra
# "samlet"



#Laver "samlet" med lon, lat og valgID om til numeric
str(samlet)

#ValgID er chr., hvilket laves om til numeric, s� det kan joines med valgdata2015 nedenfor.
samlet2 <- transform(samlet, valgID = as.numeric(valgID))

#Vi joiner samlet2 og valgdata
data2 <- left_join(samlet2, valgdata2015, by = c("valgID" = "Gruppe"))

#Juhu. Vi har nu et datas�t med hvert valgdistrikt koordinater (lon og lat) og dens stemmefordeling
#i �r 2015


# N�ste operation tager 2-3 min. - vi udregner afstande for hvert af den
# 155072 datapunkter ud fra hvert enkelt asylcenter. Bliver til en matrix
# Bruges senere, n�r vi skal udregne afstande til valgsteder. 
# Vi bruger koordinaterne fra asyl4, og s� kun data.ren s�ttet, som blev lavet tidligere,
# da det bliver mere overskueligt.

dist_asyl_valgsted  <- distm(data.ren, asyl4, fun = distHaversine)


#Tr�kker alle navne ud af asylcentre, s� de kan tilknyttes kolonnerne i dist_asyl_valgsted
names <- asylcentre$Asylcenter
colnames(dist_asyl_valgsted) <- names

#Laver matrixen om til dataframe for at kunne bind_cols

dist_asyl_valgsted_df <- as.data.frame(dist_asyl_valgsted)
View(dist_asyl_valgsted_df)
#Konverterer meter til kilometer
dist_asyl_valgsted_df_km <- with(dist_asyl_valgsted_df, dist_asyl_valgsted_df[, 1:46]/1000)

View(data2)
View(dist_asyl_valgsted_df_km)

#Merger distancerne fra hvert enkelt asylcenter til valgstedspunkterne sammen med tidy valgdata

master <- bind_cols(dist_asyl_valgsted_df_km, data2)

str(master)

View(master)

#Vi har nu et datas�t, hvor hvert asylcenters afstand til hvert konstituerende valgdistriktspunkt 
#i hele landet er regnet ud.

#Det er en smule overkill, da det vi er interesseret i, er at udregne afstanden til hvert valgdistrikt 
#og alts� ikke alle dens konstituerende punkter. 
#Det vil de n�ste linjers kode fors�ge at rode bod p�.

#Tilf�jer kolonne til master, som viser det et valgdistrikts korteste afstand til 
#n�rmeste asylcenter. Dvs. den kun viser afstanden fra alle dens konstituerende valgdistriktspunkter
#til det asylcenter med den korteste afstand



master$korteste_dist <- apply(master[, 1:46], 1, min)


#Grupperer p� valgID, for at finde den korteste, gennemsnitlige og max afstand
#distance indenfor hvert valgdistrikt til n�rmeste asylcenter

master_t <- master %>%
  group_by(valgID) %>%
  mutate(kort_dist_endelig = min(korteste_dist), kort_dist_gennemsnit = 
           mean(korteste_dist), kort_dist_max = max(korteste_dist))


View(master_t)
#Vi har nu fundet den korteste, l�ngste og gennemsnitlige afstand fra hvert valgdistrikt til n�rmeste
#asylcenter.
# 

#Vi vil regne den andelen af stemmer ud i procent. 
# Vi specificere de kolonner, som skal udregnes i en vektor (se nedenunder)

parti_stemmekolonne <- c(67:76)

#Bruger for loop.
# For hver kolonne i master_t i omr�det "parti_stemmekolonne, skal funktionen
#lave en ny kollonne, med det gamle navn + _pct til andet decimal.  

#Den skal udfylde de nye kolonner ved at dividere parti_stemme kolonne variablen med antal 
#stemmeberettiede variablen og afrunde til andet decimal.

for(col in names(master_t)[parti_stemmekolonne]) {
  master_t[paste0(col, "_pct")] = round(100*master_t[col]/master_t$`FV2015 - Afgivne stemmer`,2)
}

View(master_t)




# V�lger kun observationer for hvert af de 1419 valgdistrikter.
#S�ledes "ignoreres" de resterende 153653 andre observationer, der 
# s� at sige er dobbeltkonfekt i og med den korteste, l�ngste og
#gennemsnitlige afstand er regnet ud. 


master_t3 <- master_t %>% 
  group_by(valgID) %>% 
  slice(which.max(kort_dist_max)) %>%
  slice(which.max(kort_dist_gennemsnit)) %>%
  slice(which.max(kort_dist_endelig)) 

View(master_t3)

#Laver et afstandsdataframe, hvor kun  afstande er med.

afstand <- master_t3[1:1419, 82:84] %>%
  mutate(Forskel = kort_dist_max - kort_dist_endelig)

summary(afstand$kort_dist_endelig)
#Den formel bruges i opgaven til at sige noget om, hvor forskellige henholdvis 
#gennemsnitsafstand, l�ngste og korteste afstand er fra hinanden. 



#Laver vektor hvor un�dvendige kolonner er specificeret

fjernkol1 <- c("long", "lat","group", "pr�ve", "AfstemKod",
               "OpstilNum", "ValgstedId", "KredsNr", "StorKredsNr", "LandsdelsNr")

master_t4 <- master_t3 %>% 
  select(-one_of(fjernkol1))

View(master_t4)

# Vi har nu et datas�t med valgdatafor hvert valgdistrikt og hvert valgdistrikts afstand til 
#n�rmeste asylcenter.

# Klarg�ring af data ------------------------------------------------------


#Henter kontrolvariable fra to datas�t
kontrol <- read_xlsx("\\Users\\chemn\\OneDrive\\8. semester\\politcal data science\\Eksamensopgave\\master_kontrol_1.xlsx")
borgmestre <- read_xlsx("\\Users\\chemn\\OneDrive\\8. semester\\politcal data science\\Eksamensopgave\\borgmestre_1.xlsx")

#Omd�ber for at kunne merge p� ved en unik ID l�ngere nede.


colnames(kontrol)[1] <- "Kommune"
colnames(kontrol)[4] <- "KommuneNum"

#Omd�ber kolonner
colnames(master_t4)[75] <- "soc"
colnames(master_t4)[76] <- "RV"
colnames(master_t4)[77] <- "kons"
colnames(master_t4)[78] <- "SF"
colnames(master_t4)[79] <- "LA"
colnames(master_t4)[80] <- "krist"
colnames(master_t4)[81] <- "DF"
colnames(master_t4)[82] <- "venstre"
colnames(master_t4)[83] <- "EL"
colnames(master_t4)[84] <- "alt"

View(master_t4)

###MERGE MED KONTROL

#Konverterer master_t4 KommueNum til numeric for at kunne left_joine

master_t4 <- transform(master_t4, KommuneNum = as.numeric(KommuneNum))
borgmestre <- transform(borgmestre, KommuneNum = as.numeric(komnr))


#Merger data. 
final01 <- left_join(master_t4, kontrol)
final <- left_join(final01, borgmestre)


#Laver formue om til 100.000 kr.
final <- transform(final, formue=formue/100000)
#Laver forordelingsblokke

final$roedblok <- final$soc + final$RV + final$SF + final$EL + final$alt
final$blaablok <- final$venstre + final$kons + final$LA + final$krist + final$DF


#Laver dummy for by - KBH og FrB = 1, resten = 0
final <- mutate(final, by = ifelse(KommuneNum == 101 & 147, 1, 0))


#Dummy storby (KBH, FrB, Aarhus, Odense og Aalborg)
storbyer <- c(101, 147, 461, 751, 851)
final <- mutate(final, storby = ifelse(KommuneNum %in% storbyer, 1, 0))

View(final)

#Vi har nu det endelige datas�t.
#Vi sletter lige kolonner, som virker overfl�dige.


final <- select(final, -1:-48,-48,-71,-85,-102,-103)
View(final)

# DF regressioner ------------------------------------------------


DFbi <- lm(DF ~ kort_dist_endelig, data = final)


DF_indvandring <- lm(DF ~ kort_dist_endelig + sb_ikkevest_pr10000 + sb_vest_pr10000, data = final)

DF_demo <- lm(DF ~ kort_dist_endelig + beftaet + Gns_alder, data = final)

DF_demo_indv <- lm(DF ~ kort_dist_endelig + beftaet + Gns_alder + sb_ikkevest_pr10000 + sb_vest_pr10000, data = final)


DF_kriminalitet <- lm(DF ~ kort_dist_endelig + tyvindbr_pr1000 + vold_pr1000 , data = final)

DF_kapital <- lm(DF ~ kort_dist_endelig + formue + lvu + ledige_pct, data = final)

DF_politik <- lm(DF ~ kort_dist_endelig + borgblok, data = final)


DF_alle <-  lm(DF ~ kort_dist_endelig + beftaet + ledige_pct + sb_ikkevest_pr10000 +
                              sb_vest_pr10000 + formue + storby + Gns_alder + lvu + tyvindbr_pr1000 + vold_pr1000 + borgblok, data = final)


#Laver tabel

sjt.lm(DFbi, DF_indvandring, DF_demo, DF_demo_indv, DF_kriminalitet, DF_kapital, DF_politik, DF_alle,
       p.numeric = F, show.ci = F, show.se = T, show.fstat = T, depvar.labels = c("Bivariat", "Indvandring",
       "Demografi", "Demografi + Indvandring", "Kriminalitet", "Kapital", "Borgmester", "Alle kontroller"))
#Foruds�tnignstest
autoplot(DF_alle, which = 1:6, ncol = 3, label.size = 3)



# Interaktion  --------------------------------------------------------
#Vi vil unders�ge interaktionsvariabel for borgmesterfarve og storby/land
#F�rst borgmesterfarve.
DF_storby <- lm(DF ~ kort_dist_endelig*storby + beftaet + ledige_pct + sb_ikkevest_pr10000 +
                                  sb_vest_pr10000 + formue + Gns_alder + lvu + tyvindbr_pr1000 + 
                                  vold_pr1000, data = final)
DF_borg <- lm(DF ~ kort_dist_endelig*borgblok + beftaet + storby + ledige_pct + sb_ikkevest_pr10000 +
                    sb_vest_pr10000 + formue + Gns_alder + lvu + tyvindbr_pr1000 + 
                    vold_pr1000, data = final)

#Foruds�tningstest

autoplot(DF_borg_int, which = 1:6, ncol = 3, label.size = 3)
autoplot(DF_storby, which = 1:6, ncol = 3, label.size = 3)

# Vi laver nu det sammen men med storby, som interaktionsvariabel. Det er s�dan, 
autoplot(DF_borg, which = 1:6, ncol = 3, label.size = 3)

sjt.lm(DF_alle, DF_borg, DF_storby, p.numeric = F, show.ci = F, show.se = T, 
       show.fstat = T, depvar.labels = c("Alle Kontroller", "Borgmester", "Storby"))




# R�d blok regressioner ---------------------------------------------------

#Den afh�ngige variabel er r�d blok. Hele shittet er signifikant
r�dbi <- lm(roedblok ~ kort_dist_endelig, data = final)

r�d_indvandring <- lm(roedblok ~ kort_dist_endelig + sb_ikkevest_pr10000 + sb_vest_pr10000, data = final)

r�d_demo <- lm(roedblok ~ kort_dist_endelig + beftaet + Gns_alder, data = final)

r�d_demo_indv <- lm(roedblok ~ kort_dist_endelig + beftaet + Gns_alder + sb_ikkevest_pr10000 + sb_vest_pr10000, data = final)


r�d_kriminalitet <- lm(roedblok ~ kort_dist_endelig + tyvindbr_pr1000 + vold_pr1000 , data = final)

r�d_kapital <- lm(roedblok ~ kort_dist_endelig + formue + lvu + ledige_pct, data = final)

r�d_politik <- lm(roedblok ~ kort_dist_endelig + borgblok, data = final)


r�d_alle <-  lm(roedblok ~ kort_dist_endelig + beftaet + ledige_pct + sb_ikkevest_pr10000 +
                 sb_vest_pr10000 + formue + storby + Gns_alder + lvu + tyvindbr_pr1000 + vold_pr1000 + borgblok, data = final)


sjt.lm(r�dbi, r�d_indvandring, r�d_demo, r�d_demo_indv, r�d_kriminalitet, r�d_kapital, r�d_politik, r�d_alle,
       p.numeric = F, show.ci = F, show.se = T, show.fstat = T, 
       depvar.labels = c("Bivariat", "Indvandring",
      "Demografi", "Demografi + Indvandring", "Kriminalitet", "Kapital", 
      "Borgmester", "Alle kontroller"))


autoplot(r�d_storby, which = 1:6, ncol = 3, label.size = 3)
autoplot(r�d_alle, which = 1:6, ncol = 3, label.size = 3)



r�d_storby<-  lm(roedblok ~ kort_dist_endelig*storby + ledige_pct + beftaet + storby 
                     + sb_ikkevest_pr10000 + formue + Gns_alder + sb_vest_pr10000 +
                   + tyvindbr_pr1000 + vold_pr1000 + storby + lvu + borgblok, data = final)


sjt.lm(r�d_alle, r�d_storby,
       p.numeric = F, show.ci = F, show.se = T, show.fstat = T,
       depvar.labels = c("Alle kontrolle", "Storby"))

#Foruds�tningstest
autoplot(r�d_storby, which = 1:6, ncol = 3, label.size = 3)
autoplot(r�d_alle, which = 1:6, ncol = 3, label.size = 3)


#Plot til r�d blok storby

gg <- ggplot(final, aes(kort_dist_endelig, roedblok, colour = factor(storby))) + geom_point()

gg +  geom_smooth(color = "yellow", method ="lm", fill = "green") +
  scale_colour_manual(labels = c("Land", "By"), values = c("dodgerblue3", "firebrick3")) +
  labs(y="DF i %", 
       x="Afstand til n�rmeste asylcenter (km)", 
       title="Afstand og opbakning til R�d Blok",
       color = "Valgdistriktstype") +
  scale_fill_discrete(breaks=c("trt1","ctrl")) + 
  geom_point(aes(shape=as.factor(storby)), show.legend = F)

View(final)
