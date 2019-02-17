setwd("G:/My Drive/Dissertation/Data")
library(dplyr)

# read hh data
hh <- read.csv("NHTS/2017/hhpub.csv")
person <- read.csv("NHTS/2017/perpub.csv")
trip <- read.csv("Data/travel survey/NHTS/2017/trippub.csv")

trip_hh <- left_join(trip, hh, by="HOUSEID")

# add-on state: AZ, CA, GA, IA, MD, NC, NY, SC, TX, WI

# divided by state
ny <- trip %>% filter(HHSTATE=="NY")
ca <- trip %>% filter(HHSTATE=="CA")
tx <- trip %>% filter(HHSTATE=="TX")
or <- trip %>% filter(HHSTATE=="OR")

table(ny$TRPTRANS)
nyt <- xtabs(~TRPTRANS+TRIPPURP, data=ny)
round(prop.table(nyt, 2),3)

table(ca$TRPTRANS)
cat <- xtabs(~TRPTRANS+TRIPPURP, data=ca)
round(prop.table(cat, 2),3)

table(tx$TRPTRANS)
txt <- xtabs(~TRPTRANS+TRIPPURP, data=tx)
round(prop.table(txt, 2),3)

table(or$TRPTRANS)
ort <- xtabs(~TRPTRANS+TRIPPURP, data=or)
round(prop.table(ort, 2),3)

# divided by CBSA
sf <- trip %>% filter(HH_CBSA==41860)
table(sf$TRPTRANS)
sft <- xtabs(~TRPTRANS+TRIPPURP, data=sf)
round(prop.table(sft, 2),3)

sd <- trip %>% filter(HH_CBSA==41740)
table(sd$TRPTRANS)
sdt <- xtabs(~TRPTRANS+TRIPPURP, data=sd)
round(prop.table(sdt, 2),3)

la <- trip %>% filter(HH_CBSA==31080)
table(la$TRPTRANS)
sdt <- xtabs(~TRPTRANS+TRIPPURP, data=la)
round(prop.table(sdt, 2),3)

dalas <-trip %>% filter(HH_CBSA==19100)
table(dalas$TRPTRANS)
dalast <- xtabs(~TRPTRANS+TRIPPURP, data=dalas)
round(prop.table(dalast, 2),3)

ny <- trip %>% filter(HH_CBSA==35620)
table(ny$TRPTRANS)
nyt <- xtabs(~TRPTRANS+TRIPPURP, data=ny)
round(prop.table(nyt, 2),3)

pdx <- trip %>% filter(HH_CBSA==38900)
table(pdx$TRPTRANS)
pdxt <- xtabs(~TRPTRANS+TRIPPURP, data=pdx)
round(prop.table(pdxt, 2),3)




# 2009 --------------------------------------------------------------------

# read hh data
hh <- read.csv("NHTS/2009/HHV2PUB.CSV")
person <- read.csv("NHTS/2009/PERV2PUB.CSV")
trip <- read.csv("NHTS/2009/DAYV2PUB.CSV")

table(trip$HHSTATE)
# add-on states: CA, FL, NC, NY, SC, TX

ny <- trip %>% filter(HHSTATE=="NY")
ca <- trip %>% filter(HHSTATE=="CA")
tx <- trip %>% filter(HHSTATE=="TX")
or <- trip %>% filter(HHSTATE=="OR")

table(ny$TRPTRANS)
nyt <- xtabs(~TRPTRANS+TRIPPURP, data=ny)
round(prop.table(nyt, 2),3)

table(ca$TRPTRANS)
cat <- xtabs(~TRPTRANS+TRIPPURP, data=ca)
round(prop.table(cat, 2),3)

table(tx$TRPTRANS)
txt <- xtabs(~TRPTRANS+TRIPPURP, data=tx)
round(prop.table(txt, 2),3)

table(or$TRPTRANS)
ort <- xtabs(~TRPTRANS+TRIPPURP, data=or)
round(prop.table(ort, 2),3)
