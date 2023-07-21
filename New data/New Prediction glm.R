
install.packages("MuMIn")

library(MASS)
library(MuMIn)

WaterQuality = read.csv("Water quality 2.csv")
Macroinvertebrate = read.csv("Macroinvertebrate2.csv")
Macroinvertebrate = filter(Macroinvertebrate, EcoRegion == "P")
Macroinvertebrate[Macroinvertebrate == "CO"] = "COLEOPTERA"
Macroinvertebrate[Macroinvertebrate == "DI"] = "DIPTERA"
Macroinvertebrate[Macroinvertebrate == "OD"] = "ODONATA"
Macroinvertebrate[Macroinvertebrate == "CR"] = "CRUSTACEA" #class#
Macroinvertebrate[Macroinvertebrate == "GA"] = "GASTROPODA" #class#
Macroinvertebrate[Macroinvertebrate == "ME"] = "MEGALOPTERA"
Macroinvertebrate[Macroinvertebrate == "HE"] = "HEMIPTERA"
Macroinvertebrate[Macroinvertebrate == "EP"] = "EPHEMEROPTERA"
Macroinvertebrate[Macroinvertebrate == "DIM"] = "DIPTERA"
Macroinvertebrate[Macroinvertebrate == "PL"] = "PLECOPTERA"
Macroinvertebrate[Macroinvertebrate == "OL"] = "OLIGOCHAETA" #class#
Macroinvertebrate[Macroinvertebrate == "PE"] = "PELECYPODA" #class#
Macroinvertebrate[Macroinvertebrate == "TR"] = "TRICHOPTERA"
Macroinvertebrate[Macroinvertebrate == "OT"] = "OTHER_TAXA"
Macroinvertebrate = Macroinvertebrate[-which(Macroinvertebrate$County == "York, SC"), ]
Macroinvertebrate = Macroinvertebrate[-which(Macroinvertebrate$County == "(Danville), VA"), ]
Macroinvertebrate = Macroinvertebrate[-which(Macroinvertebrate$County == "Tallapoosa, AL"), ]
Macroinvertebrate = Macroinvertebrate[-which(Macroinvertebrate$County == "Laurens, SC"), ]
Macroinvertebrate = Macroinvertebrate[-which(Macroinvertebrate$County == "Newberry, SC"), ]

#filter#
Macro = Macroinvertebrate %>% group_by(Date, Location, County, Latitude, 
                                       Longitude, Waterbody) %>% 
  summarise(Tot = sum(Abundance))

Macro = na.omit(Macro)
Macro = as.data.frame(unclass(Macro), stringsAsFactors = TRUE)

WaterQuality = filter(WaterQuality, EcoRegion == "P")
WaterQuality = subset(WaterQuality, select = c( "Date", "County",
                                                "Waterbody","Water.Class", 
                                                "Latitude","Longitude",
                                                "Drainage", 
                                                "Temp.C","Sp_Cond", "pH_SU",
                                                "Diss_Oxy"))
WaterQuality = na.omit(WaterQuality)
WaterQuality = WaterQuality[-which(WaterQuality$Sp_Cond == 0), ]
WaterQuality = WaterQuality[-which(WaterQuality$pH_SU == 0), ]
WaterQuality = WaterQuality[-which(WaterQuality$Diss_Oxy == 0), ]
WaterQuality = as.data.frame(unclass(WaterQuality),                    
                             stringsAsFactors = TRUE)


MWQ = merge(Macro, WaterQuality, by = c("Date", "County",
                                        "Waterbody", 
                                        "Latitude","Longitude"))

MWQ$Date <- as.Date(MWQ$Date , format = "%m/%d/%y")
MWQ =MWQ %>% relocate(Water.Class, .before = Tot)
MWQ = MWQ %>% relocate(Drainage, .before = Tot)

MWQ$Tot <- as.numeric(MWQ$Tot)

summary(MWQ) #this is to find the median so I can split the data as evenly as,
#possible to create a test model.

Pre = MWQ %>% filter(Date <= '2005-07-12')

Post = MWQ %>% filter(Date > '2005-07-12')

wq.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail" )

wq = dredge(wq.glm) # this seems to be taking a long time to run
#going to try a different way#

Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )

wq = dredge(wq.glm1)



Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


# Habitats ----------------------------------------------------------------

#Lake#

Lake = filter(MWQ, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-16')

Post = Lake %>% filter(Date > '2006-08-16')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")



#Riverine#

Riverine = filter(MWQ, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2005-07-12')

Post = Riverine %>% filter(Date > '2005-07-12')
Post [ nrow(Post) +  5, ] <- NA

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(MWQ, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", 
                     "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-07-26')

Post = Wetland %>% filter(Date > '2005-07-26')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

Specific.Conductivity = glm.nb(Tot ~ Sp_Cond, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Sp_Cond)

SC.pred = predict(Specific.Conductivity, newdata = d0) %>% exp()

chisq.test(Post$Tot, SC.pred)

cor.test(Post$Tot, SC.pred, use = "everything")



#class#
# Arachnida ---------------------------------------------------------------

#ARACHNIDA#
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-11')

Post = Wetland %>% filter(Date > '2005-08-11')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed ph as best

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# BIVALVIA ----------------------------------------------------------------

BIVALVIA = filter(Macroinvertebrate, Class == "BIVALVIA")

BIVALVIA = BIVALVIA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
BIVALVIA = merge(BIVALVIA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


BIVALVIA$Date = as.Date(BIVALVIA$Date , format = "%m/%d/%y")

BIVALVIA = as.data.frame(unclass(BIVALVIA),
                          stringsAsFactors = TRUE)

BIVALVIA$Tot <- as.numeric(BIVALVIA$Tot)

summary(BIVALVIA)



#Before#
Pre = BIVALVIA %>% filter(Date <= '2004-06-27')
#After#
Post = BIVALVIA %>% filter(Date > '2004-06-27')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

WQ3 = glm.nb(Tot ~ Temp.C + Diss_Oxy + pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Temp.C,Diss_Oxy, pH_SU)

WQ3.pred = predict(WQ3, newdata = d0) %>% exp()

chisq.test(Post$Tot, WQ3.pred)

cor.test(Post$Tot, WQ3.pred, use = "everything")

#Lake#
Lake = filter(BIVALVIA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-16')

Post = Lake %>% filter(Date > '2006-08-16')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(BIVALVIA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-11-09')

Post = Riverine %>% filter(Date > '2006-11-09')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

WQ3 = glm.nb(Tot ~ Temp.C + Diss_Oxy + pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Temp.C, Diss_Oxy,pH_SU)

WQ3.pred = predict(WQ3, newdata = d0) %>% exp()

chisq.test(Post$Tot, WQ3.pred)

cor.test(Post$Tot, WQ3.pred, use = "everything")


#Wetland#

Wetland = filter(BIVALVIA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2004-04-26')

Post = Wetland %>% filter(Date > '2004-04-26')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")
# CLITELLATA --------------------------------------------------------------
CLITELLATA = filter(Macroinvertebrate, Class == "CLITELLATA")

CLITELLATA = CLITELLATA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
CLITELLATA = merge(CLITELLATA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


CLITELLATA$Date = as.Date(CLITELLATA$Date , format = "%m/%d/%y")

CLITELLATA = as.data.frame(unclass(CLITELLATA),
                          stringsAsFactors = TRUE)

CLITELLATA$Tot <- as.numeric(CLITELLATA$Tot)

summary(CLITELLATA)



#Before#
Pre = CLITELLATA %>% filter(Date <= '2005-06-29')
#After#
Post = CLITELLATA %>% filter(Date > '2005-06-29')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

WQ2 = glm.nb(Tot ~ Temp.C + Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Temp.C,Diss_Oxy)

WQ2.pred = predict(WQ2, newdata = d0) %>% exp()

chisq.test(Post$Tot, WQ2.pred)

cor.test(Post$Tot, WQ2.pred, use = "everything")

#Lake#
Lake = filter(CLITELLATA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-16')

Post = Lake %>% filter(Date > '2006-08-16')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(CLITELLATA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2005-06-27')

Post = Riverine %>% filter(Date > '2005-06-27')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

WQ2 = glm.nb(Tot ~ Temp.C + Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Temp.C, Diss_Oxy)

WQ2.pred = predict(WQ2, newdata = d0) %>% exp()

chisq.test(Post$Tot, WQ2.pred)

cor.test(Post$Tot, WQ2.pred, use = "everything")


#Wetland#

Wetland = filter(CLITELLATA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-07-27')

Post = Wetland %>% filter(Date > '2005-07-27')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

SC = glm.nb(Tot ~ Sp_Cond, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Sp_Cond)

SC.pred = predict(SC, newdata = d0) %>% exp()

chisq.test(Post$Tot, SC.pred)

cor.test(Post$Tot, SC.pred, use = "everything")
# ENOPLA  -----------------------------------------------------------------
ENOPLA = filter(Macroinvertebrate, Class == "ENOPLA")

ENOPLA = ENOPLA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ENOPLA = merge(ENOPLA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ENOPLA$Date = as.Date(ENOPLA$Date , format = "%m/%d/%y")

ENOPLA = as.data.frame(unclass(ENOPLA),
                          stringsAsFactors = TRUE)

summary(ENOPLA)



#Before#
Pre = ENOPLA %>% filter(Date <= '2003-06-23')
#After#
Post = ENOPLA %>% filter(Date > '2003-06-23')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1,na.action = "na.fail" )
wq = dredge(wq.glm)

DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot, Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ENOPLA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2003-04-14')

Post = Riverine %>% filter(Date > '2003-04-14')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Wetland#

Wetland = filter(ENOPLA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# GASTROPODA --------------------------------------------------------------
GASTROPODA = filter(Macroinvertebrate, Class == "GASTROPODA")

GASTROPODA = GASTROPODA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
GASTROPODA = merge(GASTROPODA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


GASTROPODA$Date = as.Date(GASTROPODA$Date , format = "%m/%d/%y")

GASTROPODA = as.data.frame(unclass(GASTROPODA),
                          stringsAsFactors = TRUE)

GASTROPODA$Tot <- as.numeric(GASTROPODA$Tot)

summary(GASTROPODA)



#Before#
Pre = GASTROPODA %>% filter(Date <= '2005-06-29')
#After#
Post = GASTROPODA %>% filter(Date > '2005-06-29')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

WQ3 = glm.nb(Tot ~ Temp.C + pH_SU + Sp_Cond, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Temp.C,pH_SU)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(GASTROPODA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(GASTROPODA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(GASTROPODA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# INSECTA -----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# MAXILLOPODA -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# TURBELLARIA -------------------------------------------------------------

ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")


#order#

# AMPHIPODA ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# ARCHITAENIOGLOSSA -------------------------------------------------------

ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")
# ARHYNCHOBDELLIDA --------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# BASOMMATOPHORA ----------------------------------------------------------

ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")
# BRANCHIOBDELLIDA --------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# COLEOPTERA --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# DECAPODA ----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# DIPTERA -----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# EPHEMEROPTERA -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# HAPLOTAXIDA -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# HEMIPTERA ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# HOPLONEMERTEA -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# ISOPODA -----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# LEPIDOPTERA -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# LUMBRICULIDA ------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# MEGALOPTERA -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# NEOTAENIOGLOSSA ---------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# NEUROPTERA --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# ODONATA -----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# OPISTHOPORA -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# OTHER_TAXA --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# PLECOPTERA --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# RHYNCHOBDELLIDA ---------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# TRICHOPTERA -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# TRICLADIDA --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# TROMBIDIFORMES ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# UNIONIDA ----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# VENEROIDA ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")


#Family#

# Aeshnidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Ameletidae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Ancylidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Asellidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Athericidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Baetidae ----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Baetiscidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Belostomatidae ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")


# Brachycentridae ---------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Branchiobdellidae -------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Caenidae ----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Calamoceratidae ---------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Calopterygidae ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Cambaridae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Capniidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Ceratopogonidae ---------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Chaoboridae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Chironomidae ------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Chloroperlidae ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Coenagrionidae ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Corbiculidae ------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Cordelegastridae  -------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Cordulegastridae --------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Corduliidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Corixidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Corydalidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Crambidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Crangonyctidae ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Culicidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Dipseudopsidae ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Dixidae -----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")


# Dryopidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Dugesiidae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Dytiscidae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Elmidae -----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Empididae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Enchytraeidae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Ephemerellidae ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Ephemeridae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Erpobdellidae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Gammaridae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Gerridae ----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Glossiphoniidae ---------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Glossosomatidae ---------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Gomphidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Gyrinidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Haliplidae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Hyalellidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Hydrachnidae ------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Hydrobiidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Hydrochidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Hydrometridae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Hydrophilidae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Hydropsychidae ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Hydroptilidae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Isonychiidae ------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Lepidostomatidae --------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Leptoceridae ------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Leptohyphidae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Leptophlebiidae ---------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Leuctridae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Libellulidae ------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Limnephilidae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Limoniidae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Lumbriculidae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Lymnaeidae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Metretopodidae ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Molannidae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Naididae ----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Nemouridae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Neoephemeridae ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Nepidae -----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Noteridae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Nymphalidae/Limoniidae --------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Odontoceridae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Palaemonidae ------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Pediciidae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Peltoperlidae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Perlidae ----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Perlodidae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Philopotamidae ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Phryganeidae ------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Physidae ----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Pisidiidae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Planariidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Planorbidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Pleuroceridae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Polycentropodidae -------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Polymitarcyidae ---------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Potamanthidae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Psephenidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Psychodidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Psychomyiidae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Pteronarcyidae ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Ptilodactylidae ---------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Ptilodactylidae ---------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Pyralidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Rhyacophilidae ----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Scirtidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Sialidae ----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Simuliidae --------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Siphlonuridae -----------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Sisyridae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Tabanidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Taeniopterygidae --------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Tanyderidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Tetrastemmatidae --------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Tipulidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Tubificidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Uenoidae ----------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Unionidae ---------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

# Viviparidae -------------------------------------------------------------
ARACHNIDA = filter(Macroinvertebrate, Class == "ARACHNIDA")

ARACHNIDA = ARACHNIDA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ARACHNIDA = merge(ARACHNIDA, WaterQuality, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ARACHNIDA$Date = as.Date(ARACHNIDA$Date , format = "%m/%d/%y")

ARACHNIDA = as.data.frame(unclass(ARACHNIDA),
                          stringsAsFactors = TRUE)

ARACHNIDA$Tot <- as.numeric(ARACHNIDA$Tot)

summary(ARACHNIDA)



#Before#
Pre = ARACHNIDA %>% filter(Date <= '2005-11-24')
#After#
Post = ARACHNIDA %>% filter(Date > '2005-11-24')


Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")

#Lake#
Lake = filter(ARACHNIDA, Water.Class == "Lake")

Lake = na.omit(Lake)

summary(Lake)

Pre = Lake %>% filter(Date <= '2006-08-23')

Post = Lake %>% filter(Date > '2006-08-23')

Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is pH so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = Pre)

d0 <- Post %>% dplyr::select(Tot,Diss_Oxy)

DO.pred = predict(DO, newdata = d0) %>% exp()

chisq.test(Post$Tot, DO.pred)

cor.test(Post$Tot, DO.pred, use = "everything")


#Riverine#
Riverine = filter(ARACHNIDA, Water.Class == "Riverine")

Riverine = na.omit(Riverine)

summary(Riverine)

Pre =  Riverine %>% filter(Date <= '2006-04-04')

Post = Riverine %>% filter(Date > '2006-04-04')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "everything")


#Wetland#

Wetland = filter(ARACHNIDA, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-08-25')

Post = Wetland %>% filter(Date > '2005-08-25')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

pH = glm.nb(Tot ~ pH_SU, data = Pre)

d0 <- Post %>% dplyr::select(Tot,pH_SU)

pH.pred = predict(pH, newdata = d0) %>% exp()

chisq.test(Post$Tot, pH.pred)

cor.test(Post$Tot, pH.pred, use = "everything")

