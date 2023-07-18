
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

summary(MWQ)

Pre = MWQ %>% filter(Date <= '2005-07-25')
#After#
Post = MWQ %>% filter(Date > '2005-07-25')
Post[Tot:Temp.C] <- lapply(Post[Tot:Temp.C], as.numeric)
Post [ nrow(Post) +  7, ] <- NA # adding this so I can run the correlation test#

Temperature = glm.nb(Tot ~ Temp.C, data = Pre)

d0 <- Post %>% dplyr::select(Tot:Temp.C)

Temp.pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(Post$Tot, Temp.pred)

cor.test(Post$Tot, Temp.pred, use = "eveything")


# dredge package ----------------------------------------------------------


wq.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail" )

wq = dredge(wq.glm) # this seems to be taking a long time to run
#going to try a different way#

Pre1 = subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm1 = glm.nb(Tot~ ., data = Pre1, na.action = "na.fail" )

wq1 = dredge(wq.glm1)
wq1
#from the above model Temp.C alone is the best but it may change if ,
#I use the other one#

MWQ1 = subset(MWQ, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
wq.glm1 = glm.nb(Tot~ ., data = MWQ1, na.action = "na.fail" )

wq1 = dredge(wq.glm1)
wq1



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


pH1 = predict(object = pH)

chisq.test(Post$Tot, pH1)

cor.test(Post$Tot, pH1, use = "eveything")



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


Temp1 = predict(object = Temperature)

chisq.test(Post$Tot, Temp1)#The issue with both tests is that,
#data need to be the same length and they aren't so I'm not sure what exactly
#To do with that thoughts#

cor.test(Post$Tot, Temp1, use = "eveything")


#Wetland#

Wetland = filter(MWQ, Water.Class %in% 
                   c("Freshwater Forested/Shrub Wetland", "Freshwater emergent wetland"))

Wetland = na.omit(Wetland)

summary(Wetland)

Pre =  Wetland %>% filter(Date <= '2005-07-26')

Post = Wetland %>% filter(Date > '2005-07-26')


Pre= subset(Pre, select = c("Tot", "pH_SU", "Sp_Cond","Temp.C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = Pre, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

Specific.Conductivity = glm.nb(Tot ~ Sp_Cond, data = Pre)


SC = predict(object = Specific.Conductivity)

chisq.test(Post$Tot, SC)

cor.test(Post$Tot, SC, use = "eveything")

