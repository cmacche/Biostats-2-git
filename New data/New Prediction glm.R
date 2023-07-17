library(MASS)

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

summary(MWQ)

Pre = MWQ %>% filter(Date <= '2005-07-25')
#After#
Post = MWQ %>% filter(Date > '2005-07-25')

wq.glm1 = glm.nb(Tot ~ pH_SU, data = Pre)#10#
wq.glm2 = glm.nb(Tot ~ pH_SU + Sp_Cond, data = Pre)#13#
wq.glm3 = glm.nb(Tot ~ pH_SU + Sp_Cond + Temp.C, data = Pre)#3#
wq.glm4 = glm.nb(Tot ~ pH_SU + Sp_Cond + Diss_Oxy, data = Pre)#15#
wq.glm5 = glm.nb(Tot ~ pH_SU + Temp.C + Diss_Oxy, data = Pre)#8#
wq.glm6 = glm.nb(Tot ~ pH_SU + Temp.C, data = Pre)#4#
wq.glm7 = glm.nb(Tot ~ pH_SU + Diss_Oxy, data = Pre)#12#
wq.glm8 = glm.nb(Tot ~ pH_SU + Sp_Cond + Temp.C + Diss_Oxy, data = Pre)#7#
wq.glm9 = glm.nb(Tot ~ Sp_Cond, data = Pre)#11#
wq.glm10 = glm.nb(Tot ~ Sp_Cond + Temp.C, data = Pre) #2#
wq.glm11 = glm.nb(Tot ~ Sp_Cond + Diss_Oxy, data = Pre)#14#
wq.glm12 = glm.nb(Tot ~ Sp_Cond + Temp.C + Diss_Oxy, data = Pre)#6#
wq.glm13 = glm.nb(Tot ~ Temp.C, data = Pre) #1#
wq.glm14 = glm.nb(Tot ~ Temp.C + Diss_Oxy, data = Pre)#5#
wq.glm15 = glm.nb(Tot ~ Diss_Oxy, data = Pre)#9#

summary(wq.glm1)
summary(wq.glm2)
summary(wq.glm3)
summary(wq.glm4)
summary(wq.glm5)
summary(wq.glm6)
summary(wq.glm7)
summary(wq.glm8)
summary(wq.glm9)
summary(wq.glm10)
summary(wq.glm11)
summary(wq.glm12)
summary(wq.glm13)
summary(wq.glm14)
summary(wq.glm15)

best.mod = AIC(wq.glm1,wq.glm2,wq.glm3,wq.glm4,
    wq.glm5,wq.glm6,wq.glm7,
    wq.glm8,wq.glm9,wq.glm10,
    wq.glm11,wq.glm12,wq.glm13,
    wq.glm14,wq.glm15)

#top 5 model#
wq.glm13 = glm.nb(Tot ~ Temp.C, data = Pre)
wq.glm10 = glm.nb(Tot ~ Sp_Cond + Temp.C, data = Pre)
wq.glm3 = glm.nb(Tot ~ pH_SU + Sp_Cond + Temp.C, data = Pre)
wq.glm6 = glm.nb(Tot ~ pH_SU + Temp.C, data = Pre)
wq.glm14 = glm.nb(Tot ~ Temp.C + Diss_Oxy, data = Pre)
#most of these models have Temperature involved

summary(wq.glm13) #intercept and Temp are significant in the model#
summary(wq.glm10)#Specific conductivity is not significant#
summary(wq.glm3)#Specific conductivity and pH are not significant in this case#
summary(wq.glm6)#pH is not significant in this case#
summary(wq.glm14)#Dissolved oxygen is not significant in this case#

Temp1 = predict(object = wq.glm13)

chisq.test(Post$Tot, Temp1)

cor.test(Post$Tot, Temp1)

hi

hello

meep




