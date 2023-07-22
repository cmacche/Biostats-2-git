
install.packages("MuMIn")


# library -----------------------------------------------------------------

library(MASS)
library(MuMIn)
library(tidyverse)
  
# load data -------------------------------------------
df_wq = read_csv("water_quality.csv")
df_invert = read_csv("macroinvertebrate2.csv")


# format ------------------------------------------------------------------


df_invert = filter(df_invert, EcoRegion == "P") %>% 
  mutate(order = case_when(Order == "CO" ~ "COLEOPTERA",
                           Order %in% c("DI", "DIM") ~ "DIPTERA",
                           Order == "OD" ~ "ODONATA",
                           Order == "GA" ~ "COLEOPTERA",
                           Order == "ME" ~ "MEGALOPTERA",
                           Order == "HE" ~ "HEMIPTERA",
                           Order == "EP" ~ "EPHEMEROPTERA",
                           Order == "PL" ~ "PLECOPTERA",
                           Order == "TR" ~ "TRICHOPTERA",
                           Order == "OT" ~ "OTHER_TAXA",
                           TRUE ~ as.character(Order)),
         class = case_when(Class == "CR" ~ "CRUSTACEA",
                           Class == "GA" ~ "GASTROPODA",
                           Class == "OL" ~ "OLIGOCHAETA",
                           Class == "PE" ~ "PLECYPODA",
                           TRUE ~ as.character(Class))) %>% 
  filter(!(County %in% c("York, SC",
                         "(Danville), VA",
                         "Tallapoosa, AL",
                         "Laurens, SC",
                         "Newberry, SC")))



df_invert = df_invert %>% group_by(Date,
                               Location,
                               County,
                               Latitude, 
                               Longitude,
                               Waterbody) %>% 
  summarise(Tot = sum(Abundance))

df_invert = na.omit(df_invert)
df_invert = as.data.frame(unclass(df_invert), stringsAsFactors = TRUE)

df_wq = filter(df_wq, EcoRegion == "P") %>% 
  filter(!(Sp_Cond == 0)) %>% 
  filter(!(ph_SU == 0)) %>% 
  filter(!(Diss_Oxy == 0)) %>% 
  filter(!(Temp_C == 0))
df_wq = subset(df_wq, select = c( "Date", "County",
                                                "Waterbody","Water_Class", 
                                                "Latitude","Longitude",
                                                "Drainage", 
                                                "Temp_C","Sp_Cond", "ph_SU",
                                                "Diss_Oxy"))
df_wq = na.omit(df_wq)

df_wq = as.data.frame(unclass(df_wq),                    
                             stringsAsFactors = TRUE)


df_invertwq = merge(df_invert, df_wq, by = c("Date", "County",
                                        "Waterbody", 
                                        "Latitude","Longitude"))

df_invertwq$Date <- as.Date(df_invertwq$Date , format = "%m/%d/%y")


summary(df_invertwq) #this is to find the median so I can split the data as evenly as,
#possible to create a test model.

pre_invertwq = df_invertwq %>% filter(Date <= '2005-07-12')

post_invertwq = df_invertwq %>% filter(Date > '2005-07-12')

invertwq_glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail" )

invertwq_dredge = dredge(invertwq_glm) # this seems to be taking a long time to run
#going to try a different way#

pre_invertwq = subset(pre_invertwq, 
                      select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
invertwq_glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail" )

invertwq_dredge = dredge(invertwq_glm)



temp_glm_mod = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot, Temp_C)

temp_pred = predict(temp_glm_mod, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, temp_pred)

cor.test(post_invertwq$Tot, temp_pred, use = "everything")


# habitats ----------------------------------------------------------------

#Lake#

df_lake = filter(df_invertwq, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_lake = df_lake %>% filter(Date <= '2006-08-16')

post_lake = df_lake %>% filter(Date > '2006-08-16')

pre_lake= subset(pre_lake, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
lake_glm = glm.nb(Tot~ ., data = pre_lake, na.action = "na.fail") 

lake_dredge = dredge(lake_glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



ph_glm_mod = glm.nb(Tot ~ pH_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph_glm_mod, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")



#riverine#

df_riv = filter(df_invertwq, Water_Class == "Riverine")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_riv =  df_riv %>% filter(Date <= '2005-07-12')

post_riv = df_riv %>% filter(Date > '2005-07-12')


pre_invertwq= subset(pre_riv, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
riv_glm = glm.nb(Tot~ ., data = pre_riv, na.action = "na.fail") 

riv_dredge = dredge(r.glm)

temp_glm_mod = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot, Temp_C)

temp_pred = predict(temp_glm_mod, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, temp_pred)

cor.test(post_invertwq$Tot, temp_pred, use = "everything")


#wetland#

df_wet = filter(df_invertwq, Water_Class %in% 
                   c("Freshwater Forested/Shrub Wetland", 
                     "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_wet =  df_wet %>% filter(Date <= '2005-07-26')

post_wet = df_wet %>% filter(Date > '2005-07-26')


pre_wet= subset(pre_wet, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wet_glm = glm.nb(Tot~ ., data = pre_wet, na.action = "na.fail") 

wet_dredge = dredge(wet_glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

spco_glm = glm.nb(Tot ~ Sp_Cond, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Sp_Cond)

spco_pred = predict(spco_glm, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, spco_pred)

cor.test(post_invertwq$Tot, spco_pred, use = "everything")





#class#
# Arachnida ---------------------------------------------------------------

#df_arach#
df_arach = filter(macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)

pre_invertwq = df_invertwq %>% filter(Date <= '2005-07-12')

post_invertwq = df_invertwq %>% filter(Date > '2005-07-12')

invertwq_glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail" )

invertwq_dredge = dredge(invertwq_glm) # this seems to be taking a long time to run
#going to try a different way#

pre_invertwq = subset(pre_invertwq, 
                      select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
invertwq_glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail" )

invertwq_dredge = dredge(invertwq_glm)



temp_glm_mod = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot, Temp_C)

temp_pred = predict(temp_glm_mod, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, temp_pred)

cor.test(post_invertwq$Tot, temp_pred, use = "everything")


# habitats ----------------------------------------------------------------

#Lake#

df_lake = filter(df_invertwq, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_lake = df_lake %>% filter(Date <= '2006-08-16')

post_lake = df_lake %>% filter(Date > '2006-08-16')

pre_lake= subset(pre_lake, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
lake_glm = glm.nb(Tot~ ., data = pre_lake, na.action = "na.fail") 

lake_dredge = dredge(lake_glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



ph_glm_mod = glm.nb(Tot ~ pH_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph_glm_mod, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")



#riverine#

df_riv = filter(df_invertwq, Water_Class == "Riverine")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_riv =  df_riv %>% filter(Date <= '2005-07-12')

post_riv = df_riv %>% filter(Date > '2005-07-12')


pre_invertwq= subset(pre_riv, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
riv_glm = glm.nb(Tot~ ., data = pre_riv, na.action = "na.fail") 

riv_dredge = dredge(r.glm)

temp_glm_mod = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot, Temp_C)

temp_pred = predict(temp_glm_mod, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, temp_pred)

cor.test(post_invertwq$Tot, temp_pred, use = "everything")


#wetland#

df_wet = filter(df_invertwq, Water_Class %in% 
                  c("Freshwater Forested/Shrub Wetland", 
                    "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_wet =  df_wet %>% filter(Date <= '2005-07-26')

post_wet = df_wet %>% filter(Date > '2005-07-26')


pre_wet= subset(pre_wet, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wet_glm = glm.nb(Tot~ ., data = pre_wet, na.action = "na.fail") 

wet_dredge = dredge(wet_glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

spco_glm = glm.nb(Tot ~ Sp_Cond, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Sp_Cond)

spco_pred = predict(spco_glm, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, spco_pred)

cor.test(post_invertwq$Tot, spco_pred, use = "everything")



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-11')

post_invertwq = df_wet %>% filter(Date > '2005-08-11')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed ph as best

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# BIVALVIA ----------------------------------------------------------------

BIVALVIA = filter(Macroinvertebrate, Class == "BIVALVIA")

BIVALVIA = BIVALVIA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
BIVALVIA = merge(BIVALVIA, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


BIVALVIA$Date = as.Date(BIVALVIA$Date , format = "%m/%d/%y")

BIVALVIA = as.data.frame(unclass(BIVALVIA),
                          stringsAsFactors = TRUE)

BIVALVIA$Tot <- as.numeric(BIVALVIA$Tot)

summary(BIVALVIA)



#Before#
pre_invertwq = BIVALVIA %>% filter(Date <= '2004-06-27')
#After#
post_invertwq = BIVALVIA %>% filter(Date > '2004-06-27')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

WQ3 = glm.nb(Tot ~ Temp_C + Diss_Oxy + ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Temp_C,Diss_Oxy, ph_SU)

WQ3_pred = predict(WQ3, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, WQ3_pred)

cor.test(post_invertwq$Tot, WQ3_pred, use = "everything")

#df_lake#
df_lake = filter(BIVALVIA, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-16')

post_invertwq = df_lake %>% filter(Date > '2006-08-16')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(BIVALVIA, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-11-09')

post_invertwq = df_riv %>% filter(Date > '2006-11-09')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

WQ3 = glm.nb(Tot ~ Temp_C + Diss_Oxy + ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Temp_C, Diss_Oxy,ph_SU)

WQ3_pred = predict(WQ3, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, WQ3_pred)

cor.test(post_invertwq$Tot, WQ3_pred, use = "everything")


#df_wet#

df_wet = filter(BIVALVIA, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2004-04-26')

post_invertwq = df_wet %>% filter(Date > '2004-04-26')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")
# CLITELLATA --------------------------------------------------------------
CLITELLATA = filter(Macroinvertebrate, Class == "CLITELLATA")

CLITELLATA = CLITELLATA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
CLITELLATA = merge(CLITELLATA, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


CLITELLATA$Date = as.Date(CLITELLATA$Date , format = "%m/%d/%y")

CLITELLATA = as.data.frame(unclass(CLITELLATA),
                          stringsAsFactors = TRUE)

CLITELLATA$Tot <- as.numeric(CLITELLATA$Tot)

summary(CLITELLATA)



#Before#
pre_invertwq = CLITELLATA %>% filter(Date <= '2005-06-29')
#After#
post_invertwq = CLITELLATA %>% filter(Date > '2005-06-29')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

WQ2 = glm.nb(Tot ~ Temp_C + Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Temp_C,Diss_Oxy)

WQ2_pred = predict(WQ2, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, WQ2_pred)

cor.test(post_invertwq$Tot, WQ2_pred, use = "everything")

#df_lake#
df_lake = filter(CLITELLATA, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-16')

post_invertwq = df_lake %>% filter(Date > '2006-08-16')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(CLITELLATA, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2005-06-27')

post_invertwq = df_riv %>% filter(Date > '2005-06-27')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

WQ2 = glm.nb(Tot ~ Temp_C + Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Temp_C, Diss_Oxy)

WQ2_pred = predict(WQ2, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, WQ2_pred)

cor.test(post_invertwq$Tot, WQ2_pred, use = "everything")


#df_wet#

df_wet = filter(CLITELLATA, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-07-27')

post_invertwq = df_wet %>% filter(Date > '2005-07-27')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

SC = glm.nb(Tot ~ Sp_Cond, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Sp_Cond)

SC_pred = predict(SC, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, SC_pred)

cor.test(post_invertwq$Tot, SC_pred, use = "everything")
# ENOPLA  -----------------------------------------------------------------
ENOPLA = filter(Macroinvertebrate, Class == "ENOPLA")

ENOPLA = ENOPLA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
ENOPLA = merge(ENOPLA, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


ENOPLA$Date = as.Date(ENOPLA$Date , format = "%m/%d/%y")

ENOPLA = as.data.frame(unclass(ENOPLA),
                          stringsAsFactors = TRUE)

summary(ENOPLA)



#Before#
pre_invertwq = ENOPLA %>% filter(Date <= '2003-06-23')
#After#
post_invertwq = ENOPLA %>% filter(Date > '2003-06-23')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1,na.action = "na.fail" )
wq = dredge(wq.glm)

DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot, Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(ENOPLA, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2003-04-14')

post_invertwq = df_riv %>% filter(Date > '2003-04-14')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_wet#

df_wet = filter(ENOPLA, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# GASTROPODA --------------------------------------------------------------
GASTROPODA = filter(Macroinvertebrate, Class == "GASTROPODA")

GASTROPODA = GASTROPODA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
GASTROPODA = merge(GASTROPODA, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


GASTROPODA$Date = as.Date(GASTROPODA$Date , format = "%m/%d/%y")

GASTROPODA = as.data.frame(unclass(GASTROPODA),
                          stringsAsFactors = TRUE)

GASTROPODA$Tot <- as.numeric(GASTROPODA$Tot)

summary(GASTROPODA)



#Before#
pre_invertwq = GASTROPODA %>% filter(Date <= '2005-06-29')
#After#
post_invertwq = GASTROPODA %>% filter(Date > '2005-06-29')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

WQ3 = glm.nb(Tot ~ Temp_C + ph_SU + Sp_Cond, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Temp_C,ph_SU, Sp_Cond)

WQ3_pred = predict(WQ3, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, WQ3_pred)

cor.test(post_invertwq$Tot, WQ3_pred, use = "everything")


#df_riv#
df_riv = filter(GASTROPODA, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2005-07-25')

post_invertwq = df_riv %>% filter(Date > '2006-07-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

WQ3 = glm.nb(Tot ~ Temp_C + ph_SU + Sp_Cond, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Temp_C, ph_SU, Sp_Cond)

WQ3_pred = predict(WQ3, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, WQ3_pred)

cor.test(post_invertwq$Tot, WQ3_pred, use = "everything")


#df_wet#

df_wet = filter(GASTROPODA, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-05-05')

post_invertwq = df_wet %>% filter(Date > '2005-05-05')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

# INSECTA -----------------------------------------------------------------
INSECTA = filter(Macroinvertebrate, Class == "INSECTA")

INSECTA = INSECTA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
INSECTA = merge(INSECTA, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


INSECTA$Date = as.Date(INSECTA$Date , format = "%m/%d/%y")

INSECTA = as.data.frame(unclass(INSECTA),
                          stringsAsFactors = TRUE)

INSECTA$Tot <- as.numeric(INSECTA$Tot)

summary(INSECTA)



#Before#
pre_invertwq = INSECTA %>% filter(Date <= '2005-07-25')
#After#
post_invertwq = INSECTA %>% filter(Date > '2005-07-25')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

WQ3 = glm.nb(Tot ~ Temp_C + Diss_Oxy + Sp_Cond, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Temp_C, Diss_Oxy, Sp_Cond)

WQ3_pred = predict(WQ3, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, WQ3_pred)

cor.test(post_invertwq$Tot, WQ3_pred, use = "everything")

#df_lake#
df_lake = filter(INSECTA, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-16')

post_invertwq = df_lake %>% filter(Date > '2006-08-16')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



#df_riv#
df_riv = filter(INSECTA, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2005-07-13')

post_invertwq = df_riv %>% filter(Date > '2005-07-13')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

WQ3 = glm.nb(Tot ~ Temp_C + Diss_Oxy + Sp_Cond, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Temp_C, Diss_Oxy,Sp_Cond)

WQ3_pred = predict(WQ3, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, WQ3_pred)

cor.test(post_invertwq$Tot, WQ3_pred, use = "everything")


#df_wet#

df_wet = filter(INSECTA, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-07-26')

post_invertwq = df_wet %>% filter(Date > '2005-07-26')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#



# MAXILLOPODA -------------------------------------------------------------
MAXILLOPODA = filter(Macroinvertebrate, Class == "MAXILLOPODA")

MAXILLOPODA = MAXILLOPODA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
MAXILLOPODA = merge(MAXILLOPODA, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


MAXILLOPODA$Date = as.Date(MAXILLOPODA$Date , format = "%m/%d/%y")

MAXILLOPODA = as.data.frame(unclass(MAXILLOPODA),
                          stringsAsFactors = TRUE)

MAXILLOPODA$Tot <- as.numeric(MAXILLOPODA$Tot)

summary(MAXILLOPODA)



#Before#
pre_invertwq = MAXILLOPODA %>% filter(Date <= '2005-06-04')
#After#
post_invertwq = MAXILLOPODA %>% filter(Date > '2005-06-04')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

WQ2 = glm.nb(Tot ~ Temp_C + Diss_Oxy , data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Temp_C, Diss_Oxy)

WQ2_pred = predict(WQ2, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, WQ2_pred)

cor.test(post_invertwq$Tot, WQ2_pred, use = "everything")

#df_lake#
df_lake = filter(MAXILLOPODA, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-26')

post_invertwq = df_lake %>% filter(Date > '2006-08-26')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_riv#
df_riv = filter(MAXILLOPODA, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2005-05-25 ')

post_invertwq = df_riv %>% filter(Date > '2005-05-25 ')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

WQ3 = glm.nb(Tot ~ Temp_C + Diss_Oxy + Sp_Cond, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Temp_C, Diss_Oxy, Sp_Cond)

WQ3_pred = predict(WQ3, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, WQ3_pred)

cor.test(post_invertwq$Tot, WQ3_pred, use = "everything")


#df_wet#

df_wet = filter(MAXILLOPODA, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-07-27')

post_invertwq = df_wet %>% filter(Date > '2005-07-27')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

WQ2 = glm.nb(Tot ~ ph_SU + Diss_Oxy , data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU, Diss_Oxy)

WQ2_pred = predict(WQ2, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, WQ2_pred)

cor.test(post_invertwq$Tot, WQ2_pred, use = "everything")

# TURBELLARIA -------------------------------------------------------------

TURBELLARIA = filter(Macroinvertebrate, Class == "TURBELLARIA")

TURBELLARIA = TURBELLARIA %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
TURBELLARIA = merge(TURBELLARIA, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


TURBELLARIA$Date = as.Date(TURBELLARIA$Date , format = "%m/%d/%y")

TURBELLARIA = as.data.frame(unclass(TURBELLARIA),
                          stringsAsFactors = TRUE)

TURBELLARIA$Tot <- as.numeric(TURBELLARIA$Tot)

summary(TURBELLARIA)



#Before#
pre_invertwq = TURBELLARIA %>% filter(Date <= '2005-07-18')
#After#
post_invertwq = TURBELLARIA %>% filter(Date > '2005-07-18')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

WQ4 = glm.nb(Tot ~ Temp_C + ph_SU + Sp_Cond + Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot, ph_SU, Sp_Cond, Temp_C, Diss_Oxy)

WQ4_pred = predict(WQ4, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, WQ4_pred)

cor.test(post_invertwq$Tot, WQ4_pred, use = "everything")

#df_lake#
df_lake = filter(TURBELLARIA, Water_Class == "df_lake")


#df_riv#
df_riv = filter(TURBELLARIA, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2005-07-25')

post_invertwq = df_riv %>% filter(Date > '2005-07-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

WQ4 = glm.nb(Tot ~ Temp_C + ph_SU + Sp_Cond + Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot, ph_SU, Sp_Cond, Temp_C, Diss_Oxy)

WQ4_pred = predict(WQ4, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, WQ4_pred)

cor.test(post_invertwq$Tot, WQ4_pred, use = "everything")


#df_wet#

df_wet = filter(TURBELLARIA, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-04-20')

post_invertwq = df_wet %>% filter(Date > '2005-04-20')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")




#order#
# AMPHIPODA ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# ARCHITAENIOGLOSSA -------------------------------------------------------

df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")
# ARHYNCHOBDELLIDA --------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# BASOMMATOPHORA ----------------------------------------------------------

df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")
# BRANCHIOBDELLIDA --------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# COLEOPTERA --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# DECAPODA ----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# DIPTERA -----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# EPHEMEROPTERA -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# HAPLOTAXIDA -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# HEMIPTERA ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# HOPLONEMERTEA -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# ISOPODA -----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# LEPIDOPTERA -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# LUMBRICULIDA ------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# MEGALOPTERA -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# NEOTAENIOGLOSSA ---------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# NEUROPTERA --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# ODONATA -----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# OPISTHOPORA -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# OTHER_TAXA --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# PLECOPTERA --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# RHYNCHOBDELLIDA ---------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# TRICHOPTERA -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# TRICLADIDA --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# TROMBIDIFORMES ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# UNIONIDA ----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# VENEROIDA ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")


#Family#

# Aeshnidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Ameletidae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Ancylidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Asellidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Athericidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Baetidae ----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Baetiscidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Belostomatidae ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")


# Brachycentridae ---------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Branchiobdellidae -------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Caenidae ----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Calamoceratidae ---------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Calopterygidae ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Cambaridae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Capniidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Ceratopogonidae ---------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Chaoboridae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Chironomidae ------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Chloroperlidae ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Coenagrionidae ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Corbiculidae ------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Cordelegastridae  -------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Cordulegastridae --------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Corduliidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Corixidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Corydalidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Crambidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Crangonyctidae ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Culicidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Dipseudopsidae ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Dixidae -----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")


# Dryopidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Dugesiidae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Dytiscidae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Elmidae -----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Empididae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Enchytraeidae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Ephemerellidae ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Ephemeridae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Erpobdellidae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Gammaridae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Gerridae ----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Glossiphoniidae ---------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Glossosomatidae ---------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Gomphidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Gyrinidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Haliplidae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Hyalellidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Hydrachnidae ------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Hydrobiidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Hydrochidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Hydrometridae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Hydrophilidae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Hydropsychidae ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Hydroptilidae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Isonychiidae ------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Lepidostomatidae --------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Leptoceridae ------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Leptohyphidae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Leptophlebiidae ---------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Leuctridae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Libellulidae ------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Limnephilidae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Limoniidae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Lumbriculidae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Lymnaeidae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Metretopodidae ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Molannidae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Naididae ----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Nemouridae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Neoephemeridae ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Nepidae -----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Noteridae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Nymphalidae/Limoniidae --------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Odontoceridae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Palaemonidae ------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Pediciidae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Peltoperlidae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Perlidae ----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Perlodidae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Philopotamidae ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Phryganeidae ------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Physidae ----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Pisidiidae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Planariidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Planorbidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Pleuroceridae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Polycentropodidae -------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Polymitarcyidae ---------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Potamanthidae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Psephenidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Psychodidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Psychomyiidae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Pteronarcyidae ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Ptilodactylidae ---------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Ptilodactylidae ---------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Pyralidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Rhyacophilidae ----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Scirtidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Sialidae ----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Simuliidae --------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Siphlonuridae -----------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Sisyridae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Tabanidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Taeniopterygidae --------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Tanyderidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Tetrastemmatidae --------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Tipulidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Tubificidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Uenoidae ----------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Unionidae ---------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

# Viviparidae -------------------------------------------------------------
df_arach = filter(Macroinvertebrate, Class == "df_arach")

df_arach = df_arach %>% group_by(Date, Location, County, Waterbody,Latitude,
                                   Longitude) %>% 
  summarise(Tot = sum(Abundance))
df_arach = merge(df_arach, df_wq, by = c("Date", "County",
                                                  "Waterbody","Latitude", "Longitude" ))


df_arach$Date = as.Date(df_arach$Date , format = "%m/%d/%y")

df_arach = as.data.frame(unclass(df_arach),
                          stringsAsFactors = TRUE)

df_arach$Tot <- as.numeric(df_arach$Tot)

summary(df_arach)



#Before#
pre_invertwq = df_arach %>% filter(Date <= '2005-11-24')
#After#
post_invertwq = df_arach %>% filter(Date > '2005-11-24')


pre_invertwq1 = subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
wq.glm = glm.nb(Tot~ ., data = pre_invertwq1, na.action = "na.fail" )
wq = dredge(wq.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")

#df_lake#
df_lake = filter(df_arach, Water_Class == "df_lake")

df_lake = na.omit(df_lake)

summary(df_lake)

pre_invertwq = df_lake %>% filter(Date <= '2006-08-23')

post_invertwq = df_lake %>% filter(Date > '2006-08-23')

pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
l.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

l = dredge(l.glm)#this is showing none of the waterquality would work but the
#next best is ph so will try that#



DO = glm.nb(Tot ~ Diss_Oxy, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,Diss_Oxy)

DO_pred = predict(DO, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, DO_pred)

cor.test(post_invertwq$Tot, DO_pred, use = "everything")


#df_riv#
df_riv = filter(df_arach, Water_Class == "df_riv")

df_riv = na.omit(df_riv)

summary(df_riv)

pre_invertwq =  df_riv %>% filter(Date <= '2006-04-04')

post_invertwq = df_riv %>% filter(Date > '2006-04-04')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
r.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

r = dredge(r.glm)

Temperature = glm.nb(Tot ~ Temp_C, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot:Temp_C)

Temp_pred = predict(Temperature, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, Temp_pred)

cor.test(post_invertwq$Tot, Temp_pred, use = "everything")


#df_wet#

df_wet = filter(df_arach, Water_Class %in% 
                   c("Freshwater Forested/Shrub df_wet", "Freshwater emergent wetland"))

df_wet = na.omit(df_wet)

summary(df_wet)

pre_invertwq =  df_wet %>% filter(Date <= '2005-08-25')

post_invertwq = df_wet %>% filter(Date > '2005-08-25')


pre_invertwq= subset(pre_invertwq, select = c("Tot", "ph_SU", "Sp_Cond","Temp_C","Diss_Oxy"))
w.glm = glm.nb(Tot~ ., data = pre_invertwq, na.action = "na.fail") 

w = dredge(w.glm) #showed none of the water qualtiy would be best but second best
# specfic conductivity#

ph = glm.nb(Tot ~ ph_SU, data = pre_invertwq)

d0 <- post_invertwq %>% dplyr::select(Tot,ph_SU)

ph_pred = predict(ph, newdata = d0) %>% exp()

chisq.test(post_invertwq$Tot, ph_pred)

cor.test(post_invertwq$Tot, ph_pred, use = "everything")

