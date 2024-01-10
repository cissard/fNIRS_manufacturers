library(dplyr)
library(stringr)
library(ggplot2)

#load the data
setwd("C:/Users/Cecile/Desktop/Attrition-Rate-in-Infant-fNIRS-Research-main/")
data_attrition <- read.csv("attrition_data_final.csv", na.strings=c("","Not reported ","Not reported", "Did not report attrition","NA"))
legend <- read.csv("attrition_legend.csv")
View(data_attrition)

#clean the data

#specify different custom-built devices (manually checked in the papers)
data_attrition[data_attrition$number == c(13,32),]$manufacturer <- "UCL"
data_attrition[data_attrition$number == 30,]$manufacturer <- "TechEn"

data_attrition <- data_attrition %>%
  mutate(version = str_trim(version)) %>%  #clean spaces around device names.
  mutate(version = replace(version, grepl("NIRScout",version), "NIRScout")) %>% #Unify all variants of the same device
  mutate(version = replace(version, grepl("ETG 4000",version), "ETG 4000")) %>%
  mutate(version = replace(version, grepl("ETG 100",version), "ETG 100")) %>%
  mutate(version = replace(version, grepl("NIRO-200",version), "NIRO 200")) %>%
  mutate(version = replace(version, grepl("OxiplexTS",manufacturer), "OxiplexTS")) %>%
  mutate(version = replace(version, grepl("NTS",manufacturer), "NTS")) %>%
  mutate(version = replace(version, grepl("UCL",manufacturer), "UCL")) %>%
  mutate(version = replace(version, grepl("Somanetics",manufacturer), "Somanetics")) %>%
  mutate(manufacturer = str_trim(manufacturer)) %>% #clean spaces around manufacturer names.
  mutate(manufacturer = replace(manufacturer, manufacturer == "Helinski University of Technology", "Helsinki University of Technology")) %>% #Unify all variants of the same manufaturer
  mutate(manufacturer = replace(manufacturer, grepl("Hita",manufacturer), "Hitachi")) %>% 
  mutate(manufacturer = replace(manufacturer, grepl("TechEn",manufacturer), "TechEn")) %>%
  mutate(manufacturer = replace(manufacturer, grepl("Schimadzu",manufacturer), "Shimadzu")) %>%
  mutate(manufacturer = replace(manufacturer, grepl("OxiplexTS",manufacturer)|grepl("Imagent",manufacturer), "ISS")) %>%
  mutate(manufacturer = replace(manufacturer, grepl("NTS",manufacturer)|grepl("UCL",manufacturer), "Gowerlabs Ltd.")) %>%
  mutate(manufacturer = replace(manufacturer, grepl("Somanetics",manufacturer), "Medtronic")) 

#check that there are no more redundancies in manufacturer names
sort(unique(data_attrition$manufacturer))
sort(unique(data_attrition$version))

#save the clean data
write.csv(data_attrition, file = "attrition_data_final.csv", row.names = FALSE)

#take only the subset of manufacturers that we are interested in
data_attrition <- subset(data_attrition, manufacturer == "NIRx" | manufacturer == "Gowerlabs Ltd." | manufacturer == "Hitachi" | manufacturer == "Shimadzu")
#check that we have what we need
unique(data_attrition$manufacturer)


#### Effect of the number of channels on attrition rate

#TAR
scatter_TAR_ch <- ggplot(data=data_attrition, aes(x=channels, y=attrition))+
  geom_point()+
  ylim(0,1)+
  geom_smooth(method = "lm", se = T)+
  theme_classic()+
  labs(x = "Number of channels", y = "Attrition rate", title = "Total attrition rate (due to baby and signal quality) as a function of the number of channels")
scatter_TAR_ch

#regression total attrition rate ~ nb of channels
TAR.ch.lm <- lm(attrition ~ channels, data = data_attrition)

summary(TAR.ch.lm)

#SAR
scatter_SAR_ch <- ggplot(data=data_attrition, aes(x=channels, y=attritionSig))+
  geom_point()+
  ylim(0,1)+
  geom_smooth(method = "lm", se = T)+
  theme_classic()+
  labs(x = "Number of channels", y = "Attrition rate", title = "Signal attrition rate as a function of the numberof channels")
scatter_SAR_ch

#regression signal attrition rate ~ nb of channels
SAR.ch.lm <- lm(attritionSig ~ channels, data = data_attrition)

summary(SAR.ch.lm)

#Infant Attrition Rate
scatter_IAR_ch <- ggplot(data=data_attrition, aes(x=channels, y=attritionBaby))+
  geom_point()+
  ylim(0,1)+
  geom_smooth(method = "lm", se = T)+
  theme_classic()+
  labs(x = "Number of channels", y = "Attrition rate", title = "Infant attrition rate as a function of the number of channels")
scatter_IAR_ch

#regression infant attrition rate ~ nb of channels
IAR.ch.lm <- lm(attritionBaby ~ channels, data = data_attrition)

summary(IAR.ch.lm)

#all types of attrition rates
colors <- c("Infant" = "red", "Signal" = "blue", "Total"= "green")

baby_total <- ggplot(data=data_attrition, aes(x=channels))+
  geom_point(aes(y=attritionBaby, color = "red"))+
  geom_smooth(aes(x=channels, y=attritionBaby, color = "Infant"), method = "lm", se = T, fill="red")+
  geom_point(aes(y=attritionSig), color = "blue")+
  geom_smooth(aes(x=channels, y=attritionSig, color = "Signal"), method = "lm", se = T, fill="blue")+
  geom_point(aes(y=attrition), color = "green")+
  geom_smooth( aes(x=channels, y=attrition, color = "Total"), method = "lm", se = T, fill="green")+
  ylim(0,1)+
  theme_classic()+
  labs(x = "Number of channels", y = "Attrition rate", title = "Infant and total attrition rate as a function of the number of channels")+
  scale_colour_manual(name = "Attrition rate", values = colors)+
  scale_fill_manual(values=colors)

### Manufacturers

#number of publications for each manufacturer
count <- table(data_attrition$manufacturer,data_attrition$version)

histogram <- ggplot(data=data_attrition, aes(x=manufacturer, fill = interaction(version,manufacturer)))+
  geom_bar(position="stack")+
  theme_classic()+
  labs(x = "Manufacturer", y = "Number of Publications", fill = "Device", title = "Number of Publications for each fNIRS Manufacturer and Device")


#SAR as a function of publication year for each manufacturer
scatter_SAR_year <- ggplot(data=data_attrition[!is.na(data_attrition$manufacturer),], aes(x=year, y=attritionSig, colour = manufacturer))+
  geom_point()+
  ylim(0,1)+
  geom_smooth(method = "lm", se = T)+
  theme_classic()+
  labs(x = "Publication year", y = "Attrition rate", colour = "Manufacturer", title = "Signal attrition rate as a function of the publication year, for each manufacturer")
scatter_SAR_year

#test the effect of manufacturer on SAR
SAR.year.aov <- aov(attrition ~ manufacturer * year, data=data_attrition)
summary(SAR.year.aov)

#total attrition rate as a function of the manufacturer

violin_TAR <- ggplot(data=data_attrition, aes(x=manufacturer, y=attrition, colour = manufacturer))+
  theme_classic()+
  ylim(0,1)+
  geom_violin()+
  geom_point()+ 
  stat_summary(fun = "mean", geom = "crossbar", width = 0.7)+
  labs(x="Manufacturer", y="Attrition rate", colour = "Manufacturer", title = "Total attrition rate (due to baby and signal quality) for each manufacturer")
violin_TAR

#test the effect of manufacturer on TAR
TAR.aov <- aov(attrition ~ manufacturer, data=data_attrition)
summary(TAR.aov)

#Total attrition rate as a function of the manufacturer and nb of channels

scatter_TAR <- ggplot(data=data_attrition, aes(x=channels, y=attrition, colour = manufacturer))+
  geom_point()+
  ylim(0,1)+
  geom_smooth(method = "lm", se = T)+
  theme_classic()+
  labs(x = "Number of channels", y="Attrition rate", colour = "Manufacturer", title = "Total attrition rate (due to baby and signal quality) as a function of the # of channels, for each manufacturer")
scatter_TAR


#test the interaction between manufacturer and  nb of channels on TAR
TAR.ch.aov <- aov(attrition ~ manufacturer * channels, data=data_attrition)
summary(TAR.ch.aov)


#attrition due to signal quality
violin_SAR <- ggplot(data=data_attrition, aes(x=manufacturer, y=attritionSig, colour = manufacturer))+
  theme_classic()+
  ylim(0,1)+
  geom_violin()+
  geom_point()+
  stat_summary(fun = "mean", geom = "crossbar", width = 0.7)+
  labs(x="Manufacturer", y="Attrition rate", colour = "Manufacturer", title = "Attrition rate due to signal quality for each manufacturer")
violin_SAR

#test the effect of manufacturer on SAR
SAR.aov <- aov(attritionSig ~ manufacturer, data=data_attrition)
summary(SAR.aov)
#Let's see which group differ
TukeyHSD(SAR.aov)

#plot signal attrition rate as a function of the manufacturer and nb of channels
scatter_SAR <- ggplot(data=data_attrition[!is.na(data_attrition$attritionSig),], aes(x=channels, y=attritionSig, colour = manufacturer))+
  geom_point()+
  ylim(0,1)+
  geom_smooth(method = "lm", se = T)+
  theme_classic()+
  labs(x = "Number of channels", y="Attrition rate", title = "Attrition rate due to signal quality as a function of the # of channels, for each manufacturer")
scatter_SAR

#test the interaction between manufacturer and  nb of channels on SAR
SAR.ch.aov <- aov(attritionSig ~ manufacturer * channels, data=data_attrition)
summary(SAR.ch.aov)

#attrition due to infant behavior
violin_IAR <- ggplot(data=data_attrition, aes(x=manufacturer, y=attritionBaby, colour = manufacturer))+
  theme_classic()+
  ylim(0,1)+
  geom_violin()+
  geom_point()+
  stat_summary(fun = "mean", geom = "crossbar", width = 0.7)+
  labs(x="Manufacturer", y="Attrition rate", colour = "Manufacturer", title = "Attrition rate due to infant behavior for each manufacturer")
violin_IAR

#test the effect of manufacturer on IAR
IAR.aov <- aov(attritionBaby ~ manufacturer, data=data_attrition)
summary(IAR.aov)
#Let's see which group differ
TukeyHSD(IAR.aov)

#plot attrition rate as a function of the manufacturer and nb of channels
scatter_IAR <- ggplot(data=data_attrition[!is.na(data_attrition$attritionBaby),], aes(x=channels, y=attritionBaby, colour = manufacturer))+
  geom_point()+
  ylim(0,1)+
  geom_smooth(method = "lm", se = T)+
  theme_classic()+
  labs(x = "Number of channels", y="Attrition rate", title = "Attrition rate due to infant behavior as a function of the # of channels, for each manufacturer")
scatter_IAR
