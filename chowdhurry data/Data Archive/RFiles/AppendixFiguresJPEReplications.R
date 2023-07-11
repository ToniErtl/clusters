# Loading library package

library(cluster)
library(foreign)
library(readr)
library(tidyverse)
library(haven)
library(readxl)
library(ggplot2)
library(ggrepel)
options(scipen = 999) 

########################################
#  Figure A-1 & A-2: Cluster Analyses  #
########################################

wd<-"~/Data Archive/ConstructedData"
setwd(wd)
data <- read.dta("./children_familyAggregate_stat12.dta")

# Data preparation
id <- data.frame(data$slno, data$mid)
data <- data.frame(data$patient_choicesOffspringMean, data$patient_choices_father, data$patient_choices_mother, data$binswangerOffspringMean, data$binswanger_father, data$binswanger_mother, data$spitefulOffspringMean, data$spiteful_father, data$spiteful_mother, data$altruisticOffspringMean, data$altruistic_father, data$altruistic_mother, data$egalitarianOffspringMean, data$egalitarian_father, data$egalitarian_mother, data$selfishOffspringMean, data$selfish_father, data$selfish_mother)

id <- id[complete.cases(data), ]
data <- data[complete.cases(data), ] # only pamx works with NAs
data <- scale(data)

require(cluster)

# Figure A.1 and Figure A.2:
  
require(fpc)

pamx <- pamk(data, krange=2:30,criterion="ch", usepam = TRUE,critout = T)

png("~/Data Archive/FiguresAppend/figureA1.png")

plot(pamx$crit, ylab = "Calinski-Harabasz-Index", xlab = "Number of clusters")
# 2 clusters are optimal according to Calinski-Harabasz - only without NAs, otherwise not possible

dev.off()

pamx <- pamk(data, krange=2:30,criterion="asw", usepam = TRUE,critout = T)

png("~/Data Archive/FiguresAppend/figureA2.png")

plot(pamx$crit, ylab = "Average Silhouette Width", xlab = "Number of clusters")
# 2 clusters are optimal according to average silhouette width - without NAs. Otherwise, 4?

dev.off()

######################################################################
#  Figures A-3 & A-4: Risk Taking/Prosociality <-> IQ - GPS & WB data#
######################################################################

wd<-"~/Data Archive/ConstructedData"
setwd(wd)

###########################################################################################
# Data set preparation - IQ - GPS_Data
# Data source: Global Preference Survey; Falk et al., (2018).
# Classification follows https://datahelpdesk.worldbank.org/knowledgebase/articles/906519
###########################################################################################

GPS<-read_dta("./individual_new.dta")

GPS_collapsed<-GPS%>%
  group_by(country) %>%
  summarise(IQ=mean(subj_math_skills, na.rm=TRUE), Patience=mean(patience, na.rm=TRUE), Risk=mean(risktaking, na.rm=TRUE), Altruism=mean(altruism, na.rm=TRUE), Posrecip=mean(posrecip, na.rm=TRUE), Trust=mean(trust, na.rm=TRUE))

GPS_temp<-unique(GPS[,c(1,2)])
GPS_collapsed<-merge(GPS_collapsed, GPS_temp)

WB_LIC<-c('Afghanistan', 'Guinea-Bissau',	'Sierra Leone', 'Burkina Faso',	'Haiti', 'Somalia', 'Burundi',	'South Korea', 'South Sudan', 'Central African Republic', 'Liberia',	'Sudan', 'Chad', 'Madagascar', 'Syria', 'Congo', 'Malawi', 'Tajikistan', 'Eritrea',	'Mali', 'Togo', 'Ethiopia',	'Mozambique',	'Uganda', 'Gambia', 'Niger', 'Yemen', 'Guinea',	'Rwanda')
WB_LMIC<-c('Angola', 'Honduras',	'Papua New Guinea', 'Algeria', 'India',	'Philippines', 'Bangladesh', 'Kenya', 'São Tomé and Principe', 'Benin', 'Kiribati', 'Senegal', 'Bhutan', 'Kyrgyz Republic', 'Solomon Islands', 'Bolivia', 'Lao PDR', 'Sri Lanka', 'Cabo Verde', 'Lesotho', 'Tanzania', 'Cambodia', 'Mauritania', 'Timor-Leste', 'Cameroon', 'Micronesia', 'Tunisia', 'Comoros', 'Moldova', 'Ukraine', 'Congo', 'Mongolia', 'Uzbekistan', 'Morocco', 'Vanuatu', 'Djibouti', 'Myanmar', 'Vietnam', 'Egypt', 'Nepal', 'West Bank and Gaza', 'El Salvador', 'Nicaragua', 'Zambia', 'Eswatini', 'Nigeria', 'Zimbabwe', 'Ghana', 'Pakistan')
WB_UMIC<-c('Albania', 'Fiji', 'Montenegro', 'American Samoa', 'Gabon', 'Namibia', 'Argentina', 'Georgia', 'North Macedonia', 'Armenia', 'Grenada', 'Paraguay', 'Azerbaijan', 'Guatemala', 'Peru', 'Belarus', 'Guyana', 'Russia', 'Belize', 'Indonesia', 'Samoa', 'Bosnia Herzegovina', 'Iran', 'Serbia', 'Botswana', 'Iraq', 'South Africa', 'Brazil', 'Jamaica', 'St. Lucia', 'Bulgaria', 'Jordan', 'St. Vincent and the Grenadines', 'China', 'Kazakhstan', 'Suriname', 'Colombia', 'Kosovo', 'Thailand', 'Costa Rica', 'Lebanon', 'Tonga', 'Cuba', 'Libya', 'Turkey', 'Dominica', 'Malaysia', 'Turkmenistan', 'Dominican Republic', 'Maldives', 'Tuvalu', 'Equatorial Guinea', 'Marshall Islands', 'Venezuela', 'Ecuador', 'Mexico')
WB_HIC<-c('Andorra', 'Greece', 'Palau', 'Antigua and Barbuda', 'Greenland', 'Panama', 'Aruba', 'Guam', 'Poland', 'Australia', 'Hong Kong', 'Portugal', 'Austria', 'Hungary', 'Puerto Rico', 'Bahamas', 'Iceland', 'Qatar', 'Bahrain', 'Ireland', 'Romania', 'Barbados', 'Isle of Man', 'San Marino', 'Belgium', 'Israel', 'Saudi Arabia', 'Bermuda', 'Italy', 'Seychelles', 'British Virgin Islands', 'Japan', 	'Singapore', 'Brunei', 'South Korea', 'Sint Maarten', 'Canada', 'Kuwait', 'Slovak Republic', 'Cayman Islands', 'Latvia', 'Slovenia', 'Channel Islands', 'Liechtenstein', 'Spain', 'Chile', 'Lithuania', 'St. Kitts and Nevis', 'Croatia', 'Luxembourg', 'St. Martin', 'Curaçao', 'Macao', 'Sweden', 'Cyprus', 'Malta', 'Switzerland', 'Czech Republic', 'Mauritius', 'Taiwan', 'Denmark', 'Monaco', 'Trinidad and Tobago', 'Estonia', 'Nauru', 'Turks and Caicos Islands', 'Faroe Islands', 'Netherlands', 'United Arab Emirates', 'Finland', 'New Caledonia', 'United Kingdom', 'France', 'New Zealand', 'United States', 'French Polynesia', 'Northern Mariana Islands', 'Uruguay', 'Germany', 'Norway', 'Virgin Islands', 'Gibraltar', 'Oman')

GPS_collapsed$WB<-ifelse(GPS_collapsed$country %in% WB_HIC, "1", ifelse(GPS_collapsed$country %in% WB_UMIC, "2", ifelse(GPS_collapsed$country %in% WB_LMIC, "2", ifelse(GPS_collapsed$country %in% WB_LIC, "3", NA))))

############################################
# Figure A-3: Risk Taking <-> IQ (GPS data)#
############################################

IQRisk<-GPS_collapsed %>%
  ggplot(aes(x=IQ,y=Risk, 
             xmin=3, xmax=7)) +
  geom_point(size=6, 
             aes(shape=WB, colour=WB)) +
  geom_smooth(method=lm, se=FALSE, size=2, 
              aes(colour=WB), 
              show.legend=FALSE)+
  geom_text_repel(aes(label = isocode, colour=WB), 
                  size = rel(4), 
                  point.size = 2,
                  show.legend=FALSE)+
  theme_minimal()+
  theme(axis.text=element_text(size=rel(2.5)),
        axis.title=element_text(size=rel(3),face="bold"), 
        plot.title = element_text(size = rel(4), hjust=0.5), 
        panel.border = element_blank(),
        axis.line.x = element_line(linetype = "solid", colour = "black"),
        axis.line.y = element_line(linetype = "solid", colour = "black"), 
        axis.ticks = element_line(size=1),
        axis.ticks.length=unit(0.2, "cm"),
        legend.text = element_text(size=rel(2.5)), 
        legend.position = "bottom")+
  labs(y = "Risk taking", 
       x="Subjective Math Skills (IQ)", 
       title="Correlation between Risk taking and Subjective Math Skills")+
  scale_colour_discrete(name="",
                        breaks=c("1", "2", "3"),
                         labels=c("High-Income","Middle-Income",  "Low-Income"))+
  scale_shape_discrete(name="",
                        breaks=c("1", "2", "3"),
                        labels=c("High-Income","Middle-Income",  "Low-Income"))+
  ggplot2::annotate("text", 
                    x = 3.4, y = c(1.0,0.9, 0.8),  
                    size=9, 
                    label = c("\U25CF: Corr:  0.396 (p-value: 0.037)", "\U25B2: Corr: 0.174 (p-value: 0.264)", "\U25A0: Corr: -0.389 (p-value: 0.518)"),  
                    colour=c("#F8766D", "#00BA38", "#619CFF"))
                    ggsave("~/Dropbox/Data Archive Shared/FiguresAppend/figureA3.png", width = 20, height = 10 )

temp<-filter(GPS_collapsed,WB=="1")
cor(temp$IQ, temp$Risk, use="complete.obs")
cor.test(temp$IQ, temp$Risk, use="complete.obs")

temp<-filter(GPS_collapsed,WB=="2")
cor(temp$IQ, temp$Risk, use="complete.obs")
cor.test(temp$IQ, temp$Risk, use="complete.obs")

temp<-filter(GPS_collapsed,WB=="3")
cor(temp$IQ, temp$Risk, use="complete.obs")
cor.test(temp$IQ, temp$Risk, use="complete.obs")

#############################################
# Figure A-4: Prosociality <-> IQ (GPS data)#
#############################################

GPS_collapsed$Prosociality<-rowMeans(GPS_collapsed[,c(5:7)])

IQProsociality<-GPS_collapsed %>%
  ggplot(aes(x=IQ,y=Prosociality, group=WB, colour=WB, xmin=3, xmax=7, ymin=-1, ymax=1)) +
  geom_point(size=6, 
             aes(shape=WB, colour=WB)) +
  geom_smooth(method=lm, se=FALSE, size=2, 
              aes(colour=WB), 
              show.legend=FALSE)+
  geom_text_repel(aes(label = isocode, colour=WB), 
                  size = rel(4), 
                  point.size = 2,
                  show.legend=FALSE)+
  theme_minimal()+
  theme(axis.text=element_text(size=rel(2.5)),
        axis.title=element_text(size=rel(3),face="bold"), 
        plot.title = element_text(size = rel(4), hjust=0.5), 
        panel.border = element_blank(),
        axis.line.x = element_line(linetype = "solid", colour = "black"),
        axis.line.y = element_line(linetype = "solid", colour = "black"), 
        axis.ticks = element_line(size=1),
        axis.ticks.length=unit(0.2, "cm"),
        legend.text = element_text(size=rel(2.5)), 
        legend.position = "bottom")+
  labs(y = "Prosociality", x="Subjective Math Skills (IQ)", title="Correlation between Prosociality and Subjective Math Skills")+
  scale_colour_discrete(name="",
                        breaks=c("1", "2", "3"),
                         labels=c("High-Income","Middle-Income",  "Low-Income"))+
  scale_shape_discrete(name="",
                        breaks=c("1", "2", "3"),
                        labels=c("High-Income","Middle-Income",  "Low-Income"))+
  ggplot2::annotate("text", 
                    x = 3.6, y = c(0.9, 0.8, 0.7),  
                    size=9, 
                    label = c("\U25CF: Corr: 0.665 (p-value: <0.001)", "\U25B2: Corr: 0.123 (p-value: 0.434)", "\U25A0: Corr: -0.446 (p-value: 0.452)"),  
                    colour=c("#F8766D", "#00BA38", "#619CFF"))
					ggsave("~/Dropbox/Data Archive Shared/FiguresAppend/figureA4.png", width = 20, height = 10 )

temp<-filter(GPS_collapsed,WB=="1")
cor(temp$IQ, temp$Prosociality, use="complete.obs")
cor.test(temp$IQ, temp$Prosociality, use="complete.obs")

temp<-filter(GPS_collapsed,WB=="2")
cor(temp$IQ, temp$Prosociality, use="complete.obs")
cor.test(temp$IQ, temp$Prosociality, use="complete.obs")

temp<-filter(GPS_collapsed,WB=="3")
cor(temp$IQ, temp$Prosociality, use="complete.obs")
cor.test(temp$IQ, temp$Prosociality, use="complete.obs")

#######################################################################
#  Figures A-5 & A-6: Risk Taking/Prosociality <-> MSY - GPS & UN data#
#######################################################################

#####################################################################
# Data set preparation - IQ - GPS_Data                              #
# Data source: Global Preference Survey; Falk et al., 2018).        #
# UN-Data source: MSY - http://hdr.undp.org/en/indicators/103006#   #
#####################################################################

GPS<-read_dta("./individual_new.dta")

GPS_collapsed<-GPS%>%
  group_by(country) %>%
  summarise(IQ=mean(subj_math_skills, na.rm=TRUE), Patience=mean(patience, na.rm=TRUE), Risk=mean(risktaking, na.rm=TRUE), Altruism=mean(altruism, na.rm=TRUE), Posrecip=mean(posrecip, na.rm=TRUE), Trust=mean(trust, na.rm=TRUE))

GPS_temp<-unique(GPS[,c(1,2)])
GPS_collapsed<-merge(GPS_collapsed, GPS_temp)

WB_LIC<-c('Afghanistan', 'Guinea-Bissau',	'Sierra Leone', 'Burkina Faso',	'Haiti', 'Somalia', 'Burundi',	'South Korea', 'South Sudan', 'Central African Republic', 'Liberia',	'Sudan', 'Chad', 'Madagascar', 'Syria', 'Congo', 'Malawi', 'Tajikistan', 'Eritrea',	'Mali', 'Togo', 'Ethiopia',	'Mozambique',	'Uganda', 'Gambia', 'Niger', 'Yemen', 'Guinea',	'Rwanda')
WB_LMIC<-c('Angola', 'Honduras',	'Papua New Guinea', 'Algeria', 'India',	'Philippines', 'Bangladesh', 'Kenya', 'São Tomé and Principe', 'Benin', 'Kiribati', 'Senegal', 'Bhutan', 'Kyrgyz Republic', 'Solomon Islands', 'Bolivia', 'Lao PDR', 'Sri Lanka', 'Cabo Verde', 'Lesotho', 'Tanzania', 'Cambodia', 'Mauritania', 'Timor-Leste', 'Cameroon', 'Micronesia', 'Tunisia', 'Comoros', 'Moldova', 'Ukraine', 'Congo', 'Mongolia', 'Uzbekistan', 'Morocco', 'Vanuatu', 'Djibouti', 'Myanmar', 'Vietnam', 'Egypt', 'Nepal', 'West Bank and Gaza', 'El Salvador', 'Nicaragua', 'Zambia', 'Eswatini', 'Nigeria', 'Zimbabwe', 'Ghana', 'Pakistan')
WB_UMIC<-c('Albania', 'Fiji', 'Montenegro', 'American Samoa', 'Gabon', 'Namibia', 'Argentina', 'Georgia', 'North Macedonia', 'Armenia', 'Grenada', 'Paraguay', 'Azerbaijan', 'Guatemala', 'Peru', 'Belarus', 'Guyana', 'Russia', 'Belize', 'Indonesia', 'Samoa', 'Bosnia Herzegovina', 'Iran', 'Serbia', 'Botswana', 'Iraq', 'South Africa', 'Brazil', 'Jamaica', 'St. Lucia', 'Bulgaria', 'Jordan', 'St. Vincent and the Grenadines', 'China', 'Kazakhstan', 'Suriname', 'Colombia', 'Kosovo', 'Thailand', 'Costa Rica', 'Lebanon', 'Tonga', 'Cuba', 'Libya', 'Turkey', 'Dominica', 'Malaysia', 'Turkmenistan', 'Dominican Republic', 'Maldives', 'Tuvalu', 'Equatorial Guinea', 'Marshall Islands', 'Venezuela', 'Ecuador', 'Mexico')
WB_HIC<-c('Andorra', 'Greece', 'Palau', 'Antigua and Barbuda', 'Greenland', 'Panama', 'Aruba', 'Guam', 'Poland', 'Australia', 'Hong Kong', 'Portugal', 'Austria', 'Hungary', 'Puerto Rico', 'Bahamas', 'Iceland', 'Qatar', 'Bahrain', 'Ireland', 'Romania', 'Barbados', 'Isle of Man', 'San Marino', 'Belgium', 'Israel', 'Saudi Arabia', 'Bermuda', 'Italy', 'Seychelles', 'British Virgin Islands', 'Japan', 	'Singapore', 'Brunei', 'South Korea', 'Sint Maarten', 'Canada', 'Kuwait', 'Slovak Republic', 'Cayman Islands', 'Latvia', 'Slovenia', 'Channel Islands', 'Liechtenstein', 'Spain', 'Chile', 'Lithuania', 'St. Kitts and Nevis', 'Croatia', 'Luxembourg', 'St. Martin', 'Curaçao', 'Macao', 'Sweden', 'Cyprus', 'Malta', 'Switzerland', 'Czech Republic', 'Mauritius', 'Taiwan', 'Denmark', 'Monaco', 'Trinidad and Tobago', 'Estonia', 'Nauru', 'Turks and Caicos Islands', 'Faroe Islands', 'Netherlands', 'United Arab Emirates', 'Finland', 'New Caledonia', 'United Kingdom', 'France', 'New Zealand', 'United States', 'French Polynesia', 'Northern Mariana Islands', 'Uruguay', 'Germany', 'Norway', 'Virgin Islands', 'Gibraltar', 'Oman')

GPS_collapsed$WB<-ifelse(GPS_collapsed$country %in% WB_HIC, "1", ifelse(GPS_collapsed$country %in% WB_UMIC, "2", ifelse(GPS_collapsed$country %in% WB_LMIC, "2", ifelse(GPS_collapsed$country %in% WB_LIC, "3", NA))))

UN<-read_xlsx("./Mean years of schooling (years).xlsx")
colnames(UN)<-c("country", "MSY")
UN$MSY<-as.numeric(UN$MSY)

GPS_collapsed<-merge(GPS_collapsed, UN, all.x=TRUE, by="country")

GPS_collapsed$Prosociality<-rowMeans(GPS_collapsed[,c(5:7)])

############################################
# Figure A-5: Risk Taking <-> MSY (UN data)#
############################################

MSYRisk<-GPS_collapsed %>%
  ggplot(aes(x=MSY,y=Risk, group=WB, colour=WB)) +
  geom_point(size=6, 
             aes(shape=WB, colour=WB)) +
  geom_smooth(method=lm, se=FALSE, size=2, 
              aes(colour=WB), 
              show.legend=FALSE)+
  geom_text_repel(aes(label = isocode, colour=WB), 
                  size = rel(4), 
                  point.size = 2,
                  show.legend=FALSE)+
  theme_minimal()+
  theme(axis.text=element_text(size=rel(2.5)),
        axis.title=element_text(size=rel(3),face="bold"), 
        plot.title = element_text(size = rel(4), hjust=0.5), 
        panel.border = element_blank(),
        axis.line.x = element_line(linetype = "solid", colour = "black"),
        axis.line.y = element_line(linetype = "solid", colour = "black"), 
        axis.ticks = element_line(size=1),
        axis.ticks.length=unit(0.2, "cm"),
        legend.text = element_text(size=rel(2.5)), 
        legend.position = "bottom")+
  labs(y = "Risk taking", x="Mean Years of Schooling", title="Correlation between Risk taking and Mean Years of Schooling")+
  scale_colour_discrete(name="",
                        breaks=c("1", "2", "3"),
                         labels=c("High-Income","Middle-Income",  "Low-Income"))+
  scale_shape_discrete(name="",
                        breaks=c("1", "2", "3"),
                        labels=c("High-Income","Middle-Income",  "Low-Income"))+
  ggplot2::annotate("text", 
                    x = 5.5, y = c(0.95, 0.85, 0.75),    
                    size=9, 
                    label = c("\U25CF: Corr: 0.111 (p-value: 0.575)", "\U25B2: Corr: 0.101 (p-value: 0.519)", "\U25A0: Corr: 0.124 (p-value: 0.842)"),  
                    colour=c("#F8766D", "#00BA38", "#619CFF"))
                    ggsave("~/Data Archieve/FiguresAppend/figureA5.png", width = 20, height = 10 )

temp<-filter(GPS_collapsed,WB=="1")
cor(temp$MSY, temp$Risk, use="complete.obs")
cor.test(temp$MSY, temp$Risk, use="complete.obs")

temp<-filter(GPS_collapsed,WB=="2")
cor(temp$MSY, temp$Risk, use="complete.obs")
cor.test(temp$MSY, temp$Risk, use="complete.obs")

temp<-filter(GPS_collapsed,WB=="3")
cor(temp$MSY, temp$Risk, use="complete.obs")
cor.test(temp$MSY, temp$Risk, use="complete.obs")

############################################
# Figure A-6: Prosociality <-> MSY (UN data)#
############################################

MSYProsociality<-GPS_collapsed %>%
  ggplot(aes(x=MSY,y=Prosociality, group=WB, colour=WB)) +
    geom_point(size=6, 
             aes(shape=WB, colour=WB)) +
  geom_smooth(method=lm, se=FALSE, size=2, 
              aes(colour=WB), 
              show.legend=FALSE)+
  geom_text_repel(aes(label = isocode, colour=WB), 
                  size = rel(4), 
                  point.size = 2,
                  show.legend=FALSE)+
  theme_minimal()+
  theme(axis.text=element_text(size=rel(2.5)),
        axis.title=element_text(size=rel(3),face="bold"), 
        plot.title = element_text(size = rel(4), hjust=0.5), 
        panel.border = element_blank(),
        axis.line.x = element_line(linetype = "solid", colour = "black"),
        axis.line.y = element_line(linetype = "solid", colour = "black"), 
        axis.ticks = element_line(size=1),
        axis.ticks.length=unit(0.2, "cm"),
        legend.text = element_text(size=rel(2.5)), 
        legend.position = "bottom")+
  labs(y = "Prosociality", x="Mean Years of Schooling", title="Correlation between Prosociality and Mean Years of Schooling")+
  scale_colour_discrete(name="",
                        breaks=c("1", "2", "3"),
                         labels=c("High-Income","Middle-Income",  "Low-Income"))+
  scale_shape_discrete(name="",
                        breaks=c("1", "2", "3"),
                        labels=c("High-Income","Middle-Income",  "Low-Income"))+
  ggplot2::annotate("text", 
                    x = 5.5, y = c(0.95, 0.85, 0.75),  
                    size=9, 
                    label = c("\U25CF: Corr: -0.101 (p-value: 0.610)", "\U25B2: Corr: 0.216 (p-value: 0.164)", "\U25A0: Corr: -0.830 (p-value: 0.082)"),  
                    colour=c("#F8766D", "#00BA38", "#619CFF"))
                    ggsave("~/Dropbox/Data Archive Shared/FiguresAppend/figureA6.png", width = 20, height = 10 )

temp<-filter(GPS_collapsed,WB=="1")
cor(temp$MSY, temp$Prosociality, use="complete.obs")
cor.test(temp$MSY, temp$Prosociality, use="complete.obs")

temp<-filter(GPS_collapsed,WB=="2")
cor(temp$MSY, temp$Prosociality, use="complete.obs")
cor.test(temp$MSY, temp$Prosociality, use="complete.obs")

temp<-filter(GPS_collapsed,WB=="3")
cor(temp$MSY, temp$Prosociality, use="complete.obs")
cor.test(temp$MSY, temp$Prosociality, use="complete.obs")





