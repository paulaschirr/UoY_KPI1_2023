#### LOAD PACKAGES ####

library(reshape2)
library(ggplot2)
library(ggpubr)
library(forcats)
library(viridis)
library(dplyr)
library(rcartocolor)
library(lubridate)
library(MASS)
library(FSA)

#install with install.packages("packagename"), 
#then rerun library(packagename) if any of the above are not downloaded yet

#create color palette for plotting 
#(fully color-blind friendly, 
#applicable for large groups of categorical data: different hues
#with consistent steps in lightness and saturation)
safe_pal <- carto_pal(10, "Safe")


#### FWCI over time ####

#load and prepare data
FWCIovertime<-read.csv("Field-Weighted_Citation_Impact_vs_Publication_Year.csv", skip=13)
FWCIovertime<-FWCIovertime[1:(nrow(FWCIovertime)-5),]
FWCIovertime<-FWCIovertime[c(-2,-3,-10)]
colnames(FWCIovertime)
colnames(FWCIovertime)<-c("university", "2017", "2018", "2019", "2020", "2021", "2022")


#transform data to long format for plotting, add new variable 'certainty'
FWCIovertime_long<-melt(FWCIovertime)
FWCIovertime_long<-FWCIovertime_long%>%
  group_by(variable,university)%>%
  mutate(certainty=ifelse(variable=="2022", "uncertain", "certain"))

#plot the data
ggplot()+
  geom_point(data=subset(FWCIovertime_long, certainty=="certain"), 
             aes(x=variable, y=value, color=university, group=university), size=2.2)+
  geom_point(data=subset(FWCIovertime_long, certainty=="uncertain"), 
             aes(x=variable, y=value, color=university), size=2.2, alpha=0.3)+
  geom_line(data=subset(FWCIovertime_long, university=="University of York" & certainty=="certain"), 
            aes(x=variable, y=value, group=university), lwd=1, color="#888888")+
  geom_line(data=subset(FWCIovertime_long, university=="University of York" & variable %in% c(2021, 2022)), 
            aes(x=variable, y=value, group=university), lwd=1, linetype="dotted", color="#888888")+
  geom_hline(yintercept=1, color="gray70", linetype="dashed", lwd=1.2)+
  annotate(geom="text", x=1.8, y=1.07, label="world average", color="gray70", size=6)+
  scale_color_manual(values = safe_pal)+
  labs(y="Field-weighted citation Impact", x="Publication Year", color="")+
  theme_classic(base_size = 20)

#### FWCI overtime - publication-level for UoY ####
Yorkpubs<-read.csv("Publications_at_the_University_of_York_2017_to_2021.csv", skip=15)
Yorkpubs<-Yorkpubs[1:(nrow(Yorkpubs)-1),]

#compute averages and 75th percentile
averages<-Yorkpubs%>%
  group_by(Year)%>%
  summarise(quartile75= quantile(Field.Weighted.Citation.Impact, 0.75), 
            mean=mean(Field.Weighted.Citation.Impact))

#transform data to long format
average_long<-melt(averages, id.vars="Year")
#change names
colnames(average_long)<-c("Year","Summarystats", "Field.Weighted.Citation.Impact")
#combine data
Yorkpubs1<-bind_rows(Yorkpubs, average_long)

#plot data on logarithmic scale
ggplot(Yorkpubs1)+
  geom_boxplot(data=subset(Yorkpubs1, is.na(Summarystats)), aes(x=as.factor(Year), y=Field.Weighted.Citation.Impact))+
  geom_jitter(alpha=0.05, data=subset(Yorkpubs1, is.na(Summarystats)), aes(x=as.factor(Year), y=Field.Weighted.Citation.Impact))+
  geom_point(data=subset(Yorkpubs1, Summarystats=="mean"), aes(x=as.factor(Year), y=Field.Weighted.Citation.Impact), color="darkred", size=2)+
  scale_y_continuous(trans='log10')+
  theme_classic()
#(citations are unequal in variance and not-normally distributed)

#significance testing with non-parametric test - Kruskal Wallis test is signficant 
kruskal.test(data=Yorkpubs, Field.Weighted.Citation.Impact~Year)
# post-hoc test to find which levels are different
dunnTest(data=Yorkpubs, Field.Weighted.Citation.Impact~Year)


#### FWCI overtime - by Faculty ####

#load and prepare data
faculty<-read.csv("Field-Weighted_Citation_Impact_vs_Publication_Year_byFaculty.csv", skip=13)
faculty<-faculty[1:(nrow(faculty)-5),]
faculty<-faculty[c(-2,-3,-10)]
colnames(faculty)
colnames(faculty)<-c("PURE export", "2017", "2018", "2019", "2020", "2021", "2022")

#transform the data to long format, coerce faculty name to factor, add new variable 'certainty'
faculty_long<-melt(faculty)
faculty_long$`PURE export`<-as.factor(faculty_long$`PURE export`)
faculty_long$certainty<-ifelse(faculty_long$variable=="2022", "uncertain", "certain")

#plot the data
ggplot()+
  geom_line(data=faculty_long, aes(x=variable, y=value, group=`PURE export`, color=`PURE export`),lwd=1, linetype="dashed")+
  geom_line(data=subset(faculty_long, certainty=="certain"), aes(x=variable, y=value, group=`PURE export`, color=`PURE export`),lwd=1)+
  scale_color_manual(values = safe_pal)+
  geom_hline(yintercept=1, color="gray70", linetype="dashed", lwd=1)+
  annotate(geom="text", x=1.8, y=1.15, label="world average", color="gray70", size=6)+
  labs(y="Field-weighted citation impact", x="Publication Year", color="PURE export")+
  theme_classic(base_size = 20)


#### Top 10% FWCI ####

#load and prepare data
top10overtime<-read.csv("Field-Weighted_Output_in_Top_10%_Citation_Percentiles_vs_Publication_Year.csv", skip=13)
top10overtime<-top10overtime[1:(nrow(top10overtime)-12),]
top10overtime<-top10overtime[c(-2,-3,-10)]
colnames(top10overtime)
colnames(top10overtime)<-c("university", "2017", "2018", "2019", "2020", "2021", "2022")

#transform the data to long format, add new variable 'certainty'
top10overtime_long<-melt(top10overtime)
top10overtime_long<-top10overtime_long%>%
  group_by(variable,university)%>%
  mutate(certainty=ifelse(variable=="2022", "uncertain", "certain"))

#plot data
ggplot()+
  geom_point(data=subset(top10overtime_long, certainty=="certain"), 
             aes(x=variable, y=value, color=university), size=2.2)+
  geom_point(data=subset(top10overtime_long, certainty=="uncertain"), 
             aes(x=variable, y=value, color=university), size=2.2, alpha=0.3)+
  geom_line(data=subset(top10overtime_long, university=="University of York" & certainty=="certain"), 
            aes(x=variable, y=value, group=university), lwd=1, color="#888888")+
  geom_line(data=subset(top10overtime_long, university=="University of York" & variable %in% c(2021, 2022)), 
            aes(x=variable, y=value, group=university), lwd=1, linetype="dotted", color="#888888")+
  scale_color_manual(values = safe_pal)+
  labs(y="Field-weighted outputs in\ntop 10% citation percentile [%]", x="Publication Year", color="")+
  theme_classic(base_size = 20)

#compute outputs in top 10% FWCI from raw data
Top10averages<-Yorkpubs%>%
  group_by(Year)%>%
  mutate(publicationvolume=n())%>%
  ungroup()%>%
  group_by(Year, Field.Weighted.Outputs.in.Top.Citation.Percentiles..per.percentile, publicationvolume)%>%
  summarise(toppercentiles=n())%>%
  ungroup()%>%
  group_by(Year)%>%
  mutate(toppercentilescum=cumsum(toppercentiles))%>%
  filter(Field.Weighted.Outputs.in.Top.Citation.Percentiles..per.percentile %in% c(1,5,10,25))%>%
  mutate(toppercentiles_percent=toppercentilescum/publicationvolume)

#plot data again
ggplot(Top10averages, 
       aes(x=Year, y=toppercentiles_percent, color=as.factor(Field.Weighted.Outputs.in.Top.Citation.Percentiles..per.percentile)))+
  labs(y="Field-weighted outputs in\ntop citation percentiles [%]", x="Publication Year", color="Citation percentile")+
  scale_y_continuous(labels = scales::percent)+
  geom_line()+
  theme_classic(base_size=20)

#Chisq test for trend in proportions (not signifcant)
Top10averages%>%
  filter(Field.Weighted.Outputs.in.Top.Citation.Percentiles..per.percentile==10)%>%
  with(prop.trend.test(toppercentilescum, publicationvolume))

#### Scholarly output ####

outputsovertime<-read.csv("Scholarly_Output_vs_Publication_Year.csv", skip=13)
outputsovertime<-outputsovertime[1:(nrow(outputsovertime)-1),]
outputsovertime<-outputsovertime[c(-2,-3,-10)]
colnames(outputsovertime)
colnames(outputsovertime)<-c("university", "2017", "2018", "2019", "2020", "2021", "2022")
outputsovertime_long<-melt(outputsovertime)

ggplot(outputsovertime_long, aes(x=variable, y=value, color=university))+
  geom_point(size=2.2)+
  geom_line(data=subset(outputsovertime_long, university=="University of York"), 
            aes(x=variable, y=value, group=university), lwd=1, color="#888888")+
  scale_color_manual(values = safe_pal)+
  labs(y="Scholarly outputs", x="Publication Year", color="")+
  theme_classic(base_size = 20)


#### SUSTAINABLE DEVELOPMENT GOALS ####

#load SDG data for 2017-2021 for all 10 universities
SDG<-read.csv("SDGs_UoY_comparators.csv", stringsAsFactors = T)

#rename levels of SDGs
SDG$SDG<-factor(SDG$SDG,
                levels=c("SDG 1: No Poverty",
                         "SDG 2: Zero Hunger",
                         "SDG 3: Good Health and Well-being",
                         "SDG 4: Quality Education",
                         "SDG 5: Gender Equality",
                         "SDG 6: Clean Water and Sanitation",
                         "SDG 7: Affordable and Clean Energy",
                         "SDG 8: Decent Work and Economic Growth",
                         "SDG 9: Industry, Innovation and Infrastructure",
                         "SDG 10: Reduced Inequality",
                         "SDG 11: Sustainable Cities and Communities",
                         "SDG 12: Responsible Consumption and Production",
                         "SDG 13: Climate Action",
                         "SDG 14: Life Below Water",
                         "SDG 15: Life on Land",
                         "SDG 16: Peace, Justice and Strong Institutions"))


#### FWCI SDGs ####

#plot FWCI against SDGs for each university
ggplot(subset(SDG, metric=="FWCI" & 
                university!="World average" & 
                university !="United Kingdom average"), 
       aes(x=fct_rev(SDG), y=value, color=university, group=university))+
  geom_point(size=2.2)+
  geom_line(data=subset(SDG, metric=="FWCI" & university=="University of York"), 
            aes(x=fct_rev(SDG), y=value, group=university), lwd=1)+
  geom_hline(yintercept=1, color="gray70", linetype="dashed", lwd=1)+
  annotate(geom="text", x=14, y=1.1, label="world average", color="gray70", angle=-90, size=6)+
  scale_color_manual(values = safe_pal)+
  scale_alpha_manual(values=c(0.3,1,1))+
  labs(y="Field-weighted citation impact", x="", color="")+
  coord_flip()+
  theme_classic(base_size=20)


#### Outputs SDGs ####

#add new column 'world output' for normalisation step
SDG$world_output<-subset(SDG, university=="World average" & metric=="output")$value

#normalise output data by world output in respective SDG 
#(otherwise SDG 3 is so large that cross-comparison is impossible)
SDG1<-SDG%>%
  filter(metric=="output")%>%
  group_by(university)%>%
  mutate(world_output_percent= world_output/sum(world_output))%>%
  mutate(output_percent=value/sum(value))%>%
  mutate(output_norm=output_percent/world_output_percent)

#plot normalised publication volume against SDGs for each comparator university
ggplot(subset(SDG1, university!="World average" & university !="United Kingdom average"), 
       aes(x=fct_rev(SDG), y=output_norm, 
           color=university, group=university))+
  geom_point(size=2.2)+
  geom_line(data=subset(SDG1, university=="University of York"), 
            aes(x=fct_rev(SDG), y=output_norm, group=university), lwd=1)+
  geom_hline(yintercept=1, color="gray70", linetype="dashed", lwd=1)+
  annotate(geom="text", x=14, y=1.07, label="world average", color="gray70", angle=-90, size=6)+
  scale_color_manual(values = safe_pal)+
  scale_alpha_manual(values=c(0.3,1,1))+
  labs(y="Scholarly output normalised by world output for SDG", x="", color="")+
  coord_flip()+
  theme_classic(base_size=20)
