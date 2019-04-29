

library(tidyverse)
library(ggplot2)

#### Caughley-citations; 

dc <- as.tibble(read.csv2("presentations/Citations_Caughley.txt", header=T, sep="\t"))


p1 <- ggplot(data=dc) +  
      geom_bar(aes(x=Publication.Years, y=records*10), stat="identity", fill="sienna3") + 
      geom_line(aes(x=Publication.Years, y=Cumulative), size=2, colour="dark green") + 
      theme(axis.title.y.right = element_text(colour="sienna3", size=12), 
            axis.title.y = element_text(colour="dark green", size=12))

p1 + labs(x="Year", y="Cumulative number of citations", size=5, colour="dark green") + 
  scale_y_continuous(sec.axis=sec_axis(~./10, name="Number of records"))
      

##################################################################################################
##### Descriptives; 


d <- as.tibble(read.csv2("data/Data_Extraction_form_COMPLETE.csv", header=T))


d <- d %>% mutate(Hypo=ifelse(Clearly.stated.hypothesis>1, 1, 0)) %>%
  mutate(Experiment=ifelse(Study.design=="Experimental", 1,0)) %>%
  mutate(YearLaps=abs(Year-2016))

###############################################
##### cleary stated hypotheis; 

## Plotting results; 
## Clearly stated hypothesis; 

Hypothesis_prop <- d %>% 
  count(Clearly.stated.hypothesis) %>%
  mutate(prop=prop.table(n))

Hypothesis_prop2 <- d %>%
  filter(Scope=="Ecological processes") %>%
  count(Clearly.stated.hypothesis) %>%
  mutate(prop=prop.table(n))

p <- ggplot(data=Hypothesis_prop, aes(x=Clearly.stated.hypothesis, y=prop)) +
  geom_bar(stat="identity", width=0.7, fill="sienna3") +
  ylim(0,1)+
  theme(text = element_text(size=12)) +
  labs(x="", y="Proportion", title="Presentation of hypothesis") + 
  geom_text(aes(label=paste("n =", n)), hjust=2, col="white", size=4.5)

p+scale_x_discrete(labels=c("No", "Implied", "Partly", "Clearly stated"), limits=c( 0,1,2,3)) +
  coord_flip()



p <- ggplot(data=Hypothesis_prop2, aes(x=Clearly.stated.hypothesis, y=prop)) +
  geom_bar(stat="identity", width=0.7, fill="sienna3") +
  ylim(0,1)+
  theme(text = element_text(size=12)) +
  labs(x="", y="Proportion", title="Presentation of hypothesis - studies of ecological process") + 
  geom_text(aes(label=paste("n =", n)), hjust=1, col="white", size=4.5)

p+scale_x_discrete(labels=c("No", "Implied", "Partly", "Clearly stated"), limits=c( 0,1,2,3))+
  coord_flip()


###############################################################################################
###############################################################################################
## Multiple competing hypothesis

MH_prop <- d %>% 
  filter(Scope=="Ecological processes") %>%
  count(Test.multiple.competing.hypotheses) %>%
  mutate(prop=prop.table(n))

p <- ggplot(data=MH_prop, aes(x=Test.multiple.competing.hypotheses, y=prop)) +
  geom_bar(stat="identity", width=0.7, fill="sienna3") +
  ylim(0,1)+
  theme(text = element_text(size=12)) +
  labs(x="", y="Proportion", title="Test of multiple competing hypotheses - studies of ecological processes") + 
  geom_text(aes(label=paste("n =", n)), vjust=1.6, col=c("white", 1), size=4.5)

p+scale_x_discrete(labels=c("No", "Yes"), limits=c(0,1))


########################################################################
########################################################################
### Experiments; 


Design_prop2 <- d %>% 
  filter(Scope=="Ecological processes") %>%
  count(Study.design) %>%
  mutate(prop=prop.table(n))


p <- ggplot(data=Design_prop2, aes(x=Study.design, y=prop)) +
  geom_bar(stat="identity", width=0.7, fill="sienna3") +
  ylim(0,1)+
  theme(text = element_text(size=15)) +
  labs(x="", y="Proportion", title="Study design - studies of ecological processes") + 
  geom_text(aes(label=paste("n =", n)), vjust=1.6, col=c(1,rep("white",3)), size=4.5)

p+scale_x_discrete(limits=c("Non experimental", "Quasi experimental", "BACI", "Experimental"))



#################################################################
#################################################################

##########################################
### More hypothesis when: 



################################################################3
## With experiments; 
m3 <- glm(Hypo~as.factor(Experiment), data=d, 
          family="binomial", na.action="na.exclude", subset=Scope!="Methods")
summary(m3)

## 

Experi <- d %>% 
  filter(Scope!="Methods") %>%
  count(Hypo, Experiment) %>%
  mutate(prop=prop.table(n))

p <- ggplot(data=Experi, aes(x=as.factor(Experiment), y=n, fill=as.factor(Hypo))) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  labs(x="", y="Number of papers", title="", size=2)+
  theme(text = element_text(size=15)) 

p+scale_x_discrete(labels=c("Non-experimental", "Experimental"))+
  scale_fill_brewer(palette="Reds",labels=c("No hypothesis", "Hypothesis"))+coord_flip()+theme_minimal(base_size=15)+
  theme(legend.title=element_blank())

#############################################################
## Longer studies; 

m1 <- glm(Hypo~Number.of.years, data=d, 
          family="binomial", na.action="na.exclude", subset=Scope!="Methods")
summary(m1) 

v1 <- ggplot(data=filter(d,Scope!="Methods" & Number.of.years!="NA"), aes(x=as.factor(Hypo), y=Number.of.years))+
      geom_violin(trim=TRUE, alpha=.5, colour="blue", fill="red") + 
      theme(text = element_text(size=15)) 
v1+scale_x_discrete(labels=c("No clear hypothesis", "Hypothesis")) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.35, fill="white") + 
  labs(x="", y="Number of years", title="Are clearly stated hypothesis more common in long-term studies?", size=2)
########

v1b <- ggplot(data=, filter(d,Scope=="Ecological processes"& Number.of.years!="NA"), aes(y=Hypo, x=Number.of.years))+
  stat_smooth(method="glm", method.args=list(family="binomial"), 
              colour="orange", formula=y~x, alpha=0.5, size=1, lty=3)+
  ylim(0,1)+
  labs(x="Number of years", y="Probability that a clear hypothesis is presented", title="", size=2)+
  theme(text = element_text(size=15)) 

v1b 

###############################################################
## More authors; 

m2 <- glm(Hypo~Number.of.co.authors, data=d, 
          family="binomial", na.action="na.exclude", subset=Scope!="Methods")
summary(m2)

m2b <- lm(Number.of.co.authors~as.factor(Hypo)-1, data=d, na.action="na.exclude", subset=Scope!="Methods")
summary(m2b)


v2 <- ggplot(data=filter(d,Scope!="Methods"), aes(x=as.factor(Hypo), y=Number.of.co.authors+1))+
  geom_violin(trim=TRUE, alpha=.5, colour="blue", fill="red") + 
  theme(text = element_text(size=15)) + 
  ylim(c(0, 15))

v2+scale_x_discrete(labels=c("No clear hypothesis", "Hypothesis")) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.35, fill="white") + 
  labs(x="", y="Number of authors", title="Are clearly stated hypothesis more common with more authors?", size=2)

##
d2 <- filter(d, Scope!="Methods")
boxplot((d2$Number.of.co.authors+1)~as.factor(d2$Hypo))

## 
v3 <- ggplot(data=, filter(d,Scope!="Methods"), aes(y=Hypo, x=Number.of.co.authors+1))+
  stat_smooth(method="glm", method.args=list(family="binomial"), colour="orange", formula=y~x, alpha=0.5, size=2)+
  ylim(0,1)+
  labs(x="Number of co-authors", y="Probability that a clear hypothesis is presented", title="", size=2)+
  theme(text = element_text(size=15)) 
   
 v3


################################################################3
## With experiments; 
m3 <- glm(Hypo~as.factor(Experiment), data=d, 
          family="binomial", na.action="na.exclude", subset=Scope!="Methods")
summary(m3)

## 

Experi <- d %>% 
  filter(Scope!="Methods") %>%
  count(Hypo, Experiment) %>%
  mutate(prop=prop.table(n))

p <- ggplot(data=Experi, aes(x=as.factor(Experiment), y=n, fill=as.factor(Hypo))) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  labs(x="", y="Number of papers", title="", size=2)+
  theme(text = element_text(size=15)) 

p+scale_x_discrete(labels=c("Non-experimental", "Experimental"))+
  scale_fill_brewer(palette="Greens",labels=c("No hypothesis", "Hypothesis"))+coord_flip()+theme_minimal(base_size=15)+
  theme(legend.title=element_blank())

##############################
## Year published

m4 <- glm(Hypo~Year, data=filter(d, Scope!="Methods"),
          family="binomial", na.action="na.exclude", subset=Scope!="Methods")
summary(m4)


#######################
## With Ecological process; 

Scope2 <- d %>% 
  filter(Scope %in% c("Ecological processes", "Natural history")) %>%
  count(Hypo, Scope) %>%
  mutate(prop=prop.table(n))

##

p <- ggplot(data=Scope2, aes(x=Scope, y=n, fill=as.factor(Hypo))) +
  geom_bar(stat="identity", color="black", width=0.7, position=position_dodge())+
  labs(x="", y="Number of papers", title="", size=2)+
  theme(text = element_text(size=15)) 

p+scale_x_discrete(labels=c("Ecological process","Natural history"))+
  scale_fill_brewer(palette="Greens",labels=c("No hypothesis", "Hypothesis"))+coord_flip()+theme_minimal(base_size=15)+
  theme(legend.title=element_blank())



###################################################################################################
####### EXPERIMENTS: 

## More experiments with

## Taxa; 
Experi2 <- d %>% 
  filter(Scope!="Methods") %>%
  count(Taxa.group1, Experiment) %>%
  mutate(prop=prop.table(n))

p <- ggplot(data=Experi2, aes(x=Taxa.group1, y=n, fill=as.factor(Experiment))) +
  geom_bar(stat="identity", color="black", width=0.7, position=position_dodge())+
  labs(x="", y="Number of papers", title="", size=2)+
  theme(text = element_text(size=15)) 

p+scale_x_discrete(labels=c("Invertebrates","Mixed", "Plants", "Vertebrates"))+
  scale_fill_brewer(palette="Reds",labels=c("Non-experimental", "Experimental"))+coord_flip()+
theme_minimal(base_size=15)+ 
  theme(legend.title=element_blank())


## Spatial scale; 
Experi3 <- d %>% 
  filter(Scope!="Methods") %>%
  mutate(scale_pooled=ifelse(Spatial.scale=="Global", "Regional", paste(Spatial.scale))) %>%
  count(scale_pooled, Experiment) %>%
  mutate(prop=prop.table(n))


p <- ggplot(data=Experi3, aes(x=scale_pooled, y=n, fill=as.factor(Experiment))) +
  geom_bar(stat="identity", color="black", width=0.7, position=position_dodge())+
  labs(x="", y="Number of papers", title="", size=2)+
  theme(text = element_text(size=15)) 

p+scale_x_discrete(labels=c("Country","Landscape", "Local", "Regional"))+
  scale_fill_brewer(palette="Greens",labels=c("Non-experimental", "Experimental"))+coord_flip()+
  theme_minimal(base_size=15)+ 
  theme(legend.title=element_blank())

## Duration; 

m5 <- glm(Experiment~Number.of.years, data=filter(d, Scope!="Methods"),
          family="binomial", na.action="na.exclude", subset=Scope!="Methods")
summary(m5)

m5b <- lm(Number.of.years~as.factor(Experiment)-1, data=filter(d, Scope!="Methods"))
summary(m5b)


##################################################################
#######################################################
##### Number of citations; 

M0a <- glm(Number.of.citations~offset(log(2016-Year)), 
                 family="quasipoisson", na.action="na.exclude",  data=d)

M0 <- glm(Number.of.citations~offset(log(2016-Year)), 
           family="quasipoisson", na.action="na.exclude",  data=subset(d, Scope!="Methods"))


M0p <- glm(Number.of.citations~offset(log(2016-Year)), 
          family="quasipoisson", na.action="na.exclude",  data=subset(d, Scope!="Methods" & Taxa.group1=="Plants" ))

######################################
### Scope
Ma <- glm(Number.of.citations~offset(log(2016-Year))+ Scope-1, 
          family="quasipoisson", na.action="na.exclude",  data=d)
summary(Ma)
anova(M0a, Ma, test="F")

#######################################
## Experiments
M1 <- glm(Number.of.citations~offset(log(2016-Year))+as.factor(Experiment), 
            data=filter(d, Scope!="Methods"), 
            family="quasipoisson", na.action="na.exclude")
summary(M1) 
anova(M0, M1, test="F")

#######################################
## Only plants; 
M1p <- glm(Number.of.citations~offset(log(2016-Year))+as.factor(Experiment)-1, data=subset(d, Scope!="Methods" & Taxa.group1=="Plants"), 
          family="quasipoisson", na.action="na.exclude")
summary(M2p)
anova(M0p, M1p, test="F")

#########################################
## Spatial scale
M2 <- glm(Number.of.citations~offset(log(2016-Year))+Spatial.scale-1, data=filter(d, Scope!="Methods"), 
           family="quasipoisson", na.action="na.exclude")
summary(M2)
anova(M0, M2, test="F")

##########################################
## With hypothesis

M3 <- glm(Number.of.citations~offset(log(2016-Year))+as.factor(Hypo), data=filter(d, Scope!="Methods"), 
          family="quasipoisson", na.action="na.exclude")
summary(M3)
anova(M0, M3, test="F")

#####################################################################################
#####################################################################################
### DOES IT MATTER; 


d <- d %>% mutate(Exper_citation=ifelse(Study.design=="Non experimental", 1, 
                                        ifelse(Study.design=="Experimental", 3, 2))) %>%
          mutate(scale_citation=ifelse(Spatial.scale=="Local", 1, 
                                       ifelse(Spatial.scale=="Landscape", 2, 3))) %>%
          mutate(citation_year=Number.of.citations/(2017-Year))


p <- ggplot(data=filter(d, Scope!="Methods"), aes(x=scale_citation, y=Exper_citation)) +
      geom_jitter(width=0.2, height = 0.2, aes(size=citation_year, colour=citation_year)) +
      scale_colour_gradient(low = "grey", high = "red")  
      

p + scale_x_discrete(labels=c("Local scale", "Landscape", "Larger scale"), limits=c(1,2,3)) +
    scale_y_discrete(labels=c("No experiement", "Quasi/BACI", "Full experiment"), limits=c(1,2,3)) +
    theme(axis.title=element_blank(),
          text = element_text(size=15),
          legend.title=element_blank(), 
          legend.position="right") + guides(size=FALSE) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor=element_line(colour = "white", size = 0.5))

### Alternative; 

citation_table1 <- d %>% filter(Scope!="Methods") %>%
                  group_by(Exper_citation, scale_citation) %>% 
                  summarise(N_citat=quantile(citation_year, probs=c(0.75)), antall=n()) %>%
                  mutate(prop=prop.table(antall))




p <- ggplot(data=citation_table1, aes(x=scale_citation, y=Exper_citation, size=antall)) +
  geom_point(aes(colour=N_citat)) +
  scale_colour_gradient(low = "grey", high = "yellow")  

p + scale_x_discrete(labels=c("Local scale", "Landscape", "Larger scale"), limits=c(1,2,3)) +
  scale_y_discrete(labels=c("No experiement", "Quasi/BACI", "Full experiment"), limits=c(1,2,3)) +
  theme(axis.title=element_blank(),
        text = element_text(size=15),
        legend.position="right") + guides(size=FALSE) + 
     scale_size(range = c(2, 30)) +
    labs(colour="Annual citations")

########################################################################################################
########################################################################################################
### High impact; 

d_hi <- as.tibble(read.csv2("data/Data_Extraction_High_Impact_Working.csv", header=T, sep=";"))

d_hi <- d_hi %>% mutate(Exper_citation=ifelse(Study.design=="Non experimental", 1, 
                                        ifelse(Study.design=="Experimental", 3, 2))) %>%
  mutate(scale_citation=ifelse(Spatial.Scale=="Local", 1, 
                               ifelse(Spatial.Scale=="Landscape", 2, 3))) 

citation_table2 <- d_hi %>% 
  group_by(Exper_citation, scale_citation) %>% 
  summarise(antall=n()) 



p <- ggplot(data=citation_table2, aes(x=scale_citation, y=Exper_citation, size=antall)) +
  geom_point(colour="sienna2") 

p + scale_x_discrete(labels=c("Local scale", "Landscape", "Larger scale"), limits=c(1,2,3)) +
  scale_y_discrete(labels=c("No experiement", "Quasi/BACI", "Full experiment"), limits=c(1,2,3)) +
  theme(axis.title=element_blank(),
        text = element_text(size=15),
        legend.position="none") +
  scale_size(range = c(2, 30)) 
 
###########################################

Scale1 <- d %>%
  filter(Scope!="Methods") %>%
  count(scale_citation) %>%
  mutate(prop=prop.table(n))

Scale2 <- d_hi %>%
  count(scale_hi) %>%
  mutate(prop=prop.table(n))

exp1 <- d %>%
  filter(Scope!="Methods") %>%
  count(Exper_citation) %>%
  mutate(prop=prop.table(n))

exp2 <- d_hi %>%
  count(Exper_citation) %>%
  mutate(prop=prop.table(n))

####

library(dplyr)
t2 <- full_join(citation_table1, citation_table2, by=c("Exper_citation", "scale_citation"))

t3 <- t2 %>% mutate(prop.x=ifelse(is.na(prop.x), 0, prop.x)) %>% mutate(selection=prop.y-prop.x) 
 
p <- ggplot(data=t3, aes(x=scale_citation, y=Exper_citation, size=selection)) +
  geom_point(colour="sienna2") 

p + scale_x_discrete(labels=c("Local scale", "Landscape", "Larger scale"), limits=c(1,2,3)) +
  scale_y_discrete(labels=c("No experiement", "Quasi/BACI", "Full experiment"), limits=c(1,2,3)) +
  theme(axis.title=element_blank(),
        text = element_text(size=15),
        legend.position="none") +
  scale_size(range = c(1, 30)) 



d_ch <- matrix(ncol=2, nrow=4)

temp <- filter(d, Scope!="Methods") 
d_ch[c(1,2), 1] <- mean(temp$scale_citation)
d_ch[c(3,4),1] <- mean(d_hi$scale_citation)

d_ch[c(1,2), 2] <- mean(temp$Exper_citation)
d_ch[c(3,4),2] <- mean(d_hi$Exper_citation)


