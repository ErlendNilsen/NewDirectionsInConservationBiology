


library(tidyverse)
library(ggplot2)
library(here)
library(viridis)

##################################################################################################
##### Descriptives; 

d <- as_tibble(read.csv2(here("data", "Data_LittSurvey_Box1.csv")))


d <- d %>% mutate(Hypo=ifelse(Clearly.stated.hypothesis>1, 1, 0)) %>%
  mutate(Experiment=ifelse(Study.design=="Experimental", 1,0)) %>%
  mutate(YearLaps=abs(Year-2016)) %>%
  mutate(Scope2=ifelse(Scope=="Methods", "Methods", "Biodiversity response")) %>%
  mutate(Hypothesis2=Clearly.stated.hypothesis) %>% 
  mutate(Hypothesis2=ifelse(Hypothesis2==0, "No", 
                     ifelse(Hypothesis2==1, "Partly", 
                     ifelse(Hypothesis2==2, "Implied", "Clearly stated"))))

###############################################
##### cleary stated hypotheis; 

## Plotting results; 
## Clearly stated hypothesis; 

Hypothesis_prop <- d %>% group_by(Hypothesis2, Scope2) %>%
  count(Clearly.stated.hypothesis) %>%
  mutate(prop=prop.table(n)) %>%
  mutate(Hypothesis3= factor(Hypothesis2, 
        levels=c("No", "Implied", "Partly", "Clearly stated"))) %>%
mutate(Scope3= factor(Scope2, levels=c("Methods", "Biodiversity response")))


Hypothesis_prop2 <- d %>%
  filter(Scope=="Ecological processes") %>%
  count(Clearly.stated.hypothesis) %>%
  mutate(prop=prop.table(n))

p <- ggplot(data=Hypothesis_prop, aes(x=Hypothesis3, y=n, fill=Scope3)) +
  geom_bar(stat="identity", width=0.7, ) +
  theme_minimal() + 
  theme(text = element_text(size=12), legend.position = "bottom", legend.title = element_blank()) +
  ylim(0, 100)+
  labs(x="", y="Number of articles", title="") + 
  scale_fill_brewer(palette="Blues")

p+coord_flip()



############################################################################
#### FIGURE 2: 

d <- d %>% mutate(Exper_citation=ifelse(Study.design=="Non experimental", 1, 
                                        ifelse(Study.design=="Experimental", 3, 2))) %>%
  mutate(scale_citation=ifelse(Spatial.scale=="Local", 1, 
                               ifelse(Spatial.scale=="Landscape", 2, 3))) %>%
  mutate(citation_year=Number.of.citations/(2017-Year))

citation_table1 <- d %>% filter(Scope!="Methods") %>%
  group_by(Exper_citation, scale_citation) %>% 
  summarise(N_citat=quantile(citation_year, probs=c(0.75)), antall=n()) %>%
  mutate(prop=prop.table(antall))

###################



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


###########################################################
### NUMBERS FOR THE TEXT: 

### Proportions; 

Hypothesis_prop2 <- d %>%
  mutate(Hyp_pooled=ifelse(Hypothesis2=="Partly", "Implied", Hypothesis2)) %>%
  count(Hyp_pooled) %>%
  mutate(prop=prop.table(n))

### Removing methods; 

Hypothesis_prop3 <- d %>%
  mutate(Hyp_pooled=ifelse(Hypothesis2=="Partly", "Implied", Hypothesis2)) %>%
  filter(Scope!="Methods") %>%
  count(Hyp_pooled) %>%
  mutate(prop=prop.table(n))

### Experiments; 

Exp_prop <- d %>% filter(Scope!="Methods") %>%
  count(Exper_citation) %>%
  mutate(prop=prop.table(n))














