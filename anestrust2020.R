library(tidyverse)
library(haven)
library(survey)
library(ggtext)
library(glue)
library(showtext)
library(patchwork)


anes <- read_dta("C:\\Users\\matth\\OneDrive\\Desktop\\Data\\anes\\anes_timeseries_2020_stata_20210324.dta")
anes_df <- as.data.frame(anes)

head(anes_df$V201233)



design <- svydesign(~V200010c,  #Set up survey design
                    strata=~V200010d,
                    data=anes_df,
                    weights=~V200010a,
                    nest=TRUE
                    )
anes_tab <- svytable(~V201233, design=design)  #Create Survey Tables

anes_tab




###Create weighted percentages
count <- anes_tab %>%  
  as.data.frame() %>%
  rename(trust = V201233) %>%
  filter(trust %in% seq(1:5)) %>%
  mutate(trust = factor(trust, labels = c("Always", "Most of the Time", "About Half of the Time", "Some of the Time", "Never"))) %>%
  mutate(prop = (Freq/sum(Freq))*100)

count




##Add Fonts
##font_add_google(
  ##name="Dancing Script",
  ##family = "Dancing Script")
##showtext_auto()


###Plot
ggplot(count, aes(x=trust, y=prop, fill=trust))+
  geom_col(width=0.5, alpha=0.5)+
  #scale_fill_manual(values = c("Democrat"="blue", "Republican" = "red", "Independent" = "green"))+
  labs(x="", 
       y="Trust %",
       fill="",
       caption="Source: 2020 American National Election Studies",
       subtitle = "<span style=color:'blue'>Few Americans Trust The Federal Government <br> to Do What is Right All or Most of the Time.</span>")+
  geom_text(aes(label = paste0(round(prop), "%")),
            size = 3, fontface = "bold", vjust=-.25) +
  theme_minimal()+
  theme(text = element_text(family = "Roboto Mono"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 12,face = "bold", color = "#535657", hjust = 0.5, vjust = -1),
        plot.subtitle = element_markdown(size = 10,face = "bold", color = "#535657", hjust = 0, vjust = -4),
        axis.title.x = element_text(size = 10, face = "bold", color = "#535657", vjust = -2),
        axis.title.y = element_text(size = 10, face = "bold", color = "#535657"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold", color = "#535657"),
        axis.ticks.x = element_line(colour = "#b2b2b2"),
        plot.caption = element_markdown(size = 7,face = "bold", color = "#535657", hjust = 1.5, vjust = -1),
        legend.text = element_text(size = 8,face = "bold", color = "#535657"),
        legend.title = element_text(size = 8,face = "bold", color = "#535657"))

