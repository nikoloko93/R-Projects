## Firts 14 days
Before <- stp_me_firstandlast %>%
  filter(id=="BEFORE")%>%
  group_by(savvy_id) %>%
  summarise(avg_steps = mean(steps,na.rm=TRUE)) %>% 
  cbind(id="BEFORE") %>% data.frame()
library(tidyverse)

#Last 14 days
After <- stp_me_firstandlast %>%
  filter(id=="AFTER")%>%
  group_by(savvy_id) %>%
  summarise(avg_steps = mean(steps,na.rm=TRUE)) %>% 
  cbind(id="AFTER") %>% data.frame()

## Combining the data
allstepBnA <- rbind(Before,After) %>%
  mutate(Activeness = case_when(avg_steps <= 5000~ "Inactive",
                                avg_steps >5000&avg_steps<=9999~"Somewhat Active",
                                avg_steps >=10000~ "Active"))

prop <- allstepBnA %>% 
  group_by(id, Activeness) %>% 
  summarise(cnt = n()) %>% 
  mutate(prop = cnt/20)

prop$Activeness <- factor(prop$Activeness,level=c("Inactive","Somewhat Active","Active"),order=TRUE)
prop$id <- factor(prop$id,level=c("BEFORE","AFTER"),order=TRUE)

ggplot(data = prop, aes(x=id, y=prop, group=Activeness))+ 
  geom_line(aes(color = Activeness), size = 1) +
  geom_point(aes(color=Activeness)) +
  scale_color_manual(values=c("#4897D8","#999999","#F8A055"), name = "ACTIVENESS LEVEL") +
  ggtitle("Figure 6. Activeness Category of SPE2 Participants \n Before and After the Experiment") +
  ylab("PROPORTION OF INDIVIDUALS") +
  xlab("") +
  labs(caption="*Individuals who take fewer than 5,000 steps are considered to be sedentary or inactive.  Somewhat active people usually take \n 5000 to 9,999 steps per day. People considered to be active take 10,000 or more steps per day.") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1,by=0.20)) +
  geom_label(aes(x=id,y=prop,label=scales::percent(prop),color=Activeness), 
             position = position_nudge(y = 0.025), show.legend = FALSE) +
  theme(axis.text.x = element_text(size=12),
        text = element_text(family = "sans", color = "#303030", size = 12),
        title = element_text(face = "bold", color = "#303030", size = 15),
        axis.text=element_text(size=12),
        axis.title = element_text(size=14, face="bold"),
        plot.title = element_text(size=15, hjust = 0.5),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12),
        plot.subtitle = element_text(face="italic"),
        plot.caption = element_text(hjust = 0, size=12, color ="#3A5199",face="italic"),
        panel.background = element_rect(fill="white", color ="#D3D3D3"),
        panel.grid.major.y = element_line(colour="#D3D3D3",linetype = "dashed"),
        strip.text.x = element_text(size = 12))
