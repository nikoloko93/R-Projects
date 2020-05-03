library(tidyverse)

## First 14 days
Before <- cgm_me_firstandlast %>% 
  filter(id=="BEFORE") %>% 
  mutate(TIR = between(egv,80,180))

Before_prop <- Before %>%
  group_by(savvy_id) %>%
  summarise(TIR = mean(TIR,na.rm=TRUE)) %>%
  mutate(egv_level = case_when(TIR < .75 ~ "Sometimes in-control",
                               TIR >= .75 & TIR < .90 ~ "Often in-control",
                               TIR >= .90 ~ "Always in-control")) %>% 
  cbind(id="BEFORE")

## Last 14 days
After <- cgm_me_firstandlast %>% 
  filter(id=="AFTER") %>% 
  mutate(TIR = between(egv,80,180))

After_prop <- After %>%
  group_by(savvy_id) %>%
  summarise(TIR = mean(TIR,na.rm=TRUE)) %>%
  mutate(egv_level = case_when(TIR < .75 ~ "Sometimes in-control",
                               TIR >= .75 & TIR < .90 ~ "Often in-control",
                               TIR >= .90 ~ "Always in-control")) %>% 
  cbind(id="AFTER")

## Combining the data
propall <- rbind(Before_prop,After_prop) %>%
  group_by(id,egv_level) %>%
  summarise(prop = n()/20)
propall$egv_level <- factor(propall$egv_level,
                            level=c("Sometimes in-control","Often in-control","Always in-control"),order=TRUE)
propall$id <- factor(propall$id,level=c("BEFORE","AFTER"),order=TRUE)

## Creating the plot
ggplot(data = propall, aes(x=id, y=prop, group=egv_level)) + 
  geom_line(aes(color = egv_level), size=1) +
  scale_color_manual(values=c("#4897D8","#999999","#F8A055"), name = "IN CONTROL OF DIABETES?") +
  geom_point(aes(color=egv_level)) +
  ggtitle("Figure 3. How in control are the SPE2 Participants of their Diabetes \n Before and After the Experiment Period?") +
  ylab("PROPORTION OF PARTICIPANTS") +
  xlab("") +
  labs(caption = "*A participant is categorized as always in-control if their TIR is more than 90%, often in-control if TIR is within the range of 81%-90% \n and as sometimes in-control if TIR is less than or equal to 80%.")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1,by=0.20)) +
  geom_label(aes(x=id,y=prop,label=scales::percent(prop),color=egv_level), 
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
