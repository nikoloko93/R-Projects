library(tidyverse)

step1b <- stp_me %>% 
  filter(!is.na(steps)) %>% 
  group_by(savvy_id) %>% 
  summarise(max_date=max(step_date), mean_step = scales::comma(round(mean(steps), 0)))


ggplot(data=stp_me, aes(x=step_date, y=steps))+
  geom_line(color="#303030")+
  scale_x_date(date_breaks = "1 week", date_labels = ("%b %d")) +
  ggtitle("Figure 4. Steps of SPE2 Participants over the Experiment Period")+
  facet_wrap(~savvy_id, nrow = 5)+
  labs(caption = "*Average steps over the whole period.") +
  xlab("") +
  ylab("TOTAL STEPS")+
  geom_label(data = step1b, aes(x=as.Date("2018-10-25")-4, y=40000-1500), 
             label=step1b$mean_step, size=4,color ="#3A5199")+
  coord_cartesian(ylim=c(0,40000)) +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,size=12),
        text = element_text(family = "sans", color = "#303030", size = 12),
        title = element_text(face = "bold", color = "#303030", size = 15),
        axis.text=element_text(size=12),
        axis.title = element_text(size=14, face="bold"),
        plot.title = element_text(size=16, hjust = 0.5),
        plot.subtitle = element_text(face="italic"),
        plot.caption = element_text(hjust = 0, size=14, color ="#3A5199",face="italic"),
        panel.background = element_rect(fill="white", color ="#D3D3D3"),
        panel.grid.major.y = element_line(colour="#D3D3D3",linetype = "dashed"),
        strip.text.x = element_text(size = 12))

#------------------------------ COMPARISON GROUP ------------------------------#

# pulling steps data of comp group 
step2 <- tibble()
for (i in 1:20){
  c <- step_raw %>% 
    filter(savvy_id==comp_dates$savvy_id[i]) %>% 
    filter(step_date>=as.Date(comp_dates$start_date[i])&
             step_date<=as.Date(comp_dates$new_end[i]))
  step2 <- rbind(step2, c)
}

step2b <- step2 %>% 
  group_by(savvy_id) %>% 
  summarise(mean_step = scales::comma(round(mean(steps), 0)))

step2a <- tibble()
for (i in 1:20){
  d <- tibble(savvy_id=comp_dates$savvy_id[i],
              step_date=seq.Date(comp_dates$start_date[i], comp_dates$new_end[i], by=1))
  d <- left_join(d, step2, by=c("savvy_id", "step_date"))
  step2a <- rbind(step2a, d)
}

step2b1 <- step2a %>% 
  group_by(savvy_id) %>% 
  summarise(qt = as.Date(quantile(unclass(step_date), 0.90), origin = "1970-01-01")) %>% 
  left_join(step2b)

ggplot(data=step2a, aes(x=step_date, y=steps))+
  geom_line(color="#303030")+
  scale_x_date(date_breaks = "1 week", date_labels = ("%b %d")) +
  ggtitle("Figure 5. Steps of Comparison Group over a 2-Month Period")+
  facet_wrap(~savvy_id, nrow = 5, scales = "free_x")+
  coord_cartesian(ylim=c(0,40000)) +
  labs(caption = "*Average steps over the whole period.") +
  xlab("") +
  ylab("TOTAL STEPS")+
  geom_label(data = step2b1, aes(x=qt, y=40000-1200), 
             label=step2b$mean_step, size=4,color ="#3A5199")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,size=12),
        text = element_text(family = "sans", color = "#303030", size = 12),
        title = element_text(face = "bold", color = "#303030", size = 15),
        axis.text=element_text(size=12),
        axis.title = element_text(size=14, face="bold"),
        plot.title = element_text(size=16, hjust = 0.5),
        plot.subtitle = element_text(face="italic"),
        plot.caption = element_text(hjust = 0, size=14, color ="#3A5199",face="italic"),
        panel.background = element_rect(fill="white", color ="#D3D3D3"),
        panel.grid.major.y = element_line(colour="#D3D3D3",linetype = "dashed"),
        strip.text.x = element_text(size = 12))
