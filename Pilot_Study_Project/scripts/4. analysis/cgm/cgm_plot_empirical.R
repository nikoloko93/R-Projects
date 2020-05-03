library(tidyverse)

round_up <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

#------------------------------ SPE2 ------------------------------#

cgm_spe_clean <- cgm_me %>%
  mutate(TIR = between(egv,80,180))

egv_lines <- cgm_spe_clean %>%
  filter(!is.na(egv)) %>% 
  group_by(savvy_id) %>%
  summarise(avg_egv = mean(egv))

tag <- cgm_spe_clean %>%
  mutate(tag = ifelse(egv < 80, "below",ifelse(egv > 180, "above","within"))) %>%
  group_by(savvy_id,tag) %>%
  summarise(cnt = n())
tot <- cgm_spe_clean %>%
  group_by(savvy_id) %>%
  summarise(n = n())
full_labels <- merge(tag,tot) %>%
  mutate(prop = cnt/n)

within_labels <- full_labels %>%
  filter(tag == "within") %>%
  mutate(date_local = max(cgm_spe_clean$date_local)-19, egv = round_up(max(as.vector(as.matrix(cgm_comp_clean$egv,cgm_spe_clean$egv))))-10)
above_labels <- full_labels %>%
  filter(tag == "above") %>%
  mutate(date_local = max(cgm_spe_clean$date_local)-34, egv = round_up(max(as.vector(as.matrix(cgm_comp_clean$egv,cgm_spe_clean$egv))))-10)
below_labels <- full_labels %>%
  filter(tag == "below") %>%
  mutate(date_local = max(cgm_spe_clean$date_local)-4, egv = round_up(max(as.vector(as.matrix(cgm_comp_clean$egv,cgm_spe_clean$egv))))-10)

ggplot(data = cgm_spe_clean, aes(x=lubridate::ymd(date_local),y=egv)) +
  geom_point(aes(color = ifelse(egv < 80, "below",ifelse(egv > 180, "above","within"))), 
             alpha= 0.5, shape = 15, size=0.9) +
  scale_x_date(date_breaks = '1 week', date_labels = ("%b %d")) +
  scale_color_manual(values=c("#F8A055","#4897D8","#999999")) +
  guides(color = "none") +
  ggtitle("Figure 1. EGV of SPE2 Participants over the Experiment Period") +
  ylab("EGV") +
  xlab("") +
  labs(caption="*Dots represent average EGV level per day; Line represents the average EGV level over the whole period.") +
  coord_cartesian(ylim=c(0,round_up(max(as.vector(as.matrix(cgm_comp_clean$egv,cgm_spe_clean$egv)))))) +
  stat_summary(fun.y = mean, shape = 19, color = "#3A5199", geom ="point", size=0.9 ) +
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
        strip.text.x = element_text(size = 12)) +
  facet_wrap(~ savvy_id, nrow = 5) +
  geom_hline(data=egv_lines, aes(yintercept=avg_egv), color = "#3A5199", size=0.5) +
  geom_hline(yintercept = c(80,180), color = rep(c("#999999","#999999"),20), size = rep(c(0.25,0.25),20)) +
  geom_label(data = within_labels, aes(label = scales::percent(prop)),size=4,color ="#999999") +
  geom_label(data = above_labels, aes(label = scales::percent(prop)),size=4,color ="#F8A055") +
  geom_label(data = below_labels, aes(label = scales::percent(prop)),size=4,color ="#4897D8")

#------------------------------ COMPARISON GROUP ------------------------------#

egv_lines <- cgm_comp_clean %>%
  group_by(savvy_id) %>%
  summarise(avg_egv = mean(egv))

tag <- cgm_comp_clean %>%
  mutate(tag = ifelse(egv < 80, "below",ifelse(egv > 150, "above","within"))) %>%
  group_by(savvy_id,tag) %>%
  summarise(cnt = n())
tot <- cgm_comp_clean %>%
  group_by(savvy_id) %>%
  summarise(n = n())
full_labels <- merge(tag,tot) %>%
  mutate(prop = cnt/n)

comp_dates <- cgm_comp_clean %>%
  group_by(savvy_id) %>%
  summarise(date_local = max(date_local), egv = max(egv))

within_labels <- full_labels %>%
  filter(tag == "within") %>%
  merge(comp_dates,by="savvy_id") %>%
  mutate(date_local = date_local-19, egv = egv-10)
above_labels <- full_labels %>%
  filter(tag == "above") %>%
  merge(comp_dates,by="savvy_id") %>%
  mutate(date_local = date_local-34, egv = egv-10)
below_labels <- full_labels %>%
  filter(tag == "below") %>%
  merge(comp_dates,by="savvy_id") %>%
  mutate(date_local = date_local-4, egv = egv-10)

ggplot(data = cgm_comp_clean, aes(x=lubridate::ymd(date_local),y=egv)) +
  geom_point(aes(color = ifelse(egv < 80, "below",ifelse(egv > 180, "above","within"))), alpha= 0.5, shape = 15) +
  scale_x_date(date_breaks = '1 week', date_labels = ("%b %d")) +
  scale_color_manual(values=c("#F8A055","#4897D8","#999999")) +
  guides(color = "none") +
  ggtitle("TIR") +
  xlab("DATE LOCAL") +
  ylab("EGV") +
  stat_summary(fun.y = mean, shape = 19, color = "#3A5199", geom ="point" ) +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,size=12),
        text = element_text(family = "sans", color = "#303030", size = 12),
        title = element_text(face = "bold", color = "#303030", size = 17),
        axis.text=element_text(size=12),
        axis.title = element_text(size=14, face="bold"),
        plot.title = element_text(size=17, hjust = 0.5),
        plot.subtitle = element_text(face="italic"),
        panel.background = element_rect(fill="white", color ="#D3D3D3"),
        panel.grid.major.y = element_line(colour="#D3D3D3",linetype = "dashed"),
        strip.text.x = element_text(size = 12)) +
  coord_cartesian(ylim=c(0,round_up(max(as.vector(as.matrix(cgm_comp_clean$egv,cgm_spe_clean$egv)))))) +
  facet_wrap(~ savvy_id, nrow = 5,scales="free_x") +
  geom_hline(data=egv_lines, aes(yintercept=avg_egv), color = "#3A5199", size=0.5) +
  geom_hline(yintercept = c(80,180), color = rep(c("#999999","#999999"),20), size = rep(c(0.25,0.25),20)) +
  geom_label(data = within_labels, aes(label = scales::percent(prop)),size=4,color ="#999999") +
  geom_label(data = above_labels, aes(label = scales::percent(prop)),size=4,color ="#F8A055") +
  geom_label(data = below_labels, aes(label = scales::percent(prop)),size=4,color ="#4897D8")
