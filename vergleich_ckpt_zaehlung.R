# Plot zum Vergleich Checkpoints vs manuelle Zählung


z_28_breaks <- mutate(z_28, breaks = cut(z_28$time, breaks = "5 min"))

z_28_grouped_5min <- z_28_breaks %>% group_by(breaks) %>% summarise(erfasst = sum(erfasst), nicht_erfasst = sum(nicht_erfasst))

ckpt_28_breaks <- mutate(ckpt_28, breaks = cut(ckpt_28$time, breaks = "5 min"))

ckpt_28_grouped_5min <- ckpt_28_breaks %>% group_by(breaks) %>% summarise(erfasst = frequency(type))

zaehlung_lvs_check_28_02[is.na(zaehlung_lvs_check_28_02)] <- 0
zaehlung_lvs_check_28_02 <- zaehlung_lvs_check_28_02[!(zaehlung_lvs_check_28_02$Erfasst_SG == "XXXX"),]
zaehlung_lvs_check_28_02 <- mutate(zaehlung_lvs_check_28_02,
                                   erfasst = as.numeric(Erfasst_SG) + 
                                     as.numeric(Erfasst_aK))



plot_ckpt_vs_z 
ggplot() +
  geom_freqpoly(data = checkpoint_stats_28_02,
                aes(x = time, colour = "red"), binwidth = 5) +
  geom_bar(data =zaehlung_lvs_check_28_02,
           aes(x = time,y = erfasst, colour = "blue"), stat = "identity")
