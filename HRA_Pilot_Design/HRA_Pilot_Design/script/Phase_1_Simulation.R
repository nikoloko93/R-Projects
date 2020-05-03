
# function ----------------------------------------------------------------

simulation_fn <- function(prop, seq_prop, power_val){
  
  "
  Author: Nikko Joe Ramal
  This function generates a table consist of varying propportions with 
  its effect size and sample size of our hypothesize proportion/percent value. 
  
  Parameters:
    @prop: hypothesize proportion value
    @seq_prop: a sequence of proportion 
        Example: seq_prop = seq(0.55, 0.70, 0.01)
    @power_val: power value
  
  "
  
  effect_size = NULL
  sample_size = NULL
  
  for(i in 1:length(seq_prop)){
    h = ES.h(seq_prop[i], prop)
    p <- pwr.p.test(h = h, power = power_val, sig.level = 0.05, n = NULL, alternative = "greater")
    effect_size <- rbind(effect_size, p$h)
    sample_size <- rbind(sample_size, p$n)
  }
  
  pow_dat <- cbind(as.data.frame(e_prop), effect_size, sample_size)
  
}

# parameters' values -----------------------------------------------------------------
seq_prop = seq(0.55, 0.70, 0.01)
pow_dat <- simulation_fn(prop = 0.5, seq_prop, power_val = 0.8)

# plot sample size --------------------------------------------------------

ggplot() +
  geom_line(
    data = pow_dat, aes(x = seq_prop, y = sample_size)
  ) +
  geom_point(
    aes(x = seq_prop, y = sample_size)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 18), labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  labs(
    title = "Sample Size vs Change in Percentage",
    subtitle = sprintf("Effect Size: > %.2f; Power: .80", prop),
    y = "Sample Size",
    x = "Percentage"
  )





