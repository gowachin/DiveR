
nitrox_mindepth(ppo2 = 0.209)
ppo2 <- seq(0, 1, by = 0.01)
exp <- - nitrox_mindepth(ppo2)
min <- -nitrox_maxdepth(ppo2)


data.frame(ppo2 = ppo2, exp = exp, min = min) %>%
  ggplot(aes(x = ppo2, y = exp, color = exp > 0)) +
  geom_line() +
  geom_line(y = min) +
  geom_vline(xintercept = 0.209)+
  geom_vline(xintercept = 0.18)+
  NULL
