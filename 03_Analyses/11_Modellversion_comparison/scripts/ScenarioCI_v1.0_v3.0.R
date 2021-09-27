### Comparison of EFForTS-ABM model versions v1.0 and actual developmentversion without landmarket module (landmarket? off)

library(tidyverse)
library(gridExtra)

##Results from v1.(CI: default)

#Read in csv and create tibble
v1_C0<-read_csv("data/js_2016_sc12.csv", skip = 6) %>%
  rename(step="[step]", meanconsumption = "mean [h_consumption] of turtles", stdvconsumption = "standard-deviation [h_consumption] of turtles")

# LUT-fractions
v1_C0_lut_fractions <- v1_C0 %>% 
  select(step, pcount_op, pcount_rm) %>%
  group_by(step) %>%
  summarise_all(list(mean, sd)) %>%
  ggplot(aes(x = step)) +
  geom_ribbon(aes(ymin = pcount_op_fn1 - pcount_op_fn2, ymax = pcount_op_fn1 + pcount_op_fn2), fill = "indianred3", alpha = 0.5) +
  geom_line(aes(y = pcount_op_fn1, colour = "indianred3")) +
  geom_ribbon(aes(ymin = pcount_rm_fn1 - pcount_rm_fn2, ymax = pcount_rm_fn1 + pcount_rm_fn2), fill = "darkgoldenrod1", alpha = 0.5) +
  geom_line(aes(y = pcount_rm_fn1, colour = "darkgoldenrod1")) +
  scale_color_identity() +
  scale_x_continuous(limits = c(1, 50)) +
  labs(
    x = "Simulation time", # x axis title
    y = "Land-use type fractions") +  # y axis title
  #title = "Temporal dynamics under constant price scenarios" +     # main title of figure
  theme_light() + theme(legend.position = "none")

# Consumption
v1_C0_consumption <- v1_C0 %>% 
  select(step, meanconsumption) %>%
  group_by(step) %>%
  summarise_all(list(mean, sd)) %>%
  rename(mean_consumption = "fn1", sd_consumption = "fn2") %>%
  ggplot(aes(x = step)) +
  geom_ribbon(aes(ymin = mean_consumption - sd_consumption, ymax = mean_consumption + sd_consumption), fill = "blue4", alpha = 0.5) +
  geom_line(aes(y = mean_consumption, color = "blue4")) +
  scale_color_identity() + 
  scale_x_continuous(limits = c(1, 50)) +
  labs(
    x = "Simulation time", # x axis title
    y = "Mean Consumption (USD)") +   # y axis title
  #title = "Temporal dynamics under constant price scenarios") +     # main title of figure
  theme_light() + theme(legend.position = "none")

# Carbon
v1_C0_carbon <- v1_C0 %>% 
  select(step, oilpalm_carbon, rubbermono_carbon, area_under_agriculture) %>%
  mutate(oilpalm_carbon_agr = oilpalm_carbon/area_under_agriculture, rubbermono_carbon_agr = rubbermono_carbon/area_under_agriculture) %>%
  group_by(step) %>%
  summarise_all(list(mean, sd)) %>%
  ggplot(aes(x = step)) +
  geom_ribbon(aes(ymin = oilpalm_carbon_agr_fn1 - oilpalm_carbon_agr_fn2, ymax = oilpalm_carbon_agr_fn1 + oilpalm_carbon_agr_fn2), fill = "indianred3", alpha = 0.5) +
  geom_line(aes(y = oilpalm_carbon_agr_fn1, colour = "indianred3")) +
  geom_ribbon(aes(ymin = rubbermono_carbon_agr_fn1 - rubbermono_carbon_agr_fn2, ymax = rubbermono_carbon_agr_fn1 + rubbermono_carbon_agr_fn2), fill = "darkgoldenrod1", alpha = 0.5) +
  geom_line(aes(y = rubbermono_carbon_agr_fn1, colour = "darkgoldenrod1")) +
  scale_color_identity() +
  scale_x_continuous(limits = c(1, 50)) +
  labs(
    x = "Simulation time", # x axis title
    y = "Carbon (Tons/ha)") +   # y axis title
  #title = "Temporal dynamics under constant price scenarios") +     # main title of figure
  theme_light() + theme(legend.position = "none")


######################################################################################################

##Results from v3.(C0: default)

#Read in csv and create tibble
v3_C0<-read_csv("data/acceptance_test_simple.csv") %>%
  rename(step="[step]")

# LUT-fractions
v3_C0_lut_fractions <- v3_C0 %>% 
  select(step, lut0.fraction, lut1.fraction) %>%
  group_by(step) %>%
  summarise_all(list(mean, sd)) %>%
  ggplot(aes(x = step)) +
  geom_ribbon(aes(ymin = lut0.fraction_fn1 - lut0.fraction_fn2, ymax = lut0.fraction_fn1 + lut0.fraction_fn2), fill = "indianred3") +
  geom_line(aes(y = lut0.fraction_fn1, colour = "indianred3")) +
  geom_ribbon(aes(ymin = lut1.fraction_fn1 - lut1.fraction_fn2, ymax = lut1.fraction_fn1 + lut1.fraction_fn2), fill = "darkgoldenrod1") +
  geom_line(aes(y = lut1.fraction_fn1, colour = "darkgoldenrod1")) +
  scale_color_identity() +
  scale_x_continuous(limits = c(1, 50)) +
  labs(
    x = "Simulation time", # x axis title
    y = "Land-use type fractions") +   # y axis title
  #title = "Temporal dynamics under constant price scenarios") +     # main title of figure
  theme_light() + theme(legend.position = "none")

# Consumption
v3_C0_consumption <- v3_C0 %>% 
  select(step, hh.consumption.mean) %>%
  group_by(step) %>%
  summarise_all(list(mean, sd)) %>%
  #rename(mean_consumption = "fn1", sd_consumption = "fn2") %>%
  ggplot(aes(x = step)) +
  geom_ribbon(aes(ymin = fn1 - fn2, ymax = fn1 + fn2), fill = "blue4", alpha = 0.5) +
  geom_line(aes(y = fn1, color = "blue4")) +
  scale_color_identity() +
  scale_x_continuous(limits = c(1, 50)) +
  labs(
    x = "Simulation time", 
    y = "Mean Consumption (USD)") +
  #title = "Temporal dynamics under constant price scenarios") +     
  theme_light() + theme(legend.position = "none")

# Carbon
v3_C0_carbon <- v3_C0 %>% 
  select(step, lut0.carbon, lut1.carbon) %>%
  #mutate(lut0.carbon_agr = lut0.carbon/area_under_agriculture, lut1.carbon_agr = lut1.carbon/area_under_agriculture) %>%
  group_by(step) %>%
  summarise_all(list(mean, sd)) %>%
  ggplot(aes(x = step)) +
  geom_ribbon(aes(ymin = lut0.carbon_agr_fn1 - lut0.carbon_agr_fn2, ymax = lut0.carbon_agr_fn1 + lut0.carbon_agr_fn2), fill = "indianred3", alpha = 0.5) +
  geom_line(aes(y = lut0.carbon_fn1, colour = "indianred3")) +
  geom_ribbon(aes(ymin = lut1.carbon_agr_fn1 - lut1.carbon_agr_fn2, ymax = lut1.carbon_agr_fn1 + lut1.carbon_agr_fn2), fill = "darkgoldenrod1", alpha = 0.5) +
  geom_line(aes(y = lut1.carbon_fn1, colour = "darkgoldenrod1")) +
  scale_color_identity() +
  scale_x_continuous(limits = c(1, 50)) +
  labs(
    x = "Simulation time", # x axis title
    y = "Carbon (Tons/ha)") +   # y axis title
  #title = "Temporal dynamics under constant price scenarios") +     # main title of figure
  theme_light() + theme(legend.position = "none")

#######################################################################################################

##Arrange plots of v1 and v3 for CO
grid.arrange(
  v1_C0_lut_fractions,
  v1_C0_consumption,
  v1_C0_carbon,
  v3_C0_lut_fractions,
  v3_C0_consumption,
  v3_C0_carbon,
  nrow=3,
  #heights= c(1,1,1),
  #widths= c(1,1,1),
  layout_matrix = rbind(c(1, 4),
                        c(2, 5),
                        c(3, 6)),
  top= "Temporal dynamics under constant price scenario (C0)"
)
