library('tidyverse')

prey_conv <- function(a, B, pred_L){
  exp(a + B * log(pred_L))
}

predLData <- read.csv('./lmb_stb_combined.csv') %>%
  mutate(max_prey_length = prey_conv(0.443, 0.774, length_mm),
         safety = cumulative_proportion - proportion_of_total)
predLModel <- glm(predLData$safety ~ predLData$max_prey_length,
             family = quasibinomial(logit),
             data = predLData)
predLWFitData  <-  predLData %>% 
  mutate(predict = predict.glm(predLModel, type = "response")) %>% 
  arrange(max_prey_length) %>%
  filter(max_prey_length <= 200)

ggplot(predLWFitData, aes(x = max_prey_length)) +
  theme_classic(base_size = 30) +
  labs(y = "Fraction present", x = "Length (mm)") +
  geom_point(aes(y = safety)) +
  geom_path(aes(y = predict), color = "red") +
  ggtitle("Using cum. prop. and gape limitation")

predDData <- data.frame(density = c(3, 6, 12),
                     survival = c(.87, .78, .84))

predDModel <- glm(predDData$survival ~ predDData$density,
                  family = quasibinomial(logit),
                  data = predDData)
predDWFitData <-  predDData %>% 
  mutate(predict = predict.glm(predDModel, type = "response")) %>% 
  arrange(density)

ggplot(predDWFitData, aes(x = density)) +
  theme_classic(base_size = 30) +
  labs(y = "Fraction present", x = "Density (fish / m^2)") +
  geom_point(aes(y = survival)) +
  geom_path(aes(y = predict), color = "red")
  #ggtitle("Using cum. prop. and gape limitation")