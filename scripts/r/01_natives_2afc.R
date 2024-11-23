

# load data
n_2afc <- read_csv(
  here(
    "data",
    "prev_data",
    "natives_2afc_tidy.csv"
  )
)

n_2afc_tidy <- n_2afc %>%
  select(
    participant,
    spn_variety,
    item,
    speaker_variety,
    condition,
    sentence_type,
    sentence,
    is_correct,
    rt_adj
  ) %>%
  mutate(
    caribbean_participant = ifelse(
      spn_variety %in% c("cuban","puertorican"), 1, 0
    ),
    caribbean_stimuli = ifelse(
      speaker_variety %in% c("cuban","puertorican"), 1, 0
    ),
    caribbean_participant = as.factor(caribbean_participant),
    caribbean_stimuli = as.factor(caribbean_stimuli),
    participant = as.factor(participant),
    spn_variety = as.factor(spn_variety),
    speaker_variety = as.factor(speaker_variety),
    is_correct = as.factor(is_correct)
  )

# correct response ~ caribbean stimuli

model_acc <- glmer(is_correct ~ caribbean_stimuli +
       (1 | participant),
     data = n_2afc_tidy,
     family = binomial)

predictions_acc <- ggpredict(model, terms = "caribbean_stimuli")

plot(predictions_acc) +
  labs(
    title = "Predicted Probability of Correct Response by Caribbean Stimuli",
    x = "Caribbean Stimuli (0 = No, 1 = Yes)",
    y = "Predicted Probability of Correct Response"
  ) +
  theme_minimal()

summary(model_acc)

# reaction time ~ caribbean stimuli

model_rt <- model_rt <- lmer(rt_adj ~ caribbean_stimuli + 
                               (1 | participant), 
                             data = n_2afc_tidy)

summary(model_rt)