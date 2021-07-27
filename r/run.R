# libraries --------
libri(tidyverse, arm, janitor, DescTools, patchwork, pbapply, here, glue, ggtext, scales, umich-biostatistics/SEIRfansy, maxsal/covid19india)

# params ----------
production   <- FALSE
state        <- "tt" # use `tt` for India
days_of_data <- 200

# setup -----------
today      <- Sys.Date() - 1
`%notin%`  <- Negate(`%in%`)
f          <- list.files(here("src"))
for (i in seq_along(f)) source(here("src", glue("{f[i]}")))

# Set variables based on testing or production
if (production == TRUE) {
  n_iter    <- 1e5
  burn_in   <- 1e5
  opt_num   <- 200
} else {
  n_iter    <- 1e3 #default 1e5
  burn_in   <- 1e3 #default 1e5
  opt_num   <- 10  #default 200
}

# specs -----------
state_name <- pop %>% dplyr::filter(abbrev == state) %>% pull(place)
max_date   <- as.Date(today - 1)
min_date   <- max_date - (days_of_data - 1)
obs_days   <- length(as.Date(min_date):as.Date(max_date))
t_pred     <- 150 # number of predicted days
N          <- pop %>% filter(abbrev == state) %>% pull(population)
plt        <- FALSE
save_plt   <- FALSE

# load and prepare ----------
data <- readr::read_csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv",
                        col_types = cols()) %>%
  janitor::clean_names() %>%
  dplyr::select(date = date_ymd, status, val = tolower(state)) %>%
  arrange(date) %>%
  tidyr::pivot_wider(
    names_from  = "status",
    values_from = "val",
    id_cols = "date"
  ) %>%
  dplyr::filter(date <= max_date)

data_initial <- get_init(data)
data         <- data %>% dplyr::filter(date >= min_date)
mCFR         <- tail(cumsum(data$Deceased) / cumsum(data$Deceased + data$Recovered), 1)
phases       <- get_phase(start_date = min_date, end_date = max_date)

# predict -----------
result    <- SEIRfansy::SEIRfansy.predict(
  data            = abs(data %>% dplyr::select(-date)),
  init_pars       = NULL,
  data_init       = data_initial,
  T_predict       = t_pred,
  niter           = n_iter,
  BurnIn          = burn_in,
  model           = "Multinomial",
  N               = N,
  lambda          = 1/(69.416 * 365),
  mu              = 1/(69.416 * 365),
  period_start    = phases,
  opt_num         = opt_num,
  auto.initialize = T,
  alpha_u         = 0.5,
  f               = 0.15,
  plot            = plt,
  save_plots      = save_plt
)

pred_clean <- clean_prediction(result$prediction,
                               state    = state_name,
                               obs_days = obs_days,
                               t_pred   = t_pred)

write_rds(
  x        = result,
  file     = here("output", glue("raw_seirfansy_results_{state}.rds")),
  compress = "gz"
)

write_rds(
  x        = pred_clean,
  file     = here("output", glue("pred_clean_{state}.rds")),
  compress = "gz"
  )

write_rds(
  x        = as_tibble(result$mcmc_pars, .name_repair = "unique"),
  file     = here("output", glue("prediction_pars_{state}.rds")),
  compress = "gz"
  )

# get underreporting ----------
impo <- get_impo(x = pred_clean, res = result)
write_tsv(x = impo, file = here("output", glue("important_{state}.txt")))

# preview prediction plots -----------
obs <- bind_rows(get_nat_counts(), get_state_counts()) %>%
  dplyr::select(place, date, daily_cases) %>%
  dplyr::filter(place == state_name) %>%
  dplyr::mutate(pred = 0)

# all plot ----------
(all_plot <- pred_clean %>%
  dplyr::filter(section == "positive_daily_reported" & pred == 1) %>%
  dplyr::select(place = state, date, daily_cases = mean, pred) %>%
  bind_rows(obs) %>%
  mutate(pred = as.factor(pred)) %>%
  ggplot(aes(x = date, y = daily_cases)) +
  geom_line(aes(group = pred, color = pred), size = 1) +
  labs(
    title    = glue("SEIRfansy predicted reported cases in {state_name}"),
    x        = "Date",
    y        = "Daily COVID-19 cases",
    caption  = "**\uA9 COV-IND-19 Study Group**"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = c("%m/%d/%y")) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_markdown(hjust = 0),
    legend.position = "none"
  ))

ggsave(
  filename = here("output", glue("all_plot_{state}.pdf")),
  plot     = all_plot,
  width = 7, height = 5, device = cairo_pdf
)

# predicted plot ----------
(zoom_plot <- pred_clean %>%
  dplyr::filter(section == "positive_daily_reported" & pred == 1) %>%
  dplyr::select(place = state, date, daily_cases = mean, pred) %>%
  bind_rows(obs) %>%
  dplyr::mutate(pred = as.factor(pred)) %>%
  dplyr::filter(date >= "2021-01-01" & date <= "2021-10-01") %>%
  ggplot(aes(x = date, y = daily_cases)) +
  geom_line(aes(group = pred, color = pred), size = 1) +
  labs(
    title    = glue("SEIRfansy predicted reported cases in {state_name}"),
    x        = "Date",
    y        = "Daily COVID-19 cases",
    caption  = "**\uA9 COV-IND-19 Study Group**"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = c("%m/%d/%y")) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_markdown(hjust = 0),
    legend.position = "none"
  ))

ggsave(
  filename = here("output", glue("zoom_plot_{state}.pdf")),
  plot     = zoom_plot,
  width = 7, height = 5, device = cairo_pdf
)
