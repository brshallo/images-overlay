# THIS CODE IS NOT REPRODUCIBLE For each header went to a different post and
# then did "run above" and then modified output here to void out parts of
# charts, if wanting to make more reproducible would need to preface with a
# source_rmd()... https://gist.github.com/noamross/a549ee50e8a4fd68b8b1

# From feature engineering
resamples  %>% 
  extract_dates_rset() %>% 
  plot_dates_rset() +
  theme_void()+
  theme(legend.position = "none")

# From influencing distributions
data_distribution %>% 
  mutate(quartile = (cum_dens) %/% 0.2500001 + 1,
         quartile = as.factor(quartile)) %>% 
  mutate(price_next = map_int(price, get_row_bump),
         price_dist = price_next - price) %>% 
  mutate(density_convert = proportion_raised(dens_scaled, incentive_delta, price_dist)) %>% 
  select(-dens) %>% 
  # add converted density to nearest point
  group_by(incentive) %>% 
  mutate(dens_convert_total = sum(density_convert),
         price_switch = price == min(price)) %>% 
  ungroup() %>% 
  mutate(dens_convert_total = lag(dens_convert_total),
         dens_convert_total = ifelse(price_switch, dens_convert_total, 0)) %>%
  # na's to 0's
  mutate(across(c(density_convert, dens_convert_total), ~ifelse(is.na(.x), 0, .x))) %>% 
  # adjust percentages
  mutate(dens_adj = dens_scaled - density_convert + dens_convert_total) %>%
  rename(initial_quartile = quartile) %>% 
  # graph
  ggplot(aes(x = price))+
  geom_col(aes(y = dens_adj, fill = initial_quartile))+
  scale_fill_discrete(type = c("deeppink", "orange", "yellow2", "royalblue"))+
  ylim(c(0, 0.15))+
  labs(y = "density")+
  theme_void() +
  theme(legend.position = "none")

# weighting confusion matrices
remove_stuff_cm <- function(cm){
  cm$layers[[2]] <- NULL
  cm$labels <- NULL
  cm +
    theme_void()+
    theme(legend.position = "none")
}

remove_stuff_cm(cfm1) + remove_stuff_cm(cfm2)

# Quantile Regression Forests
rf_preds_test %>% 
  mutate(covered = ifelse(
    Sale_Price >= .pred_lower & Sale_Price <= .pred_upper, 
    "covered",
    "not covered")
  ) %>% 
  mutate(across(c("Sale_Price", contains(".pred")), ~log(.x, 10))) %>% 
  mutate(.resid = Sale_Price - .pred) %>% 
  mutate(pred_original = .pred) %>% 
  mutate(across(contains(".pred"), ~(.x - .pred))) %>% 
  ggplot(aes(x = pred_original,))+
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper, colour = "pred interval"), alpha = 0.3)+
  geom_point(aes(y = .resid, colour = covered))+
  theme_bw() +
  theme_void()+ 
  theme(legend.position = "none")

# Undersampling
library(tidyverse)

devtools::source_gist("https://gist.github.com/noamross/a549ee50e8a4fd68b8b1")

source_rmd("https://raw.githubusercontent.com/brshallo/brshallo/master/content/post/2020-11-23-remember-resampling-techniques-change-the-base-rates-of-your-predictions.Rmd", skip_plots = TRUE)

classadjust <- function(condprobs, wrongprob, trueprob) {
  a <- condprobs / (wrongprob / trueprob)
  comp_cond <- 1 - condprobs
  comp_wrong <- 1 - wrongprob
  comp_true <- 1 - trueprob
  b <- comp_cond / (comp_wrong / comp_true)
  return(a / (a + b))
}

offset_intercept <- function(true_baserate, sample_baserate){
  log((base_rate / (1 - base_rate)) * ((1 - sample_baserate) / sample_baserate))
}

lodds_to_prob <- function(x) exp(x) / (exp(x) + 1)

base_rate <- summarise(train, prob = sum(target) / n()) %>% pull(prob)

offset <- offset_intercept(base_rate, 0.5)

test_preds_scaling_approaches <- test %>% 
  # preds when no resampling
  modelr::spread_predictions(mod_5_95, mod_50_50) %>% 
  mutate(pred = mod_50_50) %>%
  # preds when resampling then platt scaling
  spread_predictions(mod_50_50_rescaled_calibrated) %>%
  select(-pred) %>% 
  # preds when using intercept offset
  mutate(mod50_offset = mod_50_50 + offset) %>% 
  mutate(across(contains("mod"), list(pred = convert_lodds))) %>% 
  # preds when adjusting
  mutate(mod50_adjust_pred = classadjust(mod_50_50_pred, 0.50, base_rate)) %>% 
  rename(
    formula_adjusted = mod50_adjust_pred,
    offset = mod50_offset_pred,
    platt_scaled = mod_50_50_rescaled_calibrated_pred,
    unaltered = mod_5_95_pred
  ) %>% 
  mutate(actual = lodds_to_prob(feature))

test_preds_scaling_approaches %>% 
  ggplot(aes(x = feature))+
  geom_line(aes(y = formula_adjusted, colour = "adjusted (after downsample)"))+
  geom_line(aes(y = offset, colour = "offset (after downsample)"), linetype = "dashed")+
  geom_line(aes(y = platt_scaled, colour = "platt scaled (after downsample)"))+
  geom_line(aes(y = unaltered, colour = "unaltered (no downsampling)"), linetype = "dashed")+
  geom_line(aes(y = actual, colour = "actual probability"))+
  theme_void()+ 
  theme(legend.position = "none")
