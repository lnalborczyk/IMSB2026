## ----setup, eval = TRUE, include = FALSE, cache = FALSE--------------------------------------------------------
library(tidyverse)
library(patchwork)
library(brms)
library(imsb)

# setting up knitr options
knitr::opts_chunk$set(
  cache = TRUE, echo = TRUE,
  warning = FALSE, message = FALSE,
  fig.align = "center", dev = "svg"
  )

# setting up ggplot theme
theme_set(theme_bw(base_size = 16, base_family = "Open Sans") )


## ----echo = FALSE, out.width = "300px"-------------------------------------------------------------------------
knitr::include_graphics("figures/robot.png")


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
library(tidyverse)
library(imsb)

df <- open_data(robot)
slice_sample(df, n = 15) %>% arrange(cafe, afternoon)


## ----eval = TRUE, echo = TRUE, fig.width = 15, fig.height = 5--------------------------------------------------
df %>%
    ggplot(aes(x = factor(cafe), y = wait, fill = factor(afternoon) ) ) +
    geom_dotplot(
        stackdir = "center", binaxis = "y",
        dotsize = 1, show.legend = FALSE
        ) +
    geom_hline(yintercept = mean(df$wait), linetype = 2) +
    facet_wrap(~afternoon, ncol = 2) +
    labs(x = "Café", y = "Temps d'attente (en minutes)")


## ----eval = TRUE, echo = TRUE, ig.width = 7.5, fig.height = 5--------------------------------------------------
ggplot(data = data.frame(x = c(0, 10) ), aes(x = x) ) +
    stat_function(
        fun = dcauchy,
        args = list(location = 0, scale = 2), size = 1.5
        )


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
library(brms)

mod1 <- brm(
  formula = wait ~ 1,
  prior = c(
    prior(normal(5, 10), class = Intercept),
    prior(cauchy(0, 2), class = sigma)
    ),
  data = df,
  chains = 4, cores = 4
  )


## ----eval = TRUE, echo = TRUE, warning = FALSE-----------------------------------------------------------------
posterior_summary(x = mod1, probs = c(0.025, 0.975), pars = c("^b_", "sigma") )


## ----eval = TRUE, echo = TRUE, fig.width = 14, fig.height = 7--------------------------------------------------
plot(
  x = mod1, combo = c("dens_overlay", "trace"),
  theme = theme_bw(base_size = 16, base_family = "Open Sans")
  )


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
mod2 <- brm(
  formula = wait ~ 0 + factor(cafe),
  prior = c(
    prior(normal(5, 10), class = b),
    prior(cauchy(0, 2), class = sigma)
    ),
  data = df,
  chains = 4, cores = 4
  )


## ----eval = TRUE, echo = TRUE, warning = FALSE-----------------------------------------------------------------
posterior_summary(x = mod2, pars = "^b_")


## ----eval = TRUE, echo = TRUE, out.width = "33%"---------------------------------------------------------------
y1 <- rnorm(n = 1e4, mean = 5, sd = 1)
y2 <- rnorm(n = 1e4, mean = 0, sd = 1) + 5

data.frame(y1 = y1, y2 = y2) %>%
    pivot_longer(cols = 1:2, names_to = "x", values_to = "y") %>%
    ggplot(aes(x = y, colour = x) ) +
    geom_density(show.legend = FALSE)


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
mod3 <- brm(
  formula = wait ~ 1 + (1 | cafe),
  prior = c(
    prior(normal(5, 10), class = Intercept),
    prior(cauchy(0, 2), class = sigma),
    prior(cauchy(0, 2), class = sd)
    ),
  data = df,
  warmup = 1000, iter = 5000,
  chains = 4, cores = 4
  )


## ----echo = FALSE, fig.width = 14, fig.height = 8--------------------------------------------------------------
library(wesanderson) # for plotting
post <- as_draws_df(mod3) # extracts posterior samples

df %>%
    group_by(cafe) %>%
    summarise(Observed = mean(wait) ) %>%
    mutate(Estimated = coef(mod3)$cafe[, , ] %>% data.frame %>% pull(Estimate) ) %>%
    gather(type, Observed, Observed:Estimated) %>%
    ggplot(aes(x = cafe, y = Observed, fill = type) ) +
    geom_hline(yintercept = mean(post$b_Intercept), linetype = 2) +
    geom_point(pch = 21, size = 5, alpha = 0.8, colour = "white", show.legend = TRUE) +
    scale_color_manual(values = rev(wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_fill_manual(values = rev(wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_x_continuous(name = "Café", breaks = 1:20) +
    ylab("Temps d'attente (en minutes)") +
    theme(legend.title = element_blank() )


## ----echo = FALSE, fig.align = "center", out.width = "66%"-----------------------------------------------------
knitr::include_graphics("figures/stein1.png")


## ----echo = FALSE, fig.align = "center", out.width = "75%"-----------------------------------------------------
knitr::include_graphics("figures/stein2.png")


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
# calcul du WAIC et ajout du WAIC à chaque modèle
mod1 <- add_criterion(mod1, "waic")
mod2 <- add_criterion(mod2, "waic")
mod3 <- add_criterion(mod3, "waic")

# comparaison des WAIC de chaque modèle
w <- loo_compare(mod1, mod2, mod3, criterion = "waic")
print(w, simplify = FALSE)


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
posterior_summary(mod1, pars = c("^b", "sigma") )
posterior_summary(mod3, pars = c("^b", "sigma") )


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
df2 <- open_data(robot_unequal) # nouveau jeu de données

mod4 <- brm(
  formula = wait ~ 1 + (1 | cafe),
  prior = c(
    prior(normal(5, 10), class = Intercept),
    prior(cauchy(0, 2), class = sigma),
    prior(cauchy(0, 2), class = sd)
    ),
  data = df2,
  warmup = 1000, iter = 5000,
  chains = 4, cores = 4
  )


## ----echo = FALSE, fig.width = 12, fig.height = 6--------------------------------------------------------------
post <- as_draws_df(mod4)

df2 %>%
    group_by(cafe) %>%
    summarise(Observed = mean(wait) ) %>%
    mutate(Estimated = coef(mod4)$cafe[, , ] %>% data.frame %>% pull(Estimate) ) %>%
    gather(type, Observed, Observed:Estimated) %>%
    ggplot(aes(x = cafe, y = Observed, fill = type) ) +
    geom_hline(yintercept = mean(post$b_Intercept), linetype = 2) +
    geom_point(pch = 21, size = 5, alpha = 0.8, colour = "white", show.legend = TRUE) +
    scale_color_manual(values = rev(wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_fill_manual(values = rev(wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_x_continuous(name = "Café (du moins visité au plus visité)", breaks = 1:20) +
    ylab("Temps d'attente (en minutes)") +
    theme(legend.title = element_blank() )


## ----echo = FALSE, out.width = "800px"-------------------------------------------------------------------------
knitr::include_graphics("figures/bivariate.png")


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
sigma_a <- 1
sigma_b <- 0.75
rho <- 0.7
cov_ab <- sigma_a * sigma_b * rho
(Sigma1 <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), ncol = 2) )


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
(sigmas <- c(sigma_a, sigma_b) ) # standard deviations
(Rho <- matrix(c(1, rho, rho, 1), nrow = 2) ) # correlation matrix
(Sigma2 <- diag(sigmas) %*% Rho %*% diag(sigmas) )


## ----echo = FALSE, fig.width = 14, fig.height = 7, cache = TRUE------------------------------------------------
library(ggdist)

expand.grid(eta = c(0.5, 2, 5, 10), K = c(2, 3, 4, 5) ) %>%
  ggplot(
      aes(
          y = ordered(eta), dist = "lkjcorr_marginal",
          arg1 = K, arg2 = eta, fill = as.factor(eta)
          )
      ) +
  stat_dist_slab(p_limits = c(0, 1), alpha = 0.8) +
  facet_grid(~paste0(K, "x", K) ) +
  labs(x = expression(rho), y = "Densité de probabilité (par prior)") +
  scale_fill_manual(
      values = c("steelblue", "orangered", "purple", "darkgreen"),
      labels = c(
        expression(paste(zeta, " = ", "0.5") ),
        expression(paste(zeta, " = ", "2") ),
        expression(paste(zeta, " = ", "10") ),
        expression(paste(zeta, " = ", "50") )
        )
      ) +
    theme(
        legend.title = element_blank(),
        legend.text.align = 0,
        legend.background = element_rect(size = 0.5, colour = "black")
        )


## ----eval = FALSE, echo = TRUE---------------------------------------------------------------------------------
# Reaction ~ Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE---------------------------------------------------------------------------------
# Reaction ~ Days + (1 + Days | Subject)
# Reaction ~ 1 + Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE---------------------------------------------------------------------------------
# Reaction ~ 1 + Days + (1 | Subject)
# Reaction ~ 1 + Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE---------------------------------------------------------------------------------
# Reaction ~ Days + (1 + Days || Subject)


## ----eval = FALSE, echo = TRUE---------------------------------------------------------------------------------
# brm(formula = Reaction ~ 1 + Days + (1 + Days | Subject), family = lognormal() )


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
mod5 <- brm(
  formula = wait ~ 1 + afternoon + (1 + afternoon | cafe),
  prior = c(
    prior(normal(0, 10), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 2), class = sigma),
    prior(cauchy(0, 2), class = sd)
    ),
  data = df,
  warmup = 1000, iter = 5000,
  chains = 4, cores = 4
  )


## ----eval = TRUE, echo = TRUE, fig.width = 9, fig.height = 6---------------------------------------------------
post <- as_draws_df(x = mod5) # extracts posterior samples
R <- rethinking::rlkjcorr(n = 16000, K = 2, eta = 2) # samples from prior

data.frame(prior = R[, 1, 2], posterior = post$cor_cafe__Intercept__afternoon) %>%
    gather(type, value, prior:posterior) %>%
    ggplot(aes(x = value, color = type, fill = type) ) +
    geom_histogram(position = "identity", alpha = 0.2) +
    labs(x = expression(rho), y = "Nombre d'échantillons")


## ----eval = TRUE, echo = FALSE, fig.width = 12, fig.height = 8-------------------------------------------------
a1 <- sapply(1:20, function(i) mean(df$wait[df$cafe == i & df$afternoon == 0]) )
b1 <- sapply(1:20, function(i) mean(df$wait[df$cafe == i & df$afternoon == 1]) ) - a1

no_pooling <-
  data.frame(Intercept = a1, afternoon = b1) %>%
  mutate(model = "no pooling")

partial_pooling <-
  data.frame(coef(mod5)$cafe[, 1, 1:2]) %>%
  mutate(model = "partial pooling")

shrinkage <- bind_rows(no_pooling, partial_pooling)

mu <- c(mean(post$b_Intercept), mean(post$b_afternoon) )
rho <- mean(post$cor_cafe__Intercept__afternoon)
sda <- mean(post$sd_cafe__Intercept)
sdb <- mean(post$sd_cafe__afternoon)
cov_ab <- sda * sdb * rho
sigma <- matrix(c(sda^2, cov_ab, cov_ab, sdb^2), ncol = 2)

##############################################################################
# Helper function to make ellipse, credits to Tristan Mahr                   #
# https://tjmahr.github.io/plotting-partial-pooling-in-mixed-effects-models/ #
##############################################################################

library(ellipse)

make_ellipse <- function(cov_mat, center, level) {
    
    ellipse(cov_mat, centre = center, level = level) %>%
        as.data.frame() %>%
        add_column(level = level)
    
}

levels <- c(.1, .3, .5, .7)

df_ellipse <-
    levels %>%
    purrr::map_df(~ make_ellipse(sigma, mu, level = .x) ) %>% 
    rename(Intercept = x, afternoon = y)

shrinkage %>%
    mutate(id = rep(1:20, 2) ) %>%
    ggplot(aes(x = Intercept, y = afternoon, color = model) ) +
    scale_color_manual(values = wesanderson::wes_palette(n = 2, name = "Chevalier1") ) +
    geom_point(size = 5, show.legend = FALSE) +
    # connecting lines
    geom_path(
        aes(group = id, color = NULL),
        arrow = arrow(length = unit(.015, "npc"), type = "closed"), 
        show.legend = FALSE
        ) +
    # ellipses
    geom_path(
        aes(group = level, color = NULL),
        data = df_ellipse,
        linetype = "dashed", color = "grey40", alpha = 0.8
        ) +
    labs(x = "Intercept", y = "Slope")


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
# comparaison des WAIC de chaque modèle
mod5 <- add_criterion(mod5, "waic")
w <- loo_compare(mod1, mod2, mod3, mod5, criterion = "waic")
print(w, simplify = FALSE)
model_weights(mod1, mod2, mod3, mod5, weights = "waic")


## ----eval = TRUE, echo = TRUE, warning = FALSE-----------------------------------------------------------------
posterior_summary(mod1, pars = c("^b", "sigma") )
posterior_summary(mod3, pars = c("^b", "sigma") )
posterior_summary(mod5, pars = c("^b", "sigma") )


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
library(tidyverse)
library(imsb)

# import des données
absence_data <- open_data(absence_multilevel)

# on affiche 12 lignes "au hasard" dans ces données
absence_data %>% slice_sample(prop = 1) %>% head()


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
prior6 <- c(
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(cauchy(0, 1), class = sd),
    prior(lkj(2), class = cor)
    )


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
mod6 <- brm(
    formula = presence | trials(total) ~ 1 + reminder + (1 + reminder | researcher), 
    family = binomial(link = "logit"),
    prior = prior6,
    data = absence_data,
    sample_prior = TRUE,
    warmup = 2000, iter = 10000,
    chains = 4, cores = 4,
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr"
    )


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6--------------------------------------------------
mod6 %>%
    plot(
        combo = c("dens_overlay", "trace"), pars = c("^b_", "^cor_"), widths = c(1, 1.5),
        theme = theme_bw(base_size = 16, base_family = "Open Sans")
        )


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
posterior_summary(x = mod6, pars = c("^b_", "^sd_") )


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
a <- fixef(mod6)[1] # on récupère la valeur de l'intercept
exp(a) / (1 + exp(a) ) # on "convertit" l'intercept en probabilité (équivalent à plogis(a))


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
fixef(mod6)[2, c(1, 3, 4)] %>% exp()


## ----echo = FALSE, eval = TRUE, message = FALSE, warning = FALSE, fig.width = 12, fig.height = 7---------------
library(tidybayes)
library(modelr)

absence_data %>%
    group_by(researcher, total) %>%
    data_grid(reminder = seq_range(reminder, n = 1e2) ) %>%
    add_fitted_samples(mod6, newdata = ., n = 100, scale = "linear") %>%
    mutate(estimate = plogis(estimate) ) %>%
    group_by(reminder, .iteration) %>%
    summarise(estimate = mean(estimate) ) %>%
    ggplot(aes(x = reminder, y = estimate, group = .iteration) ) +
    geom_hline(yintercept = 0.5, lty = 2) +
    geom_line(aes(y = estimate, group = .iteration), size = 0.5, alpha = 0.1) +
    labs(x = "Mail de rappel", y = "Pr(présent)")


## ----echo = TRUE, eval = TRUE, message = FALSE, warning = FALSE, fig.width = 16, fig.height = 6----------------
absence_data %>%
    group_by(researcher, total) %>%
    data_grid(reminder = seq_range(reminder, n = 1e2) ) %>%
    add_linpred_draws(object = mod6, newdata = ., ndraws = 200) %>%
    mutate(estimate = plogis(.linpred) ) %>%
    ggplot(aes(x = reminder, y = estimate, group = .draw) ) +
    geom_hline(yintercept = 0.5, lty = 2) +
    geom_line(aes(y = estimate, group = .draw), size = 0.5, alpha = 0.1) +
    facet_wrap(~researcher, nrow = 2) +
    labs(x = "Mail de rappel", y = "Pr(présent)")


## ----echo = TRUE-----------------------------------------------------------------------------------------------
(hyp1 <- hypothesis(x = mod6, hypothesis = "reminder = 0") ) # Savage-Dickey Bayes factor
1 / hyp1$hypothesis$Evid.Ratio # BF10 = 1 / BF01 (and BF01 = 1 / BF10)


## ----echo = TRUE, fig.width = 10, fig.height = 7---------------------------------------------------------------
plot(hyp1, plot = FALSE, theme = theme_bw(base_size = 20, base_family = "Open Sans") )[[1]] +
  geom_vline(xintercept = 0, linetype = 2) +
  coord_cartesian(xlim = c(-5, 5) )


## ----echo = TRUE, fig.width = 10, fig.height = 7---------------------------------------------------------------
library(bayestestR)
bf <- bayesfactor_parameters(posterior = mod6, null = 0)
plot(bf)


## ----echo = TRUE, fig.width = 14, fig.height = 6---------------------------------------------------------------
data.frame(prior = hyp1$prior_samples$H1, posterior = hyp1$samples$H1) %>%
    gather(type, value) %>%
    mutate(type = factor(type, levels = c("prior", "posterior") ) ) %>%
    ggplot(aes(x = value) ) +
    geom_histogram(bins = 50, alpha = 0.8, col = "white", fill = "steelblue") +
    geom_vline(xintercept = 0, lty = 2, size = 1) +
    facet_wrap(~type, scales = "free") +
    labs(x = expression(beta[reminder]), y = "Nombre d'échantillons")


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
prior7 <- c(
    prior(normal(0, 10), class = Intercept, coef = ""),
    prior(exponential(1), class = sd),
    prior(lkj(2), class = cor)
    )

mod7 <- brm(
    presence | trials(total) ~ 1 + reminder + (1 + reminder | researcher), 
    family = binomial(link = "logit"),
    prior = prior7,
    data = absence_data,
    # this line is important for bridgesampling
    save_pars = save_pars(all = TRUE),
    warmup = 2000, iter = 1e4, cores = 4,
    control = list(adapt_delta = 0.95) )

mod8 <- brm(
    presence | trials(total) ~ 1 + (1 + reminder | researcher), 
    family = binomial(link = "logit"),
    prior = prior7,
    data = absence_data,
    save_pars = save_pars(all = TRUE),
    warmup = 2000, iter = 1e4, cores = 4,
    control = list(adapt_delta = 0.95) )


## ----eval = FALSE, echo = TRUE---------------------------------------------------------------------------------
# # returns the median BF based on 10 repetitions of the algorithm
# bayes_factor(mod7, mod8, repetitions = 10, cores = 10)


## ----eval = TRUE, echo = FALSE, results = "hide"---------------------------------------------------------------
bf <- bayes_factor(mod7, mod8, repetitions = 10, cores = 10)


## ----eval = TRUE, echo = FALSE---------------------------------------------------------------------------------
bf


## ----echo = TRUE, eval = TRUE----------------------------------------------------------------------------------
waic(mod7, mod8, compare = FALSE)


## ----echo = TRUE, fig.width = 12, fig.height = 6---------------------------------------------------------------
absence_data %>%
    ggplot(aes(x = presence / total) ) +
    geom_density(fill = "grey20")


## ----echo = TRUE, fig.width = 12, fig.height = 6---------------------------------------------------------------
pp_check(object = mod7, ndraws = 1e2)


## ----echo = TRUE, fig.width = 12, fig.height = 8---------------------------------------------------------------
pp_check(object = mod7, ndraws = 1e3, type = "stat_2d")


## ----eval = FALSE, echo = TRUE---------------------------------------------------------------------------------
# mod9 <- brm(
#     formula = presence | trials(total) ~ 1 + reminder + (1 + reminder | researcher),
#     family = binomial(link = "logit"),
#     data = absence_data,
#     warmup = 2000, iter = 1e4,
#     chains = 4, cores = 4,
#     control = list(adapt_delta = 0.95) # adjusting the delta step size
#     )


## ----echo = FALSE, out.width = "66%"---------------------------------------------------------------------------
knitr::include_graphics("figures/bayes_workflow_1.png")


## ----echo = FALSE, out.width = "50%"---------------------------------------------------------------------------
knitr::include_graphics("figures/bayes_workflow_2.png")


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
library(lme4)
data(sleepstudy)
head(sleepstudy, 20)


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6--------------------------------------------------
sleepstudy %>%
    ggplot(aes(x = Days, y = Reaction) ) +
    geom_smooth(method = "lm", colour = "black") +
    geom_point() +
    facet_wrap(~Subject, nrow = 2) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8) )


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
fmod0 <- lm(Reaction ~ Days, sleepstudy)
fmod1 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
fmod2 <- lmer(Reaction ~ Days + (1 + Days | Subject), sleepstudy)

anova(fmod1, fmod2)


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
mod10 <- brm(
  Reaction ~ 1 + Days,
  prior = c(
    prior(normal(200, 100), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sigma)
    ),
  data = sleepstudy,
  warmup = 1000, iter = 5000,
  chains = 4, cores = 4
  )


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
posterior_summary(mod10)


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
mod11 <- brm(
  Reaction ~ 1 + Days + (1 | Subject),
  prior = c(
    prior(normal(200, 100), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sigma),
    prior(cauchy(0, 10), class = sd)
    ),
  data = sleepstudy,
  warmup = 1000, iter = 5000,
  chains = 4, cores = 4
  )


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
posterior_summary(mod11, pars = c("^b", "sigma") )


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
mod12 <- brm(
  Reaction ~ 1 + Days + (1 + Days | Subject),
  prior = c(
    prior(normal(200, 100), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sigma),
    prior(cauchy(0, 10), class = sd)
    ),
  data = sleepstudy,
  warmup = 1000, iter = 5000,
  chains = 4, cores = 4
  )


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
posterior_summary(mod12, pars = c("^b", "sigma") )


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
# calcul du WAIC et ajout du WAIC à chaque modèle
mod10 <- add_criterion(mod6, "waic")
mod11 <- add_criterion(mod7, "waic")
mod12 <- add_criterion(mod8, "waic")

# comparaison des WAIC de chaque modèle
w <- loo_compare(mod10, mod11, mod12, criterion = "waic")
print(w, simplify = FALSE)

# calcul du poids relatif de chaque modèle
model_weights(mod10, mod11, mod12, weights = "waic")


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
d <- open_data(popular)
head(d, 10)


## ----echo = FALSE, out.width = "500px"-------------------------------------------------------------------------
knitr::include_graphics("figures/cat.gif")


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6--------------------------------------------------
d %>%
    ggplot(aes(x = popular) ) +
    geom_histogram() +
    facet_wrap(~sex) +
    scale_x_continuous(breaks = 1:10, limits = c(1, 10) )


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6--------------------------------------------------
d %>%
    ggplot(aes(x = texp, y = popular) ) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "lm", colour = "black") +
    facet_wrap(~sex)


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
d <- d %>%
    mutate(
        # using a sum contrast for gender
        sex = ifelse(sex == "boy", -0.5, 0.5),
        # centering and standardising teacher experience
        texp = scale(texp) %>% as.numeric
        )

prior13 <- c(
    prior(normal(5, 2.5), class = Intercept),
    prior(cauchy(0, 10), class = sd),
    prior(cauchy(0, 10), class = sigma)
    )

mod13 <- brm(
    formula = popular ~ 1 + (1 | school),
    data = d,
    prior = prior13,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4, cores = 4
    )


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
prior14 <- c(
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(cauchy(0, 1), class = sd),
    prior(cauchy(0, 10), class = sigma)
    )

mod14 <- brm(
    formula = popular ~ 1 + texp + (1 | school),
    data = d,
    prior = prior14,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4, cores = 4
    )


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
prior15 <- c(
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(cauchy(0, 1), class = sd),
    prior(cauchy(0, 10), class = sigma),
    prior(lkj(2), class = cor)
    )

mod15 <- brm(
    formula = popular ~ 1 + sex + texp + (1 + sex | school),
    data = d,
    prior = prior15,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4, cores = 4
    )


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
mod16 <- brm(
    formula = popular ~ 1 + sex + texp + sex:texp + (1 + sex | school),
    data = d,
    prior = prior13,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4, cores = 4
    )


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
# calcul du WAIC et ajout du WAIC à chaque modèle
mod13 <- add_criterion(mod13, "waic")
mod14 <- add_criterion(mod14, "waic")
mod15 <- add_criterion(mod15, "waic")
mod16 <- add_criterion(mod16, "waic")


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
# comparaison des WAIC de chaque modèle
model_comparison_table <- loo_compare(mod13, mod14, mod15, mod16, criterion = "waic") %>%
  data.frame() %>%
  rownames_to_column(var = "model")

weights <- data.frame(weight = model_weights(mod13, mod14, mod15, mod16, weights = "waic") ) %>%
  round(digits = 3) %>%
  rownames_to_column(var = "model")

left_join(model_comparison_table, weights, by = "model")


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5--------------------------------------------------
pp_check(object = mod16, ndraws = 1e2)


## ----eval = TRUE, echo = FALSE, fig.width = 14, fig.height = 6-------------------------------------------------
library(patchwork)

p1 <-
  d %>% 
  ggplot(aes(x = popular, fill = ..x..) ) +
  geom_histogram(binwidth = 0.5, size = 0) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10) ) +
  labs(x = "Popularité", y = "Nombre de réponses") +
  theme_bw(base_size = 16, base_family = "Open Sans") +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = "none"
    )

p2 <-
  d %>%
  count(popular) %>%
  mutate(pr_k = n / nrow(d), cum_pr_k = cumsum(pr_k) ) %>% 
  ggplot(aes(x = popular, y = cum_pr_k, color = popular, fill = popular) ) +
  geom_line() +
  geom_point(shape = 21, color = "white", size = 2.5, stroke = 1) +
  labs(x = "Popularité", y = "Proportion cumulée") +
  theme_bw(base_size = 16, base_family = "Open Sans") +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10) ) +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = "none"
    )

p3 <-
  d %>%
  count(popular) %>%
  mutate(cum_pr_k = cumsum(n / nrow(d) ) ) %>% 
  filter(popular < 9) %>% 
  ggplot(aes(
    x = popular, y = log(cum_pr_k / (1 - cum_pr_k) ),
    color = popular, fill = popular
    ) ) +
  geom_line() +
  geom_point(shape = 21, colour = "white", size = 2.5, stroke = 1) +
  labs(x = "Popularité", y = "Log cote cumulée") +
  theme_bw(base_size = 16, base_family = "Open Sans") +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10) ) +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = "none"
    )

(p1 | p2 | p3)


## ----eval = TRUE, echo = FALSE, fig.width = 6, fig.height = 6--------------------------------------------------
d_plot <- d %>%
  count(popular) %>%
  mutate(pr_k = n / nrow(d), cum_pr_k = cumsum(n / nrow(d) ) ) %>%
  mutate(discrete_probability = ifelse(popular == 1, cum_pr_k, cum_pr_k - pr_k) )

text <- tibble(
  text = 2:9,
  popular = seq(from = 2.25, to = 9.25, by = 1),
  cum_pr_k = d_plot$cum_pr_k - 0.065
  )

d_plot %>% 
  ggplot(aes(x = popular, y = cum_pr_k, color = cum_pr_k, fill = cum_pr_k) ) +
  geom_line() +
  geom_point(shape = 21, colour = "white", size = 2.5, stroke = 1) +
  geom_linerange(aes(ymin = 0, ymax = cum_pr_k), alpha = 0.5) +
  geom_linerange(
    aes(
      x = popular + .025,
      ymin = ifelse(popular == 1, 0, discrete_probability),
      ymax = cum_pr_k),
    color = "black"
    ) +
  geom_text(data = text,aes(label = text), size = 4) +
  scale_x_continuous(breaks = 2:9) +
  labs(x = "Popularité", y = "Proportion cumulée") +
  theme_bw(base_size = 16, base_family = "Open Sans") +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = "none"
    )


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
mod17 <- brm(
    popular ~ 1 + sex + texp + sex:texp + (1 | school),
    data = d,
    prior = prior14,
    warmup = 1000, iter = 5000,
    chains = 4, cores = 4,
    threads = threading(threads = 2),
    file = "models/mod17", backend = "cmdstanr"
    )


## ----eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------
prior18 <- c(
    brms::prior(normal(0, 10), class = Intercept),
    brms::prior(normal(0, 10), class = b),
    brms::prior(cauchy(0, 10), class = sd)
    )

mod18 <- brm(
    popular ~ 1 + sex + texp + sex:texp + (1 | school),
    data = d,
    family = cumulative(link = "logit"),
    prior = prior18,
    chains = 4, cores = 4,
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    threads = threading(threads = 2),
    file = "models/mod18", backend = "cmdstanr"
    )


## ----eval = TRUE, echo = TRUE----------------------------------------------------------------------------------
waic(mod17, mod18, compare = FALSE)


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6--------------------------------------------------
pp_check(mod18, ndraws = 1e2, type = "bars", prob = 0.95, freq = FALSE) +
  scale_x_continuous(breaks = 1:9) +
  labs(x = "Popularité", y = "Proportion")

