########################################################################################
# KL exacte vs entropie croisée estimée
######################################################################################

library(tidyverse)

# 1) "vraie distribution" p : Normal(0, 1)
n <- 1000
x <- rnorm(n = n, mean = 0, sd = 1)

# 2) Modèles candidats q_k = Normal(mu_k, sd_k)
models <- data.frame(
    model = c("q1", "q2", "q3", "q4"),
    mu = c(0, 0, 2, 0),
    sd = c(1, 1.2, 1, 0.7)
    )

# 3) KL exacte entre deux normales: p=N(m0,s0), q=N(m1,s1)
KL_normal <- function (m0, s0, m1, s1) {
    
    # KL(p||q) = log(s1/s0) + (s0^2 + (m0-m1)^2)/(2*s1^2) - 1/2
    log(s1 / s0) + (s0^2 + (m0 - m1)^2) / (2 * s1^2) - 0.5
    
}

# Entropie croisée "exacte" H(p,q) pour deux normales
# H(p,q) = -E_p[log q(X)] = 0.5*log(2π s1^2) + (s0^2 + (m0-m1)^2)/(2 s1^2)
H_cross_normal <- function (m0, s0, m1, s1) {
    
    0.5 * log(2 * pi * s1^2) + (s0^2 + (m0 - m1)^2) / (2 * s1^2)
    
}

# 4) KL exacte et entropie croisée estimée (moyenne empirique)
m0 <- 0
s0 <- 1

res <- within(models, {
    # KL exacte
    KL_exact <- mapply(KL_normal, m0, s0, mu, sd)
    
    # H(p,q) exacte
    H_exact  <- mapply(H_cross_normal, m0, s0, mu, sd)
    
    # Estimation empirique de H(p,q)
    H_hat <- sapply(seq_along(mu), function (i) {
        -mean(dnorm(x, mean = mu[i], sd = sd[i], log = TRUE) )
    })
    
    # "Negative log-score"
    neg_log_score <- H_hat * n
})

# 5) Plot KL (x) vs entropie croisée estimée (y)
res %>%
    ggplot(aes(x = KL_exact, y = H_hat, label = model) ) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", colour = "black", se = FALSE) +
    geom_label(nudge_y = 0.1) +
    labs(
        x = "KL divergence exacte",
        y = "Entropie croisée estimée)"
        ) +
    theme_bw(base_size = 12)

# 6) Plot KL (x) vs negative log-score (y)
res %>%
    ggplot(aes(x = KL_exact, y = neg_log_score, label = model) ) +
    geom_point(size = 3) +
    geom_line() +
    geom_label(nudge_y = 0.02 * diff(range(res$neg_log_score) ) ) +
    labs(
        x = "KL Divergence (exacte)",
        y = "Negative log-score"
        ) +
    theme_bw(base_size = 12)

# 7) Vérification théorique
H_p <- 0.5 * log(2 * pi * exp(1) * s0^2)

# on affiche tout
res %>%
    mutate(
        H_theory = H_p + KL_exact,
        err = H_hat - H_theory
        )
