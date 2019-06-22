tabela_resultados <- function(modelo, caption = "\\label{tab:modelo}Resultados do Modelo.") {
  # precisa de kableExtra, knitr, broom e dplyr
  broom::tidy(modelo, conf.int = TRUE) %>% 
    dplyr::select(-statistic) %>%
    dplyr::mutate(p.value = format.pval(p.value, eps = 0.001, digits = 2)) %>% 
    knitr::kable(
      caption = caption,
      col.names = c("Termo", "Estimativa", "EP", "p-valor", "Inf.", "Sup."),
      booktabs = T
      ) %>%
    kableExtra::add_header_above(c(" ", " ", " ", " ", "IC(95%)" = 2))
}