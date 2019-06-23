plot_prediction <- function(model, response, response_name = "resposta") {
  # precisa de dplyr, ggplot2
  
  p <- predict.lm(model, interval = "prediction") %>% 
    dplyr::as_tibble() %>% dplyr::mutate(response = response) %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_line(aes(fit, fit), col = "chocolate") + 
    ggplot2::geom_point(aes(fit, response), col = "cadetblue") + 
    ggplot2::geom_ribbon(aes(x = fit, ymin = lwr, ymax = upr), fill = "coral", alpha = 0.3) + 
    ggplot2::labs(x = "predicao", y = response_name) + ggplot2::theme_bw()
  p
}

plot_residuals <- function(model, binwidth = NULL, bins = NULL) {
  # precisa de ggplot2, gridExtra, qqplotr
  
  residuals <- data.frame(
    residual = rstandard(model), 
    fitted = model$fitted.values, 
    index = 1:length(model$fitted.values)
  )
  
  p <- ggplot2::ggplot(residuals) + 
    ggplot2::labs(y = "residuo studentizado") + ggplot2::theme_bw()
  
  gridExtra::grid.arrange(
    p + ggplot2::geom_point(aes(x = index, y = residual), col = "gray30", alpha = 0.80) + 
      ggplot2::labs(x = "indice"), 
    p + ggplot2::geom_point(aes(x = fitted, y = residual), col = "gray30", alpha = 0.80) + 
      ggplot2::labs(x = "valores ajustados"), 
    p + ggplot2::geom_histogram(aes(x = residual), fill = "gray30", col = "gray80"), 
    ggplot2::ggplot(residuals, aes(sample = residual)) + 
      qqplotr::stat_qq_band(bandType = "pointwise") + 
      qqplotr::stat_qq_line() + 
      qqplotr::stat_qq_point(col = "gray20", alpha = 0.80) + 
      ggplot2::labs(x = "quantil teorico", y = "quantil amostral") + ggplot2::theme_bw(), 
    nrow = 2
  )
}