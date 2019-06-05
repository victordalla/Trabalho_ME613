plot_prediction <- function(model, values, value_name = "value") {
  # precisa de dplyr, broom, ggplot2
  
  p <- predict.lm(model, interval = "prediction") %>% 
    dplyr::as_tibble() %>% mutate(values = values) %>% 
    ggplot(aes(fit, values)) + 
    geom_point(col = "cadetblue") + 
    geom_line(aes(y = fit), col = "chocolate") + 
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "coral", alpha = 0.3) + 
    labs(x = "predição", y = value_name)
  theme_classic()
  p
}

plot_residuals <- function(model) {
  # precisa de ggplot2, gridExtra, qqplotr
  
  residuals <- data.frame(
    residual = rstandard(model), 
    fitted = model$fitted.values, 
    index = 1:length(model$fitted.values)
  )
  p <- ggplot(residuals) + labs(y = "resíduo") + theme_classic()
  gridExtra::grid.arrange(
    p + geom_point(aes(x = index, y = residual), col = "gray30", alpha = 0.80) + labs(x = "índice"), 
    p + geom_point(aes(x = fitted, y = residual), col = "gray30", alpha = 0.80) + labs(x = "índice"), 
    p + geom_histogram(aes(x = residual), fill = "gray30", col = "gray80"), 
    ggplot(residuals, aes(sample = residual)) + 
      qqplotr::stat_qq_band(bandType = "pointwise") + qqplotr::stat_qq_line() + qqplotr::stat_qq_point() + 
      labs(x = "quantil teórico", y = "quantil amostral") + theme_classic(), 
    nrow = 2
  )
}
