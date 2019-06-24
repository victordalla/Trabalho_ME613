# fit.model: saída do modelo ajustado
# m.C: matriz C relativa às hipóteses de interesse
# m.M : matriz M relativa às hipóteses de interesse
testeF.CBM <- function(fit.model,m.C,m.M)
{
    v.beta <- cbind(fit.model$coef) # vetor com a estimativa dos parâmetros
    n <- nrow(model.matrix(fit.model)) # número de observa??es
    e.p <- nrow(v.beta) # número de parâmetros
    e.q <- nrow(m.C)  # número de linhas da matriz C
    m.cov.beta <- (vcov(fit.model)) # matriz de covariâncias dos parâmetros do modelo
    # Estatística do teste
    e.F <- t(m.C %*% v.beta -m.M) %*%
        solve(m.C %*% m.cov.beta %*% t(m.C)) %*%
        (m.C %*% v.beta-m.M) / e.q
    e.pvalor <- 1 - pf(e.F, e.q, n - e.p) # p-valor
    #cat("Estatistica F = ",round(e.F,2),"\n")
    #cat("pvalor = ",round(e.pvalor,4),"\n")
    #cat("Matriz C :","\n")
    #print(m.C)
    #cat("Matriz M :","\n")
    #print(m.M)
    return(e.pvalor)
}

