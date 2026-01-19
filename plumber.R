# plumber.R
source("functions.R")

#* Calcula EAP e retorna curva
#* @param sample String binário de 0 e 1
#* @param area LC, CH, CN ou MT
#* @param ano Ano da prova
#* @param codigo Código da prova
#* @param lingua Tipo de lingua (0=Inglês, 1=Espanhol)
#* @get /calc
function(sample, area, ano, codigo, lingua) {
  
  load("constantes.rda")
  nome_itens <- paste0("itens_", ano)
  load(nome_itens)
  itens_db_total <- get(nome_itens)
  
  cod_prova <- codigo
  tp_lingua <- as.numeric(lingua)
  
  theta <- seq(-4, 4, length.out = 40)
  cci_3pl <- function(theta, a, b, c) c + ((1 - c) / (1 + exp(-a * (theta - b))))
  p_theta <- stats::dnorm(theta, mean = 0, sd = 1)
  
  # Converte string em vetor numérico
  score_i <- as.numeric(unlist(strsplit(sample, "")))
  
  # Seleciona itens do caderno
  pars <- itens_db_total[itens_db_total$CO_PROVA == cod_prova, ]
  if (nrow(pars) == 0) stop("Caderno não encontrado")
  
  # Ordena itens
  if (ano > 2009) pars <- pars[order(pars$TP_LINGUA, pars$CO_POSICAO), ]
  else pars <- pars[order(pars$CO_POSICAO), ]
  
  # Filtra língua
  if (area == "LC") {
    if (tp_lingua == 1) pars <- pars[pars$TP_LINGUA != 0, ]
    else pars <- pars[pars$TP_LINGUA != 1, ]
  }
  
  # Remove itens anulados
  idx_anulados <- which(pars$IN_ITEM_ABAN == 1)
  if (length(idx_anulados) > 0) {
    score_i <- score_i[-idx_anulados]
    pars <- pars[-idx_anulados, ]
  }
  
  # Calcula likelihood
  n_itens <- min(length(score_i), nrow(pars))
  list_probs <- lapply(1:n_itens, function(q) {
    res <- score_i[q]
    p_item <- pars[q, ]
    if (is.na(res) || is.na(p_item$NU_PARAM_A)) return(rep(1, length(theta)))
    p1 <- cci_3pl(theta, p_item$NU_PARAM_A, p_item$NU_PARAM_B, p_item$NU_PARAM_C)
    if (res == 1) p1 else (1 - p1)
  })
  
  L_theta <- Reduce(`*`, list_probs)
  
  # Posterior
  posterior <- L_theta * p_theta
  posterior <- posterior / sum(posterior)
  
  # EAP
  theta_EAP <- sum(theta * posterior)
  k_val <- constantes[constantes$area == area, 'k']
  d_val <- constantes[constantes$area == area, 'd']
  eap_transf <- round(theta_EAP * k_val + d_val, 1)
  
  # Retorna curva + EAP
  list(
    theta = theta,
    posterior = posterior,
    eap = eap_transf
  )
}
