# plumber.R

#* Calcula EAP e retorna curva
#* @param sample String binário de 0 e 1
#* @param area LC, CH, CN ou MT
#* @param ano Ano da prova
#* @param codigo Código da prova
#* @param lingua Tipo de lingua (0=Inglês, 1=Espanhol)
#* @get /calc
function(sample, area, ano, codigo, lingua) {
  tryCatch({
    
    dir_base <- dirname(normalizePath("plumber.R"))
    
    # Carrega arquivos com caminho absoluto
    load(file.path(dir_base, "constantes.rda"))
    nome_itens <- paste0("itens_", ano)
    load(file.path(dir_base, paste0(nome_itens, ".rda")))
    itens_db_total <- get(nome_itens)
    
    cod_prova <- codigo
    tp_lingua <- as.numeric(lingua)
    
    # Grade de theta
    theta <- seq(-4, 4, length.out = 100)  # mais pontos para precisão
    cci_3pl <- function(theta, a, b, c) c + ((1 - c) / (1 + exp(-a * (theta - b))))
    p_theta <- stats::dnorm(theta, mean = 0, sd = 1)
    
    # Converte string em vetor numérico
    score_i <- as.numeric(strsplit(sample, "")[[1]])
    
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
    
    # Calcula log-verossimilhança para estabilidade numérica
    n_itens <- min(length(score_i), nrow(pars))
    logL <- rep(0, length(theta))
    for (q in 1:n_itens) {
      res <- score_i[q]
      p_item <- pars[q, ]
      if (is.na(res) || is.na(p_item$NU_PARAM_A)) next
      p1 <- cci_3pl(theta, p_item$NU_PARAM_A, p_item$NU_PARAM_B, p_item$NU_PARAM_C)
      p1 <- pmin(pmax(p1, 1e-10), 1 - 1e-10)  # evita log(0)
      logL <- logL + if (res == 1) log(p1) else log(1 - p1)
    }
    L_theta <- exp(logL)
    
    # Posterior
    posterior <- L_theta * p_theta
    posterior <- posterior / sum(posterior)
    
    # EAP
    theta_EAP <- sum(theta * posterior)
    k_val <- constantes[constantes$area == area, "k"]
    d_val <- constantes[constantes$area == area, "d"]
    eap_transf <- round(theta_EAP * k_val + d_val, 1)
    
    # Retorna curva + EAP
    list(
      theta = theta,
      posterior = posterior,
      eap = eap_transf
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}
