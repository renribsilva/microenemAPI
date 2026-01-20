calc <- function(sample, area, ano, codigo, lingua) {
  
  tryCatch({
    
    dir_base <- dirname(normalizePath("plumber.R"))
    
    # Carrega arquivos com caminho absoluto
    load(file.path(dir_base, "constantes.rda"))
    nome_itens <- paste0("itens_", ano)
    load(file.path(dir_base, paste0(nome_itens, ".rda")))
    itens_db_total <- get(nome_itens)
    
    theta <- seq(-4, 4, length.out = 40)
    cci_3pl <- function(theta, a, b, c) c + ((1 - c) / (1 + exp(-a * (theta - b))))
    p_theta <- stats::dnorm(theta, mean = 0, sd = 1)
    
    prod_prob <- list()
    
    # 1. PEGA O BANCO DO CADERNO E ORDENA
    pars <- itens_db_total[itens_db_total$CO_PROVA == codigo, ]
    
    if (nrow(pars) == 0) { prod_prob <- NULL; next }
    
    if (area == "LC") {
      if (lingua == 1) { # ESPANHOL
        # manter apenas TP_LINGUA != 0 e não NA
        pars <- pars[(pars$TP_LINGUA == 1 | is.na(pars$TP_LINGUA)), ]
      } else { # INGLÊS
        # manter apenas TP_LINGUA != 1 e não NA
        pars <- pars[(pars$TP_LINGUA == 0 | is.na(pars$TP_LINGUA)), ]
      }
    }
    
    # Ordenação inicial para garantir Inglês/Espanhol
    if (ano > 2009) {
      pars <- pars[base::order(pars$TP_LINGUA, pars$CO_POSICAO), ]
    } else {
      pars <- pars[base::order(pars$CO_POSICAO), ]
    }
    
    # 3. REMOÇÃO DE ITENS ANULADOS (IN_ITEM_ABAN == 1)
    # Identificamos quais posições da string/score devem sumir
    idx_anulados <- which(pars$IN_ITEM_ABAN == 1)
    
    # Converte a string em vetor numérico
    score_i <- as.numeric(strsplit(sample, "")[[1]])
    
    # Transforma em matriz 1 linha x 45 colunas
    score_i <- matrix(score_i, nrow = 1)
    
    # if (length(idx_anulados) > 0) {
    #   score_i <- score_i[-idx_anulados] # Remove do score
    #   pars <- pars[-idx_anulados, ]     # Remove do banco
    # }
    
    # 4. CÁLCULO DA LIKELIHOOD (LINHA POR LINHA)
    n_itens <- min(length(score_i), nrow(pars))
    list_probs <- lapply(1:n_itens, function(q) {
      res <- score_i[q]
      p_item <- pars[q, ]
      
      # Se o item não tem parâmetro ou a resposta é inválida, probabilidade neutra (1)
      if (is.na(res) || is.na(p_item$NU_PARAM_A)) return(rep(1, length(theta)))
      
      p1 <- cci_3pl(theta, p_item$NU_PARAM_A, p_item$NU_PARAM_B, p_item$NU_PARAM_C)
      return(if (res == 1) p1 else (1 - p1))
    })
    
    prod_prob <- list(Reduce(`*`, list_probs))
    
    # 5. EAP E TRANSFORMAÇÃO
    # Remove nulos (casos onde o caderno não foi encontrado)
    prod_prob <- prod_prob[!sapply(prod_prob, is.null)]
    
    theta_EAP <- sapply(prod_prob, function(L_theta) {
      posterior <- L_theta * p_theta
      posterior <<- posterior
      sum(theta * posterior) / sum(posterior)
    })
    
    k_val <- constantes[constantes$area == area, 'k']
    d_val <- constantes[constantes$area == area, 'd']
    
    eap_transf <- round(theta_EAP * k_val + d_val, 1)
    
    log_likelihood <- log(prod_prob[[1]] + 1e-300)
    
    # --- CÁLCULO DO IMPACTO MARGINAL (DIFERENÇA DE NOTA) ---
    # Função interna rápida para calcular o EAP a partir de uma lista de probabilidades
    calc_eap_internal <- function(probs_list) {
      L_theta <- Reduce(`*`, probs_list)
      post <- L_theta * p_theta
      th_eap <- sum(theta * post) / sum(post)
      return(th_eap * k_val + d_val)
    }
    
    original_score_transf <- eap_transf # Nota original já calculada
    impacto_array <- sapply(1:n_itens, function(i) {
      
      # Cria uma cópia da lista de probabilidades
      temp_probs <- list_probs
      
      # Pega o parâmetro do item atual
      p_item <- pars[i, ]
      if (is.na(p_item$NU_PARAM_A)) return(0) # Se item inválido, impacto zero
      
      # Inverte a resposta: se era 1 vira 0, se era 0 vira 1
      new_res <- if (score_i[i] == 1) 0 else 1
      
      # Calcula a nova probabilidade para esse item específico
      p1 <- cci_3pl(theta, p_item$NU_PARAM_A, p_item$NU_PARAM_B, p_item$NU_PARAM_C)
      temp_probs[[i]] <- if (new_res == 1) p1 else (1 - p1)
      
      # Calcula a nova nota e subtrai da original
      nova_nota <- calc_eap_internal(temp_probs)
      return(round(nova_nota - original_score_transf, 2))
    })
    
    list(
      theta = theta,
      posterior = log_likelihood, # Substituindo a escala para o gráfico
      eap = eap_transf,
      theta_eap = theta_EAP,
      impacto_individual = impacto_array
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}

sample <- "000000000000000000000000000000000000000000000"
ano <- 2019
codigo <- 511
lingua <- 0
area <- "LC"

calc(sample = sample, area = area, ano = ano, codigo = codigo, lingua = lingua)

