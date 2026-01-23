# plumber.R

#* Calcula EAP e retorna curva
#* @param sample String binário de 0 e 1
#* @param area LC, CH, CN ou MT
#* @param ano Ano da prova
#* @param codigo Código da prova
#* @param lingua Tipo de lingua (0=Inglês, 1=Espanhol)
#* @get /calc
#* Calcula EAP e retorna curva
#* @param sample String binário de 0 e 1
#* @param area LC, CH, CN ou MT
#* @param ano Ano da prova
#* @param codigo Código da prova
#* @param lingua Tipo de lingua (0=Inglês, 1=Espanhol)
#* @get /calc
function(sample, area, ano, codigo, lingua) {
  
  tryCatch({
    
    # CONVERSÕES NECESSÁRIAS (Plumber recebe strings)
    ano <- as.numeric(ano)
    codigo <- as.numeric(codigo)
    lingua <- as.numeric(lingua)
    
    dir_base <- dirname(normalizePath("plumber.R"))
    
    load(file.path(dir_base, "constantes.rda"))
    nome_itens <- paste0("itens_", ano)
    load(file.path(dir_base, paste0(nome_itens, ".rda")))
    itens_db_total <- get(nome_itens)
    
    theta <- seq(-4, 4, length.out = 40)
    cci_3pl <- function(theta, a, b, c) c + ((1 - c) / (1 + exp(-a * (theta - b))))
    p_theta <- stats::dnorm(theta, mean = 0, sd = 1)
    
    prod_prob <- list()
    
    pars <- itens_db_total[itens_db_total$CO_PROVA == codigo, ]
    
    tem_digital <- "TP_VERSAO_DIGITAL" %in% names(pars)
    versoes <- if (tem_digital) {
      unique(na.omit(pars$TP_VERSAO_DIGITAL[pars$CO_PROVA == codigo]))
    } else {
      "X"
    }
    if (length(versoes) == 0) versoes <- "X"
    
    if (nrow(pars) == 0) { stop("Falha na seleção de itens: banco vazio") }
    
    if (versoes != "X") {
      if (area == "LC") {
        if (lingua == 1) { 
          pars <- pars[(pars$TP_VERSAO_DIGITAL == 1), ]
        } else { 
          pars <- pars[(pars$TP_VERSAO_DIGITAL == 0), ]
        }
      }
    } else {
      if (area == "LC") {
        if (lingua == 1) { 
          pars <- pars[(pars$TP_LINGUA == 1 | is.na(pars$TP_LINGUA)), ]
        } else { 
          pars <- pars[(pars$TP_LINGUA == 0 | is.na(pars$TP_LINGUA)), ]
        }
      } 
    }
    
    pars <- pars[base::order(pars$TP_LINGUA, pars$CO_POSICAO), ]
    
    score_i <- as.numeric(strsplit(sample, "")[[1]])
    score_i <- matrix(score_i, nrow = 1)
    
    n_itens <- min(length(score_i), nrow(pars))
    list_probs <- lapply(1:n_itens, function(q) {
      res <- score_i[q]
      p_item <- pars[q, ]
      if (is.na(res) || is.na(p_item$NU_PARAM_A)) return(rep(1, length(theta)))
      p1 <- cci_3pl(theta, p_item$NU_PARAM_A, p_item$NU_PARAM_B, p_item$NU_PARAM_C)
      return(if (res == 1) p1 else (1 - p1))
    })
    
    prod_prob <- list(Reduce(`*`, list_probs))
    prod_prob <- prod_prob[!sapply(prod_prob, is.null)]
    
    theta_EAP <- sapply(prod_prob, function(L_theta) {
      posterior <- L_theta * p_theta
      sum(theta * posterior) / sum(posterior)
    })
    
    k_val <- constantes[constantes$area == area, 'k']
    d_val <- constantes[constantes$area == area, 'd']
    
    eap_transf <- round(theta_EAP * k_val + d_val, 1)
    log_likelihood <- log(prod_prob[[1]] + 1e-300)
    
    calc_eap_internal <- function(probs_list) {
      L_theta <- Reduce(`*`, probs_list)
      post <- L_theta * p_theta
      th_eap <- sum(theta * post) / sum(post)
      return(th_eap * k_val + d_val)
    }
    
    original_score_transf <- eap_transf
    impacto_individual <- lapply(1:n_itens, function(i) {
      id_item <- as.character(pars$CO_ITEM[i])
      p_item  <- pars[i, ]
      posicao <- pars$CO_POSICAO[i]
      
      if (is.na(p_item$NU_PARAM_A)) {
        val <- NA
      } else {
        temp_probs <- list_probs
        new_res <- if (score_i[i] == 1) 0 else 1
        p1 <- cci_3pl(theta, p_item$NU_PARAM_A, p_item$NU_PARAM_B, p_item$NU_PARAM_C)
        temp_probs[[i]] <- if (new_res == 1) p1 else (1 - p1)
        nova_nota <- calc_eap_internal(temp_probs)
        val <- round(nova_nota - original_score_transf, 2)
      }
      
      # CORREÇÃO DE RETORNO:
      res_obj <- setNames(list(list(posicao = posicao, valor = val)), id_item)
      return(res_obj)
    })
    
    impacto_individual <- unlist(impacto_individual, recursive = FALSE)
    
    list(
      theta = theta,
      posterior = log_likelihood,
      eap = eap_transf,
      theta_eap = theta_EAP,
      impacto_individual = impacto_individual
    )
    
  }, error = function(e) {
    list(error = e$message)
  })
}