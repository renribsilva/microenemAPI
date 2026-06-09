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
  # Verificação da presença dos argumentos
  if (
    missing(sample) ||
      is.null(sample) ||
      sample == "" ||
      missing(area) ||
      is.null(area) ||
      area == "" ||
      missing(ano) ||
      is.null(ano) ||
      ano == "" ||
      missing(codigo) ||
      is.null(codigo) ||
      codigo == "" ||
      missing(lingua) ||
      is.null(lingua) ||
      lingua == ""
  ) {
    return(NULL)
  }

  tryCatch(
    {
      # Conversões necessárias (Plumber recebe strings)
      ano <- as.numeric(ano)
      codigo <- as.numeric(codigo)
      lingua <- as.numeric(lingua)
      area <- toupper(area)

      # Define o diretório base
      dir_base <- dirname(normalizePath("plumber.R"))

      # Carrega as constantes de transformação da escala
      load(file.path(dir_base, "constantes.rda"))

      # Carrega o caderno de itens da prova
      nome_itens <- paste0("itens_", ano)
      load(file.path(dir_base, paste0(nome_itens, ".rda")))
      itens_db_total <- get(nome_itens)

      # Prepara constantes e funções de cálculo da TRI
      theta <- seq(-4, 4, length.out = 40)
      cci_3pl <- function(theta, a, b, c) {
        c + ((1 - c) / (1 + exp(-a * (theta - b))))
      }
      p_theta <- stats::dnorm(theta, mean = 0, sd = 1)

      # Prepara lista para receber likelihood
      prod_prob <- list()

      # Seleciona os itens da prova selecionada
      pars <- itens_db_total[itens_db_total$CO_PROVA == codigo, ]

      # Verifica versões digitais
      tem_digital <- "TP_VERSAO_DIGITAL" %in% names(pars)
      versoes <- if (tem_digital) {
        unique(na.omit(pars$TP_VERSAO_DIGITAL[pars$CO_PROVA == codigo]))
      } else {
        "X"
      }

      # Se não há versões digitais, retorna X
      if (length(versoes) == 0) {
        versoes <- "X"
      }

      # Etapa de segurança
      if (nrow(pars) == 0) {
        stop("Falha na seleção de itens: banco vazio")
      }

      # Filtra caderno de itens de acordo com a lingua, versão
      # e área do conhecimento
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

      # Ordena o cardeno de acordo com língua e posição, nesta ordem
      pars <- pars[base::order(pars$TP_LINGUA, pars$CO_POSICAO), ]

      # Prepara a sequência de erros e acertos para a iteração
      score_i <- as.numeric(strsplit(sample, "")[[1]])
      score_i <- matrix(score_i, nrow = 1)

      # Etapa de seguraça
      if (length(score_i) != 45) {
        stop(paste(
          "O sample enviado tem tamanho",
          length(score_i),
          "mas o esperado é 45."
        ))
      }
      if (nrow(pars) != 45) {
        stop(paste(
          "Foram encontrados",
          nrow(pars),
          "itens para esta prova, mas o esperado é 45."
        ))
      }

      n_itens <- 45

      # Para cada item válido da prova, retorna um vetor com
      # 40 probabilidades, sendo que cada uma é associada a uma
      # possível proficiência (-4 < theta < 4)
      list_probs <- lapply(1:n_itens, function(q) {
        res <- score_i[q]
        p_item <- pars[q, ]
        if (is.na(res) || is.na(p_item$NU_PARAM_A)) {
          return(rep(1, length(theta)))
        }
        p1 <- cci_3pl(
          theta,
          p_item$NU_PARAM_A,
          p_item$NU_PARAM_B,
          p_item$NU_PARAM_C
        )
        if (res == 1) p1 else (1 - p1)
      })

      # Para cada participante da prova, faz o produtório das
      # probabilidades de erros e acertos para cada uma das 40
      # proficiências pré-estabelecidas (theta), guardando o resultado
      # um vetor com 40 valores de verossimilhança.
      prod_prob <- list(Reduce(`*`, list_probs))
      prod_prob <- prod_prob[!sapply(prod_prob, is.null)]

      # Encontra, por meio do EAP, o 'centro de gravidade' dos 40
      # valores de verossimilhança, isto é, dentre esses valores, qual
      # melhor representa a proficiência do participante do exame
      theta_eap <- sapply(prod_prob, function(l_theta) {
        posterior <- l_theta * p_theta
        sum(theta * posterior) / sum(posterior)
      })

      # Reproduz a equalização, que é o procedimento que garante que a nota de
      # edições e anos diferentes possam ser comparadas diretamente entre si.
      constantes_dt <- get("constantes")

      k_val <- constantes_dt[constantes_dt$area == area, "k"]
      d_val <- constantes_dt[constantes_dt$area == area, "d"]

      eap_transf <- round(theta_eap * k_val + d_val, 1)
      log_likelihood <- log(prod_prob[[1]] + 1e-300)

      # Função auxiliar para calcular o impacto virutal de cada item do exame
      # sobre a nota final
      calc_eap_internal <- function(probs_list) {
        l_theta <- Reduce(`*`, probs_list)
        post <- l_theta * p_theta
        th_eap <- sum(theta * post) / sum(post)
        th_eap * k_val + d_val
      }

      # Itera sobre todos os itens da sequência de erros e acertos e guarda
      # o impacto individual do item em um objeto
      original_score_transf <- eap_transf
      impacto_individual <- lapply(1:n_itens, function(i) {
        id_item <- as.character(pars$CO_ITEM[i])
        p_item <- pars[i, ]
        posicao <- pars$CO_POSICAO[i]

        if (is.na(p_item$NU_PARAM_A)) {
          val <- NA
        } else {
          temp_probs <- list_probs
          new_res <- if (score_i[i] == 1) 0 else 1
          p1 <- cci_3pl(
            theta,
            p_item$NU_PARAM_A,
            p_item$NU_PARAM_B,
            p_item$NU_PARAM_C
          )
          temp_probs[[i]] <- if (new_res == 1) p1 else (1 - p1)
          nova_nota <- calc_eap_internal(temp_probs)
          val <- round(nova_nota - original_score_transf, 2)
        }

        res_obj <- setNames(list(list(posicao = posicao, valor = val)), id_item)
        res_obj
      })

      impacto_individual <- unlist(impacto_individual, recursive = FALSE)

      # Retorna uma lista para o cliente
      list(
        theta = theta,
        posterior = log_likelihood,
        eap = eap_transf,
        theta_eap = theta_eap,
        impacto_individual = impacto_individual
      )
    },
    error = function(e) {
      list(error = e$message)
    }
  )
}
