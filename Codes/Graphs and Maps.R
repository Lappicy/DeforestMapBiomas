# Fazer gráfico das correlações por país ####
correlacoes.anuais <-
  function(tabela.dados = NULL,
           nome.colunas = c("Desmatamento", "Crescimento_Urbano",
                            "Crescimento_Mineracao", "Crescimento_Pastagem",
                            "Crescimento_Agricultura", "Agua", "Outros"),
           salvar = "Banana Verde/Resultados/Imagens/Correlacao_"){
    
    # Nome dos paises
    nome.paises <- unique(tabela.dados$Pais)
    
    # rodar a correlação e fazer gráfico por país 
    for(i in unique(nome.paises)){
      
      # Tabela apenas com esse pais em especifico
      tabela.dados.pais <- tabela.dados[tabela.dados$Pais == i,]
      
      # Não precisa normalizar os indicadores porque são mesma escala (km2)
      tabela.dados.pais <-
        tabela.dados.pais[,which(colnames(tabela.dados.pais) %in% nome.colunas)]
      
      # Retirar linhas com NA
      tabela.dados.pais <- na.omit(tabela.dados.pais)
      
      # Reorganizar ordem das colunas
      tabela.dados.pais <- tabela.dados.pais[,c(1,3,4,5,2,6,7)]
      
      # Correlacionar eles
      dados.cor.pais <- stats::cor(tabela.dados.pais)
      
      # Trocar nomes da tabela 
      colnames(dados.cor.pais) <- rownames(dados.cor.pais) <-
        c("Desmatamento", "Mineração", "Pastagem", "Agricultura",
          "Infraestrutura Urbana", "Agua", "Outros")
      
      # Pegar só alguns dados da tabela
      dados.cor.pais <- dados.cor.pais[1:5, 1:5]
      
      # Preparar para salvar o gráfico
      png(file = paste0(salvar, i, ".png"), height = 900, width = 1000)
      
      # Fazer o gráfico
      gg.cor.pais <-
        corrplot::corrplot.mixed(dados.cor.pais,
                                 lower = "number", upper = "color",
                                 lower.col = c("red", "darkblue"),
                                 tl.pos = "lt", diag = "u",
                                 tl.col = "black",
                                 tl.cex = 2, cl.cex = 2, number.cex = 2,
                                 mar = c(1, 1, 1, 1))
      
      # Salvar o gráfico desse país
      dev.off()
      
    }
    
    # Retornar nada
    return(NULL)
  }



# Gráficos dos crescimentos x desmatamento para apenas um pais ####
gg.crescimento <-
  function(tabela.interna,
           agrupamento = "AP",
           Tipo.um = "Crescimento",
           Tipo.dois = "Desmatamento",
           cor.um = c("darkorange", "grey50", "#EA9999"), #"brown1"
           cor.dois = "darkgreen",
           salvar = "Banana Verde/Resultados/Imagens/Desmatamento_Crescimento_",
           cor.texto = "black",
           ingles = TRUE){
    
    # Puxar a biblioteca
    require(ggplot2)
    
    # Transformar tabela em data.frame
    tabela.interna.geral <- as.data.frame(tabela.interna)
    
    # Nome dos paises em ingles
    if(ingles){
      tabela.interna$Pais[tabela.interna$Pais == "Brasil"] <- "Brazil"
      tabela.interna$Pais[tabela.interna$Pais == "Guiana Francesa"] <-
        "French Guiana (a French Overseas Territory)"
    }
    
    # Fazer agrupamentos e quais os nomes deles
    Agrupamento.nomes <- unique(tabela.interna.geral[,agrupamento])
    
    for(i.interno in Agrupamento.nomes){
      
      # Puxar apenas o pais escolhido
      tabela.interna <-
        tabela.interna.geral[tabela.interna.geral[, agrupamento] ==
                               i.interno,]
      
      # Retirar infraestutura urbana
      if(any(colnames(tabela.interna) == "Crescimento_Urbano")){
        tabela.interna <-
          tabela.interna[,-which(colnames(tabela.interna) == "Crescimento_Urbano")]
      }
      
      # # Corrigir nomes, trocoando "_" por " "
      # nome_tipo_um <- unlist(strsplit(Tipo_um, split = "_"))
      # if(length(nome_tipo_um) > 1) nome_tipo_um <- paste(nome_tipo_um[1], nome_tipo_um[2])
      # nome_tipo_dois <- unlist(strsplit(Tipo_dois, split = "_"))
      # if(length(nome_tipo_dois) > 1) nome_tipo_dois <- paste(nome_tipo_dois[1], nome_tipo_dois[2])
      
      # Colunar que tem o "crescimento" (tipo_um)
      colunas.tipo.um <-
        colnames(tabela.interna)[grep(Tipo.um, colnames(tabela.interna))]
      
      colunas.tipo.dois <-
        colnames(tabela.interna)[grep(Tipo.dois, colnames(tabela.interna))]
      
      
      # Fazer uma análise de correlação ####
      # Pegar só colunas importantes
      tabela.cor <-
        tabela.interna[,which(colnames(tabela.interna) %in%
                                c("Desmatamento", colunas.tipo.um))]
      
      # Retirar linhas em branco (NA)
      tabela.cor.NA <- na.omit(tabela.cor)
      
      # Correlacionar eles
      suppressWarnings(dados.cor <- stats::cor(tabela.cor.NA)[,1])
      
      # Verificar qual a maior correlação que existe
      info.cor <- round(max(abs(dados.cor[2:length(dados.cor)]), na.rm = T), digits = 2)
      
      ifelse(max(abs(dados.cor[2:length(dados.cor)]), na.rm = T) ==
               max(dados.cor[2:length(dados.cor)], na.rm = T),
             info.cor <- paste0("+", info.cor),
             info.cor <- paste0("-", info.cor))
      
      if(nchar(info.cor) == 4) info.cor <- paste0(info.cor, "0")
      
      names(info.cor) <-
        c("Mining", "Pasture", "Agriculture")[which.max(abs(dados.cor[2:length(dados.cor)]))]
      
      
      # Preparar tabela pro ggplot2 ####
      tabela.interna <-
        tidyr::pivot_longer(tabela.interna,
                            cols = c(colunas.tipo.dois, colunas.tipo.um),
                            names_to = "Classe",
                            values_to = "Valor") |>
        dplyr::group_by_at(c("Pais", "Ano", agrupamento, "Classe")) |>
        dplyr::reframe(Valor = sum(Valor, na.rm = TRUE)) |>
        as.data.frame()
      
      
      # Gráfico ggplot2 ####
      gg.interno <-
        ggplot2::ggplot(data = tabela.interna,
                        aes(x = Ano, y = Valor, color = Classe)) +
        
        # So o desmatamento
        ggplot2::geom_line(data = tabela.interna[tabela.interna$Classe == "Desmatamento",],
                           color = "darkgreen", linewidth = 1, alpha = 0.5) +
        
        # Outras classes
        ggplot2::geom_line(linewidth = 1, alpha = 0.3) +
        ggplot2::geom_point(alpha = 0.5) +
        
        ggplot2::scale_color_manual(values = c(cor.um, cor.dois),
                                    labels = c("Agriculture", "Mining",
                                               "Pasture", #"Urban infrastructure",
                                               "Deforestation")) +
        
        ggplot2::labs(title = paste0("Brazil: ", unique(tabela.interna[, agrupamento])),
                      subtitle = paste0("Greatest correlation with ",
                                        names(info.cor), " (", info.cor, ")"),
                      x = "Year", y = bquote("[" ~ km^2 ~"]"), color = "Class:") +
        ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                decimal.mark = ".",
                                                                scientific = F)) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "top",
                       legend.text = element_text(size = 12, color = "black"),
                       legend.justification = "left",
                       legend.box.just = "left",
                       title = element_text(size = 12, color = "black", face = "bold"),
                       text = element_text(size = 12, color = "black"),
                       axis.text = element_text(size = 12, color = "black"))
      
      # Se o nome "salvar" não for nulo, salvar o gráfico
      if(!is.null(salvar)){
        ggplot2::ggsave(filename = paste0(salvar, i.interno, ".png"),
                        plot = gg.interno,
                        width = 3000, height = 1500, units = "px", dpi = 300)
      }
      
    }
    
    # Retorna nada ####
    return(NULL)
  }


# Gráficos de Classes feitas anteriormente ####
gg.desmatamento <-
  function(tabela.interna,
           eixo.y.interno = "Desmatamento",
           salvar = paste0("Resultados/Imagens/Imagens Finais/",
                           "Desmatamento Total.png"),
           n.col.interno = 2,
           cor.interno = "chartreuse4", #c(chartreuse4, grey50, #EA9999)
           cor.texto = "black",
           escala.grafico = "fixed",
           ingles = TRUE){
    
    # Puxar a biblioteca ####
    require(ggplot2)
    
    # Trabalhar com os dados da tabela ####
    # Transformar tabela em data.frame
    tabela.interna <- as.data.frame(tabela.interna)
    
    # Nome dos paises em ingles
    if(ingles){
      tabela.interna$Pais[tabela.interna$Pais == "Brasil"] <- "Brazil"
      tabela.interna$Pais[tabela.interna$Pais == "Guiana Francesa"] <-
        "French Guiana (a French Overseas Territory)"
    }
    
    # Qual coluna da tabela é o escolhido
    col.eixo.y.interno <- which(colnames(tabela.interna) == eixo.y.interno)
    
    # Nome do eixo y
    nome.eixo.y <- unlist(strsplit(eixo.y.interno, split = "_"))
    if(length(nome.eixo.y) > 1) nome.eixo.y <- paste(nome.eixo.y[1], nome.eixo.y[2])
    if(ingles) nome.eixo.y <- "Deforestation"
    
    # gráfico ggplot2 ####
    # Se tiver AP
    if("AP" %in% colnames(tabela.interna)){
      gg.interno <-
        ggplot2::ggplot(data = tabela.interna,
                        aes(x = tabela.interna[, "Ano"],
                            y = tabela.interna[, col.eixo.y.interno],
                            col = AP)) +
        ggplot2::scale_color_manual(values = c("#E68613", "#8494FF"),
                                    labels = c("Non protected Areas",
                                               "Protected Areas")) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~Pais, ncol = n.col.interno, scales = escala.grafico) +
        
        # # Em 1989, Balbina: 1066.9950 | Balbina Completo: 1361.1609
        # ggplot2::geom_segment(data = data.frame(Ano = rep(as.integer(2021), 2),
        #                                         Pais = rep("Brasil", 2),
        #                                         Desmatamento = c(615.3876, #321.2217
        #                                                          1682.3826)),
        #                       aes(x = Ano[1], y = Desmatamento[1],
        #                           xend = Ano[2], yend = Desmatamento[2]),
        #                       col = "red") +
        
        ggplot2::labs(x = "Year",
                      y = bquote(.(nome.eixo.y) ~ " [" ~ km^2 ~"]"),
                      color = NULL) +
        ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                decimal.mark = ".",
                                                                scientific = F)) +
        ggplot2::theme_bw() +
        ggplot2::theme(text = element_text(size = 12, color = "black"),
                       axis.text = element_text(size = 12, color = "black"),
                       strip.text = element_text(size = 12, color = cor.texto),
                       strip.background = element_rect(fill = cor.interno),
                       legend.position = "top")
    }
    
    # Se não tiver AP
    if(!("AP" %in% colnames(tabela.interna))){
      gg.interno <-
        ggplot2::ggplot(data = tabela.interna,
                        aes(x = tabela.interna[, "Ano"],
                            y = tabela.interna[, col.eixo.y.interno])) +
        ggplot2::geom_line(col = cor.interno) +
        ggplot2::geom_point(col = cor.interno) +
        ggplot2::facet_wrap(~Pais, ncol = n.col.interno, scales = escala.grafico) +
        
        ggplot2::labs(x = "Year",
                      y = bquote(.(nome.eixo.y) ~ " [" ~ km^2 ~"]"),
                      color = NULL) +
        ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                decimal.mark = ".",
                                                                scientific = F)) +
        ggplot2::theme_bw() +
        ggplot2::theme(text = element_text(size = 12, color = "black"),
                       axis.text = element_text(size = 12, color = "black"),
                       strip.text = element_text(size = 12, color = "white"),
                       strip.background = element_rect(fill = cor.interno),
                       legend.position = "none")
    }
    
    
    # Se o nome "salvar" não for nulo, salvar o gráfico
    if(!is.null(salvar)){
      ggplot2::ggsave(filename = salvar,
                      plot = gg.interno,
                      width = 3000, height = 1500, units = "px", dpi = 300)
    }
    
    # Retorna o gráfico se quiser usar ainda ####
    return(gg.interno)
  }



# Gráficos de Classes feitas anteriormente para apenas um pais ####
gg_classe_Pais <-
  function(tabela_interna,
           Tipo_um = "Crescimento_Mineracao",
           Tipo_dois = "Desmatamento",
           cor_um = "grey50", #EA9999
           cor_dois = "darkgreen",
           salvar = paste0("Mapas e Imagens/Escudo Guianas/",
                           "CrescimentoMineracao_Suriname_USGS_MAPBIOMAS4.png"),
           cor_texto = "black"){
    
    # Puxar a biblioteca
    require(ggplot2)
    
    # Transformar tabela em data.frame
    tabela_interna <- as.data.frame(tabela_interna)
    
    # Corrigir nomes, trocoando "_" por " "
    nome_tipo_um <- unlist(strsplit(Tipo_um, split = "_"))
    if(length(nome_tipo_um) > 1) nome_tipo_um <- paste(nome_tipo_um[1], nome_tipo_um[2])
    nome_tipo_dois <- unlist(strsplit(Tipo_dois, split = "_"))
    if(length(nome_tipo_dois) > 1) nome_tipo_dois <- paste(nome_tipo_dois[1], nome_tipo_dois[2])
    
    # gráfico ggplot2
    gg_interno <-
      ggplot2::ggplot(data = tabela_interna,
                      aes(x = tabela_interna[, "Ano"],
                          y = tabela_interna[, Tipo_um])) +
      ggplot2::geom_line(aes(color = "Tipo 1"), linewidth = 1, alpha = 0.5) +
      ggplot2::geom_point(aes(color = "Tipo 1")) +
      ggplot2::geom_line(aes(y = tabela_interna[, Tipo_dois],
                             color = "Tipo 2"),
                         linewidth = 1, alpha = 0.5) +
      ggplot2::geom_point(aes(y = tabela_interna[, Tipo_dois],
                              color = "Tipo 2")) +
      
      ggplot2::scale_color_manual(values = c(cor_um, cor_dois),
                                  labels = c(nome_tipo_um, nome_tipo_dois)) +
      
      ggplot2::labs(title = unique(tabela_interna$Pais),
                    x = "Ano", y = bquote("[" ~ km^2 ~"]"), color = "Classe:") +
      ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ".",
                                                              decimal.mark = ",",
                                                              scientific = F)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top",
                     legend.text = element_text(size = 12, color = "black"),
                     legend.justification = "left",
                     legend.box.just = "left",
                     title = element_text(size = 12, color = "black", face = "bold"),
                     text = element_text(size = 12, color = "black"),
                     axis.text = element_text(size = 12, color = "black"))
    
    # Se o nome "salvar" não for nulo, salvar o gráfico
    if(!is.null(salvar)){
      ggplot2::ggsave(filename = salvar,
                      plot = gg_interno,
                      width = 3000, height = 1500, units = "px", dpi = 300)
    }
    
    # Retorna o gráfico se quiser usar ainda
    return(gg_interno)
  }



# Função para criar um arquivo só de um argumento para TODOS os anos ####
fun_metricas_arq <-
  function(arg_level = "class",
           pais = "Brasil"){
    
    # Nome da pasta (Tem que ter uma pasta pro país e por ano)
    nome_pasta <- paste0("Métricas/", pais)
    
    
    # Quais são os arquivos e os número de iterações
    nomes_arq <-
      list.files(nome_pasta)[!is.na(as.numeric(list.files(nome_pasta)))]
    n_anos <- length(nomes_arq)
    
    
    # Fazer o RBIND para cada arquivo, filtrando o que se quer
    for(i in seq_len(n_anos)){
      
      # Ano correto da análise
      ano_proxy <- as.numeric(nomes_arq[i])
      
      # Nome da pasta com ano
      nome_proxy <- paste0(nome_pasta, "/", ano_proxy)
      
      # E quantidade de arquivos nesse wd
      n_arq <- length(list.files(path = nome_proxy))
      
      
      # Para cada ano, puxar todas os arquivos (de todas as áreas)
      for(j in seq_len(n_arq)){
        
        # Definir qual a área de estudo em questão
        nome_ae <-
          substr(list.files(path = nome_proxy)[j], 20,
                 nchar(list.files(path = nome_proxy)[j]) - 4) %>%
          as.numeric()
        
        
        # Puxar arquivo
        arq_proxy <-
          read.table(file = list.files(path = nome_proxy)[j],
                     header = TRUE,
                     sep = "\t",
                     stringsAsFactors = TRUE,
                     fileEncoding = "UTF-8") %>%
          filter(level == arg_level) %>%
          mutate(cod_uniq = nome_ae)
        
        
        # Mudar o ano se estiver errado
        arq_proxy$Ano <- ano_proxy
        
        
        if(j == 1 & i == 1) arq_proxy_final <- arq_proxy
        if(!(j == 1 & i == 1)) arq_proxy_final <- rbind(arq_proxy_final,
                                                        arq_proxy)
      }
    }
    
    
    # NÃO ESTÃ COM A PARTE DE SALVAR ATIVA (melhor fazer por fora) !!!!!!!!!!!!
    # Salvar resumo do arquivo na pasta certa
    #setwd(wd_save)
    #write.table(arq_proxy_final,
    #            row.names = FALSE,
    #            fileEncoding = "UTF-8",
    #            file = paste0("Resumo_", pais, "_", arg_level, ".txt"),
    #            sep = "\t")
    
    
    # Voltar para o diretório original
    setwd(wd_arq)
    
    
    # retornar valor do df
    return(arq_proxy_final)
  }

# Mapa para um ano em específico e uma classe específica ####
mapa_malha <- function(arq_geo = "Banana Verde/Dados/BananaVerde.gpkg",
                       arq_classe = Analise_BananaVerde_Malha,
                       arq_pais = "Banana Verde/Dados/BananaVerde.gpkg",
                       arq_contorno = "Banana Verde/Dados/BananaVerde_Tudo.gpkg",
                       arq_APs = "Banana Verde/Dados/BananaVerde.gpkg",
                       arq_America_do_Sul = NULL,
                       malha_mapa = "Banana Verde/Dados/BananaVerde_malha_01.gpkg",
                       grid_malha = "Buffer",
                       oceanos = NULL,
                       classe = "Desmatamento",
                       Ano = "todos",
                       limites = c(0, 5, 10, 30), #c(0, 30, 60, 100),
                       cores = c("white", "#E5E200", "#FC780D", "red", "darkred"),
                       salvar = "Banana Verde/Resultados/Mapas/Malha01.png"){
  
  # # Depois apagar ####
  # arq_geo = "Banana Verde/Dados/BananaVerde.gpkg"
  # arq_classe = resumo_tabela_interna
  # arq_pais = "Banana Verde/Dados/BananaVerde.gpkg"
  # arq_contorno = "Banana Verde/Dados/BananaVerde_Tudo.gpkg"
  # arq_APs = "Banana Verde/Dados/BananaVerde.gpkg"
  # arq_America_do_Sul = NULL
  # malha_mapa = malha_mapa_01
  # oceanos = NULL
  # classe = "Desmatamento"
  # Ano = "todos"
  # limites = c(0, 5, 10, 30) #c(0, 30, 60, 100)
  # cores = c("white", "#E5E200", "#FC780D", "red", "darkred")
  # salvar = "Banana Verde/Resultados/Mapas/Desmatamento Malha 01.png"
  # 
  # 
  # Preparo dos dados ####
  # Ler arquivos geoespaciais como sf
  arq_geo <- ler_geo(arq_geo)
  arq_pais <- ler_geo(arq_pais)
  arq_APs <- ler_geo(arq_APs)
  arq_contorno <- ler_geo(arq_contorno)
  arq_America_do_Sul <- ler_geo(arq_America_do_Sul)
  malha_mapa <- ler_geo(malha_mapa)
  oceanos <- ler_geo(oceanos)
  
  # # Fazer a malha direito
  # malha_mapa <- criar_malha(arq_geo = malha_mapa[malha_mapa$AP == "sim",],
  #                           tamanho_malha = 0.25)
  
  # Transformar colun a da classe em "Valor" para uso na função
  numero_coluna_certo <- which(colnames(arq_classe) == classe)
  colnames(arq_classe)[numero_coluna_certo] <- "Valor"
  
  # Definir se o nome da classe é feminino ou masculino
  genero_artigo <- ifelse(substr(classe, nchar(classe), nchar(classe)) %in%
                            c("o", "s"), "o", "a")
  
  # Transformar "Ano" em numérico se Ano == "TODOS"
  if(any(toupper(Ano) == "TODOS")) Ano <- sort(unique(arq_classe$Ano))
  
  # Pegar apenas as colunas importantes do data.frame arq_classe
  arq_classe <- arq_classe[arq_classe$Ano %in% Ano,
                           c("ID_malha", "Pais", "Ano", "Valor")]
  
  # Fazer a soma da classe nos anos
  arq_classe <-
    arq_classe |>
    dplyr::mutate(Valor = ifelse(Valor <= 0, 0, Valor)) |>
    dplyr::group_by(ID_malha) |>
    dplyr::reframe(Pais = first(Pais),
                   Valor = ifelse(all(is.na(Valor)), 0,
                                  ifelse(classe %in% c("Desmatamento", "Reflorestamento"),
                                         sum(Valor, na.rm = T),
                                         sum(Valor, na.rm = T))))
  
  # Transformar dados em classes pré-definidas e fatores
  suppressWarnings({
    if(exists("limites")){
      
      for(i in 1:nrow(arq_classe)){
        
        # Acessar o valor ""
        x_var <- arq_classe$Valor[i]
        
        # Se for NA, pular!
        if(is.na(x_var)) next
        
        arq_classe$Valor_classe[i] <-
          if(x_var == 0){1} else
            if(findInterval(x_var, sort(c(limites[1], limites[2]))) == 1L){2} else
              if(findInterval(x_var, sort(c(limites[2], limites[3]))) == 1L){3} else
                if(findInterval(x_var, sort(c(limites[3], limites[4]))) == 1L){4} else
                  if(findInterval(x_var, sort(c(limites[4], Inf))) == 1L){5}
        
      }
      
      arq_classe <- as.data.frame(arq_classe)
      arq_classe$Valor_classe <- as.factor(arq_classe$Valor_classe)
    }
  })
  
  # Se os valores não forem pre-definidos
  if(!exists("limites")) arq_classe$Valor_classe <- arq_classe$Valor
  
  # Juntar o ano que se quer com o arquivo geoespacial
  mapa_df <- base::merge(x = malha_mapa, y = arq_classe)
  
  # Passar pro ingles
  if(classe == "Desmatamento") classe <- "Deforestation"
  if(classe == "Crescimento_Mineracao") classe <- "Mining"
  if(classe == "Crescimento_Agricultura") classe <- "Agriculture"
  if(classe == "Crescimento_Pastagem") classe <- "Pasture"
  
  # Gráfico em sí ####
  # Fazer o ggplot2
  mapa_interno <-
    ggplot() +
    
    # América do Sul de fundo e o oceano
    # geom_sf(data = America_do_Sul$geom, fill = "#DADADA",
    #         color = "black", lwd = 0.6) +
    # geom_sf(data = oceanos$geom, fill = "#AADFFC", color = "transparent") +
    
    # Mapa com a classe escolhida
    geom_sf(data = mapa_df, aes(fill = Valor_classe), color = "transparent") +
    
    # Mapa com as áreas protegidas por cima (limites geográficos de outra cor)
    geom_sf(data = malha_mapa$geom[malha_mapa$AP == "Buffer"],
            fill = "transparent", alpha = 0.1, color = "darkgreen",
            linetype = "solid", lwd = 0.2) +
    
    # Mapa com os paises por cima (limites geográficos)
    geom_sf(data = arq_pais$geometry, fill = "transparent",
            color = "black", lwd = 0.6) +
    
    # Mapa com o limite do escudo por cima
    geom_sf(data = arq_contorno$geometry, fill = "transparent",
            color = "purple", lwd = 1) +
    
    # Qual a cor do preenchimento se não tiver limites
    {if(!exists("limites")){
      scale_fill_continuous(low = "#22B34D", high = "red")}} +
    
    # Qual a cor do preenchimento se tiver limites
    {if(exists("limites")){
      scale_fill_manual(values = cores,
                        labels = c(paste0("No ", tolower(classe)),
                                   paste0("Between ", limites[1], " and ", limites[2], " km2"),
                                   paste0("Between ", limites[2], " and ", limites[3], " km2"),
                                   paste0("Between ", limites[3], " and ", limites[4], " km2"),
                                   paste0("More than ", limites[4]," km2")))}} +
    
    
    labs(title = paste0(classe, " in ", Ano),
         x = "Longitude", y = "Latitude",
         fill = paste0(classe, "\nin km2")) +
    
    # Se forem vários anos, trocar o título
    {if(length(Ano) != 1){
      labs(title = paste0("Acumulated ", classe,
                          " between ", min(Ano), " and ", max(Ano)))}} +
    
    # Colocar norte geografico e escala
    annotation_scale(location = "br",
                     bar_cols = c("black", "white")) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                           style = north_arrow_orienteering(fill = c("black", "white"),
                                                            line_col = "grey20")) +
    
    # Cortar o gráfico
    # coord_sf(xlim = c(-70.8, -51.1), ylim = c(-2.3, 8.7)) +
    
    # Mexer no tema do gráfico
    theme_bw() +
    theme(legend.position = "none")
  
  
  # Salvar o gráfico ####
  # Se o nome "salvar" não for nulo, salvar o gráfico
  if(!is.null(salvar)){
    ggplot2::ggsave(filename = salvar,
                    plot = mapa_interno,
                    width = 3000, height = 1500, units = "px", dpi = 300)
  }
  
  # Retorna o gráfico se quiser usar ainda ####
  return(mapa_interno)
  
}

mapa_desmatamento <-
  mapa_malha(Ano = "Todos", classe = "Desmatamento",
             salvar = "Banana Verde/Resultados/Mapas/Desmatamento Acumulado Malha 01.png")

mapa_mineracao <-
  mapa_malha(classe = "Crescimento_Mineracao", Ano = "Todos",
             salvar = "Banana Verde/Resultados/Mapas/Mineracao Acumulada Malha 01.png")

mapa_pastagem <-
  mapa_malha(classe = "Crescimento_Pastagem", Ano = "Todos",
             salvar = "Banana Verde/Resultados/Mapas/Pastagem Acumulada Malha 01.png")

mapa_agricultura <-
  mapa_malha(classe = "Crescimento_Agricultura", Ano = "Todos",
             salvar = "Banana Verde/Resultados/Mapas/Agricultura Acumulada Malha 01.png")
# 
# mapa_urbano <-
#   mapa_malha(classe = "Urbano", Ano = "Todos", salvar = NULL)
# 
# mapa_4_classes <-
#   cowplot::plot_grid(mapa_desmatamento, mapa_mineracao,
#                      mapa_pastagem, mapa_agricultura)
# 
# mapa_4_classes_legenda <-
#   ggpubr::ggarrange(mapa_desmatamento, mapa_mineracao,
#                     mapa_pastagem, mapa_agricultura,
#                     common.legend = T)
# 
# ggplot2::ggsave(filename = "teste2.png",
#                 plot = mapa_4_classes_legenda,
#                 width = 4000, height = 2500, units = "px", dpi = 300)

# End -------------------------------------------------------------------------