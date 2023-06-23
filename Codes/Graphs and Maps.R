# Graph for Deforestation x Other classes growth ####
gg.deforestation.cor <-
  function(proxy.table,
           type.one = "Growth",
           type.two = "Deforestation",
           color.one = c("purple", "grey50", "#EA9999", "darkorange"), 
           color.two = "darkgreen",
           save.as = "Results/Deforestation vs Growth.png",
           title.name = "Analysis for Uaça 1 e 2",
           different.groups = NULL){
    
    # Libraries required ####
    require(ggplot2)
    require(stats)
    
    
    # Function itself ####
    # Prepare data ####
    # Make sure the data is in a data.frame format.
    proxy.table <- as.data.frame(proxy.table)
    
    # Make sure the Year column is numeric
    proxy.table$Year <- as.numeric(proxy.table$Year)

    # Which columns have "type.one" or "type.two" in their names
    columns.type.one <-
      colnames(proxy.table)[grep(type.one, colnames(proxy.table))]
    
    columns.type.one.names <- substr(x = columns.type.one,
                                     start = nchar(type.one) + 2,
                                     stop = nchar(columns.type.one))
    
    colnames(proxy.table)[colnames(proxy.table) %in% columns.type.one] <-
      columns.type.one.names
    
    columns.type.one <- columns.type.one.names

    columns.type.two <-
      colnames(proxy.table)[grep(type.two, colnames(proxy.table))]
    
    
    # Correlation analysis ####
    # Only get type.one and type.two columns
    cor.table <- proxy.table[,which(colnames(proxy.table) %in%
                                      c(columns.type.two, columns.type.one))]
 
    # Remove any blank data (NA)
    cor.table <- na.omit(cor.table)
      
    # Correlate the data
    suppressWarnings(cor.data <- stats::cor(cor.table)[,1])
      
    # Which class has the higher correlation
    cor.information <-
      round(max(abs(cor.data[2:length(cor.data)]), na.rm = TRUE), digits = 2)
    
    # Write a + or - sign accordingly
    ifelse(max(abs(cor.data[2:length(cor.data)]), na.rm = TRUE) ==
             max(cor.data[2:length(cor.data)], na.rm = TRUE),
           cor.information <- paste0("+", cor.information),
           cor.information <- paste0("-", cor.information))
    
    # Make sure it always has 5 characters  
    if(nchar(cor.information) == 4) cor.information <- paste0(cor.information, "0")
      
    # Name the values
    names(cor.information) <-
      columns.type.one[which.max(abs(cor.data[2:length(cor.data)]))]
      
      
    # Prepare table for ggplot2 ####
    proxy.table <-
      tidyr::pivot_longer(data = proxy.table,
                          cols = c(columns.type.two, columns.type.one),
                          names_to = "Classes",
                          values_to = "NumValue") |>
      dplyr::group_by_at(c("Year", different.groups, "Classes")) |>
      dplyr::reframe(NumValue = sum(NumValue, na.rm = TRUE)) |>
      as.data.frame()
    
    # Alfabetical order of type.one and type.two
    order.types <- order(c(columns.type.one, columns.type.two))
    
    
    # GGPLOT2 graph ####
    gg.internal <-
      
      # Data being plotted
      ggplot2::ggplot(data = proxy.table,
                      aes(x = Year, y = NumValue, color = Classes)) +
      
      # Plot the deforestation only
      ggplot2::geom_line(data = proxy.table[proxy.table$Classes %in%
                                              columns.type.two,],
                         color = color.two, linewidth = 1, alpha = 0.5) +
      
      # Plot other classes
      ggplot2::geom_line(linewidth = 1, alpha = 0.3) +
      ggplot2::geom_point(alpha = 0.5) +
      
      ggplot2::scale_color_manual(values = c(color.one, color.two)[order.types]) +
      
      ggplot2::labs(title = title.name,
                    subtitle = paste0("Greatest correlation with ",
                                      names(cor.information), " (",
                                      cor.information, ")"),
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
      
    
    # save where the user setted it to
    ggplot2::ggsave(filename = save.as, plot = gg.internal,
                    width = 3000, height = 1500, units = "px", dpi = 300)
      
    
    # Returns (NULL) ####
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