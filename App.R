# Carregando Pacotes  -----------------------------------------------------
library(shiny)
library(tidyverse)
library(colourpicker) # Esolher a cor do gráfico
library(GGally) # Matriz de correlação 
library(shinydashboard) # Formato do App em Dashboard
library(shinythemes) # fazer teste com shinythemes::themeSelector()
library(googlesheets4) # conectar o R com o Google drive
library(googledrive) # # conectar o R com o Google drive
library(rio) # Leitura generica de bases
library(readxl) # Le XLSX
library(xml2) # Le HTML
library(utils) # Le CSV
library(readODS) # Le ODS
library(patchwork) # to combine separate ggplots into the same graphic
library(DT) # Tabelas 
library(plotly) # Graficos interativos 
library(shinyjqui) # reordenar facetas no boxplot
library(hexbin) # grafico de calor parte de big data
library(shinyWidgets)
library(shinycssloaders) # spinner de carregamento gráficos
library(summarytools)


authenticate <- function(new_authentication= F){
  if (new_authentication==T){
    if ( length(list.files(".secrets"))>0 ){
      file.remove(paste(".secrets", list.files(".secrets"), sep = "/")) }
    options(
      gargle_oauth_email = TRUE,
      gargle_oauth_cache = ".secrets"
    )
    googledrive::drive_auth()
    googlesheets4::gs4_auth()}
  
  if (new_authentication==F) {
    if ( length(list.files(".secrets"))!=2 ){
      return("Uma nova autenticação é necessária") }
    else{
      options(
        gargle_oauth_email = TRUE,
        gargle_oauth_cache = ".secrets")} }
  
  if (new_authentication!=T & new_authentication!=F) {
    return("O argumento new_authentication precisa ser lógico")}
  
}



get_data_base <- function(google_drive=F, set_directory=NULL, sheet=NULL, data_base=NULL){
  if (google_drive==T){
    #  adicionar o script para importar do google drive
    sheet_id <- googledrive::drive_get(sheet)$id
    #entre  o nome da planilha desejada (podemos ter isso de input no APP)
    dados <- read_sheet(sheet_id)
    return(dados)}
  
  else if(is.null(data_base)==T & is.null(set_directory)==F){
    dados <- rio::import(set_directory, header=T)
    dados <- as.data.frame(dados)
    #dados <- rio::import(set_directory)
    return(dados)
  }
  else if(is.null(data_base)==F & is.null(set_directory)==T){
    dados <- data_base
    return(dados)
  }
  else if(is.null(data_base)==T & is.null(set_directory)==T){
    return("Precisa escolher uma opcao para a base de dados")
  }
  else if(is.null(data_base)==F & is.null(set_directory)==F){
    return("Precisa escolher apenas uma opcao para a base de dados")
  }
}



standardize <- function(data_base, categorical_vars, numeric_vars_discrete,
                        numeric_vars_continuous, levels_catecorical_vars = NULL){
  attach(data_base)
  # transforma as variáveis categóricas em fatores
  for (var in categorical_vars) {
    # ideia: Vectorize(teste_fun)(c(1,5,8,9))
    quantas_vezes_aparece_na_lista <- sum(names(levels_catecorical_vars) == var)
    # se não houver argumento na lista sobre a ordem dos níveis de alguma variável
    # só transformar em factor
    if (quantas_vezes_aparece_na_lista!=1){ data_base[[var]] <- as.factor(data_base[[var]]) }
    else{
      ordem_niveis <- levels_catecorical_vars[[which(names(levels_catecorical_vars)==var)]]
      data_base[[var]] <- factor(data_base[[var]],
                                 levels = ordem_niveis)}
  }
  
  # transforma as variáveis numéricas discretas em numeric
  for (var in numeric_vars_discrete) {
    data_base[[var]] <- as.numeric(data_base[[var]])}
  
  # transforma as variáveis numéricas continuas em numeric
  for (var in numeric_vars_continuous) {
    data_base[[var]] <- as.numeric(data_base[[var]])}
  
  detach(data_base)
  return(data_base)
}




choose_layout <- function(layout_option= "flaty_blue"){
  if (layout_option == "superhero") {
    usar <- HTML('
    
      .st-table td, .st-table th {
       color: black;
       padding: 8px;
       }
        
        
          .st-table {
       width: auto;
       table-layout: auto;
       margin-top: 20px;
       margin-bottom: 20px;
       max-width: 100%;
       background-color: white;
       border-collapse: collapse;
       }
        
        
        
      /* Cabeçalho faixa direita */
        .skin-black .main-header .navbar {
          background-color:#7b848e;
        }
        .skin-black .main-header .navbar>.sidebar-toggle {
          color:#333;
          border-right:1px solid #7b848e;
        }
      /* Cabeçalho faixa esquerda */
        .skin-black .main-header>.logo {
          background-color:#ea8113;
          border-right:1px solid #ea8113;
        }
      /* Cor do fundo */
        .content-wrapper, .right-side {
          background-color: #40556f;
        }
      /* Cor do sidebar */
        .skin-black .left-side,.skin-black .main-sidebar,.skin-black .wrapper {
          background-color:#40556f;
        }
      /* Ajustes painel de escolha das abas */
        a {
          color:white;
        }
        .nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover {
          border:2px solid #ddd;
        }
        .nav-tabs {
          border-bottom:1px solid transparent;
        }
      /* Borda nos quadros de escrita */
        .form-control {
          border: 2px solid #ea8113;
        }
      /* Cor painel input */
        .shiny-input-panel {
          background-color:#7b848e;
          border:1px solid #7b848e;
        }
      /* Borda nos quadros de input */
        .selectize-input {
          border: 2px solid #ea8113;
        }
        
      /* Fonte do texto */
        body {
          font-family:"Helvetica Neue",Helvetica,Arial,sans-serif;
          font-size:14px;
          line-height:1.42857143;
          color:white;
          background-color:#fff
        }

                 ')
    return(usar)}
  # flaty_blue -------
  if (layout_option=="flaty_blue"){
    usar <- HTML('
      /* Cabeçalho faixa direita */
        .skin-black .main-header .navbar {
          background-color:#114762;
        }
        .skin-black .main-header .navbar>.sidebar-toggle {
          color:white;
          border-right:1px solid #114762;
        }
      /* Cabeçalho faixa esquerda */
        .skin-black .main-header>.logo {
          background-color:#0d3244;
          color:white;
          border-right:1px solid #0d3244;
        }
      /* Cor do fundo */
        .content-wrapper, .right-side {
          background-color: white;
        }
      /* Cor do sidebar */
        .skin-black .left-side, .skin-black .main-sidebar, .skin-black .wrapper {
          background-color: transparent;
        }
        .skin-black .sidebar-menu>li.active>a,.skin-black .sidebar-menu>li:hover>a {
          color:#114762;
          background:#ecf4f4;
        }
        .skin-black .sidebar a {
          color:#114762;
        }
      
      /* Cor painel input */
        .shiny-input-panel {
          background-color:#ecf4f4;
          border:1px solid #ecf4f4;
          border-radius: 8px;
        }
      /* Fonte texto */
        body {
          font-family:"Helvetica Neue",Helvetica,Arial,sans-serif;
          font-size:14px;
          line-height:1.42857143;
          color:#114762;
          background-color:#fff
        }
        ')
    return(usar)
  }
  if (layout_option=="cerulean") {
    usar <- HTML('
      /* Cabeçalho faixa direita */
        .skin-black .main-header .navbar {
          background-color:#00cfffd6;
        }
        .skin-black .main-header .navbar>.sidebar-toggle {
          color:#333;
          border-right:1px solid #00cfffd6;
        }
      /* Cabeçalho faixa esquerda */
        .skin-black .main-header>.logo {
          background-color:#2b88bf;
          color:white;
          border-right:1px solid #2b88bf;
        }
      /* Cor do fundo */
        .content-wrapper, .right-side {
          background-color: white;
        }
      /* Cor do sidebar */
        .skin-black .left-side, .skin-black .main-sidebar, .skin-black .wrapper {
          background-color: white;
        }
        .skin-black .sidebar-menu>li.active>a,.skin-black .sidebar-menu>li:hover>a {
          color:#2b6cbf;
          background:#e6e6e6;
        }
        .skin-black .sidebar a {
          color:#2b6cbf;
        }

      /* Cor painel input */
        .shiny-input-panel {
          background-color:#f5f5f5;
          border:1px solid #e3e3e3;
          border-radius:10px;
        }

      /* Fonte texto */
        body {
          font-family:"Helvetica Neue",Helvetica,Arial,sans-serif;
          font-size:14px;
          line-height:1.42857143;
          color:#2b6cbf;
          background-color:#fff
        }
        ')
    return(usar)
  }
  # flaty_green -------
  if (layout_option=="flaty_green"){
    usar <- HTML('
      /* Cabeçalho faixa direita */
        .skin-black .main-header .navbar {
          background-color:#116229;
        }
        .skin-black .main-header .navbar>.sidebar-toggle {
          color:#333;
          border-right:1px solid #116229;
        }
      /* Cabeçalho faixa esquerda */
        .skin-black .main-header>.logo {
          background-color:#0d4410;
          color:white;
          border-right:1px solid #0d4410;
        }
      /* Cor do fundo */
        .content-wrapper, .right-side {
          background-color: white;
        }
      /* Cor do sidebar */
        .skin-black .left-side, .skin-black .main-sidebar, .skin-black .wrapper {
          background-color: transparent;
        }
        .skin-black .sidebar-menu>li.active>a,.skin-black .sidebar-menu>li:hover>a {
          color:#116229;
          background:#ecf7e9;
        }
        .skin-black .sidebar a {
          color:#116229;
        }
      /* Ajustes painel de escolha das abas */
        a {
          color:#3cbc5c;
        }
      /* Cor painel input */
        .shiny-input-panel {
          background-color:#ecf7e9;
          border:1px solid #ecf7e9;
          border-radius: 8px;
        }
        .selectize-input {
          border: 1px solid #116229;
        }
        .form-control {
          border-color:#116229;
        }
        .nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover {
          color:#116229;
        }

      /* Fonte texto */
        body {
          font-family:"Helvetica Neue",Helvetica,Arial,sans-serif;
          font-size:14px;
          line-height:1.42857143;
          color:#116229;
          background-color:#fff
        }
        ')
    return(usar)
  }
  # flaty_red -------
  if (layout_option=="flaty_red"){
    usar <- HTML('
      /* Cabeçalho faixa direita */
        .skin-black .main-header .navbar {
          background-color:#974646;
        }
        .skin-black .main-header .navbar>.sidebar-toggle {
          color:#333;
          border-right:1px solid #974646;
        }
      /* Cabeçalho faixa esquerda */
        .skin-black .main-header>.logo {
          background-color:#601d1d;
          color:white;
          border-right:1px solid #601d1d;
        }
      /* Cor do fundo */
        .content-wrapper, .right-side {
          background-color: white;
        }
      /* Cor do sidebar */
        .skin-black .left-side, .skin-black .main-sidebar, .skin-black .wrapper {
          background-color: transparent;
        }
        .skin-black .sidebar-menu>li.active>a,.skin-black .sidebar-menu>li:hover>a {
          color:#974646;
          background:#fff0f0;
        }
        .skin-black .sidebar a {
          color:#974646;
        }
      /* Ajustes painel de escolha das abas */
        a {
          color:#f94c4c;
        }
      /* Cor painel input */
        .shiny-input-panel {
          background-color:#fff0f0;
          border:1px solid #fff0f0;
          border-radius: 8px;
        }
        .selectize-input {
          border: 1px solid #974646;
        }
        .form-control {
          border-color:#974646;
        }
        .nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover {
          color:#974646;
        }

      /* Fonte texto */
        body {
          font-family:"Helvetica Neue",Helvetica,Arial,sans-serif;
          font-size:14px;
          line-height:1.42857143;
          color:#974646;
          background-color:#fff
        }
        ')
    return(usar)
  }
}




create_analysis_app <- function(new_authentication= F,
                                data_base=NULL, google_drive=F, sheet=NULL, set_directory=NULL,
                                categorical_vars, numeric_vars_discrete,numeric_vars_continuous,
                                levels_catecorical_vars=NULL,
                                layout_option= "flaty_blue",
                                mostrar_banco = TRUE,
                                big_data = TRUE){
  authenticate(new_authentication)
  dados <- get_data_base(google_drive, set_directory, sheet, data_base)
  dados <- standardize(dados, categorical_vars, numeric_vars_discrete,
                       numeric_vars_continuous, levels_catecorical_vars)
  
  numeric_vars <- c(numeric_vars_continuous, numeric_vars_discrete)
  factor_vars <- categorical_vars
  colunas <- c(numeric_vars, categorical_vars)
  
  # numeric_vars <- transformados[[1]][1]
  # factor_vars <- transformados[[1]][2]
  # colunas <- transformados[[1]][3]
  
  
  if (layout_option=="cerulean"){
    cor_graficos <- "#61BBDC"
    cor_gradiente <- "#0066a2"
  } else if(layout_option=="flaty_blue"){
    cor_graficos <- "#317b9f"
    cor_gradiente <- "#0d3244"
  } else if(layout_option=="flaty_green"){
    cor_graficos <- "#3f8253"
    cor_gradiente <- "#003503"
  } else if(layout_option=="flaty_red"){
    cor_graficos <- "#b04343"
    cor_gradiente <- "#5b0000"
  } else if(layout_option=="superhero"){
    cor_graficos <- "#6987a8"
    cor_gradiente <- "#223244"
  }
  
  
  
  # UI - Interface do Usuário  ----------------------------------------------
  ui <- fluidPage(#theme = shinytheme("superhero"),
    dashboardPage(skin = "black",
                  dashboardHeader(title = "Análise Exploratória"),
                  dashboardSidebar(
                    if (mostrar_banco == TRUE & big_data == T) {
                      sidebarMenu(
                        menuItem("Banco de Dados", tabName = 'banco_de_dados'),
                        menuItem("Resumo dos Dados", tabName = 'resumo_dados'),
                        menuItem('Múltiplos Gráficos', tabName = 'multi_plot'),
                        menuItem("Variável Numérica", tabName = 'var_num'),
                        menuItem("Variável Categórica", tabName = 'var_cat'),
                        menuItem("Variável Numérica e Categórica", tabName = 'var_num_e_cat'),
                        menuItem("Big Data", tabName = 'big_data')
                        #menuItem("Feedback", tabName = 'feedback')
                      )
                    }
                    else if (mostrar_banco == F & big_data == T) {
                      sidebarMenu(
                        menuItem("Resumo dos Dados", tabName = 'resumo_dados'),
                        menuItem('Múltiplos Gráficos', tabName = 'multi_plot'),
                        menuItem("Variável Numérica", tabName = 'var_num'),
                        menuItem("Variável Categórica", tabName = 'var_cat'),
                        menuItem("Variável Numérica e Categórica", tabName = 'var_num_e_cat'),
                        menuItem("Big Data", tabName = 'big_data')
                        #menuItem("Feedback", tabName = 'feedback')
                      )
                    }
                    else if (mostrar_banco == F & big_data == F) {
                      sidebarMenu(
                        menuItem("Resumo dos Dados", tabName = 'resumo_dados'),
                        menuItem('Múltiplos Gráficos', tabName = 'multi_plot'),
                        menuItem("Variável Numérica", tabName = 'var_num'),
                        menuItem("Variável Categórica", tabName = 'var_cat'),
                        menuItem("Variável Numérica e Categórica", tabName = 'var_num_e_cat')
                        #menuItem("Big Data", tabName = 'big_data')
                        #menuItem("Feedback", tabName = 'feedback')
                      )
                    }
                    else if (mostrar_banco == T & big_data == F) {
                      sidebarMenu(
                        menuItem("Banco de Dados", tabName = 'banco_de_dados'),
                        menuItem("Resumo dos Dados", tabName = 'resumo_dados'),
                        menuItem('Múltiplos Gráficos', tabName = 'multi_plot'),
                        menuItem("Variável Numérica", tabName = 'var_num'),
                        menuItem("Variável Categórica", tabName = 'var_cat'),
                        menuItem("Variável Numérica e Categórica", tabName = 'var_num_e_cat')
                        #menuItem("Big Data", tabName = 'big_data')
                        #menuItem("Feedback", tabName = 'feedback')
                      )
                    }
                  ),
                  
                  dashboardBody(
                    tags$head(tags$style(     choose_layout(layout_option)     )),
                    tabItems(
                      tabItem(tabName = "multi_plot",
                              
                              titlePanel("Visualização Simultânea"),
                              inputPanel(selectInput("MG_graficos_var_x", label = "Variável eixo x",
                                                     choices = numeric_vars, selected = numeric_vars[1]),
                                         selectInput("MG_graficos_var_y", label = "Variável eixo Y",
                                                     choices = numeric_vars, selected = numeric_vars[2])),
                              withSpinner(plotOutput("MG_graficos"), type = 5, color = cor_graficos)
                      ),
                      
                      tabItem(tabName = 'banco_de_dados',
                              box(
                                title = "Status summary",
                                width = 35,
                                withSpinner(DTOutput("BD_tabela_dados"),
                                            type = 5, color = cor_graficos)
                              )
                      ),
                      
                      tabItem(tabName = "resumo_dados",
                              tabsetPanel(
                                tabPanel('Resumos',
                                         withSpinner(htmlOutput("RD_Summary"), type = 5, color = cor_graficos) ),
                                
                                tabPanel('Descritivas',
                                         withSpinner(htmlOutput("RD_decritivas"), type = 5, color = cor_graficos) ),
                                
                                tabPanel('Estratificação',
                                         inputPanel(
                                           selectInput("RD_estratificacao_input", label = "Variável para estratificação",
                                                       choices = c(categorical_vars))),
                                         "As tabelas abaixo são apresentadas para os seguintes níveis da variável escolhida",
                                         textOutput('RDE_niveis'),
                                         withSpinner(htmlOutput("RD_estratificacao"), type = 5, color = cor_graficos) ),
                                
                                tabPanel('Frequência',
                                         inputPanel(
                                           selectInput("RD_Frequencia_input", label = "Variável",
                                                       choices = c(categorical_vars,numeric_vars_discrete))),
                                         withSpinner(htmlOutput("RD_Frequencia_output"), type = 5, color = cor_graficos) ),
                                
                                tabPanel('Frequência Cruzada',
                                         inputPanel(
                                           selectInput("RD_Frequencia_cruzada_1", label = "Variável 1",
                                                       choices = c(categorical_vars,numeric_vars_discrete)),
                                           selectInput("RD_Frequencia_cruzada_2", label = "Variável 2",
                                                       choices = c(categorical_vars,numeric_vars_discrete),
                                                       selected = categorical_vars[2])),
                                         box(
                                           withSpinner(htmlOutput("RD_Frequencia_cruzada_output"),
                                                       type = 5, color = cor_graficos),
                                           width = 30,
                                           style = "overflow-x: scroll"
                                         )
                                         
                                ),
                                
                              )),
                      
                      
                      tabItem(tabName = "var_num",
                              tabsetPanel(
                                tabPanel("Histograma",
                                         br(),
                                         inputPanel(
                                           selectInput("VN_histograma_num", label = "Variável numérica",
                                                       choices = numeric_vars_continuous),
                                           #colourInput("VN_histograma_cor", "Cor", cor_graficos)
                                           ),
                                         
                                         withSpinner(plotlyOutput("VN_histograma"), type = 5, color = cor_graficos)
                                ),
                                
                                
                                
                                # tabPanel("Gráfico de Barras",
                                #          br(),
                                #          inputPanel(
                                #          selectInput("input_9", label = "Variável gráfico de barras",
                                #                      choices = c(categorical_vars,numeric_vars_discrete))),
                                #          plotlyOutput("plot_9")),
                                
                                
                                tabPanel("Dispersão",
                                         br(),
                                         inputPanel(
                                           selectInput("VN_dispercao_var_x", label = "Variável eixo x",
                                                       choices = numeric_vars, selected = numeric_vars[1]),
                                           selectInput("VN_dispercao_var_y", label = "Variável eixo Y",
                                                       choices = numeric_vars, selected = numeric_vars[2]),
                                           selectInput("VN_dispercao_var_cor", label = "Variável Cor",
                                                       choices = colunas, selected = colunas[3])),
                                         
                                         
                                         withSpinner(plotOutput("VN_dispercao"), type = 5, color = cor_graficos)
                                ),
                                
                                
                                tabPanel("Correlação",
                                         br(),
                                         # aq é melhor um check box, vai selecionar + de uma var
                                         selectInput("VN_correlacao_vars", label = "Variáveis matriz de correlação",
                                                     choices = numeric_vars, multiple = T, 
                                                     selected = c(numeric_vars[1], numeric_vars[2])),
                                         withSpinner(plotOutput("VN_correlacao"), type = 5, color = cor_graficos)
                                ))),
                      
                      
                      
                      tabItem(tabName = "var_num_e_cat",
                              tabsetPanel(
                                tabPanel('Boxplot',
                                         br(),
                                         
                                         inputPanel(
                                           radioButtons("VNC_boxplot_versao", "Opções de visualização",
                                                        c("Crescente" = "cresc",
                                                          "Decrescente" = "decresc",
                                                          "Escolher a ordem" = "escolha_ordem")),
                                           selectInput("VNC_boxplot_var_num", label = "Variável numérica",
                                                       choices = numeric_vars),
                                           selectInput("VNC_boxplot_var_cat", label = "Variável categorica",
                                                       choices = factor_vars) ),
                                         conditionalPanel( condition = "input.VNC_boxplot_versao == 'escolha_ordem' ",
                                                           orderInput(inputId = "VNC_boxplot_ordem", label = "Ordenação Manual (arraste os botões)",
                                                                      items = c('0', '1')) ),
                                         # orderInput(inputId = "levels_teste", label = "Factor level order",
                                         #            items = levels('plot_5_var_cat')),
                                         # mudando aq 11/07
                                         withSpinner(plotlyOutput("VNC_boxplot"), type = 5, color = cor_graficos)),
                                
                                
                                tabPanel('Poligno de Frequencia',
                                         br(),
                                         inputPanel(selectInput("VNC_poligono_var_num", label = "Variável Numérica",
                                                                choices = numeric_vars_continuous),
                                                    selectInput("VNC_poligono_var_cat", label = "Variável categórica",
                                                                choices = factor_vars)),
                                         withSpinner(plotlyOutput("VNC_poligono"), type = 5, color = cor_graficos)
                                ))),
                      
                      
                      tabItem(tabName = "var_cat",
                              tabsetPanel(
                                tabPanel('Associação', 
                                         br(),
                                         
                                         inputPanel(
                                           radioButtons("VC_associacao", "Opções de visualização",
                                                        c("Calor" = "Calor",
                                                          "Bolinhas" = "Bolinhas")),
                                           selectInput('VC_associacao_var_x', label = 'Primeira variável',
                                                       choices = factor_vars, selected = factor_vars[1]),
                                           selectInput('VC_associacao_var_y', label = 'Segunda variável',
                                                       choices = factor_vars, selected = factor_vars[2])
                                         ),
                                         
                                         
                                         withSpinner(plotOutput("VC_associacao_plot"), type = 5, color = cor_graficos)
                                         
                                ),
                                
                                tabPanel("Gráfico de Barras",
                                         br(),
                                         inputPanel(
                                           selectInput("VC_barras_var_cat", label = "Variável gráfico de barras",
                                                       choices = c(categorical_vars,numeric_vars_discrete))),
                                         withSpinner(plotlyOutput("VC_barras"), type = 5, color = cor_graficos)),
                                
                                tabPanel('Extratificação',
                                         br(),
                                         inputPanel(
                                           selectInput("VC_extratificacao_var_cat", label = "Variável Facetas",
                                                       choices = factor_vars),
                                           selectInput("VC_extratificacao_var_num", label = "Variável numéica",
                                                       choices = numeric_vars)),
                                         
                                         withSpinner(plotlyOutput("VC_extratificacao"), type = 5, color = cor_graficos)))
                      ),
                      
                      
                      tabItem(tabName = "big_data",
                              tabsetPanel(
                                tabPanel('Mapa de Calor',
                                         inputPanel(
                                           selectInput("BD_HeatMap_var_x", label = "Variável eixo x",
                                                       choices = numeric_vars, selected = numeric_vars[1]),
                                           
                                           selectInput("BD_HeatMap_var_y", label = "Variável eixo Y",
                                                       choices = numeric_vars, selected = numeric_vars[2])
                                         ),
                                         
                                         withSpinner(plotlyOutput("BD_HeatMap"), type = 5, color = cor_graficos)
                                ),
                                tabPanel('BoxPlot',
                                         inputPanel(
                                           radioButtons("BD_boxplot_opcao", "Opções de visualização",
                                                        c("Intervalo tamanhos iguais" = "interval",
                                                          "Mesmo Número de observações" = "number")),
                                           selectInput("BD_Box_var_x", label = "Variável eixo x",
                                                       choices = numeric_vars, selected = numeric_vars[1]),
                                           selectInput("BD_Box_var_y", label = "Variável eixo Y",
                                                       choices = numeric_vars, selected = numeric_vars[2]),
                                           sliderInput("BD_Box_num_interv", "Número de Boxplot", value = 10, min = 1, max = 25)
                                           
                                         ),
                                         
                                         withSpinner(plotOutput("BD_Box"), type = 5, color = cor_graficos),
                                         
                                         
                                )# fecha a parte do boxplot big data
                              )
                      ),
                      # fecha a parte do big data
                      
                      
                      tabItem(tabName = "feedback",
                              titlePanel("De sua opinião"),
                              textInput("nome", "Qual seu nome?"),
                              passwordInput("email", "Qual seu email?"),
                              textAreaInput("feedback", "Como podemos melhorar?", rows = 5))
                    ))))
  
  
  
  # Server - Back End (Parte reativa do APP) --------------------------------
  
  server <- function(input, output, session) {
    
    
    ### TAB ITEM RESUMO DOS DADOS ##################################################
    
    output$RD_Summary <- renderUI({summarytools::view(summarytools::dfSummary(dados, headings = FALSE), method = "render") })
    
    output$RD_decritivas <- renderUI({summarytools::view(summarytools::descr(dados, stats = c('mean','sd','min','q1','med', 'q3', 'max'),headings = FALSE), method = "render") })
    
    # .data[[input$RD_Frequencia_input]]
    output$RD_Frequencia_output <- renderUI({summarytools::view(summarytools:: freq(dados[input$RD_Frequencia_input], report.nas = FALSE, headings = FALSE), method = "render") })
    
    # Exite o argumento  headings = FALSE
    # para ser dinamico precisaria acessar 
    # RD_Frequencia_cruzada_1 e RD_Frequencia_cruzada_2

    output$RD_Frequencia_cruzada_output <- renderUI({summarytools::view(summarytools::ctable(x = get(input$RD_Frequencia_cruzada_1, pos = dados), 
                                                                                             y = get(input$RD_Frequencia_cruzada_2, pos = dados), 
                                                                                             dnn = c(paste0('Variável 1: ',input$RD_Frequencia_cruzada_1), 
                                                                                                     paste0('Variável 2: ',input$RD_Frequencia_cruzada_2)),
                                                                                             prop = 'r', headings = F, style = 'grid') , method = "render") })
    
    
    # output$RD_estratificacao <- renderUI({summarytools::view(summarytools::stby(data= dados ,INDICES = dados[input$RD_estratificacao_input], FUN = descr, stats='common'),
    #                                                          method = "render") })
    
    output$RD_estratificacao <- renderUI({print(summarytools::stby(data= dados ,INDICES = dados[input$RD_estratificacao_input],
                                                                   FUN = descr, stats='common', transpose=T, headings = T),
                                                             method = "render") })
    
    output$RDE_niveis <- renderText({
      paste('Nivel:', paste(levels(dados[[input$RD_estratificacao_input]])), "  ", collapse = ',  ')
      })
    
    ##################################################################################
    
    
    output$BD_tabela_dados <- renderDT({  DT::datatable(dados) })
    
    
    output$VN_histograma<- renderPlotly({
      ggplot(dados,aes(x=.data[[input$VN_histograma_num]]))+geom_histogram(color= 'black' , fill=cor_graficos)})
    
    
    
    output$VN_dispercao <- renderPlot({
      if ( sum(categorical_vars==input$VN_dispercao_var_cor) == 0 ){
        ggplot(dados, aes(x = .data[[input$VN_dispercao_var_x]], y = .data[[input$VN_dispercao_var_y]])) +
          geom_point( aes(colour = .data[[input$VN_dispercao_var_cor]]) ) +
          scale_colour_gradient(low = "grey80", high = cor_gradiente)
      } else if (sum(categorical_vars=="clarity") != 0){ggplot(dados, aes(x = .data[[input$VN_dispercao_var_x]], y = .data[[input$VN_dispercao_var_y]])) +
          geom_point( aes( colour = .data[[input$VNC_poligono_var_cat]] ) )
      }
      })
    
    
    
    #11/09  ordenamento boxplot
    observeEvent(input$VNC_boxplot_var_cat, {
      niveis <- levels(as.list(dados[,names(dados)==input$VNC_boxplot_var_cat])[[1]])
      updateOrderInput(session, inputId = "VNC_boxplot_ordem",
                       items = niveis)
    })
    output$VNC_boxplot <- renderPlotly({
      if (input$VNC_boxplot_versao == "escolha_ordem"){
        ggplot(dados, aes(x = factor(.data[[input$VNC_boxplot_var_cat]], levels = input$VNC_boxplot_ordem),
                          y = .data[[input$VNC_boxplot_var_num]]))+
          xlab(input$VNC_boxplot_var_cat)+
          geom_boxplot(fill = cor_graficos)
      } else if (input$VNC_boxplot_versao == "cresc"){
        ggplot(dados,
               aes(x = reorder(.data[[input$VNC_boxplot_var_cat]], .data[[input$VNC_boxplot_var_num]], median),
                   y = .data[[input$VNC_boxplot_var_num]]))+
          xlab(input$VNC_boxplot_var_cat)+
          geom_boxplot(fill = cor_graficos)
      } else if (input$VNC_boxplot_versao == "decresc"){
        ggplot(dados,
               aes(x = reorder(.data[[input$VNC_boxplot_var_cat]], .data[[input$VNC_boxplot_var_num]], median, decreasing=T),
                   y = .data[[input$VNC_boxplot_var_num]]))+
          xlab(input$VNC_boxplot_var_cat)+
          geom_boxplot(fill = cor_graficos)
      }
      
    })
    
    
    output$VC_extratificacao <- renderPlotly({
      ggplot(dados, aes(x = .data[[input$VC_extratificacao_var_num]]))+
        geom_histogram( color='black', fill = cor_graficos)+
        facet_wrap(~.data[[input$VC_extratificacao_var_cat]],nrow=2)})
    
    ## aq pra baixo vai ser o plot 8
    output$VN_correlacao <- renderPlot({
      
      x <- dados
      
      ggpairs((x), columns = input$VN_correlacao_vars ,title="Matriz de Correlação")
    })
    
    output$VC_barras <- renderPlotly({
      ggplot(dados,aes(x=.data[[input$VC_barras_var_cat]]))+geom_bar( color='black', fill = cor_graficos)})
    
    
    output$VNC_poligono <- renderPlotly({
      ggplot(dados,aes(x = .data[[input$VNC_poligono_var_num]], colour = .data[[input$VNC_poligono_var_cat]]))+geom_freqpoly()})
    
    
    output$MG_graficos <- renderPlot({ 
      p1 <-  ggplot(dados) + geom_point(aes(.data[[input$MG_graficos_var_x]], .data[[input$MG_graficos_var_y]]))
      p2 <-  ggplot(dados) + geom_histogram(aes(.data[[input$MG_graficos_var_y]]), color='black', fill = cor_graficos)
      p3 <- ggplot(dados) + geom_smooth(aes(.data[[input$MG_graficos_var_x]], .data[[input$MG_graficos_var_y]]), color = cor_graficos)
      p4 <- ggplot(dados) + geom_histogram(aes(.data[[input$MG_graficos_var_x]]), color='black', fill = cor_graficos)
      (p1 | p2 ) / (p3 |p4 )
    })
    
    ################### 13/09
    
    output$VC_associacao_plot <- renderPlot({ if (input$VC_associacao == "Bolinhas"){
      ggplot(data = dados) +
        geom_count(mapping = aes(x = .data[[input$VC_associacao_var_x]],  y = .data[[input$VC_associacao_var_y]]), color = cor_graficos)
    } else if (input$VC_associacao == "Calor"){
      dados %>%
        count(.data[[input$VC_associacao_var_x]], .data[[input$VC_associacao_var_y]]) %>%
        ggplot( mapping = aes(x=.data[[input$VC_associacao_var_x]], y = .data[[input$VC_associacao_var_y]])) +
        geom_tile(mapping = aes(fill= n))+
        scale_fill_gradient(low = "white", high = cor_gradiente)
    }})
    
    ######################################
    
    output$BD_HeatMap <- renderPlotly({
      ggplot(data = dados) + geom_hex(aes(x = .data[[input$BD_HeatMap_var_x]], y = .data[[input$BD_HeatMap_var_y]]))+
        scale_fill_gradient(low = "grey", high = cor_gradiente)
     })
    
    ################ 13/09
    
    output$BD_Box<- renderPlot({ if (input$BD_boxplot_opcao == "number"){
      ggplot(data =  dados, aes(x = .data[[input$BD_Box_var_x]], y = .data[[input$BD_Box_var_y]]))+
        geom_boxplot(fill = cor_graficos, mapping = aes(group = cut_number(x = .data[[input$BD_Box_var_x]],n = input$BD_Box_num_interv)))
    } else if (input$BD_boxplot_opcao == "interval"){
      ggplot(data =  dados, aes(x = .data[[input$BD_Box_var_x]], y = .data[[input$BD_Box_var_y]]))+
        geom_boxplot(fill = cor_graficos, mapping = aes(group = cut_interval(x = .data[[input$BD_Box_var_x]], n = input$BD_Box_num_interv)))
    }})
    
    ################################
    
    } # essa chave fecha o server function
  
  
  
  # Juntando UI + Server ----------------------------------------------------
  
  return(shinyApp(ui,server)) }



# create_analysis_app(new_authentication= F,
#                     data_base=mtcars, google_drive=F, sheet=NULL, set_directory=NULL,
#                     categorical_vars = c('vs','am'),
#                     numeric_vars_discrete = c('cyl','carb','gear'),
#                     numeric_vars_continuous = c('mpg','disp','hp','drat','wt','qsec'),
#                     levels_catecorical_vars= 
#                       list(vs= c(0,1),
#                            am= c(1,0))
#                     )

create_analysis_app(new_authentication= F, google_drive=F, sheet=NULL,
                    data_base=diamonds, set_directory=NULL,
                    categorical_vars = c('cut','color','clarity'),
                    numeric_vars_discrete = c(),
                    numeric_vars_continuous = c('carat','depth','table','price',
                                                'x','y','z'),
                    levels_catecorical_vars= 
                      list(cut= c("Ideal","Fair","Good","Very Good", "Premium"),
                           color= c("I", "J", "D", "E", "F", "G", "H"),
                           clarity= c("I1","SI2","SI1","VS2","VS1","VVS2","VVS1","IF")),
                    layout_option= "flaty_blue",
                    mostrar_banco = T, big_data = T )

