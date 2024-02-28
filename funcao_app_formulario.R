# Carregando os pacotes  --------------------------------------------------

library(tidyverse)
library(shiny)
library(shinysurveys)
library(googledrive)
library(googlesheets4)
library(shinydashboard)
library(shinythemes)

# Autenticação ------------------------------------------------------------
# Funcao para autenticar ou usar chave de autenticacao anterior:

authenticate <- function(new_authentication= F, create_google_sheet= F, sheet= "Shiny_Survey_application"){
  if (new_authentication==T){
    # Para novas autenticacoes, deletar pasta .secrets pre-existente e criar outra com novas tokens
    if ( length(list.files(".secrets"))>0 ){
      file.remove(paste(".secrets", list.files(".secrets"), sep = "/")) }
    options(
      gargle_oauth_email = TRUE,
      gargle_oauth_cache = ".secrets"
    )
    googledrive::drive_auth()
    googlesheets4::gs4_auth()
    
    # Caso não exista uma planilha no Google, criar agora
    if (create_google_sheet== T){googlesheets4::gs4_create(name = sheet)}
    }
  
  if (new_authentication==F) {
    if ( length(list.files(".secrets"))!=2 ){
      # Caso não haja duas token, uma nova autenticacao se faz necessaria
      return("Uma nova autenticação é necessária") }
    else{
      options(
        gargle_oauth_email = TRUE,
        gargle_oauth_cache = ".secrets"
      )
      # Caso não exista uma planilha no Google, criar agora
      if (create_google_sheet== T){googlesheets4::gs4_create(name = sheet)} }
  }
  if (new_authentication!=T & new_authentication!=F) {
    return("O argumento new_authentication precisa ser lógico")
  }
  
}


# Função para criar data frame de Perguntas  --------------------------------------------------

create_questions_df <- function(questions_list){
  question <- option <- input_type <- input_id <- dependence <- dependence_value <- required <- NULL
  count_loop <- 0
  for (i in questions_list) {
    count_loop <- count_loop + 1
    if (i$input_type == "mc" | i$input_type == "select" | i$input_type == "y/n"){
      question <- c(question, rep(i$question, length(i$option)) )
      option <- c(option, i$option )
      input_type <- c(input_type, rep(i$input_type, length(i$option)) )
      input_id <- c(  input_id, rep( names(questions_list)[count_loop], length(i$option) )  )
      if (class(i$dependence) == "NULL"){ dependence <- c(dependence, rep(NA, length(i$option)) ) }
      else{ dependence <- c(dependence, rep(i$dependence, length(i$option)) ) }
      if (class(i$dependence_value) == "NULL"){ dependence_value <- c(dependence_value, rep(NA, length(i$option)) ) }
      else{ dependence_value <- c(dependence_value, rep(i$dependence_value, length(i$option)) ) }
      required <- c(required, rep(i$required, length(i$option)) )
    }
    
    else{
      question <- c(question, i$question)
      option <- c(option, "resposta")
      input_type <- c(input_type, i$input_type)
      input_id <- c(input_id, names(questions_list)[count_loop])
      if (class(i$dependence) == "NULL"){ dependence <- c(dependence, NA) }
      else{ dependence <- c(dependence, i$dependence) }
      if (class(i$dependence_value) == "NULL"){ dependence_value <- c(dependence_value, NA) }
      else{ dependence_value <- c(dependence_value, i$dependence_value) }
      required <- c(required, i$required)
    }
  }
  data_compara <- data.frame(question = question,
                             option = option,
                             input_type = input_type,
                             input_id = input_id,
                             dependence = dependence,
                             dependence_value = dependence_value,
                             required = required)
  return(data_compara)
}



# Função para escolher layout --------------------------------------------------

choose_layout <- function(layout_option= "flaty_blue"){
  if (layout_option == "superhero") {
    usar <- HTML('
      /* Cabeçalho faixa direita */
        .skin-blue .main-header .navbar {
          background-color:#7b848e;
        }
      /* Cabeçalho faixa esquerda */
        .skin-blue .main-header .logo {
          background-color:#ea8113;
        }
      /* Cor do fundo */
        .content-wrapper, .right-side {
          background-color: #40556f;
        }
      /* Margens do questionário */
        .container-fluid {
          padding-right:180px;
          padding-left:180px;
          margin-right:auto;
          margin-left:auto;
        }
      /* Ajustes nos quadros de pergunstas */
        .container-fluid .survey .questions {
          display: grid;
          background-color: #7b848e;
            /*! border: 0.5px solid white; */
            /*! border-radius: 30px; */
          margin-bottom: 30px;
          padding: 17px;
          min-height: 13px;
          font-size: 1.4rem;
        }
      /* Borda nos quadros de pergunta */
        .form-control {
          border:5px double #ea8113;
        }
      /* Margem dentro dos quadros de pergunta */
        .shiny-input-container:not(.shiny-input-container-inline) {
          width:800px;
          max-width:100%;
        }
      /* Fonte do texto */
        body {
          font-family:"Helvetica Neue",Helvetica,Arial,sans-serif;
          font-size:14px;
          line-height:1.42857143;
          color:white;
          background-color:#fff
        }
      /* Cor do asterisco */
        .required {
          color: #5bffa3;
        }
      /* Botão de submeter */
        .btn-default {
          background-color:#ea8113;
          color:white;
          border-color:#ddd;
        }
      /* Cor da fonte na caixa de submissão */
        .h1,.h2,.h3,.h4,.h5,.h6,h1,h2,h3,h4,h5,h6 {
          font-family:inherit;
          font-weight:500;
          line-height:1.1;
          color: #f47c23;
        }

                 ')
    return(usar)}
  if (layout_option=="flaty_blue"){
    usar <- HTML('
      /* Coloca uma linha debaido do 2º título */
        .container-fluid .survey .title-description {
          border-top: 3px solid #365782;
        }
      /* Cabeçalho faixa direita */
        .skin-blue .main-header .navbar {
          background-color:#114762;
        }
      /* Cabeçalho faixa esquerda */
        .skin-blue .main-header .logo {
          background-color:#0d3244;
        }
      /* Cor do fundo */
        .content-wrapper, .right-side {
          background-color: white;
        }
      /* Margens do questionário */
        .container-fluid {
          padding-right:180px;
          padding-left:180px;
          margin-right:auto;
          margin-left:auto;
        }
      /* Ajustes quadros das pergunstas */
        .container-fluid .survey .questions {
          display: grid;
          background-color: #ecf4f4;
          /*! border: 0.5px solid white; */
          border-radius: 10px;
          margin-bottom: 10px;
          padding: 15px;
          min-height: 13px;
          font-size: 1.4rem;
        }
      /* Margem dentro dos quadros de pergunta */
        .shiny-input-container:not(.shiny-input-container-inline) {
          width:800px;
          max-width:100%;
        }
      /* Fonte texto */
        body {
          font-family:"Helvetica Neue",Helvetica,Arial,sans-serif;
          font-size:14px;
          line-height:1.42857143;
          color:#114762;
          background-color:#fff
        }
      /* Cor do asterisco */
        .required {
          color: #48d25e;
        }
      /* Botão de submeter */
        .btn-default {
          background-color:#114762;
          color:white;
          border-color:#ddd;
        }
        ')
    return(usar)
  }
  if (layout_option=="cerulean") {
    usar <- HTML('
      /* Cabeçalho faixa direita */
        .skin-blue .main-header .navbar {
          background-color:#00cfffd6;
        }
      /* Cabeçalho faixa esquerda */
        .skin-blue .main-header .logo {
          background-color:#2b88bf;
        }
      /* Fonte 3° titulo BOA TARDE */
        .h1, .h2, .h3, .h4, .h5, .h6, h1, h2, h3, h4, h5, h6 {
          font-family:inherit;
          font-weight:600;
          line-height:1.1;
          color:inherit
        }
      /* Cor do fundo */
        .content-wrapper, .right-side {
          background-color: white;
        }
      /* Margens do questionário */
        .container-fluid {
          padding-right:180px;
          padding-left:180px;
          margin-right:auto;
          margin-left:auto;
        }
      /* Ajustes quadros das pergunstas */
        .container-fluid .survey .questions {
          display: grid;
          background-color: #f2f2f2;
          border: 0.5px solid #d7d7d7;
          border-radius: 10px;
          margin-bottom: 10px;
          padding: 15px;
          min-height: 13px;
          font-size: 1.4rem;
        }
      /* Ajustes quadros das pergunstas (margem e peso fonte)*/
        label {
          display:inline-block;
          max-width:100%;
          margin-bottom:15px;
          font-weight:500;
        }
      /* Margem dentro dos quadros de pergunta */
        .shiny-input-container:not(.shiny-input-container-inline) {
          width:800px;
          max-width:100%;
        }
      /* Fonte texto */
        body {
          font-family:"Helvetica Neue",Helvetica,Arial,sans-serif;
          font-size:14px;
          line-height:1.42857143;
          color:#2b6cbf;
          background-color:#fff
        }
      /* Cor do asterisco */
        .required {
          color: #48d25e;
        }
      /* Botão de submeter */
        .btn-default {
          background-color:#3bbef0;
          color:white;
          border-color:#ddd;
        }
        ')
    return(usar)
  }
  if (layout_option=="flaty_green"){
    usar <- HTML('
      /* Coloca uma linha debaido do 2º título */
        .container-fluid .survey .title-description {
          border-top: 3px solid #116229;
        }
      /* Cabeçalho faixa direita */
        .skin-blue .main-header .navbar {
          background-color:#116229;
        }
      /* Cabeçalho faixa esquerda */
        .skin-blue .main-header .logo {
          background-color:#0d4410;
        }
      /* Cor do fundo */
        .content-wrapper, .right-side {
          background-color: white;
        }
      /* Margens do questionário */
        .container-fluid {
          padding-right:180px;
          padding-left:180px;
          margin-right:auto;
          margin-left:auto;
        }
      /* Ajustes quadros das pergunstas */
        .container-fluid .survey .questions {
          display: grid;
          background-color: #ecf7e9;
          /*! border: 0.5px solid white; */
          border-radius: 10px;
          margin-bottom: 10px;
          padding: 15px;
          min-height: 13px;
          font-size: 1.4rem;
        }
      /* Margem dentro dos quadros de pergunta */
        .shiny-input-container:not(.shiny-input-container-inline) {
          width:800px;
          max-width:100%;
        }
      /* Borda (cor) dos quadros de pergunta */
        .form-control {
          border:1px solid #a4dd8f;
          border-radius:0;
          box-shadow:none;
          border-color:#a4dd8f;
        }
      /* Fonte texto */
        body {
          font-family:"Helvetica Neue",Helvetica,Arial,sans-serif;
          font-size:14px;
          line-height:1.42857143;
          color:#116229;
          background-color:#fff
        }
      /* Cor do asterisco */
        .required {
          color: #48d25e;
        }
      /* Botão de submeter */
        .btn-default {
          background-color:#116229;
          color:white;
          border-color:#ddd;
        }
        ')
    return(usar)
  }
  if (layout_option=="flaty_red"){
    usar <- HTML('
      /* Coloca uma linha debaido do 2º título */
        .container-fluid .survey .title-description {
          border-top: 3px solid #974646;
        }
      /* Cabeçalho faixa direita */
        .skin-blue .main-header .navbar {
          background-color:#974646;
        }
      /* Cabeçalho faixa esquerda */
        .skin-blue .main-header .logo {
          background-color:#601d1d;
        }
      /* Cor do fundo */
        .content-wrapper, .right-side {
          background-color: white;
        }
      /* Margens do questionário */
        .container-fluid {
          padding-right:180px;
          padding-left:180px;
          margin-right:auto;
          margin-left:auto;
        }
      /* Ajustes quadros das pergunstas */
        .container-fluid .survey .questions {
          display: grid;
          background-color: #fff0f0;
          /*! border: 0.5px solid white; */
          border-radius: 10px;
          margin-bottom: 10px;
          padding: 15px;
          min-height: 13px;
          font-size: 1.4rem;
        }
      /* Margem dentro dos quadros de pergunta */
        .shiny-input-container:not(.shiny-input-container-inline) {
          width:800px;
          max-width:100%;
        }
      /* Borda (cor) dos quadros de pergunta */
        .form-control {
          border:1px solid #974646;
          border-radius:0;
          box-shadow:none;
          border-color:#974646;
        }
      /* Fonte texto */
        body {
          font-family:"Helvetica Neue",Helvetica,Arial,sans-serif;
          font-size:14px;
          line-height:1.42857143;
          color:#974646;
          background-color:#fff
        }
      /* Cor do asterisco */
        .required {
          color: #48d25e;
        }
      /* Botão de submeter */
        .btn-default {
          background-color:#974646;
          color:white;
          border-color:#ddd;
        }
        ')
    return(usar)
  }
}



# Função para gerar Questionario --------------------------------------------------

generate_survey_app <- function( new_authentication= F, create_google_sheet= F, sheet= "Shiny_Survey_application",
                                 institute_name, survey_name,
                                 greeting= "Saudações", survey_description= "Pesquisa de Campo",
                                 logo= NULL, hiperlink_logo= NULL, layout_option= "flaty_blue", questions_list){
  authenticate(new_authentication, create_google_sheet, sheet)
  
  df_perguntas <- as_tibble(create_questions_df(questions_list))
  ui <- dashboardPage(
    dashboardHeader(title= institute_name,
                    titleWidth = 400),
    dashboardSidebar(disable = T),
    dashboardBody(
      fluidPage(
        #shinythemes::themeSelector(),
        titlePanel(title=div(
          tags$strong(survey_name),tags$a(href=hiperlink_logo,
                                          img(src=logo, width = '16%', align="right"))),
          windowTitle = 'Pesquisa de Campo'),
        surveyOutput(df = df_perguntas,
                     survey_title = greeting,
                     survey_description = survey_description,
                     theme= NULL)),
      tags$head(tags$style(     choose_layout(layout_option)     )))
  )
  
  server <- function(input, output, session) {
    renderSurvey()
    
    observeEvent(input$submit, {
      showModal(modalDialog(
        title = "Obrigado por responder!"))
      response_data <- getSurveyData()
      respostas <- as.data.frame(t(response_data$response))
      sheet_id <- drive_get(sheet)$id
      sheet_append(data = respostas, ss = sheet_id )
      stopApp()
      
    })
  }
  
  return(shinyApp(ui, server))
}

# Gerando Questionario --------------------------------------------------

# argumentos necessários: q, tipo, obg, resp, dep_q, dep_r # nomes padrão shinysurveys
survey_questions_list <- list(
  q1 = list(question = "Em qual curso você matriculado na UFMG? (Por favor, digite o nome completo)",
            input_type = "text",
            required = FALSE),
  q2 = list(question = "Qual é a sua idade? (Digite apenas número inteiro)",
            input_type = "numeric",
            required = FALSE),
  q3 = list(question = "Quantos irmãos você tem?",
            input_type = "numeric",
            required = TRUE),
  q4 = list(question = "Com qual etnia você se identifica?",
            input_type = "mc",
            required = TRUE,
            option = c('Branco', 'Preto', 'Pardo','Amarelo', 'Indigena')),
  q5 = list(question = "Com qual gênero você se identifica?",
            input_type = "mc",
            required = TRUE,
            option = c('Masculino', 'Feminino','Outros')),
  q6 = list(question = "Com qual gênero você se identifica?",
            input_type = "text",
            required = TRUE,
            dependence = "q5",
            dependence_value = "Outros"),
  q7 = list(question = "Qual é a sua altura? (Responda em metros, utilize vírgula como separador decimal)",
            input_type = "numeric",
            required = TRUE),
  q8 = list(question = "Em qual cidade você residia antes de iniciar os estudos na UFMG? (Responda apenas o nome da cidade, favor não utilizar siglas)",
            input_type = "text",
            required = TRUE),
  q9 = list(question = "Quanto tempo em minutos, em média, você gasta para chegar até a UFMG (Campus Pampulha)?",
            input_type = "numeric",
            required = TRUE),
  q10 = list(question = "Quanto tempo de atividade física, em média, você realiza por semana? (Informe em minutos)",
             input_type = "numeric",
             required = TRUE),
  q11 = list(question = "Quantos dias na semana você realiza atividades físicas? (Responda com um número)",
             input_type = "numeric",
             required = TRUE),
  q12 = list(question = "Por volta de quantos livros de literatura você leu em 2021?",
             input_type = "numeric",
             required = TRUE),
  q13 = list(question = "Quanto tempo por semana, em média, você gasta assistindo televisão ou serviços de streaming? (Informe em minutos)",
             input_type = "numeric",
             required = TRUE),
  q14 = list(question = "Qual tipo de escola você frequentou durante o Ensino Médio?",
             input_type = "mc",
             required = TRUE,
             option = c('Municipal','Estadual','Federal','Particular','Outros')),
  q15 = list(question = "Em qual ano do curso você se encontra?",
             input_type = "text",
             required = TRUE),
  q16 = list(question = "Em quantas disciplinas você está matriculado(a)? (Responda com um número)",
             input_type = "numeric",
             required = TRUE),
  q17 = list(question = "Além do tempo dedicado a assistir aulas presenciais, quanto tempo (em média) você dedica aos estudos por semana? (Informe em minutos)?",
             input_type = "numeric",
             required = TRUE),
  q18 = list(question = "Em quantos idiomas (além da sua língua materna) você é fluente?",
             input_type = "numeric",
             required = TRUE),
  q19 = list(question = "Qual a sua pretensão salarial quando recém formado?",
             input_type = "text",
             required = TRUE),
  q20 = list(question = "Possui animais de estimação?",
             input_type = "text",
             required = TRUE),
  q21 = list(question = "Possui carteira de habilitação?",
             input_type = "mc",
             required = TRUE,
             option = c('Sim - para carro','Sim - para motocicleta','Sim - para carro e motocicleta','Não')),
  q22 = list(question = "Você consome bebidas alcoólicas?",
             input_type = "y/n",
             required = TRUE,
             option = c('Sim','Nao')),
  q23 = list(question = "Você fuma?",
             input_type = "y/n",
             required = TRUE,
             option = c('Sim','Nao'))
)


generate_survey_app( new_authentication= F,
                     create_google_sheet= F,
                     sheet= "planilha_IC_Shiny",
                     institute_name= "Universidade Federal de Minas Gerais",
                     survey_name= "Questionário perfil de Alunos UFMG",
                     greeting= "Boa tarde",
                     survey_description= "Pesquisa sobre o perfil dos alunos  da UFMG",
                     logo= "logo.png",
                     hiperlink_logo= "http://www.est.ufmg.br",
                     questions_list= survey_questions_list)
