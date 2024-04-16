#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(writexl)
library(DT)
library(dplyr)
library(shinydashboard)

#teste

sinapi_comp <- read_xlsx("www/DB/DESONERADO/SINAPI_Custo_Ref_Composicoes_Sintetico_SC_202402_Desonerado.xlsx",skip = 4)
table_sinapi <- sinapi_comp[c("CODIGO  DA COMPOSICAO", "DESCRICAO DA COMPOSICAO","UNIDADE","CUSTO TOTAL")]

# Definindo novos nomes abreviados para as colunas
names(table_sinapi) <- c("COD", "DESCRICAO DA COMPOSICAO","UNID","PREÇO")

gsub(",",".","10,4")

table_sinapi$PREÇO<-as.numeric(gsub(",",".",table_sinapi$PREÇO))
# adiconar botão


ui <- fluidPage(
  titlePanel("Orçamento"),
  
  # Emulando uma 'box' com 'wellPanel' e estilos personalizados
  wellPanel(
    style = "padding: 20px; border: solid 1px #337ab7; border-radius: 5px; background-color: #fff;",
    title = h3("Meu Painel", style = "color: #337ab7;"),  # Título da 'box'
    
    # Formulário de seleção no topo da página
    fluidRow(
      column(2, selectInput("tableType", "Tipo de Tabela", 
                            choices = c("SINAPI", "PRÓPRIA"))),
      column(3, selectInput("searchType", "Tipo de Pesquisa", 
                            choices = c("INSUMO", "COMPOSIÇÕES"))),
      column(3, selectInput("taxStatus", "Desoneração", 
                            choices = c("ONERADO", "DESONERADO", "INDEFINIDO"))),
      column(2, selectInput("State", "Local", 
                            choices = c("SC", "PR"))),
      column(2, selectInput("reference", "Referência", 
                            choices = c("2024-02", "2024-01")))
    )),
    
    wellPanel(
      style = "padding: 20px; border: solid 1px #337ab7; border-radius: 5px; background-color: #fff;",
      title = h3("Meu Painel", style = "color: #337ab7;"),  # Título da 'box'
    
    fluidRow(
      div(style = "background-color: #f2f2f2; padding: 0px;",
          h3("Lista de Serviços e Materiais", style = "text-align: center;"),
      ),
      div(style = "height: 5px;"),
      
      column(12, DTOutput("materialTable"), style = "margin: auto;")
    )
  ),
  
  fluidRow(
    column(8, textInput("searchBox", "Pesquisar Material")),
    column(4, actionButton("addButton", "Adicionar ao Orçamento",width="100%"))
  ),
  
  
  wellPanel(
    style = "padding: 20px; border: solid 1px #337ab7; border-radius: 5px; background-color: #fff;",
    title = h3("budget", style = "color: #337ab7;"),  # Título da 'box',
    
    fluidRow(
      column(3, selectInput("ajusteDecimal", "Ajuste Decimal", 
                            choices = c("ARREDONDAMENTO", "TRUCAMENTO"))),
      column(3, selectInput("tipoBDI", "Tipo de BDI", 
                            choices = c("ÚNICO", "SEM BDI"))),
      
      column(3, conditionalPanel(
        condition = "input.tipoBDI === 'ÚNICO'",
        numericInput("valorBDI", "Valor do BDI (%):", value = 0)
      ))
      
    ),

    fluidRow(
      div(style = "background-color: #f2f2f2;",
          h3("Orçamento", style = "text-align: center;"),
      ),
      div(style = "height: 5px;"),
      column(12, DTOutput("budgetTable"), style = "margin: auto;"),
      div(style = "height: 5px;"),
      div(style = "background-color: #f2f2f2; padding: 20px;text-align: right; font-weight: bold",
          h4("Totais", style = "text-align: right;"),
          textOutput("totalSemBDI"),
          textOutput("totalComBDI")
      )
      
      )
    
    )

)


server <- function(input, output, session) {
  
  selectedMaterials <- reactiveVal(data.frame(Codigo = character(), 
                                              Descricao = character(), 
                                              Unidade = character(), 
                                              CustoTotal = numeric(), 
                                              QTD = numeric(), 
                                              `TOTAL S/BDI` = numeric(),
                                              `TOTAL C/BDI` = numeric()))
  

  datatableData <- table_sinapi
  

  output$materialTable <- renderDT({
    
    #adicionarButtons = sapply(1:nrow(datatableData), function(i) { as.character(actionButton(inputId = paste0("incluir_", i), label = "Incluir", onclick = sprintf("Shiny.setInputValue('incluir_item', %d, {priority: 'event'})", i)))})
    
    # Criar uma nova coluna com botões de adição
    #adicionarButtons <- sprintf('<button onclick="Shiny.setInputValue(\'add_item_%s\', TRUE, {priority: \'event\'});" class="btn btn-primary btn-sm">INCLUIR</button>', 1:nrow(datatableData))
    adicionarButtons <- sapply(1:nrow(datatableData), function(i) {
      sprintf("<button onclick='Shiny.setInputValue(\"addButton\", %d, {priority: \"event\"})' class='btn btn-primary'>INCLUIR</button>", i)
    })
    
    datatableData <- cbind(datatableData,ADD = adicionarButtons)
    
    # Renderizar a datatable sem a numeração automática das linhas e limitando a exibição a 5 linhas
    datatable(datatableData, 
              escape = FALSE, 
              selection = 'single', 
              options = list( paging = TRUE, pageLength = 3,
                              columnDefs = list(
                                list(width = '50%', targets = 1)  # Ajuste o índice para corresponder à sua coluna
                              )),
              rownames = FALSE) # Desativa a numeração automática das linhas
  })
  
  
  # Observar o evento do botão para adicionar o item ao orçamento

  observeEvent(input$addButton, {
    req(input$materialTable_rows_selected)
    selectedRow <- datatableData[input$materialTable_rows_selected, , drop = FALSE]
    selectedRow$QTD <- 0
    selectedRow$`TOTAL S/BDI` <- 0
    selectedRow$`TOTAL C/BDI` <- 0
    currentSelected <- selectedMaterials()
    selectedMaterials(rbind(currentSelected, selectedRow))
  })
  
  output$budgetTable <- renderDT({
    df <- selectedMaterials()
    if (nrow(df) > 0) {
  
      df$DEL <- sprintf('<button onclick="Shiny.setInputValue(\'delete_row\', %s, {priority: \'event\'});" class="btn btn-danger btn-sm">🗑️</button>', 1:nrow(df))
      datatable(df,
                extensions = 'Buttons', # Habilitar extensões de botões
                editable = list(target = 'cell', disable = list(columns = c(0, 1, 2, 3, 5, 6))), 
                escape = FALSE,
                selection = 'none',
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('copy','excel', 'pdf'),
                  pageLength = 10,
                  columnDefs = list(
                    list(width = '50%', targets = 1)  # Ajustar o índice para corresponder à sua coluna
                  )
                ),
                rownames = FALSE,
      )
      
    } else {
      datatable(df, selection = 'none')
    }
  }, server = FALSE)
  
  
  proxy <- dataTableProxy('budgetTable')
  
  # Observar mudanças e recalcular o total
  observeEvent(input$budgetTable_cell_edit, {
    info <- input$budgetTable_cell_edit
    str(info)  # Debugging output para o console
    i <- info$row
    j <- info$col
    v <- as.numeric(info$value)
    
    # Recuperar o dataframe atual
    df_temp <- selectedMaterials()
    # Calcular o novo valor se as colunas corretas forem editadas
    if (j == 4) {  # Coluna Quantidade
      df_temp [i, 5] <- v
      df_temp [i, 6] <- floor(df_temp [i, 5]*df_temp [i, 4] * 100) / 100   # Recalcular total
      
      if(input$tipoBDI == 'ÚNICO'){df_temp [i, 7] <- floor(df_temp [i, 5]*df_temp [i, 4]*(1+input$valorBDI/100) * 100) / 100
      }else{
        df_temp [i, 7] <- floor(df_temp [i, 5]*df_temp [i, 4]*(1+0) * 100) / 100
      }
      
    }
    selectedMaterials(df_temp )  # Atualizar o valor reativo
    })
  
  # observar TIPO de BDI
  observeEvent(input$tipoBDI, {
    df_temp <- selectedMaterials()
    
    if(input$tipoBDI == 'ÚNICO'){df_temp [, 7] <- floor(df_temp [, 5]*df_temp [, 4]*(1+input$valorBDI/100) * 100) / 100
    }else{
      df_temp [, 7] <- floor(df_temp [, 5]*df_temp [, 4]*(1+0) * 100) / 100
    }
    selectedMaterials(df_temp )  # Atualizar o valor reativo
  })
  
  # observar mundança de BDI
  observeEvent(input$valorBDI, {
    df_temp <- selectedMaterials()
    
    if(input$tipoBDI == 'ÚNICO'){df_temp [, 7] <- floor(df_temp [, 5]*df_temp [, 4]*(1+input$valorBDI/100) * 100) / 100
    }else{
      df_temp [, 7] <- floor(df_temp [, 5]*df_temp [, 4]*(1+0) * 100) / 100
    }
    selectedMaterials(df_temp )  # Atualizar o valor reativo
  })
  
  # observar DELETE LINHA
  observeEvent(input$delete_row, {
    df <- selectedMaterials()
    if (!is.null(df) && nrow(df) >= input$delete_row) {
      df <- df[-as.numeric(input$delete_row), ]
      selectedMaterials(df)
    }
    
    
  }, ignoreNULL = TRUE)
  
  
  # Calcular e mostrar os totais
  output$totalSemBDI <- renderText({
    paste("Total sem BDI: R$", 2323)
  })
  output$totalComBDI <- renderText({
    paste("Total com BDI: R$", 2332)
  })
  
  
}

shinyApp(ui, server)


