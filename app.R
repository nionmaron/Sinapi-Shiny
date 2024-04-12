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

table_sinapi$`CUSTO TOTAL`

ui <- fluidPage(
  titlePanel("Orçamento"),
  
  # Emulando uma 'box' com 'wellPanel' e estilos personalizados
  wellPanel(
    style = "padding: 20px; border: solid 1px #337ab7; border-radius: 5px; background-color: #fff;",
    title = h3("Meu Painel", style = "color: #337ab7;"),  # Título da 'box'
    
    # Formulário de seleção no topo da página
    fluidRow(
      column(3, selectInput("tableType", "Tipo de Tabela", 
                            choices = c("SINAPI", "PRÓPRIA"))),
      column(3, selectInput("State", "Local", 
                            choices = c("SC", "PR"))),
      column(3, selectInput("reference", "Referência", 
                            choices = c("2024-02", "2024-01"))),
      column(3, selectInput("searchType", "Tipo de Pesquisa", 
                            choices = c("INSUMO", "COMPOSIÇÕES"))),
      column(3, selectInput("taxStatus", "Desoneração", 
                            choices = c("ONERADO", "DESONERADO", "INDEFINIDO")))
    ),
    
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
      div(style = "background-color: #f2f2f2;",
          h3("Orçamento", style = "text-align: center;"),
      ),
      div(style = "height: 5px;"),
      column(12, DTOutput("budgetTable"), style = "margin: auto;"))
    )

)

server <- function(input, output) {
  output$materialTable <- renderDT({ datatable(iris) })
  output$budgetTable <- renderDT({ datatable(mtcars) })
}


server <- function(input, output, session) {
  selectedMaterials <- reactiveVal(data.frame(Codigo = character(), 
                                              Descricao = character(), 
                                              Unidade = character(), 
                                              CustoTotal = numeric(), 
                                              Quantidade = numeric(), 
                                              Total = numeric()))
  

  
  output$materialTable <- renderDT({
    datatableData <- table_sinapi[grepl(input$searchBox, table_sinapi$`DESCRICAO DA COMPOSICAO`), ]
    
    # Criar uma nova coluna com botões de adição
    adicionarButtons <- sprintf('<button onclick="Shiny.setInputValue(\'add_item_%s\', TRUE, {priority: \'event\'});" class="btn btn-primary btn-sm">Adicionar</button>', 1:nrow(datatableData))
    datatableData <- cbind(Adicionar = adicionarButtons, datatableData)
    
    # Renderizar a datatable sem a numeração automática das linhas e limitando a exibição a 5 linhas
    datatable(datatableData, 
              escape = FALSE, 
              selection = 'single', 
              options = list( paging = TRUE, pageLength = 3,
                              columnDefs = list(
                                list(width = '50%', targets = 2)  # Ajuste o índice para corresponder à sua coluna
                              )),
              rownames = FALSE) # Desativa a numeração automática das linhas
  })
  
  
  observeEvent(input$addButton, {
    req(input$materialTable_rows_selected)
    selectedRow <- table_sinapi[input$materialTable_rows_selected, , drop = FALSE]
    selectedRow$Quantidade <- 0
    selectedRow$Total <- 0
    currentSelected <- selectedMaterials()
    selectedMaterials(rbind(currentSelected, selectedRow))
  })
  
  output$budgetTable <- renderDT({
    df <- selectedMaterials()
    if (nrow(df) > 0) {
  
      df$Deletar <- sprintf('<button onclick="Shiny.setInputValue(\'delete_row\', %s, {priority: \'event\'});" class="btn btn-danger btn-sm">🗑️</button>', 1:nrow(df))
      datatable(df, 
                editable = list(target = 'cell', 
                                disable = list(columns = c(0, 1, 2, 3,5,6))), 
                escape = FALSE, 
                selection = 'none',
                options = list( paging = TRUE,
                                columnDefs = list(
                                  list(width = '100%', targets = 1)  # Ajuste o índice para corresponder à sua coluna
                                )),
                rownames = FALSE)
      
    } else {
      datatable(df, selection = 'none')
    }
  }, server = FALSE)
  
  proxy <- dataTableProxy('budgetTable')
  
  observeEvent(input$budgetTable_cell_edit, {
    # Código para lidar com a edição da tabela...
  })
  
  observeEvent(input$delete_row, {
    df <- selectedMaterials()
    if (!is.null(df) && nrow(df) >= input$delete_row) {
      df <- df[-as.numeric(input$delete_row), ]
      selectedMaterials(df)
    }
  }, ignoreNULL = TRUE)
}

shinyApp(ui, server)


