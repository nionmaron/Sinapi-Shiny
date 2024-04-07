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

#teste

sinapi_compd <- read_xlsx("www/DB/DESONERADO/SINAPI_Custo_Ref_Composicoes_Sintetico_SC_202402_Desonerado.xlsx",skip = 4)

sinapi_compd$`CODIGO  DA COMPOSICAO`


table_sinapi <- sinapi_compd[c("CODIGO  DA COMPOSICAO", "DESCRICAO DA COMPOSICAO","UNIDADE","CUSTO TOTAL")]

colnames(sinapi_compd)


ui <- fluidPage(
  titlePanel("Seleção de Materiais para Orçamento"),
  sidebarLayout(
    sidebarPanel(
      textInput("searchBox", "Pesquisar Material"),
      actionButton("addButton", "Adicionar ao Orçamento")
    ),
    mainPanel(
      DTOutput("materialTable"),
      DTOutput("budgetTable")
    )
  )
)

server <- function(input, output, session) {
  selectedMaterials <- reactiveVal(data.frame(Codigo = character(), 
                                              Descricao = character(), 
                                              Unidade = character(), 
                                              CustoTotal = numeric(), 
                                              Quantidade = numeric(), 
                                              Total = numeric()))
  
  output$materialTable <- renderDT({
    datatable(table_sinapi[grepl(input$searchBox, table_sinapi$`DESCRICAO DA COMPOSICAO`), ], selection = 'single')
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
      datatable(df, editable = list(target = 'cell', disable = list(columns = c(0, 1, 2, 3, 6))), escape = FALSE, selection = 'none')
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


