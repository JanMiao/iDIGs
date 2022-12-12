repositoryPage <- function(id){
  tabPanel(
    title = "Repository",
    div(class = "inlay", style = "height:100px;width:100%;background-color: white;"),
    fluidRow(
      width=12,
      column(
        width=10,
        offset=2,
        shinydashboardPlus::box(title = "PLS distribution of all samples",
                                status = "primary",
                                solidHeader = T,
                                width = 10,
                                plotlyOutput(outputId = "rep_pca", width = "100%",height="700px")
        )
      )
    ),
    fluidRow(
      column(
        width=10,
        offset=2,
        box(
          title = "All sample info",
          status = "success",
          solidHeader = T,
          width = 10,
          DT::dataTableOutput("info_table", width = "100%"),
          style="margin-left:20px;margin-top:30px;"
        )
      )
    ),
    fluidRow(
    column(
      width=10,
      offset=2,
      box(
        title = div("All breed info"),
        status = "success",
        solidHeader = T,
        width = 10,
        DT::dataTableOutput("breed_table", width = "100%"),
        style="margin-left:20px;margin-top:30px;"
      )
    )
  )
  )
}