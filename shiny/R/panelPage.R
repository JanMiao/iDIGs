panelPage <- function(id, metadata=metadata){
  tabPanel(
    title = "Panel Design",
    div(class = "inlay", style = "height:100px;width:100%;background-color: white;"),
    tags$div(tags$h3("Design a small panel for breed identification of some specfic breeds"),
             style="margin-left:8.33%;margin-bottom:50px;"),
    fluidRow(
      width=12,
      column(
        width=3,
        offset=1,
        box(
          width = 12,
          status = "success",
          title = div("First step: Select panel density",id=NS(id,"file_upload"),style="font-size:x-large;"),
          solidHeader = F,
          div(
            id=NS(id,"reference_ori"),
            pickerInput(
              inputId = NS(id,"gbc_select_ref"),
              label = "Select version of reference genome",
              multiple=F,
              choices = c("Suscrofa11.1","Suscrofa10.2")
            ),
            bsPopover(id=NS(id,"reference_ori"),
                      title="Reference origin:",
                      content="You can select the version of reference genome based on your data.",
                      placement="right",trigger="hover", options = list(container = "body"))
          ),
          br(),
          div(id=NS(id,"panel_density"),
              numericInput(NS(id,"panelNum"), "The number of markers used", value = 50, min = 1, max = 10000),
              bsPopover(id=NS(id,"panelNum"),
                        title="Panel density:",
                        content="Choose the number of SNPs used in panel.",
                        placement="right",trigger="hover", options = list(container = "body"))
          )
        )
      ),
      column(
        width=3,
        offset=1,
        box(
          status = "warning",
          width = 12,
          title = div("Second step: Choose reference breeds",id=NS(id,"candicate_breeds"),style="font-size:x-large;"),
          solidHeader = F,
          div(
            id=NS(id,"reference_breed"),
            pickerInput(
              inputId = NS(id,"breed_used"),
              label = "Select reference breeds",
              multiple=T,
              choices = metadata$Breed$Breed_FullName
            ),
            bsPopover(id=NS(id,"reference_breed"),
                      title="Select reference breeds:",
                      content="Please select the breeds you want to distinguish from each other. You can either select multiple breed IDs or single breed IDs",
                      placement="right",trigger="hover", options = list(container = "body"))
          )
        )
      ),
      column(
        width=3,
        offset=1,
        box(
          status = "danger",
          width = 12,
          title = div("Third step: E-mail address and submit",style="font-size:x-large;"),
          solidHeader = F,
          textInput(inputId = NS(id,"email"), label = "", width = "400px"),
          bsPopover(NS(id,"email"), title='Your Email address', content='We will send you an email with analysis results to this address as soon as the analysis is done', 
                    placement="bottom",trigger="hover", options = list(container = "body")),
          div(class = "inlay", style = "height:30px;width:100%;background-color: white;"),
          actionButton(
            inputId = NS(id,"submit_bttn"),
            label = "Submit",
            class = "btn-info",
            icon=icon("paper-plane"),
            style='font-size:150%;width:200px;background-color:#3c8dbc'
          ),
          bsPopover(NS(id,"submit_bttn"), title='Submit', content='Please make sure you have uploaded files and set parameters properly before you submit', 
                    placement="bottom",trigger="hover", options = list(container = "body"))
        )
      )
    )
  )
}