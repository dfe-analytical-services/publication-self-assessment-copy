

rm(list = ls())
#useShinyalert()

server <- function(input, output, session){
  
  #start_data <- readRDS("new_tracker_data.rds")
  
  #vals_trich<-reactiveValues()
  
  all_data <- reactive({
    readRDS("new_tracker_data.rds") %>% dplyr::filter(Publication == input$publication_choice)
  })
  
  
  ### interactive dataset 
  all_data<-reactiveValues()
  # all_data$Data<- all_data$Data
  # 
  
 # all_data$Data<-readRDS("new_tracker_data.rds") %>% dplyr::filter(Publication == input$publication_choice)
  
  all_data$Data<-readRDS("new_tracker_data.rds") 
  #all_data$Data<-all_data$Data
  #all_data$Data<-start_data %>% dplyr::filter(Publication == input$publication_choice)
  
  
  #### MainBody_trich is the id of DT table
  output$MainBody_trich<-renderUI({
    fluidPage(
      hr(),
      column(6,offset = 6,
             HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
             ### tags$head() This is to change the color of "Add a new row" button
             tags$head(tags$style(".butt2{background-color:#231651;} .butt2{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Add_row_head",label = "Add", class="butt2") ),
             #tags$head(tags$style(".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}")),
             #div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "mod_row_head",label = "Edit", class="butt4") ),
             #tags$head(tags$style(".butt3{background-color:#590b25;} .butt3{color: #e6ebef;}")),
             #div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Del_row_head",label = "Delete", class="butt3") ),
             ### Optional: a html button 
             # HTML('<input type="submit" name="Add_row_head" value="Add">'),
             HTML('</div>') ),
      
      column(12,dataTableOutput("Main_table_trich")),
      tags$script("$(document).on('click', '#Main_table_trich button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
      
    ) 
  })
  
  #### render DataTable part ####
  output$Main_table_trich <- renderDataTable({
    
    DT = all_data$Data %>% dplyr::filter(Publication == input$publication_choice)
    
    datatable(t(DT),
              selection = 'single',
              escape = F,
              class = list(stripe = FALSE),
              options = list(
                dom = 't', # simple table output (add other letters for search, filter etc)
                headerCallback = JS("function(thead, data, start, end, display){","  $(thead).remove();","}"), # removes header
                pageLength = 25
              )
    ) %>% 
      formatStyle(' ', #rownames col (replace with V1, V2 etc for others)
                  backgroundColor = '#363b40',
                  fontWeight = 'bold')  %>% 
      formatStyle(#columns=colnames(t(DT)),
                  1:20,
                  color = '#c8c8c8',
                  background = '#363b40', # background colour for app is '#363b40'
                  target = 'row') %>%
      formatStyle(1:20,
                  backgroundColor = styleEqual(c('Red', 'Green', 'Amber'),
                                               c('#d45859', '#70ad47', '#e87421'))) %>% 
      formatStyle(1:20, `text-align` = 'center') %>%
      formatStyle(1:20, border = '1px solid #4d5154')
    
  })
  
  
  observeEvent(input$Add_row_head, {
    
    DT = all_data$Data %>% dplyr::filter(Publication == input$publication_choice)
    
    
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Add a new row",
                          div(class = "row",
                              div(class = "col-sm-6","ï..Date"),
                              div(class = "col-sm-6",  dateInput("T1_add", label = NULL, value = Sys.Date()))),
                          
                          #dateInput(paste0("Date_add", input$Add_row_head), "Date:", value = Sys.Date()),
                          
                          div(class = "row",
                              div(class = "col-sm-6","G6"),
                              div(class = "col-sm-6", textInput("T2_add",label = NULL, value = t(DT)[2,ncol(t(DT))]))),
                          div(class = "row",
                              div(class = "col-sm-6","TL"),
                              div(class = "col-sm-6", textInput("T3_add",label = NULL, value = t(DT)[3,ncol(t(DT))]))),
                          div(class = "row",
                              div(class = "col-sm-6","Publication"),
                              div(class = "col-sm-6", textInput("T4_add",label = NULL, value=input$publication_choice))),
                          hr(),
                          div(class = "row",
                              div(class = "col-sm-6","Published.on.EES"),
                              div(class = "col-sm-6", textInput("T5_add",label = NULL, value = t(DT)[5,ncol(t(DT))]))),
                          div(class = "row",
                              div(class = "col-sm-6","Time.series.length"),
                              div(class = "col-sm-6", textInput("T6_add",label = NULL, value = t(DT)[6,ncol(t(DT))]))),
                          hr(),
                          strong("RAP levels - Good"),
                          br(),
                          br(),
                          rag_it("Processing is done with code","T7_add",7,DT),
                          rag_it("Sensible folder /  file structure","T8_add",8,DT),
                          rag_it("Documentation","T9_add",9,DT),
                          rag_it("All source data stored in single database","T10_add",10,DT),
                          rag_it("Files meet data standards","T11_add",11,DT),
                          rag_it("Basic automated QA","T12_add",12,DT),
                          rag_it("Use approporiate tools","T13_add",13,DT),
                          hr(),
                          strong("RAP levels - Great"),
                          br(),
                          br(),
                          rag_it("Recyclable code for future use","T14_add",14,DT),
                          rag_it("Publication specifc automated QA","T15_add",15,DT),
                          rag_it("Version controlled final versions of code","T16_add",16,DT),
                          rag_it("Single production script(s)","T17_add",17,DT),
                          rag_it("Automated summaries generated for insight","T18_add",18,DT),
                          rag_it("Peer review of code within team","T19_add",19,DT),
                          hr(),
                          strong("RAP levels - Best"),
                          br(),
                          br(),
                          rag_it("Collab and develop code using git","T20_add",20,DT),
                          rag_it("Subject specific automated insights", "T21_add",21,DT),
                          rag_it("Single production script s  with integrated QA", "T22_add",22,DT),
                          rag_it("Clean final code", "T23_add",23,DT),
                          rag_it("Peer review of code from outside the team", "T24_add",24,DT),
                          div(class = "row",
                              div(class = "col-sm-6","Any.L.D.requests.or.needs"),
                              div(class = "col-sm-6", textInput("T25_add",label = NULL, value = t(DT)[25,ncol(t(DT))]))),
                          actionButton("go", "Add item"),
                          easyClose = TRUE, footer = NULL
                          , size = "m" 
                          ))
    
    
  })
  ### Add a new row to DT  
  observeEvent(input$go, {
    new_row=data.frame(
      #Date=as.character( input[[paste0("Date_add", input$Add_row_head)]] ),
      
      ï..Date = as.character( input[["T1_add"]] ), #Sys.Date(), #input[["T1_add"]],
      G6 = input[["T2_add"]],
      TL = input[["T3_add"]],                                           
      Publication = input$publication_choice, #input[["T4_add"]],
      Published.on.EES = input[["T5_add"]],
      Time.series.length = input[["T6_add"]],
      Processing.is.done.with.code = input[["T7_add"]],
      Sensible.folder...file.structure = input[["T8_add"]],
      Documentation = input[["T9_add"]],             
      All.source.data.stored.in.single.database = input[["T10_add"]],
      Files.meet.data.standards = input[["T11_add"]],
      Basic.automated.QA = input[["T12_add"]],
      Use.approporiate.tools = input[["T13_add"]],
      Recyclable.code.for.future.use = input[["T14_add"]],
      Publication.specifc.automated.QA = input[["T15_add"]],
      Version.controlled.final.versions.of.code = input[["T16_add"]],
      Single.production.script.s. = input[["T17_add"]],
      Automated.summaries.generated.for.insight = input[["T18_add"]],
      Peer.review.of.code.within.team = input[["T19_add"]],
      Collab.and.develop.code.using.git = input[["T20_add"]],
      Subject.specific.automated.insights = input[["T21_add"]],
      Single.production.script.s..with.integrated.QA = input[["T22_add"]],
      Clean.final.code = input[["T23_add"]],
      Peer.review.of.code.from.outside.the.team = input[["T24_add"]],
      Any.L.D.requests.or.needs = input[["T25_add"]]

    )
    
    all_data$Data<-rbind(all_data$Data,new_row )
    removeModal()
  })
  
  
  
  
  ### save to RDS part 
  observeEvent(input$Updated_trich,{
    saveRDS(all_data$Data, "new_tracker_data.rds")
    shinyalert(title = "Saved!", type = "success")
  })
  
  
  shinyjs::showElement(id = "home_page")
  
  observeEvent(input$progresspage, {
    shinyjs::hideElement(id = "home_page")
    shinyjs::showElement(id = "progress_page")
  })
  
  
  ### delete selected rows part
  ### this is warning messge for deleting
  # observeEvent(input$Del_row_head,{
  #   showModal(
  #     if(length(input$Main_table_trich_rows_selected)>=1 ){
  #       modalDialog(
  #         title = "Warning",
  #         paste("Are you sure delete",length(input$Main_table_trich_rows_selected),"rows?" ),
  #         footer = tagList(
  #           modalButton("Cancel"),
  #           actionButton("ok", "Yes")
  #         ), easyClose = TRUE)
  #     }else{
  #       modalDialog(
  #         title = "Warning",
  #         paste("Please select row(s) that you want to delect!" ),easyClose = TRUE
  #       )
  #     }
  #     
  #   )
  # })
  
  ### If user say OK, then delete the selected rows
  # observeEvent(input$ok, {
  #   all_data$Data=all_data$Data[-input$Main_table_trich_rows_selected]
  #   removeModal()
  # })
  
  ### edit button
  # observeEvent(input$mod_row_head,{
  #   showModal(
  #     if(length(input$Main_table_trich_rows_selected)>=1 ){
  #       modalDialog(
  #         fluidPage(
  #           h3(strong("Modification"),align="center"),
  #           hr(),
  #           dataTableOutput('row_modif'),
  #           actionButton("save_changes","Save changes"),
  #           tags$script(HTML("$(document).on('click', '#save_changes', function () {
  #                            var list_value=[]
  #                            for (i = 0; i < $( '.new_input' ).length; i++)
  #                            {
  #                            list_value.push($( '.new_input' )[i].value)
  #                            }
  #                            Shiny.onInputChange('newValue', list_value) });")) ), size="l" )
  #     }else{
  #       modalDialog(
  #         title = "Warning",
  #         paste("Please select the row that you want to edit!" ),easyClose = TRUE
  #       )
  #     }
  #     
  #   )
  # })
  
  
  
  
  #### modify part
  # output$row_modif<-renderDataTable({
  #   selected_row=input$Main_table_trich_rows_selected
  #   old_row=all_data$Data[selected_row]
  #   row_change=list()
  #   for (i in colnames(old_row))
  #   {
  #     if (is.numeric(all_data$Data[[i]]))
  #     {
  #       row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
  #     } 
  #     else if( is.Date(all_data$Data[[i]])){
  #       row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="date" id=new_  ',i,'  ><br>') 
  #     }
  #     else 
  #       row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
  #   }
  #   row_change=as.data.table(row_change)
  #   setnames(row_change,colnames(old_row))
  #   DT=row_change
  #   DT 
  # },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE),selection="none" )
  
  
  
  ### This is to replace the modified row to existing row
  # observeEvent(input$newValue,
  #              {
  #                newValue=lapply(input$newValue, function(col) {
  #                  if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
  #                    as.numeric(as.character(col))
  #                  } else {
  #                    col
  #                  }
  #                })
  #                DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
  #                colnames(DF)=colnames(all_data$Data)
  #                all_data$Data[input$Main_table_trich_rows_selected]<-DF
  #                
  #              }
  # )
  
  
  ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
  ### can download the table in csv
  output$Trich_csv<- downloadHandler(
    filename = function() {
      paste("All data", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(all_data$Data), file, row.names = F)
    }
  )
  
  # Stop app ---------------------------------------------------------------------------------
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}