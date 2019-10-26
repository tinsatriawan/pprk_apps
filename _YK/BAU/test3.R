library(shiny)
library(shinyjs)
library(DT)
library(data.table)
library(DBI)
library(pool)

#Database
pool <- dbPool(drv =RSQLite::SQLite(),dbname="")
onStop(function() {
  poolClose(pool)
})

dbGetQuery(pool,"CREATE TABLE customer(customer_first TEXT, customer_last TEXT, gender TEXT,dob TEXT);")

dbGetQuery(pool, 'INSERT INTO customer (customer_first, customer_last, gender, dob) 
       VALUES ("Amit","Kumar","male","1988-01-01");')

dbGetQuery(pool, 'INSERT INTO customer (customer_first, customer_last, gender, dob) 
       VALUES ("Bhatia","Krouz","male","1975-01-01");')
#SQLlite create and use rowid as default auto_increament id
#Example
#dbGetQuery(pool, 'SELECT * FROM customer where rowid=1')
#dbGetQuery(pool, 'SELECT rowid FROM customer where customer_last="Kumar"')

#App
shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition="input.conditionedPanels == 0",helpText("This is About")),
        conditionalPanel(
          condition="input.conditionedPanels == 1",
          textInput("customer_first", "First Name", ""),
          textInput("customer_last", "Last Name", ""),
          selectInput(inputId="gender" ,"1. Gender" , choices=c("Male"=0,"Female"=1) , selected =0, selectize=FALSE ) ,
          dateInput("dob","Date of birth:",value="1980-01-01",format="yyyy-mm-dd"),
          actionButton("submit", "Create")
          
        ),
        
        conditionalPanel(
          condition="input.conditionedPanels == 2",
          div(id="abu",
              textInput("customer_first_s", "First Name", ""),
              textInput("customer_last_s", "Last Name", ""),
              dateInput("dob_s","Date:",value = "",format = "yyyy-mm-dd")),
          actionButton("search", "Search"))
      ), #sidebarPanel
      
      mainPanel(tabsetPanel(id="conditionedPanels",
                            tabPanel("About",value=0,h3("About the application")),
                            navbarMenu("CRM",
                                       tabPanel("New Customer", value=1,br(),br(),p("Please save the customer ID for future reference. In case customer id has been lost then please use the customer patient panel for search.")),
                                       tabPanel("Existing Customer",value=2,DT::dataTableOutput("res"))
                            ) #navbarMenu
                            
      ) #tabSetPanel
      
      ,
      tags$script("$(document).on('click', '#res button', function () {
              Shiny.onInputChange('lastClickId',this.id);
              Shiny.onInputChange('lastClick', Math.random())
              });")) #mainPanel
      
      
    )), #ui
  server = function(input, output, session) {
    
    observeEvent(input$submit, {
      tryCatch(
        poolWithTransaction(pool, function (conn) {
          dbExecute(conn,paste0("INSERT INTO customer (customer_first, customer_last, gender, dob) values ","('",input$customer_first,"','",input$customer_last,"',",input$gender,",'",input$dob,"')", ";"))
          id <- dbGetQuery(conn, "select last_insert_rowid();")[1,1]
          showModal(modalDialog(
            title = "Record created successfully",
            span('New customer record was created with ID:',strong(em(id)))
          ))
        }), 
        error = function(e){
          showModal(modalDialog(
            title = "Create new record not successful",
            tags$i("Please enter valid values and try again"),br(),br(),
            tags$b("Error:"),br(),
            tags$code(e$message)
          ))
        })
      reset("customer_first");reset("customer_last") #reset from shinyjs
    })
    
    
    select_dat <- eventReactive(input$search,
                                dbGetQuery(pool,paste0("select rowid AS 'Customer ID', customer_first AS 'First name' , customer_last 'Last name', dob AS DOB from customer where customer_first like","'%",ifelse(input$customer_first_s=="","^",input$customer_first_s),"%'"," OR customer_last like","'%",ifelse(input$customer_last_s=="","^",input$customer_last_s),"%'"," OR dob like","'",input$dob_s,"'",";"))
                                
    )
    
    values = reactiveValues(data=NULL)
    observe({
      input$search
      values$data <- select_dat()
    })
    
    
    output$res <- DT::renderDataTable({
      req(values$data)
      DT <- values$data
      if(nrow(DT)>=1){
        
        DT[["Actions"]]<-
          paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
             <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(DT),'>Delete</button>
             <button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(DT),'>Modify</button>
             </div>
             
             ')} else {return(DT)}
      datatable(DT,escape=F,selection="none",options = list(columnDefs = list(list(className = 'dt-center',targets=1:5))))})
    
    output$row_modif<-renderDataTable({
      selected_row=as.numeric(gsub("modify_","",input$lastClickId))
      #start from 2nd coulmn Because I don't want user has access to the ID
      old_row= values$data[selected_row,2:4]
      row_change=list()
      for (i in colnames(old_row))
      {
        if (is.numeric(old_row[[i]]))
        {
          row_change[[i]]<-paste0('<input class="new_input" type="number" id=new_',i,'><br>')
        }
        else
          row_change[[i]]<-paste0('<input class="new_input" type="text" id=new_',i,'><br>')
      }
      
      row_change=as.data.table(row_change)
      setnames(row_change,colnames(old_row))
      DT=rbind(old_row,row_change)
      rownames(DT)<-c("Current values","New values")
      DT
    },escape=F,options=list(dom='t',ordering=F),selection="none")
    
    
    ##Managing in row deletion
    modal_modify<-modalDialog(
      fluidPage(
        h3(strong("Record modification"),align="center"),
        hr(),
        dataTableOutput('row_modif'),
        actionButton("save_changes","Save changes"),
        
        tags$script(HTML("$(document).on('click', '#save_changes', function () {
                     var list_value=[]
                     for (i = 0; i < $( '.new_input' ).length; i++)
                     {
                     list_value.push($( '.new_input' )[i].value)
                     }
                     Shiny.onInputChange('newValue', list_value)
  });"))
      ),size="l")
    
    observeEvent(input$newValue,{
      newValue=lapply(input$newValue, function(col) {
        if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
          as.numeric(as.character(col))
        } else {
          col
        }
      })
      DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
      id_row <- values$data[as.numeric(gsub("modify_","",input$lastClickId)),1]
      query <- sqlInterpolate(pool,"update customer set customer_first=?f,customer_last=?l,dob=?d where rowid=?id;",f=as.character(DF[1,1]),l=as.character(DF[1,2]),d=as.character(DF[1,3]),id=id_row)
      
      if(!isTruthy(tryCatch(dbGetQuery(pool,query), error=function(e) NA))){
        showModal(
          modalDialog(
            title = "Unvalid Modification",
            "Please enter non null values", easyClose = TRUE, footer = NULL
          )
        )
        return()
      }
      dbGetQuery(pool,query) 
      values$data[as.numeric(gsub("modify_","",input$lastClickId)),2:4]<- c(as.character(DF[1,1]),as.character(DF[1,2]),as.character(DF[1,3]))
      
    })
    
    
    observeEvent(input$lastClick,
                 {
                   if (input$lastClickId%like%"delete")
                   {
                     row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
                     query <- sqlInterpolate(pool,"delete from customer where rowid=?id;",id=values$data[row_to_del,1])
                     
                     if(!isTruthy(tryCatch(dbGetQuery(pool,query), error=function(e) NA))){
                       showModal(
                         modalDialog(
                           title = "Unvalid Deletion",
                           "Delete customer record who has sales record is prohibited", easyClose = TRUE, footer = NULL
                         )
                       )
                       return()
                     }
                     
                     dbGetQuery(pool,query) 
                     values$data=values$data[-row_to_del,]
                     
                   }
                   else if (input$lastClickId%like%"modify")
                   {
                     showModal(modal_modify)
                   }
                 }
    )
    
    
    
  }
)