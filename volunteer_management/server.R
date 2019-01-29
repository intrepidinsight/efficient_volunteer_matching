library(shiny)
library(DT)
library('shinyjs')
library('data.table')
library('stringr')

date_downloaded<-str_replace_all(as.character(Sys.Date()),"-","")

fieldsMandatory <- c("person_id", "job_name", "priority", "pref_stub")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

server <- function(input, output, session){
  myData <- reactive({
    inFile <- input$file1
    shinyjs::reset("priority")
    shinyjs::reset("job_name")
    shinyjs::reset("person_id")
    if (is.null(inFile)) return(NULL)
    if (tools::file_ext(inFile$datapath)=="csv"){
    data <- tryCatch({
      data.table(read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE, colClasses = 'character'))
      
    }, error=function(err){
      shinyjs::show("warning2")
    })
    } else{
      shinyjs::show("warning2")
    }

  })
  observeEvent(input$submit,{
    shinyjs::disable("submit")
  }, ignoreNULL = FALSE, once=TRUE)
  
  observe({
    updateSelectInput(session = session, inputId = "person_id", choices = c("", colnames(myData())))
    updateSelectInput(session = session, inputId = "job_name", choices = c("",colnames(myData())))
    updateSelectInput(session = session, inputId = "priority", choices = c("",colnames(myData())))
  })
  
  
  output$contents <- DT::renderDataTable({
    DT::datatable(myData(), rownames= FALSE, options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all"))) )       
  })

  
  observe({
    req(myData())
    # hide bad data warning if there was one.
    shinyjs::hide("warning2")
    req(input$person_id)
    req(input$job_name)
    req(input$priority)
    
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    ##check that people are uniquely identified.
    test1<-subset(myData(), get(input$person_id)!="" & !is.na(get(input$person_id)), select=input$person_id)
    people_unique <- nrow(test1)==nrow(unique(test1))
    ## check that there is more than one row and at least one job and person
    test1<-subset(myData(), get(input$person_id)!="", select=input$person_id)
    test2<-subset(myData(), get(input$job_name)!="", select=input$person_id)
    more_one<-nrow(test1)>0 & nrow(test2)>0
    ## check that preferences are fully filled out.
    test1<-subset(myData(), get(input$job_name)!="" & !is.na(get(input$job_name)), select=input$job_name)
    pref_complete<-length(colnames(myData())[str_detect(colnames(myData()), input$pref_stub)])>=nrow(unique(test1))
    #print(length(colnames(myData())[str_detect(colnames(myData()), input$pref_stub)]))
    #print(nrow(unique(test1)))
    ## check that priority is unique.
    test1<-subset(myData(), get(input$person_id)!="" & !is.na(get(input$person_id)), select=input$priority)
    priority_unique<- nrow(test1)==nrow(unique(test1))
    # activate alert if false.
    #print(priority_unique)
    if (priority_unique==FALSE){
      shinyjs::show("warning")
    } else{
      shinyjs::hide("warning")
    }
    if (more_one==FALSE){
      shinyjs::show("warning_one")
    }else{
      shinyjs::hide("warning_one")
    }
    check_priority <- input$accept_warning==TRUE | priority_unique==TRUE
    mandatoryFilled_main <- all(mandatoryFilled, people_unique, pref_complete,check_priority, more_one)
      # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled_main)
  })
  
  observeEvent(input$submit, {
    withProgress(message="Algorithm Processing", value=0,{
    isolate({
    tried<-FALSE
    shinyjs::reset("form")
    shinyjs::hide("warning")
    shinyjs::hide("form")
    shinyjs::hide("main")
    shinyjs::hide("raw_data_table")
    shinyjs::hide("warning_one")
    shinyjs::show("output_datatable")
    shinyjs::toggleState(id = "submit")
    shinyjs::show("thankyou_msg")
    updateCheckboxInput(session, "accept_warning", value = TRUE)
    processed<-data.table(myData())
    processed[,c(get(input$person_id), get(input$job_name), get(input$priority), colnames(myData())[str_detect(colnames(myData()), input$pref_stub)])]
    #print(colnames(processed))
    setnames(processed, old=c(as.character(input$person_id), as.character(input$job_name), as.character(input$priority), colnames(processed)[str_detect(colnames(processed), input$pref_stub)]),
             new = c("name", "position", "priority",paste0("pref", seq(1:length(colnames(processed)[str_detect(colnames(processed), input$pref_stub)])))))
    
    # if priority is blank for anyone or there are ties, we fill it in using a random number generator with this rule
    ## if there are any priority numbers, we take these as given. Then all blanks will be ranomd+Max(given).
    processed[,num_priority:= as.double(frank(priority, ties.method="min"))]
	#processed[,num_priority:= as.numeric(priority)]
    
    processed[is.na(num_priority), num_priority:= max(processed$num_priority, na.rm=TRUE)+1]
    ## For ties, we add a random number between 0 and 1.
    processed[duplicated(processed, by="num_priority") | duplicated(processed, by="num_priority", fromLast = TRUE), num_priority:=num_priority+runif(.N,0,1)]
    # step 1: each job selects either its current owner or the highest num_priority person.
    #print(processed[name!="" & position!="",])
    processed<-processed[name!="" & position!="",r1_name:=name]
    processed<-processed[name=="" & position!="", r1_name:=processed$name[which(processed$num_priority==min(processed$num_priority, na.rm = TRUE))]]
    processed<-processed[, pref_current:=pref1]
    
    # step 2: define recursive method to check for cycles
    d<-copy(processed[, raword:=.I])
    
    # induce an order within each job that has duplicates (multiple slots). We prioritize the positions with a person first.
    d<-d[order(num_priority, raword), ind_ord:=1:.N , by=position]
    
    people<-copy(d[name!=""][order(num_priority)])
    jobs<-copy(d[position!=""][order(ind_ord)])
    
    findcycle<-function(x,h, ob_num, result){
      x_next <- x +1

      if (x_next==1){
        hold=as.character(jobs[match(people$pref_current[ob_num], jobs$position)[1],r1_name])
        result<-cbind(people$name[ob_num], people$pref_current[ob_num], jobs[match(people$pref_current[ob_num], jobs$position)[1],raword])
      } else if (x_next %% 2 ==0){
        hold=as.character(people[match(h, people$name)[1],pref_current])
        result<-rbind(result, cbind(h,hold, jobs$raword[match(hold, jobs$position)[1]]))
      } else if (x_next %% 2 !=0){
        hold=as.character(jobs[match(h, jobs$position)[1],r1_name])
        result<-rbind(result, cbind(hold,h ,jobs$raword[match(hold, jobs$r1_name)[1]]))
      }
      
      # termination conditions
      if (is.na(hold)){
        result<-NULL
      } else if (hold == people$name[ob_num] & x_next!=1){
        # delete last row since it is the end.
        result<-result[-x_next,]
      } else if (x_next>2*nrow(d) ){
        #print("no cycle")
        result<-NULL
      } else {
        return(findcycle(x_next, hold, ob_num, result))
      }
      return(result)
      
    }
    
    # do once
    tot<-nrow(d)
    outcome<-lapply(1:nrow(d), function(x) findcycle(0,"", x) )
    outcome<-outcome[!unlist(lapply(outcome, is.null))]
    outcome<-unique(do.call(rbind.data.frame, outcome))
    overall_matched<-outcome
    #print(d)
 # print(overall_matched)
  
    # do until all people are matched.
    still_people=TRUE
    i = 0
    prog<-0
    while (still_people == TRUE) {
      i = i+1
      output$counter <- renderText({
        paste0("Iterations Completed: ", nrow(d))
      })
      # remove the matched people and the matched jobs.remove their num_priority
      d<-d[d$name %in% overall_matched$h, num_priority:=NA][d$name %in% overall_matched$h, name:=""][d$raword %in% outcome$V3, position:=""][position!="" | name!=""]
      incProgress((tot-nrow(d))/tot-prog, detail = paste("Volunteers Matched:", tot-nrow(d)))
      prog<-(tot-nrow(d))/tot
      # if d now has no people, terminate
      if (nrow(d[name!=""])==0){
        still_people = FALSE
        break
      } 
      
      # set r1 based on which people are left.
      #print(d)
      d<-d[,r1_name:=""][, r1_name:=name][r1_name=="", r1_name:=d$name[which(d$num_priority==min(d[name!="",]$num_priority, na.rm = TRUE))]]
      
      # change num_priority until everyone has a position that still exists.
      d$pref_current=""
      counter = TRUE
      x = 1
      while (counter==TRUE){
        #print(x)
        pref_var = paste0("pref", as.character(x))
        if (!exists(pref_var, where = d) | nrow(d[ name!="" & pref_current==""])==0){
          counter= 0
          break
        } 
        d<-d[name!="" & pref_current=="" & get(pref_var) %in% d$position, pref_current:=get(pref_var)]
        counter = any(d$pref_current=="" & d$name=="")
        x = x +1 
      }
      
      # remake the people and the jobs parts
      people<-copy(d[name!=""][order(num_priority)])
      jobs<-copy(d[position!=""][order(ind_ord)])
      
      outcome<-lapply(1:nrow(d), function(x) findcycle(0,"", x) )
      outcome<-outcome[!unlist(lapply(outcome, is.null))]
      #print(outcome)
      outcome<-unique(do.call(rbind.data.frame, outcome))
      overall_matched<-rbind(overall_matched, outcome)
      
    }
    
    # d should now only contain the unmatched jobs or people.
    d<-d[,c("name", "position")]
    not_matched<-nrow(d)
    setnames(d, c("Volunteer", "Position"))
    #print(overall_matched)
    overall_matched<-data.table(overall_matched)
    setnames(overall_matched, c("Volunteer", "Position", "number"))
    overall_matched<-overall_matched[,c("Volunteer", "Position")]
    overall_matched<-rbind(overall_matched, d)
    setkey(overall_matched, "Volunteer", "Position")
    output$matchings <- DT::renderDataTable({
      DT::datatable(overall_matched, rownames= FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    output$downloadBtn <- downloadHandler(
      filename = paste0("i2_volunteer_match", date_downloaded, ".csv"),
      content = function(file) {
        write.csv(overall_matched, file, row.names = FALSE)
      })
  })
  })
  })
  observeEvent(input$submit_another, {
    shinyjs::reset("form")
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
    shinyjs::hide("output_datatable")
    shinyjs::show("form")
    shinyjs::show("main")
    shinyjs::show("raw_data_table")

  })
  
  
  i2<-a("Powered By Intrepid Insight", href="https://www.intrepidinsight.com", target="_blank")
  output$powered<-renderUI({
    tagList(i2)
  })

  
  
}