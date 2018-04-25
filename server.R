library(shiny)
library(ggplot2)
shinyServer(function(input, output, session) {
    df<- reactive({
        get(input$dataset)
    })
    observe({
        updateSelectInput(session, "x",
        choices=names(df())
        )
    })
    observe({
        updateSelectInput(session, "y",
                          choices=names(df()), selected=names(df())[2]
        )
    })
    observe({
        updateSelectInput(session, "z",
                          choices=c(names(df()),"none"), selected="none")
        })

    selectedData <- reactive({
        if(input$z=="none"){
            df()[, c(input$x, input$y)]
        } else{
        df()[, c(input$x, input$y, input$z)]
        }
    })
    
    model <- reactive({
        brushed_data <- brushedPoints(selectedData(), input$brush1,
                                      xvar = as.character(input$x), yvar = as.character(input$y))
        if(nrow(brushed_data) < 2){
            return(NULL)
        }
        lm(brushed_data[,2] ~ brushed_data[,1], data = brushed_data)
    })

    output$modout <- renderText({
        if(is.null(model())){
            "No Model Found"
        } else {
            paste("Slope:", model()[[1]][2], "Intercept:", model()[[1]][1]) 
            
        }
    })
    output$corout <- renderText({
       paste("Correlation:", cor(df()[,1], df()[,2]))
    })
    
    predictdf<- reactive({
        brushed_data <- brushedPoints(selectedData(), input$brush1,
                                      xvar = as.character(input$x), yvar = as.character(input$y))
        if(is.null(model())==TRUE ){
            data.frame(x=c(0,0),ypred=c(0,0))
            
        }
        
        data.frame(x=brushed_data[,1], ypred=predict(model(), brushed_data))
        
        
    })    
    
    
    output$plot1 <-renderPlot({
        
        p1<-ggplot(data = selectedData(), aes(x=selectedData()[,1], y=selectedData()[,2]))
        
        if(input$z=="none"){
            p1<-p1 +  geom_point(aes(x=selectedData()[,1], y=selectedData()[,2]))
            p1<-p1 +  labs(title = " ", x = as.character(input$x), y = as.character(input$y))
        }else {
            p1<-p1 +  geom_point(aes(x=selectedData()[,1], y=selectedData()[,2], color=selectedData()[,3]))
        p1<-p1 + labs(title = " ", x = as.character(input$x), y = as.character(input$y), color = as.character(input$z))
        }
        if(is.null(model())==TRUE){
            p1<-p1
        }else{
            p1<-p1+geom_abline(slope=model()[[1]][2], intercept=model()[[1]][1])
        }
        p1
        })
    
})
   

    
    
    

  

