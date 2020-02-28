library(shiny)
library(rgl)
library(car)

iris_data<-read.csv("iris.csv", header=TRUE, sep = ",")

#Create PCA and write into dataframe data
data3<-iris_data[,1:ncol(iris_data)-1]
log.iris = log(data3[,1:4])
pca_data<-prcomp(log.iris, center = TRUE, scale. = TRUE)
pca_data_importance<-data.frame(Summary=rownames(summary(pca_data)$importance), summary(pca_data)$importance )

rotated_pca_data <- data.frame(pca_data$x)

data<-data.frame(Sample=rownames(pca_data$rotation),PC1=pca_data$rotation[,1], PC2=pca_data$rotation[,2], PC3=pca_data$rotation[,3], PC4=pca_data$rotation[,4])
pca_data_frame<-data
data<-data[-1]



# Define UI for app
ui<-navbarPage("Iris Dataset",
  tabPanel("2D Data Exploration",
	fluidPage(
  	headerPanel('2D Data Exploration'),
  	sidebarPanel(
   		 selectInput('xcol2', 'X Variable', names(iris_data[,1:ncol(iris_data)-1])),
  		 selectInput('ycol2', 'Y Variable', names(iris_data[,1:ncol(iris_data)-1]), selected=names(iris_data)[[2]]),
  	),
  	mainPanel(
    	plotOutput('plot2ddata')
  	),
	uiOutput('page2ddata')
	)
	),

  tabPanel("3D Data Exploration",
  	headerPanel('3D Data Exploration'),
  	sidebarPanel(
   		 selectInput('xcol3', 'X Variable', names(iris_data[,1:ncol(iris_data)-1])),
  		 selectInput('ycol3', 'Y Variable', names(iris_data[,1:ncol(iris_data)-1]), selected=names(iris_data)[[2]]),
  		 selectInput('zcol3', 'Z Variable', names(iris_data[,1:ncol(iris_data)-1]), selected=names(iris_data)[[3]]),
  	),
  	mainPanel(
    	rglwidgetOutput('plot3ddata',width = "800px", height = "600px")
  	)
	),

  tabPanel("PCA",
	fluidPage(
  	headerPanel('2D Data PCA'),
  	sidebarPanel(
   		 selectInput('xcol', 'X Variable', names(data[,1:ncol(data)])),
  		 selectInput('ycol', 'Y Variable', names(data[,1:ncol(data)]), selected=names(data)[[2]]),
 		 fluidRow(
        		column(6,tableOutput('table'))
      		 ),
 		 fluidRow(
        		column(6,tableOutput('table2'))
      		 ),
  	),
  	mainPanel(
    	plotOutput('plot1')
  	),
	uiOutput('page1')
	)
	)


)



server<- function(input, output, session) {
    # Combine the selected variables into a new data frame
    selectedData2D <- reactive({
        rotated_pca_data[c(input$xcol, input$ycol)]
    })

    selectedData2Diris <- reactive({
        iris_data[c(input$xcol2, input$ycol2)]
    })

    selectedData3Diris <- reactive({
        iris_data[c(input$xcol3, input$ycol3, input$zcol3)]
    })

    output$table <- renderTable(pca_data_frame)

    output$table2 <- renderTable(pca_data_importance )

    output$plot1 <- renderPlot({
       plot(selectedData2D(),col=factor(iris_data$species), pch = 20, cex = 1)
    })

    output$plot2ddata <- renderPlot({
        plot(selectedData2Diris(),col=factor(iris_data$species), pch = 20, cex = 1)
     })


    output$plot3ddata <- renderRglwidget({
	names(selectedData3Diris()[1])
        rgl.open(useNULL=T)
        scatter3d(x=selectedData3Diris()[,1],y=selectedData3Diris()[,2],z=selectedData3Diris()[,3], xlab=input$xcol3, ylab=input$ycol3, zlab=input$zcol3, groups = iris_data$species, surface=FALSE, axis.col=c("black", "black", "black"), axis.ticks=TRUE)
        rglwidget()
    })

}


shinyApp(ui = ui, server = server)
