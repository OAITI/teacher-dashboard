library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(aplpack)
library(reshape2)

addResourcePath(prefix = "images", directoryPath = "images/")
addResourcePath(prefix = "js", directoryPath = "js/")

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                h4("This dashboard enables teachers to view student's scores and grade them. It allows for viewing class results as well as individual results, and can help teachers assess how the grading scheme and the assessment type affect the class results."),
                hr(),
                
                tabsetPanel(
                    tabPanel("Options",
                             wellPanel(
                                 fluidRow(
                                     column(width = 6,
                                            h4("Grading Data"),
                                            helpText("In order to grade students the type of assessment (Homework/Lab/Project/Quiz/Test) should be specified. There is an option for specifying the maximum possible points for each type of assessment."),
                                            
                                            fileInput("data", "Upload Data"),
                                            checkboxInput("specify_poss", "Specify Points Possible"),
                                            fluidRow(
                                                column(width = 6,
                                                       uiOutput("moreControls")
                                                ),
                                                column(width = 6,
                                                       conditionalPanel(condition = "input.specify_poss == true",
                                                                        uiOutput("moreControls2")
                                                       )
                                                )
                                            )
                                     ),
                                     
                                     column(width = 6,
                                            h4("Grading Configuration"),
                                            helpText("The final Grade can be calculated using the US system of A to F, or A to F with +/-, or just as a Pass/Fail. The cutoffs for each grade can be automatic or specified manually"),
                                            radioButtons("gradescale", "Grading Scale", choices = c("US (A-F)" = "traditional", "US (With +/-)" = "plusminus", "Pass/Fail" = "passfail"), inline = TRUE),
                                            radioButtons("method", "Grading Method", c("Automatic" = "automatic", "Manual" = "manual"), inline = TRUE),
                                            conditionalPanel(condition = "input.method == 'manual' && input.gradescale != 'passfail'",
                                                             fluidRow(
                                                                 conditionalPanel(condition = "input.gradescale == 'plusminus'", 
                                                                                  column(width = 3, 
                                                                                         numericInput("a+", "A+ (min)", value = 97, min = 0, max = 100, step = 1),
                                                                                         numericInput("b+", "B+ (min)", value = 87, min = 0, max = 100, step = 1),
                                                                                         numericInput("c+", "C+ (min)", value = 77, min = 0, max = 100, step = 1)
                                                                                  )
                                                                 ),
                                                                 column(width = 3, 
                                                                        numericInput("a", "A (min)", value = 93, min = 0, max = 100, step = 1),
                                                                        numericInput("b", "B (min)", value = 83, min = 0, max = 100, step = 1),
                                                                        numericInput("c", "C (min)", value = 73, min = 0, max = 100, step = 1),
                                                                        numericInput("d", "D (min)", value = 63, min = 0, max = 100, step = 1)
                                                                 ),
                                                                 conditionalPanel(condition = "input.gradescale == 'plusminus'", 
                                                                                  column(width = 3, 
                                                                                         numericInput("a-", "A- (min)", value = 90, min = 0, max = 100, step = 1),
                                                                                         numericInput("b-", "B- (min)", value = 80, min = 0, max = 100, step = 1),
                                                                                         numericInput("c-", "C- (min)", value = 70, min = 0, max = 100, step = 1)
                                                                                  )
                                                                 )
                                                             )
                                            ),
                                            conditionalPanel(condition = "input.gradescale == 'passfail' || input.method == 'automatic'",
                                                             fluidRow(
                                                                 column(width = 3, 
                                                                        numericInput("pass", "Passing (min)", value = 60, min = 0, max = 100, step = 1)
                                                                 )
                                                             )
                                            ),
                                            
                                            hr(),
                                            
                                            h4("Grading Weights"),
                                            helpText("The are specified in terms of percentage weights for each type of assessment."),
                                            fluidRow(
                                                column(width = 3, 
                                                       numericInput("Homeworkperc", "Homework Percent", value = 20, min = 0, max = 100)
                                                ),
                                                column(width = 3, 
                                                       numericInput("Labperc", "Lab Percent", value = 10, min = 0, max = 100)
                                                ),
                                                column(width = 3, 
                                                       numericInput("Quizperc", "Quiz Percent", value = 10, min = 0, max = 100)
                                                ),
                                                column(width = 3, 
                                                       numericInput("Projectperc", "Project Percent", value = 20, min = 0, max = 100)
                                                ),
                                                column(width = 3, 
                                                       numericInput("Testperc", "Test Percent", value = 40, min = 0, max = 100)
                                                )
                                            )   
                                     )
                                 )
                             )
                    ),
                    tabPanel("Class",
                             helpText("This tab displays class statistics. It shows histograms for final grades and scores, how similar or Different students are, and correlation in % scores between different types of assessments"),
                             fluidRow(
                                 column(width = 6,
                                        plotOutput("plot1"),
                                        plotOutput("plot2")
                                 ),
                                 column(width = 6,
                                        plotOutput("plot3"),
                                        plotOutput("plot4"),
                                        fluidRow(
                                            column(width = 6,
                                                   selectizeInput("x", "X Variable", choices=NULL)
                                            ),
                                            column(width = 6,
                                                   selectizeInput("y", "Y Variable", choices=NULL)
                                            )
                                        )
                                 )
                             ),
                             dataTableOutput("grades")
                    ),
                    tabPanel("Individual",
                             HTML("The different assessment types are represented by different features of the face. If the student recieves a high score on a particular assessment, the size of the corresponding facial feature is increased.<br><br>
Test - Head size<br>
Projects - Mouth<br>
Homework - Nose<br>
Lab - Eye<br>
Quiz - Hair"),
                             hr(),
                             fluidRow(
                                 column(width = 3,
                                        selectizeInput("student", "Student", choices = NULL),
                                        textOutput("stuid"),
                                        textOutput("stugrade")
                                 ),
                                 column(width = 9,
                                        plotOutput("face", height = "700px", width = "700px"),
                                        dataTableOutput("studentmean"),
                                        dataTableOutput("studentscores"),
                                        dataTableOutput("test"),
                                        dataTableOutput("test2"),
                                        dataTableOutput("test3")
                                 )
                             )
                    )))

server <- ## Server definition
    shinyServer(function(input, output, session) {
        
        set.seed(20141106)
        
        ###
        ### The following out.* functions load UI elements based on the datset
        ###
        out.lst <- reactive({
            if (is.null(grade.initial())) return(NULL)
            
            assns <- unique(grade.initial()$assignmentname)
            
            my.list <- list()
            for (i in 1:length(assns)) {
                my.list[[i]] <- selectizeInput(paste("var", i, sep = ""), assns[i], c("Homework", "Lab", "Quiz", "Project", "Test"))
            }
            
            my.list
        })
        
        out.lst2 <- reactive({
            if (is.null(grade.initial())) return(NULL)
            
            assns <- unique(grade.initial()$assignmentname)
            
            my.list <- list()
            for (i in 1:length(assns)) {
                my.list[[i]] <- numericInput(paste("poss", i, sep = ""), paste(assns[i], "Points Possible"), value = 10, min = 0, max = 100)
            }
            
            my.list
        })
        
        out.df <- reactive({
            if (is.null(grade.data())) return(NULL)
            
            assns <- unique(grade.data()$assignmentname)
            
            vec <- c()
            for (i in 1:length(assns)) {
                vec <- c(vec, input[[paste("var", i, sep = "")]])
            }
            
            out.df <- data.frame(Assignment = assns, Type = vec)
            
            out.df$perc <- apply(out.df, 1, function(x) {
                perc <- input[[paste(x[2], "perc", sep = "")]]
                
                perc / nrow(subset(out.df, Type == x[2]))
            })
            
            out.df
        })
        
        ## Update the inputs based on the dataset
        observe({
            if (!is.null(grade.initial())) {
                updateSelectizeInput(session, "x", choices = as.character(unique(grade.initial()$assignmentname)), selected = as.character(unique(grade.initial()$assignmentname))[1])
                updateSelectizeInput(session, "y", choices = as.character(unique(grade.initial()$assignmentname)), selected = as.character(unique(grade.initial()$assignmentname))[2])
                
                vec <- grade.initial()$studentid
                names(vec) <- paste(grade.initial()$firstname, grade.initial()$lastname)
                
                updateSelectizeInput(session, "student", choices = vec, selected = vec[1])
            }
        })
        
        ## Compute the percentages for each assignment
        grade.scoresinit <- reactive({
            if (is.null(out.df())) return(NULL)
            
            mygrades <- grade.data()
            
            mygrades$perc <- apply(mygrades, 1, function(x) {
                perc <- as.numeric(out.df()$perc[out.df()$Assignment == x[4]])
                
                perc * (as.numeric(x[5]) / as.numeric(x[6]))
            })
            
            return(mygrades)
        })
        
        ## With the percentages, average over all assignment types
        grade.scores <- reactive({
            if (is.null(grade.scoresinit())) return(NULL)
            
            final <- summarise(group_by(grade.scoresinit(), studentid, lastname, firstname), finalscore = sum(perc))
            
            return(final)
        })
        
        ## Now that we have the final scores, compute final grades for everyone
        grade.grades <- reactive({
            if (is.null(grade.scores()) || length(unique(out.df()$Type)) == 1) return(NULL)
            
            mygrades <- arrange(as.data.frame(grade.scores()), desc(finalscore))
            grades <- mygrades$finalscore
            
            if (input$method == "automatic" & input$gradescale != "passfail") {
                passing <- rep("Pass", length(grades))
                passing[grades < input$pass] <- "f"
                kval <- if (input$gradescale == "plusminus") 10 else 4
                
                prcomp.score <- princomp(grades[passing != "f"])$scores[,1]
                kmean.score <- kmeans(prcomp.score, kval)
                
                clustvec <- kmean.score$cluster
                clustmeans <- as.numeric(kmean.score$centers)
                
                posgrades <- if (input$gradescale == "plusminus") c("a+", "a", "a-", "b+", "b", "b-", "c+", "c", "c-", "d") else letters[1:4]
                
                lettergrades <- c()
                for (i in 1:length(clustmeans)) {
                    lettergrades <- c(lettergrades, rep(posgrades[i], length(which(clustvec == order(-clustmeans)[i]))))
                }
                passing[passing == "Pass"] <- lettergrades
                mygrades$finalletter <- toupper(passing)
            } else {
                mygrades$finalletter <- apply(mygrades, 1, function(x) {
                    grade <- round(as.numeric(x[4]))
                    if (input$gradescale == "passfail") {
                        if (grade >= input$pass) "Pass" else "Fail"
                    } else {
                        mins <- unlist(sapply(paste(letters[1:4], "", sep = ""), function(y){input[[y]]}))
                        if (input$gradescale == "plusminus") mins <- c(mins, unlist(sapply(paste(letters[1:4], "-", sep = ""), function(y){input[[y]]})), unlist(sapply(paste(letters[1:4], "+", sep = ""), function(y){input[[y]]})))
                        mins <- c(mins, f = 0)
                        mins <- sort(mins, decreasing = TRUE)
                        
                        toupper(as.character(names(mins[grade >= mins][1])))   
                    }   
                })
            }
            
            mygrades
        })
        
        ###
        ### The four plots
        ###
        output$plot1 <- renderPlot({
            if (is.null(grade.grades())) return(NULL)
            
            print(
                qplot(finalletter, data = grade.grades(), fill = finalletter) +
                    theme(legend.position = "bottom")
            )
        })
        
        output$plot2 <- renderPlot({
            if (is.null(grade.grades())) return(NULL)
            
            print(
                qplot(finalscore, data = grade.grades())    
            )
        })
        
        output$grades <- renderDataTable({
            return(grade.grades())
        })
        
        grade.first <- reactive({
            if (is.null(input$data)) return(read.csv("www/example/data-new.csv"))
            
            grades.temp <- read.csv(input$data$datapath)
            grades.temp[is.na(grades.temp)] <- 0
            
            return(grades.temp)
        })
        
        observe({
            input$method
            
            set.seed(20141106)
        })
        
        output$plot4 <- renderPlot({
            if (is.null(grade.first())) return(NULL)
            
            ind1 <- which(names(grade.first()) == input$x) - 3
            ind2 <- which(names(grade.first()) == input$y) - 3
            poss1 <- if (input$specify_poss) input[[paste("poss", ind1, sep = "")]] else max(grade.first()[,input$x])
            poss2 <- if (input$specify_poss) input[[paste("poss", ind2, sep = "")]] else max(grade.first()[,input$y])
            
            print(
                qplot(100 * grade.first()[,input$x] / poss1, 100 * grade.first()[,input$y] / poss2) +
                    geom_smooth(method = "lm") +
                    xlab(paste(input$x, "(%)")) +
                    ylab(paste(input$y, "(%)")) +
                    xlim(c(0, 100)) + ylim(c(0, 100))
            )
        })
        
        output$plot3 <- renderPlot({
            if (is.null(grade.grades())) return(NULL)
            
            dists <- mahalanobis(grade.first()[,-(1:3)], colMeans(grade.first()[,-(1:3)]), cov(grade.first()[,-(1:3)]))
            
            mydat <- grade.grades()
            mydat$dists <- dists
            mydat$pass <- !(grade.grades()$finalletter %in% c("Fail", "F"))
            
            mydat <- mydat %>% arrange(desc(dists))
            mydat$studentid <- factor(mydat$studentid, levels = mydat$studentid)
            
            print(
                qplot(studentid, dists, data = mydat, colour = pass) +
                    theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) +
                    xlab("Student ID") +
                    ylab("Distance") +
                    geom_text(aes(x = studentid, y = dists, label = lastname), size = 3, hjust = 0, vjust = 0) +
                    geom_hline(yintercept = mean(dists))
            )
        })
        
        ## The initial dataset uploaded, after melting
        grade.initial <- reactive({
            if (is.null(grade.first())) return(NULL)
            
            return(melt(grade.first(), id.vars = c("studentid", "lastname", "firstname"), value.name = "points", variable.name = "assignmentname"))
        })
        
        ## Get the points possible for each assignment
        grade.data <- reactive({
            if (is.null(grade.initial()) | is.null(out.lst()) | is.null(out.lst2())) return(NULL)
            
            my.dat <- grade.initial()
            my.dat$pointsposs <- 1
            
            assns <- unique(my.dat$assignmentname)
            
            for (i in 1:length(assns)) {
                my.dat$pointsposs[my.dat$assignmentname == assns[i]] <- if (input$specify_poss) input[[paste("poss", i, sep = "")]] else max(my.dat$points[my.dat$assignmentname == assns[i]])
            }
            
            return(my.dat)
        })
        
        ## A testing data table
        output$test3 <- renderDataTable({
            if (is.null(grade.scoresinit())) return(NULL)
            
            return(grade.scoresinit())
        })
        
        student.means <- reactive({
            if (is.null(grade.scoresinit())) return(NULL)
            
            mydat <- grade.scoresinit()
            mydat$type <- rep(out.df()$Type, each = nrow(grade.first()))
            
            newdat <- summarise(group_by(mydat, studentid, type), mean = mean(perc))
            
            vec <- c()
            for (i in unique(newdat$type)) {
                vec <- c(vec, input[[paste(i, "perc", sep = "")]])
            }
            
            counts <- summarise(group_by(out.df(), Type), length = length(Assignment))$length
            
            newdat$mean <- newdat$mean * (100 / vec) * counts
            
            return(newdat)
        })
        
        ## Generate the face data in the right format
        faces.dat <- reactive({
            if (is.null(student.means())) return(NULL)
            
            my.dat <- dcast(student.means(), studentid ~ type)
            
            return(my.dat)
        })
        
        ## Testing function
        output$test2 <- renderDataTable({
            if (is.null(faces.dat())) return(NULL)
            
            return(faces.dat())
        })
        
        ## Compute the dataset for the face generation
        plotdat <- reactive({
            if (is.null(faces.dat())) return(NULL)
            
            # now create data.frame for plotting.
            plotdat <- matrix(1, nrow = nrow(faces.dat()), ncol = 15) # all constant
            rownames(plotdat) <- faces.dat()$studentid
            
            # now set only the columns aka face components of interest
            plotdat[,c(1, 2)] <- if (is.null(faces.dat()$Test)) 1 else faces.dat()$Test
            plotdat[,c(4, 5)] <- if (is.null(faces.dat()$Project)) 1 else faces.dat()$Project
            plotdat[,c(7, 8)] <- if (is.null(faces.dat()$Lab)) 1 else faces.dat()$Lab
            plotdat[,c(9, 10)] <- if (is.null(faces.dat()$Quiz)) 1 else faces.dat()$Quiz
            plotdat[,c(12, 13)] <- if (is.null(faces.dat()$Homework)) 1 else faces.dat()$Homework
            
            return(plotdat)
        })
        
        ## Generate the face plot
        faces.gen <- reactive({
            if (is.null(plotdat())) return(NULL)
            
            return(faces(plotdat(), face.type = 0, plot = FALSE))
        })
        
        ## Return the means for all students
        output$studentmean <- renderDataTable({
            if (is.null(student.means())) return(NULL)
            
            return(filter(student.means(), studentid == input$student))
        })
        
        ## Return the student id
        output$stuid <- renderText({
            return(paste("Student ID:", input$student))
        })
        
        ## From the final grades, get the student's grade
        output$stugrade <- renderText({
            if (is.null(grade.grades())) return(NULL)
            
            mygrade <- filter(grade.grades(), studentid == input$student)$finalletter
            
            return(paste("Final Grade:", mygrade))
        })
        
        ## From the original dataset, get the student's scores
        output$studentscores <- renderDataTable({
            if (is.null(grade.first())) return(NULL)
            
            return(filter(grade.first(), studentid == input$student))
        })
        
        ## Display the face
        output$face <- renderPlot({
            if (is.null(faces.gen())) return(NULL)
            
            mydat <- faces.gen()
            mydat$faces <- mydat$faces[names(mydat$faces) == input$student]
            print(
                plot(mydat)    
            )
        }, height = 700)
        
        
        ##
        ## More controls - Display what's generated in the out functions
        ##
        output$moreControls <- renderUI({
            if (is.null(grade.initial())) return(NULL)
            
            out.lst()
        })
        
        output$moreControls2 <- renderUI({
            if (is.null(grade.initial())) return(NULL)
            
            out.lst2()
        })
    })

runApp(ui = ui, server = server)
