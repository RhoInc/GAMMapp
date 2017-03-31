
server = function(input, output, session){
  
  
  ## data upload ----
  datafile <-  reactive({
    
    if(is.null(input$datafile)){
      return(NULL)
    }else{
      input$datafile
    }
  })
  
  ## Create reactive data based on data upload OR example data selection ----
  dd <-  reactive({	
    
    validate(
       need(! is.null(datafile()) | input$dataButton=='Use example data','')
     )

    if(input$dataButton == 'Use example data'){
   #   return(data.frame(icata_vis))
      return(data.frame(dat))
    }
    else {
      if (length(grep(".csv", datafile(), ignore.case = TRUE)) > 0){
        return(
          data.frame(read.csv(datafile()$datapath, na.strings=NA))
        )
      }else if(length(grep(".sas7bdat", datafile(), ignore.case = TRUE)) > 0){
        return(
          data.frame(
            haven::read_sas(datafile()$datapath) 
          ))
      }else{return(NULL)}
    }  
  })

  observe({
    updateSelectInput(session, inputId = 'id', choices = names(dd()))
  })
  
  
  observe({
    dd <- dd()
    y_choices = setdiff(names(dd), input$id)
    updateSelectInput(session, inputId = 'y', choices = y_choices)
  })
  
  observe({
    dd <- dd()
    x_choices = setdiff(names(dd), c(input$id, input$y))
    updateSelectInput(session, inputId = 'x', choices = x_choices)
  })
  
  observe({
    dd <- dd()
    
    isolate({
      vars <- c(input$x, input$y, input$d)
      choices <- dd[,setdiff(names(dd), vars)]
      g <- choices %>% summarise_each(funs(n_distinct)) < 6
      c <- colnames(g)[which(g==T)]
    })
    
    if (input$group){
      updateSelectInput(session, inputId = 'groupvar', choices = c)
    } 
  })
  
  ## If group is selected, make factor ---
  dd2 <-  eventReactive(input$go,{

    isolate({
      dd <- dd()

      if (input$group==TRUE){
        dd[,input$groupvar] <- as.factor(dd[,input$groupvar])
        dd <- na.omit(dd[,c(input$y, input$x, input$groupvar, input$id)])
      } else {
        dd <- na.omit(dd[,c(input$y, input$x, input$id)])
      }
    })
    
    return(dd)
  })
  
  ## Indicator: is y binomial? ----
  ybin <- eventReactive(input$go,{
    dd2 <- dd2()
    return(n_distinct(dd2[,input$y])==2)
  })


  #### NON - LINEAR MODELING ----
  # m0 <- eventReactive(input$go,{
  #   dd2 <- dd2()
  # 
  #   dd2$idfact <- as.factor(dd2[,input$id])
  #   dd2$time <- dd2[,input$x]
  # 
  #   if(input$bs){
  #     if(input$group){
  #       f <- as.formula(paste0(input$y, '~ s(', input$x, ', k=', as.integer(input$k), ', bs="cc")'))
  #     }
  #     else{
  #      f <- as.formula(paste0(input$y, '~ s(', input$x, ', k=', as.integer(input$k),', bs="cc")'))
  #     }
  #   } else{
  #     if(input$group){
  #       f <- as.formula(paste0(input$y, '~ s(', input$x, ', k=', as.integer(input$k), ')'))
  #     }
  #     else{
  #      f <- as.formula(paste0(input$y, '~ s(', input$x, ', k=', as.integer(input$k),')'))
  #     }
  #   }
  # 
  # 
  #   if(input$rand=='Intercept and slope'){
  #     if (ybin()==1){
  #       dd2[,input$y] <- as.factor(dd2[,input$y])
  #       fit <- gamm(f, family=binomial, data=dd2,
  #                   random = list(idfact=~time),,
  #                   control=list(maxIter=100))
  #     }
  #     else{
  #       fit <- gamm(f, family=gaussian, data=dd2,
  #                   random = list(idfact=~time),
  #                   control=list(maxIter=100))
  #     }
  #   } else if (input$rand=='Intercept only'){
  #     if (ybin()==1){
  #       dd2[,input$y] <- as.factor(dd2[,input$y])
  #       fit <- gamm(f, family=binomial, data=dd2,
  #                   random = list(idfact=~1),
  #                   control=list(maxIter=100))
  #     }
  #     else{
  #       fit <- gamm(f, family=gaussian, data=dd2,
  #                   random = list(idfact=~1),
  #                   control=list(maxIter=100))
  #     }
  #   } else{
  #     if (ybin()==1){
  #       dd2[,input$y] <- as.factor(dd2[,input$y])
  #       fit <- gamm(f, family=binomial, data=dd2,
  #                   control=list(maxIter=100))
  #     }
  #     else{
  #       fit <- gamm(f, family=gaussian, data=dd2,
  #                   control=list(maxIter=100))
  #     }
  #   }
  # 
  #   return(fit)
  # })

  m1 <- eventReactive(input$go,{
      dd2 <- dd2()
      
      dd2$idfact <- as.factor(dd2[,input$id])
      dd2$time <- dd2[,input$x]

      if(input$bs=='Yes'){
        if(input$group){
          f <- as.formula(paste0(input$y, '~', input$groupvar, '+ s(', input$x, ', by=', input$groupvar, ', k=', as.integer(input$k), ', bs="cc")'))
        } else{
          f <- as.formula(paste0(input$y, '~ s(', input$x, ', k=', as.integer(input$k),', bs="cc")'))
        }
      } else{
        if(input$group){
          f <- as.formula(paste0(input$y, '~', input$groupvar, '+ s(', input$x, ', by=', input$groupvar, ', k=', as.integer(input$k), ')'))
        } else{
          f <- as.formula(paste0(input$y, '~ s(', input$x, ', k=', as.integer(input$k),')'))
        }
      }


      if(input$rand=='Intercept and slope'){
        if (ybin()==1){
          dd2[,input$y] <- as.factor(dd2[,input$y])
          fit <- safe_gamm(f, family=binomial, data=dd2,
                      random = list(idfact=~time),
                      control=list(maxIter=100))
        }
        else{
          fit <- safe_gamm(f, family=gaussian, data=dd2,
                      random = list(idfact=~time),
                      control=list(maxIter=100))
        }
      } else if (input$rand=='Intercept only'){
          if (ybin()==1){
            dd2[,input$y] <- as.factor(dd2[,input$y])
            fit <- safe_gamm(f, family=binomial, data=dd2,
                        random = list(idfact=~1),
                        control=list(maxIter=100))
          }
          else{
            fit <- safe_gamm(f, family=gaussian, data=dd2,
                        random = list(idfact=~1),
                        control=list(maxIter=100))
          }
      } else{
        if (ybin()==1){
          dd2[,input$y] <- as.factor(dd2[,input$y])
          fit <- safe_gamm(f, family=binomial, data=dd2,
                      control=list(maxIter=100))
        }
        else{
          fit <- safe_gamm(f, family=gaussian, data=dd2,
                      control=list(maxIter=100))
        }
      }

      return(fit)
    })

  rr1 <- eventReactive(input$go,{
      m1 <- m1()$result
      dd2 <- dd2()
      
      req(!is.null(m1))

      if (input$group){
        mm1 <- expand.grid(x=seq(min(dd2[,input$x]), max(dd2[,input$x]), length=100),
                           gr=levels(dd2[,input$groupvar]))
        names(mm1) <- c(input$x, input$groupvar)
      } else{
        mm1 <- expand.grid(x=seq(min(dd2[,input$x]), max(dd2[,input$x]), length=100))
        names(mm1) <- c(input$x)
      }
      pp1 <- predict.gam(m1$gam,mm1,se.fit=T,type='response')
      rr1 <- data.frame(dv=input$y,mm1,pp1[1],pp1[2])
      rr1$lower <- rr1$fit - rr1$se.fit
      rr1$upper <- rr1$fit + rr1$se.fit

      return(rr1)

    })


  ### LINEAR MODELING ----

  mL <- eventReactive(input$go,{
    dd2 <- dd2()

    dd2$idfact <- as.factor(dd2[,input$id])


    if (input$group){
      if(input$rand=='Intercept and slope'){

        f <- as.formula(paste0(input$y, '~ ', input$groupvar, '+ (1+', input$x, '|', input$id,')'))

        if (ybin()==1){
          dd2[,input$y] <- as.factor(dd2[,input$y])
          fit <- glmer(f, family=binomial, data=dd2)
        }
        else{
          fit <- lmer(f, data=dd2)
        }
      } else if (input$rand=='Intercept only'){


        f <- as.formula(paste0(input$y, '~ ', input$groupvar,'+',input$x, '+',input$groupvar,'*',input$x, '+ (1|', input$id,')'))


        if (ybin()==1){
          dd2[,input$y] <- as.factor(dd2[,input$y])
          fit <- glmer(f, family=binomial, data=dd2)
        }
        else{
          fit <- lmer(f, data=dd2)
        }
      } else{

        f <- as.formula(paste0(input$y, '~ ', input$x, '*', input$groupvar))

        if (ybin()==1){
          dd2[,input$y] <- as.factor(dd2[,input$y])
          fit <- glm(f, data=dd2, family=binomial)
        }
        else{
          fit <- lm(f, data=dd2 )
        }
    }

    } else {
      if(input$rand=='Intercept and slope'){

          f <- as.formula(paste0(input$y, '~ (1+', input$x, '|', input$id,')'))

          if (ybin()==1){
            dd2[,input$y] <- as.factor(dd2[,input$y])
            fit <- glmer(f, family=binomial, data=dd2)
          }
          else{
            fit <- lmer(f, data=dd2)
          }
        } else if (input$rand=='Intercept only'){


          f <- as.formula(paste0(input$y, '~ ', input$x,  '+ (1|', input$id,')'))


          if (ybin()==1){
            dd2[,input$y] <- as.factor(dd2[,input$y])
            fit <- glmer(f, family=binomial, data=dd2)
          }
          else{
            fit <- lmer(f, data=dd2)
          }
        }else{


          f <- as.formula(paste0(input$y, '~ ', input$x))


          if (ybin()==1){
            dd2[,input$y] <- as.factor(dd2[,input$y])
            fit <- glm(f, data=dd2, family=binomial)
          }
          else{
            fit <- lm(f, data=dd2 )
          }
        }

    }

    return(fit)
  })

  rr2 <- eventReactive(input$go,{
    m2 <- mL()
    dd2 <- dd2()

    if (input$group){
      mm2 <- expand.grid(x=seq(min(dd2[,input$x]), max(dd2[,input$x]), length=100),
                         gr=levels(dd2[,input$groupvar]))
      names(mm2) <- c(input$x, input$groupvar)
    } else{
      mm2 <- expand.grid(x=seq(min(dd2[,input$x]), max(dd2[,input$x]), length=100))
      names(mm2) <- c(input$x)
    }

    if (input$rand=='None'){
      pp2 <- predict(m2, mm2, se.fit=T, type='response')
    }else{
      pp2 <- AICcmodavg::predictSE(m2, mm2, se.fit=T, type='response')
    }
    rr2 <- data.frame(dv=input$y,mm2,pp2[1],pp2[2])
    rr2$lower <- rr2$fit - rr2$se.fit
    rr2$upper <- rr2$fit + rr2$se.fit

    return(rr2)

  })

  
  #### define common y limits ----
  lim <- eventReactive(input$go,{
    upper <- max(c(rr1()$upper), c(rr2()$upper))*1.25
    lower <- min(c(rr1()$lower), c(rr2()$lower))*0.75
    return(c(lower, upper))
  })

  #### set y - limits ----
  observeEvent(input$go, {

    maxval <- ceiling(lim()[2]*1.2)

    output$ylim <- renderUI({sliderInput("ylim", "Set y-axis limits", min=0, max=maxval,
                                         value = c(floor(lim()[1]), ceiling(lim()[2])),
                                         step=0.1)})
  })
  
  #### create checkbox for viewing model output ----
  observeEvent(input$go, {
    output$resulttext <- renderUI({checkboxInput('resulttext','Show model output')})
  })

  ### Non-linear plot gammm----
    output$gamPlot <- renderPlot({

      if (is.null(input$ylim)){return()}

      ylimit <- input$ylim


      isolate({
        
        rr1 <- rr1()

        # f <-summary(m1()$result$gam)$formula %>% as.character
        # ff <- paste(f[2], f[1], f[3])
        
        if(input$group==TRUE){
          p <- ggplot(data=rr1, aes_string(x=input$x, y='fit', color = input$groupvar))+
            geom_line(size=2) +
            theme_bw() +
            geom_ribbon(data=rr1, aes_string(ymin='lower', ymax='upper', fill=input$groupvar), alpha=0.3)
          #+
           # theme(legend.position = c(0.9, 0.75))
        } else{
            p <- ggplot(data=rr1, aes_string(x=input$x, y='fit')) +
              geom_line(size=2) +
              theme_bw() +
              geom_ribbon(data=rr1, aes(ymin=lower, ymax=upper), alpha=0.3)
        }

        p <- p  +
          #  scale_x_continuous(breaks=seq(1,12,1),
          #                     labels=c('J','F','M','A','M','J','J','A','S','O','N','D')) +
            labs(x=input$x,
                 y=toupper(rr1$dv),
                 title='Non-Linear Plot') +
          theme(axis.text.x = element_text(size=14),
                axis.text.y = element_text(size=14),
                axis.title = element_text(size=14, face='bold'),
                plot.title = element_text(size=16, face='bold'))+
          ylim(ylimit[1], ylimit[2])

      })

    #  print(p) 

      if (! is.null(m1()$result)){
        print(p)
      }
      else{}
      
    })


   #### Linear plot lm/lmer/glm/glmer ----
    output$glmPlot <- renderPlot({

      if (is.null(input$ylim)){return()}

      ylimit <- input$ylim

      isolate({

        rr2 <- rr2()

        # f <- summary(mL())$call %>% as.character
        # ff <- paste(f[2])

        if(input$group==TRUE){
          p <- ggplot(data=rr2, aes_string(x=input$x, y='fit', color = input$groupvar)) +
            geom_line(size=2) +
            theme_bw() +
            geom_ribbon(data=rr2, aes_string(ymin='lower', ymax='upper', fill=input$groupvar), alpha=0.3)
        } else{
          p <- ggplot(data=rr2, aes_string(x=input$x, y='fit'))+
            geom_line(size=2) +
            theme_bw() +
            geom_ribbon(data=rr2, aes(ymin=lower, ymax=upper), alpha=0.3)
        }

        p <- p  +
          labs(x=input$x,
               y=toupper(rr2$dv),
               title='Linear Plot') + 
          theme(axis.text.x = element_text(size=14),
                axis.text.y = element_text(size=14),
                axis.title = element_text(size=14, face='bold'),
                plot.title = element_text(size=16, face='bold'))+
          ylim(ylimit[1], ylimit[2])


      })

      print(p)
    })

  # 
  #   output$linmodelSummary <- renderJsonedit({
  #     jsonedit(mL())
  #   })
    output$linmodelSummary <- renderPrint(
      summary(mL())
    )


    output$modelSummary <- renderPrint(
      
      if (! is.null(m1()$result)){
        summary(m1()$result$gam)
      }
      else{
   #     paste('Warnings: ', m1()$warning)
        paste('Error: ', m1()$error)
      }
    )
    # 
    # output$modelCompare <- renderPrint({
    #   
    #   m1 <- m1()
    #   m0 <- m0()
    #   
    #   isolate({
    #     if(input$group==T){
    #       a <- anova(m1$gam, m0$gam) 
    #     } else{
    #       a <- 'to-do item'
    #     }
    #   })
    #   
    #   print(a)
    # })
    # 
    
    output$gamPlot2 <- renderPlot({
      
      if (is.null(input$ylim)){return()}
      
      ylimit <- input$ylim
      input$plotX
      input$plotY
      input$plotTitle
      
      
      isolate({
        
        rr1 <- rr1()
        
        # f <-summary(m1()$result$gam)$formula %>% as.character
        # ff <- paste(f[2], f[1], f[3])
        
        if(input$group==TRUE){
          p <- ggplot(data=rr1, aes_string(x=input$x, y='fit', color = input$groupvar))+
            geom_line(size=2) +
            theme_bw() +
            geom_ribbon(data=rr1, aes_string(ymin='lower', ymax='upper', fill=input$groupvar), alpha=0.3) +
            theme(legend.position = c(0.9, 0.75))
        } else{
          p <- ggplot(data=rr1, aes_string(x=input$x, y='fit'))+
            geom_line(size=2) +
            theme_bw() +
            geom_ribbon(data=rr1, aes(ymin=lower, ymax=upper), alpha=0.3)
        }
        
        p <- p  +
          labs(x=input$plotX,
               y=input$plotY, 
               title=input$plotTitle) +
          theme(axis.text.x = element_text(size=14),
                axis.text.y = element_text(size=14),
                axis.title = element_text(size=14, face='bold'),
                plot.title = element_text(size=16, face='bold'))+
          ylim(ylimit[1], ylimit[2])
        
      })

      if (! is.null(m1()$result)){
        print(p)
      }
      else{}
      
    })

    output$modelSummary2 <- renderPrint({
      if (! is.null(m1()$result)){
        summary(m1()$result$gam)
      }
      else{
        paste('Error: ', m1()$error)
      }  
    }
    )
    
    output$report <- downloadHandler(
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in case we don't
        # have write permissions to the current working dir (which can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("template/report.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(data = rr1(),
                       title = input$reporttitle,
                       name = input$name, 
                       date = input$date,
                       text = input$reporttext,
                       plotTitle = input$plotTitle,
                       plotX = input$plotX,
                       plotY = input$plotY,
                       x = input$x,
                       group = input$group, 
                       groupvar = input$groupvar,
                       m1 = m1()$result$gam,
                       ylim = input$ylim)
        
        rmarkdown::render(tempReport,
                          output_file = file,
                          params = params,  ## pass in params
                          envir = new.env(parent = globalenv())  ## eval in child of global env
        )
      }
    )
    
    output$about <- renderUI({
      HTML('<h1> <b> Longitudinal and seasonal patterns using <a href="https://cran.r-project.org/web/packages/mgcv/index.html">
            mgcv::gamm() </a> </b> </h1>  
            <p> While many smoothing options are available using SAS and R plotting libraries, the <code> mgcv </code> 
            package offers an advantage to those who are interested in making inference or publishing their results.  
            The <code> mgcv </code> package offers both generalized additive models and generalized additive mixed models, 
            suiting both cross sectional and repeated measures data sets.  Many outcome distributions are accommodated, 
            as well as various model parameters. 
            </p> <p> The results of these models can be visualized by plotting the predicted values of the fixed effects terms.
            These visualizations are useful within clinical trials research, where seasonal patterns are studied (see image to the 
<a href="http://www.jacionline.org/article/S0091-6749(16)30883-1/abstract" target="_top">right</a>).  Additionally,
            nested models can be compared using anova() to assess the significance of different parameters or smoothed
            terms.  </p>
            <p> Crafting your model is somewhat of an art. It is helpful to begin with a linear 
            representation of the data and slowly add complexity. For this purpose, this app offers a side-by-side linear and 
            non-linear model for comparison purposes.  Occasionally, convergence issues arise, a signal to  
            take a step back in the level of complexity. Having the graphic available at the time of modeling is very helpful 
            to ensure the appropriate smoothing term is selected. The app also accomodates cross sectional data, if no random effects are selected. Packgaes utilized by this
           app include: <code> shiny </code>, <code> tidyverse</code>, <code> ggplot2</code>, <code> mgcv</code>, 
           <code> shinyBS</code>, <code> lme4</code>, and <code> AICcmodavg</code>. 
            </p>  <p>I would like to continue to develop this app so it can be both educational and useful to the common statistician.
            </p> 
            <p> The code for this app can be found <a href="https://github.com/RhoInc/GAMMapp">here </a>. Please contact
             <a href="mailto:rebecca_krouse@rhoworld.com?Subject=Longitudinal%20Model%20App" target="_top">Becca Krouse</a> 
           with any comments or suggestions. </p>')
    })
    output$manu <- renderImage({
      list(src = 'img/img_manu.png', height=600)
    }, deleteFile=FALSE)
    
    output$aboutData <- renderUI({
      HTML('<p> The default data for this app is the 
           <a href="https://www.hsph.harvard.edu/fitzmaur/ala2e/toenail.txt" target="_top">toenail</a> 
           dataset. Feel free to upload your own data and try it out!
           </p>')
    })
  
}