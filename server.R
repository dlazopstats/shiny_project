library(shiny)
library(caret)
library(gbm)

shinyServer(function(input, output) {

    observeEvent(input$runProfileAlgorithm,{

        data(cars)
        indx  <- createDataPartition(cars$Price, p=input$porc, list=FALSE)
        Train <- cars[indx, ]
        Test  <- cars[-indx, ]
        cars$indx<-1:nrow(cars)

        fitControl <- trainControl(method = "repeatedcv",
                                   number = 3,
                                   repeats = 5)

        # Multiple linear regression model
        model_lm <- train(Price ~ .,
                          data = Train,
                          method = "lm",
                          trControl=fitControl)
        pred_lm <<- predict(model_lm,newdata = Test)

        #Lasso regression model
        model_lasso <- train(Price ~ .,
                             data = Train,
                             method = "lasso",
                             trControl=fitControl)
        pred_lasso <- predict(model_lasso,newdata = Test)

        # GAM
        model_gam <- train(Price ~ .,
                           data = Train,
                           method = "gam",
                           trControl=fitControl)
        pred_gam <- predict(model_gam,newdata = Test)

        # XGBOOST
        model_gbm <- train(Price ~ .,
                           data = Train,
                           method = "gbm",
                           trControl=fitControl,
                           verbose=FALSE)
        pred_gbm <- predict(model_gbm,newdata = Test)

        results <- resamples(list(lm=model_lm,
                                  lasso=model_lasso,
                                  gam=model_gam,
                                  gbm=model_gbm))



        output$plot_l1 <- renderPlot({
            plot(Test$Price,ylab = "Price",col="blue",type="l",
                 main="Real Price (Blue) vs Predicted Price (Red)",sub="Linear Model")
            lines(pred_lm,col="red")
        })


       output$plot_l2 <- renderPlot({
               plot(varImp(model_lm))
       })

      output$plot_l2 <- renderPlot({
          plot(varImp(model_lm))
      })

       output$plot_la1 <- renderPlot({
           plot(Test$Price,ylab = "Price",col="blue",type="l",
                main="Real Price (Blue) vs Predicted Price (Red)",sub="Lasso Model")
           lines(pred_lasso,col="red")
       })

       output$plot_la2 <- renderPlot({
           plot(varImp(model_lasso))
       })

       output$plot_ga1 <- renderPlot({
           plot(Test$Price,ylab = "Price",col="blue",type="l",
                main="Real Price (Blue) vs Predicted Price (Red)",sub="GAM Model")
           lines(pred_gam,col="red")
       })

       output$plot_ga2 <- renderPlot({
           plot(varImp(model_gam))
       })

       output$plot_gb1 <- renderPlot({
           plot(Test$Price,ylab = "Price",col="blue",type="l",
                main="Real Price (Blue) vs Predicted Price (Red)",sub="GBM Model")
           lines(pred_gbm,col="red")
       })

       output$plot_gb2 <- renderPlot({
           plot(varImp(model_gbm))
       })

       output$comp1 <- renderPlot({
           bwplot(results, metric="MAE")
       })

       output$comp2 <- renderPlot({
           bwplot(results, metric="Rsquared")
       })



        shinyalert(title = "Algorithm executed correctly", type = "success")

    }
    )

})

