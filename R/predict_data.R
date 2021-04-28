library('fpp2')


#' @title get_residuals
#' @description Get the residuals of a model. We use this metric to decide the
#' best model (lower residual is better). Future implementations might include AIC
#' @param model_name (character): The name of the model to use. One of 'base',
#' 'snaive', 'ets', arima'. NOTE: 'base' currently disabled due to poor of accuracy
#' @param ts_data (dataframe): an events dataframe
#' @return a list with model_name, model_fit and standard_error 
get_residuals <- function(model_name, ts_data){
  # get the value as a time series
  Y <- ts(ts_data$value, start = min(ts_data$date))
  # make data stationary (required by some models)
  DY <- diff(Y)
  if (model_name=='base'){
    # the base model is a simple linear regression
    ts_data$DY <- c(0,diff(ts_data$value))
    y <- ts_data$DY[seq(2, nrow(ts_data))]
    x <- ts_data$date[seq(2, nrow(ts_data))]
    model_fit <- lm(y ~ x)
  } else if (model_name == 'snaive'){
    # snaive needs stationary data
    model_fit <- snaive(DY)
  } else if(model_name == 'ets'){
    # ets does not need stationary data
    model_fit <- ets(Y)
  } else if(model_name == 'arima'){
    # arima will make the data stationary itself
    model_fit <- auto.arima(Y, d = 1, seasonal = FALSE, 
                      stepwise = FALSE, 
                      approximation = TRUE, 
                      trace = FALSE)
  }
  # calculate the standard error
  standard_error <- sd(model_fit$residuals, 
                       na.rm = TRUE)/sqrt(length(model_fit$residuals))
  # assemble output
  return(list(model_name  = model_name,
              model_fit = model_fit, 
              standard_error = standard_error))
}

#' @title get_best_model
#' @description Find the best model between the implemented one: 'base',
#' 'snaive', 'ets', arima'. The best model is the one with the lowest residuals
#' @param ts_data (dataframe): an events dataframe
#' @param train_window (numeric): the number of days prior to the last 
#' observation to use for training
#' @return a list with model_name, model_fit and standard_error
get_best_model <- function(ts_data, train_window){
  
  # keep only data for training (i.e. more recent than latest-training_window)
  ts_data <- ts_data[seq(nrow(ts_data)-train_window, nrow(ts_data)),]
  
  # find the best model
  lowest_residuals <- Inf
  best_model <- NULL
  for (i in c(
    #'base', # base is currently disabled
    #'snaive', 
    #'ets', 
    'arima'
    )){
    this_model <- get_residuals(i, ts_data)
    if (!is.na(this_model$standard_error) && this_model$standard_error < lowest_residuals){
      # we found a model better than the one we had: save it and change the reference
      # lowest_residuals
      best_model <- this_model
      lowest_residuals <- this_model$standard_error
    }
  }
  
  return(best_model)
}

#' @title get_predictions
#' @description Calculate the predictions from the model
#' @param model (list): the output of get_best_model()
#' @param days_predict (numeric): How many days in the future to predict
#' @return a list with the predictions and the confidence intervals at 80% and 95%
get_predictions <- function(model, days_predict){
  # predictions for the model 'base': a simple linear regression
  if (model$model_name == 'base'){
    pred <- predict(model$model_fit,
                    data.frame(
                      x = seq(max(model$model_fit$model$x)+1,
                              max(model$model_fit$model$x)+days_predict,
                              1)
                    )
    )
    # at the moment we do not report confidence interval for the base model
    # since  it is disabled anyway
    low_80 <- rep(NA, length(pred))
    high_80 <- rep(NA, length(pred))
    low_95 <- rep(NA, length(pred))
    high_95 <- rep(NA, length(pred))
  } else if (model$model_name%in% c('snaive','ets', 'arima')){
    # predictions for the other models
    predictions <- forecast(model$model_fit,
                            h=days_predict)
    low_80 <- as.vector(predictions$lower[,1])
    high_80 <- as.vector(predictions$upper[,1])
    low_95 <- as.vector(predictions$lower[,2])
    high_95 <- as.vector(predictions$upper[,2])
    pred <- (low_80+high_80)/2
  }
  # assmble output
  predictions <- list(pred = pred,
                      low_80 = low_80,
                      high_80 = high_80,
                      low_95 = low_95,
                      high_95 = high_95
                      )
  return(predictions)
}

#' @title assemble_data_plot_predictions
#' @description Clean up and format the data
#' @param predictions (list): the output of get_predictions()
#' @param days_predict (numeric): How many days in the future to predict
#' @param ts_data (dataframe): an events dataframe
#' @param train_window (numeric): the number of days prior to the last 
#' @param model (list): the output of get_best_model()
#' @param confidence (numeric): Which level of confidence we want to keep. 
#' One of 80, 95 or NULL. If NULL, we do not keep any confidence
#' @return a list with the predictions and the confidence intervals at 80% and 95%
assemble_data_plot_predictions <- function(predictions, ts_data, 
                                           days_predict, model,
                                           training_window, confidence){
  pred <- predictions$pred
  low_80 <- predictions$low_80
  high_80 <- predictions$high_80
  low_95 <- predictions$low_95
  high_95 <- predictions$high_95
  
  res_x <- ts_data$date 
  res_y <- ts_data$value
  
  if (model$model_name %in% c('base', 'snaive')){
    ts_data2 <- ts_data[seq(nrow(ts_data)-training_window, nrow(ts_data)),]
    Y <- ts(ts_data2$value, start = min(ts_data2$date))
    DY <- diff(Y)
    pred <- c(as.vector(DY), pred)
    pred <- cumsum(pred)
    pred <- pred[seq(length(DY)+1,length(DY)+days_predict,1)]
  }
  
  pred_dates <- seq(max(ts_data$date)+1, 
                    max(ts_data$date)+days_predict, 1)
  categories <- c(rep('data', length(res_x)),
                  rep('prediction', length(pred)),
                  rep('confidence_80', length(pred)),
                  rep('confidence_80', length(pred)),
                  rep('confidence_95', length(pred)),
                  rep('confidence_95', length(pred)))
  
  
  final_df <-data.frame(date = c(res_x, pred_dates,
                                  pred_dates,pred_dates,
                                  pred_dates,pred_dates),
                        value = c(res_y, pred, low_80,
                                   high_80, low_95,
                                   high_95),
                        category = categories)
  
  if (is.null(confidence)){
    final_df <- final_df %>%
      dplyr::filter(!category %in% c('confidence_80', 'confidence_95'))
  } else if (confidence == 80){
    final_df <- final_df %>%
      filter(!category %in% c('confidence_95'))
  } else if (confidence == 95){
    final_df <- final_df %>%
      filter(!category %in% c('confidence_80'))
  }
  
  final_df$value[final_df$value<0] <- NA
  
  return(final_df)
}


#' @title assemble_data_plot_predictions
#' @description Clean up and format the data
#' @param days_predict (numeric): How many days in the future to predict
#' @param events (dataframe): an events dataframe
#' @param train_window (numeric): the number of days prior to the last 
#' @param confidence (numeric): Which level of confidence we want to keep. 
#' One of 80, 95 or NULL. If NULL, we do not keep any confidence
#' @return a list with the predictions and the confidence intervals at 80% and 95%
#' @export
predict_data <- function(events, days_predict=60, 
                         training_window=14, 
                         confidence=NULL){
  spCountry <- split(events, events$Country)
  resCountry <- lapply(spCountry, function(thisCountry){
    spVariable <- split(thisCountry, thisCountry$variable)
    resVariable <- lapply(spVariable, function(thisVariable){
      best_model <- tryCatch({
        get_best_model(thisVariable, training_window)
      },error = function(msg){
        return(NULL)
      })
      predictions <- get_predictions(best_model, days_predict)
      
      final_df <- assemble_data_plot_predictions(predictions, thisVariable,
                                                 days_predict, best_model,
                                                 training_window, confidence)
      final_df$variable <- unique(thisVariable$variable)
      final_df$Country <- unique(thisVariable$Country)
      return(final_df)
    })
    assembledCountry <- dplyr::bind_rows(resVariable)
    return(assembledCountry)
  })
  full_df <- dplyr::bind_rows(resCountry)
  
  return(full_df)
}




