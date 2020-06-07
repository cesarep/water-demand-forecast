library(shiny)
library(DT)
library(readxl)
library(forecast)
library(ggplot2)
library(qqplotr)

theme_update(plot.title = element_text(hjust = 0.5, size = 16))

shinyServer(function(input, output, session) {

	#### IMPORT TAB ####
	## Import file

	import = reactive({
		#req(input$file)

		if(is.null(input$file))
			return(data.frame(demand=c(2098064,2131560,2155893,2148904,2139178,2092157,2000241,1986054,2086004,
								   2002469,2207489,2068330,2213972,2400832,2162789,2182637,2074590,2044090,
								   2058373,2038225,2048551,2074917,2216076,2115768,2223332,2200935,2124300,
								   2096102,2137916,2130628,2001614,2108760,2112368,2061940,2116044,2181326,
								   2259042,2280324,2231722,2231215,2178884,2059325,2076815,2097688,2153082,
								   2130012,2224171,2209526,2345104,2314853,2294366,2154050,2194208,2191145,
								   2212914,2215803,2282421,2244625,2243165,2317752,2332073,2333264,2295320)))

		filetype = tolower(tail(strsplit(input$file$name, "\\.")[[1]], 1))
		file = input$file$datapath

		output$filetype <- reactive({ filetype })
		outputOptions(output, "filetype", suspendWhenHidden = FALSE)

		if(filetype == 'csv') {
			data = read.csv(file, input$csv_header, input$csv_separator, input$csv_quote, input$csv_decimal)
		}

		if(filetype == 'xls' || filetype == 'xlsx'){
			data = read_excel(file, input$excel_sheet)
		}

		updateSelectInput(session, 'data_column', choices = colnames(data))

		return(data)
	})

	## Display file preview
	output$table <- renderDataTable({
		datatable(import(), rownames = FALSE)
	})

	#### OPTIONS TAB ####


	time_series = debounce(reactive({
		req(import())
		try({
			data = import()[[input$data_column]]

			if(!is.null(input$file))
				updateTextInput(session, 'data_label', value=input$data_column)

			start = input$data_year + (as.numeric(input$data_period)-1)/12

			time_s = ts(data, start = start, frequency = 12)

			tsclean(time_s)

			if(input$data_validation_cb){
				#end = start + (length(data) - input$data_validation-1)/12
				end = length(data) - input$data_validation
				train = subset(time_s, start = 1, end = end)
				valid = subset(time_s, start = end+1)
				return(list(train, valid))
			}

			return(list(time_s, FALSE))
		}, T)
	}), 1000)

	data_label = debounce(reactive({input$data_label}), 2500)

	output$option_plot <- renderPlot({
		req(time_series()[[1]])
		autoplot(time_series()[[1]], xlab = "Period", ylab=data_label(), main='Data Preview')
	}, )

	#### PRE ANALYSIS TAB ####

	output$analy_acf <- renderPlot({
		req(time_series()[[1]])
		ggAcf(time_series()[[1]], main='ACF')
	})

	output$analy_pacf <- renderPlot({
		req(time_series()[[1]])
		ggPacf(time_series()[[1]], main='PACF')
	})

	output$analy_stl <- renderPlot({
		req(time_series()[[1]])
		autoplot(stl(time_series()[[1]],s.window='periodic'))
	})

	#### MODEL TAB ####
	## ETS ##

	ets_model <- reactive({
		req(time_series()[[1]])
		return(ets(time_series()[[1]]))
	})

	output$model_ets_acf <- renderPlot({
		ggAcf(ets_model()$residuals, main = 'Residuals ACF')
	})

	output$model_ets_qqplot <- renderPlot({
		ggplot(data.frame(res = ets_model()$residuals), aes(sample = res)) + stat_qq_band() +
			stat_qq_point(shape = 16) + stat_qq_line(size=0) +
			ggtitle('Normal Q-Q Plot') + xlab('Theorical Quantiles') + ylab('Sample Quantiles')

	})

	output$model_ets_summary <- renderUI({
		info = summary(ets_model())
		residuals = ets_model()$residuals
		res = list(
			model = paste(ets_model()$components[-4], collapse =''),
			MAE   = info[,'MAE'],
			MAPE  = info[,'MAPE'],
			RMSE  = info[,'RMSE'],
			shap = shapiro.test(residuals)$p.value,
			box = Box.test(residuals, min(length(residuals), 24))$p.value
		)

		return(withTags(
			table(class='results',
				tr(td('Model'), td(res$model)),
				tr(td('MAE'), td( sprintf(res$MAE, fmt='%#.2f') )),
				tr(td('MAPE'), td( sprintf(res$MAPE, fmt='%#.3f%%') )),
				tr(td('RMSE'), td( sprintf(res$RMSE, fmt='%#.2f') )),
				tr(td('Shapiro-Wilk Normality Test p-value'), td( sprintf(res$shap, fmt='%#.4f') )),
				tr(td('Box-Pierce Test p-value'), td( sprintf(res$box, fmt='%#.4f') ))
			)
		))

	})

	## ARIMA ##

	arima_model <- reactive({
		req(time_series()[[1]])
		return(auto.arima(time_series()[[1]]))
	})

	output$model_arima_acf <- renderPlot({
		ggAcf(arima_model()$residuals, main = 'Residuals ACF')
	})

	output$model_arima_qqplot <- renderPlot({
		ggplot(data.frame(res = arima_model()$residuals), aes(sample = res)) + stat_qq_band() +
			stat_qq_point(shape = 16) + stat_qq_line(size=0) +
			ggtitle('Normal Q-Q Plot') + xlab('Theorical Quantiles') + ylab('Sample Quantiles')

	})

	output$model_arima_summary <- renderUI({
		info = summary(arima_model())
		residuals = arima_model()$residuals
		res = list(
			model = with(as.list(arimaorder(arima_model())), {
				HTML(paste0('(', p, ',', q, ',', d, ')×(', P, ',', Q, ',', D, ')<sub>', Frequency, '</sub>'))
				}),
			MAE   = info[,'MAE'],
			MAPE  = info[,'MAPE'],
			RMSE  = info[,'RMSE'],
			shap = shapiro.test(residuals)$p.value,
			box = Box.test(residuals, min(length(residuals), 24))$p.value
		)

		return(withTags(
			table(class='results',
				  tr(td('Model'), td(res$model)),
				  tr(td('MAE'), td( sprintf(res$MAE, fmt='%#.2f') )),
				  tr(td('MAPE'), td( sprintf(res$MAPE, fmt='%#.3f%%') )),
				  tr(td('RMSE'), td( sprintf(res$RMSE, fmt='%#.2f') )),
				  tr(td('Shapiro-Wilk Normality Test p-value'), td( sprintf(res$shap, fmt='%#.4f') )),
				  tr(td('Box-Pierce Test p-value'), td( sprintf(res$box, fmt='%#.4f') ))
			)
		))
	})


	#### FORECAST TAB ####

	periods <- reactive({
		if(input$data_validation_cb)
			return(input$data_validation)
		else
			return(3)
	})

	## ETS

	ets_forecast <- reactive({
		return(forecast(ets_model(), h=periods()))
	})

	output$fore_ets <- renderPlot({
		autoplot(ets_forecast(), xlab="Period", ylab=data_label()) +
			autolayer(fitted(ets_forecast()), color='blue')
	})

	output$fore_ets_val <- renderPlot({
		autoplot(ets_forecast()$mean, ylab=data_label()) +
			autolayer(ets_forecast()) +
			autolayer(time_series()[[2]], series='Validation data')
	})

	output$fore_ets_summary <- renderUI({

	})

	output$fore_ets_table <- renderTable({
		ets = ets_forecast()
		x=t(data.frame(u95 = ets$upper[,2],
					   u80 = ets$upper[,1],
					   	 m = ets$mean,
					   l80 = ets$lower[,1],
					   l95 = ets$lower[,2]
		))
		rownames(x) = c('Upper 95%', 'Upper 80%', 'Mean', 'Lower 80%', 'Lower 95%')
		t = as.numeric(time(ets$mean))
		y = floor(t)
		m = floor(12*(t%%1))
		dates = as.Date(paste(y, m+1, 1, sep = '-'))
		colnames(x) = tools::toTitleCase(format(dates, '%b %Y'))
		return(x)
	}, rownames = T)

	# output$fore_ets_summary <- renderUI({
	# 	ets = ets_forecast()
	# 	x=data.frame(u95 = ets$upper[,2],
	# 				   u80 = ets$upper[,1],
	# 				   m = ets$mean,
	# 				   l80 = ets$lower[,1],
	# 				   l95 = ets$lower[,2]
	# 	)
	# 	#rownames(x) = c('Upper 95%', 'Upper 80%', 'Mean', 'Lower 80%', 'Lower 95%')
	# 	t = as.numeric(time(ets$mean))
	# 	y = floor(t)
	# 	m = floor(12*(t%%1))
	# 	dates = as.Date(paste(y, m+1, 1, sep = '-'))
	# 	#colnames(x) = tools::toTitleCase(format(dates, '%b %Y'))
	#
	# 	return(withTags(
	# 		table(id='forecast',
	# 			thead(tr(
	# 					th(),
	# 					th(colspan=length(dates), data_label())
	# 				),tr(
	# 					th(),
	# 					lapply(dates, function(date) th(format(date, '%b %Y')))
	# 				)
	# 			),
	# 			tbody(
	# 				tr(
	# 					th('Upper 95%'),
	# 					lapply(1:nrow(x), function(i) td(x[i, 1]))
	# 				),
	# 				tr(
	# 					th('Upper 80%'),
	# 					lapply(1:nrow(x), function(i) td(x[i, 2]))
	# 				),
	# 				tr(
	# 					th('Mean'),
	# 					lapply(1:nrow(x), function(i) td(x[i, 3]))
	# 				),
	# 				tr(
	# 					th('Lower 80%'),
	# 					lapply(1:nrow(x), function(i) td(x[i, 4]))
	# 				),
	# 				tr(
	# 					th('Lower 95%'),
	# 					lapply(1:nrow(x), function(i) td(x[i, 5]))
	# 				)
	# 			)
	# 		)
	# 	))
	# })

	## ARIMA

	arima_forecast <- reactive({
		return(forecast(arima_model(), h=periods()))
	})

	output$fore_arima <- renderPlot({
		autoplot(arima_forecast(), xlab="Period", ylab=data_label()) +
			autolayer(fitted(arima_forecast()), color='blue')
	})

	output$fore_arima_val <- renderPlot({
		autoplot(arima_forecast()$mean, ylab=data_label()) +
			autolayer(arima_forecast()) +
			autolayer(time_series()[[2]], series='Validation data')
	})

	output$fore_arima_table <- renderTable({
		arima = arima_forecast()
		x=t(data.frame(u95 = arima$upper[,2],
					   u80 = arima$upper[,1],
					   	 m = arima$mean,
					   l80 = arima$lower[,1],
					   l95 = arima$lower[,2]
		))
		rownames(x) = c('Upper 95%', 'Upper 80%', 'Mean', 'Lower 80%', 'Lower 95%')
		t = as.numeric(time(arima$mean))
		y = floor(t)
		m = floor(12*(t%%1))
		dates = as.Date(paste(y, m+1, 1, sep = '-'))
		colnames(x) = tools::toTitleCase(format(dates, '%b %Y'))
		return(x)

	}, rownames = T)

})
