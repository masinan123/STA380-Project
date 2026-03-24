local({
  
  data("wpp", package = "Sta380Project", envir = environment())
  
  observeEvent(input$alpha, {
    req(input$alpha) 
    if (input$alpha < 0.001 || input$alpha > 0.20) {
      showNotification(
        "Error: Significance level must be between 0.001 and 0.20!",
        type = "error",      
        duration = NULL,     
        id = "alpha_error"   
      )
    } else {
      removeNotification("alpha_error") 
    }
  })
  
  observeEvent(input$B, {
    req(input$B)
    if (input$B < 100) {
      showNotification(
        "Error: Number of permutations must be at least 100!",
        type = "error",
        duration = NULL,
        id = "B_error"
      )
    } else {
      removeNotification("B_error")
    }
  })
  
  indicator_label <- reactive({
    switch(input$indicator,
           "birth_rate" = "Birth rate",
           "life_exp" = "Life expectancy",
           "mortality_rate" = "Mortality rate",
           input$indicator)
  })
  
  cleaned_df <- reactive({
    req(input$indicator)
    
    perm_data_extract(
      df = wpp,
      outcome_col = !!rlang::sym(input$indicator),
      group_col = dev_group
    )
  })
  
  perm_res <- reactive({
    req(cleaned_df())
    
    validate(
      need(input$B >= 100, "Please enter at least 100 permutations.")
    )
    
    validate(
      need(nrow(cleaned_df()) > 1, "Not enough data after filtering."),
      need(length(unique(cleaned_df()$group)) == 2, "Exactly two groups are required.")
    )
    
    perm_test_two_group_ks(
      df = cleaned_df(),
      B = input$B,
      seed = input$seed,
      # alternative = input$alternative
      alternative = "two.sided"
    )
  })
  
  group_counts <- reactive({
    df <- cleaned_df()
    table(df$group)
  })
  
  output$obs_text <- renderText({
    round(perm_res()$observed, 4)
  })
  
  output$pval_text <- renderText({
    round(perm_res()$p_value, 4)
  })
  
  output$decision_text <- renderText({
    validate(
      need(input$alpha >= 0.001 && input$alpha <= 0.20, "Error: Alpha value must be between 0.001 and 0.20")
    )
    
    if (perm_res()$p_value < input$alpha) {
      "Reject H0"
    } else {
      "Fail to reject H0"
    }
  })
  
  output$n_text <- renderText({
    cnt <- group_counts()
    paste(
      names(cnt)[1], "=", as.integer(cnt[1]), "|",
      names(cnt)[2], "=", as.integer(cnt[2])
    )
  })
  
  output$perm_plot <- renderPlot({
    res <- perm_res()
    plot_perm_dist(res, bins = input$bins)
  })
  
  output$ecdf_plot <- renderPlot({
    df <- cleaned_df()
    plot_two_ECDFs(df$outcome, df$group)
  })
  
  output$data_preview <- renderTable({
    head(cleaned_df(), 12)
  })
  
  output$summary_text <- renderPrint({
    df <- cleaned_df()
    cat("Indicator:", indicator_label(), "\n")
    cat("Number of observations:", nrow(df), "\n\n")
    print(summary(df$outcome))
  })
  
  output$hypothesis_text <- renderText({
    validate(
      need(input$alpha >= 0.001 && input$alpha <= 0.20, "Error: Alpha must be between 0.001 and 0.20")
    )
    
    paste0(
      "Null hypothesis: the ", indicator_label(),
      " distribution is the same for More developed and Less developed countries.\n",
      "Alternative hypothesis: the distributions differ according to the selected alternative = ",
      input$alternative, ".\n\n",
      "Observed KS statistic = ", round(perm_res()$observed, 4),
      "\nPermutation p-value = ", round(perm_res()$p_value, 4),
      "\nAlpha = ", input$alpha
    )
  })
  
})

# AI usage:
# Generative AI (ChatGPT) was used to assist with:
# - structuring reactive expressions for data preparation and permutation testing
# - help with connecting existing package functions to Shiny outputs (renderPlot, renderText, renderTable)
# - troubleshooting file path and data loading issues within the Shiny server environment
#
# All statistical methodology, including the permutation test implementation
# and interpretation of the results, was designed and verified by the author.
