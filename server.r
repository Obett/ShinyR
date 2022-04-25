#################################################
#               Basic Text Analysis             #
#################################################

shinyServer(function(input, output,session) {
  set.seed=2092014   
  
  dataset <- reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      
      if(file_ext(input$file$datapath)=="txt"){
        Document = readLines(input$file$datapath)
        #colnames(Document) <- c("Doc.id","Document")
        Doc.id=seq(1:length(Document))
        calib=data.frame(Doc.id,Document)
        print(input$file$name)
        return(calib)}
    else{
      Document = read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F)
      Document[,1] <- str_to_title(Document[,1])
      Document[,1] <- make.names(Document[,1], unique=TRUE)
      Document[,1] <- tolower(Document[,1])
      Document[,1] <- str_replace_all(Document[,1],"\\.","_")
      Document<-Document[complete.cases(Document), ]
      Document <- Document[!(duplicated(Document[,1])), ]
      rownames(Document) <- Document[,1]
      
     # colnames(Document) <- c("Doc.id","Document")
      #Doc.id=seq(1:length(Document))
      # calib=data.frame(Doc.id,Document)
      #print(input$file$name)
      
      return(Document)
      }
      
    }
  })
  
  cols <- reactive({colnames(dataset())})
  
  output$pre_proc1 <- renderUI({if(is.null(dataset())){
    return(NULL)
  }else{
    
    checkboxInput('html',"Remove HTML tags",value = TRUE)
                        
      }
  })
  
  output$pre_proc2 <- renderUI({if(is.null(dataset())){
    return(NULL)
  }else{
    checkboxInput('num',"Remove Numbers",value = TRUE)
    
  }
  })
  
  
  y_col <- reactive({
    text_data <- match(input$text_data,cols())
    y_col <- cols()[-text_data]
    return(y_col)
    
    })
  
  output$id_var <- renderUI({
    print(cols())
    selectInput("text_data","Select ID Column",choices = cols())
  })
  
  
  output$doc_var <- renderUI({
    selectInput("y","Select Text Column",choices = y_col())
  })
  
 
  
  
  doc_term_matx_tcm =  eventReactive(input$apply,{
    
    textb = dataset()[,input$y]
    ids = dataset()[,input$text_data]
    
    doc_term_matx.tcm = doc_term_matx.tcm.creator(text = textb,
                              id = ids,
                              std.clean = TRUE,
                              std.stop.words = TRUE,
                              stop.words.additional = unlist(strsplit(input$stopw,",")),
                              bigram.encoding = TRUE,
                              # bigram.min.freq = 20,
                              min.doc_term_matx.freq = input$freq,
                              skip.grams.window = 10,
                              html_tags=input$html,
                              numbers = input$num)
    #if (input$ws == "weightTf") {
      doc_term_matx = as.matrix(doc_term_matx.tcm$doc_term_matx)  
      doc_term_matx
      doc_term_matx_tcm_obj = list(doc_term_matx = doc_term_matx)#, tcm = tcm)
      doc_term_matx_tcm_obj
  #  } 
    
    # if (input$ws == "weightTfIdf"){
    #   model_tfidf = TfIdf$new()
    #   doc_term_matx = round(as.matrix(model_tfidf$fit_transform(doc_term_matx.tcm$doc_term_matx)),2)
    #   
    #   tempd = doc_term_matx*0
    #   tempd[doc_term_matx > 0] = 1
    #   doc_term_matx = doc_term_matx + tempd
    # }  
    # 
    # # tcm = doc_term_matx.tcm$tcm
    # doc_term_matx_tcm_obj = list(doc_term_matx = doc_term_matx)#, tcm = tcm)
  })
  

  
  
  doc_term_matx_idf =  eventReactive(input$apply,{
    
    textb = dataset()[,input$y]
    ids = dataset()[,input$text_data]
    
    doc_term_matx.tcm = doc_term_matx.tcm.creator(text = textb,
                              id = ids,
                              std.clean = TRUE,
                              std.stop.words = TRUE,
                              stop.words.additional = unlist(strsplit(input$stopw,",")),
                              bigram.encoding = TRUE,
                              # bigram.min.freq = 20,
                              min.doc_term_matx.freq = input$freq,
                              skip.grams.window = 10)
    # if (input$ws == "weightTf") {
    #   doc_term_matx = as.matrix(doc_term_matx.tcm$doc_term_matx)  
    # } 
    
   # if (input$ws == "weightTfIdf"){
      model_tfidf = TfIdf$new()
      doc_term_matx = round(as.matrix(model_tfidf$fit_transform(doc_term_matx.tcm$doc_term_matx)),2)
      
      tempd = doc_term_matx*0
      tempd[doc_term_matx > 0] = 1
      doc_term_matx = doc_term_matx + tempd
   # }  
    
    # tcm = doc_term_matx.tcm$tcm
    doc_term_matx_tcm_obj = list(doc_term_matx = doc_term_matx)#, tcm = tcm)
    doc_term_matx_tcm_obj
  })
  
  
  
  
  ordered_doc_term_matx_idf<- reactive({if (is.null(input$file)) {return(NULL)}
    else{
      matrix1= doc_term_matx_idf()$doc_term_matx
      a = colSums(matrix1)
      x = order(-a)     # nice syntax for ordering vector in decr order  
      matrix2 = matrix1[,x]
      return(matrix2)
      
    }
    
  })
  
  
  
  ordered_doc_term_matx <- reactive({if (is.null(input$file)) {return(NULL)}
    else{
      matrix1= doc_term_matx_tcm()$doc_term_matx
      a = colSums(matrix1)
      x = order(-a)     # nice syntax for ordering vector in decr order  
      matrix2 = matrix1[,x]
      return(matrix2)
      
    }
    
  })
  
  
  output$idf_table <- renderDataTable({
    
    temp <- ordered_doc_term_matx_idf()[1:10,1:10]
    #  temp <- tem[1:10,1:10]
    return(temp)
    
    # a = colSums(matrix1)  # collect colsums into a vector obj a
    
    # 
    # diag(matrix2) =  0
    
  })
  
  
  
  output$doc_term_matx_table <- renderDataTable({
    
    temp <- ordered_doc_term_matx()[1:10,1:10]
    #  temp <- tem[1:10,1:10]
    return(temp)
    
    # a = colSums(matrix1)  # collect colsums into a vector obj a
    
    # 
    # diag(matrix2) =  0
    
  })
  
  
  
  idfwordcounts = reactive({
    
    return(doc_term_matx.word.count(doc_term_matx_idf()$doc_term_matx))
    
  }) 
  
  
  wordcounts = reactive({
    
    return(doc_term_matx.word.count(doc_term_matx_tcm()$doc_term_matx))
    
  }) 
  
  
  
  output$idf_wordcloud <- renderPlot({
    
    if (is.null(input$file)) {return(NULL)}
    else{
      tsum = idfwordcounts()
      tsum = tsum[order(tsum, decreasing = T)]
      doc_term_matx.word.cloud(count = tsum,max.words = input$max,title = 'TF-IDF Wordcloud')
    }
    
    
  })
  
  
  
  output$wordcloud <- renderPlot({
    
    if (is.null(input$file)) {return(NULL)}
    else{
      tsum = wordcounts()
      tsum = tsum[order(tsum, decreasing = T)]
      doc_term_matx.word.cloud(count = tsum,max.words = input$max,title = 'Term Frequency Wordcloud')
    }
    
    
  })
  
  
  
  
  output$cog.idf <- renderVisNetwork({
    
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else{
      distill.cog.tcm(matrix1=doc_term_matx_idf()$doc_term_matx, # input TCM MAT
                      mattype = "Doc_Term_Tatx",
                      title = "COG from TF-IDF Adjacency", # title for the net_graph
                      s=input$nodes,    # no. of central nodes
                      k1 = input$connection)  # No. of Connection with central Nodes
    }
    
    
  })
  
  
  
  output$cog.doc_term_matx <- renderVisNetwork({
    
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else{
      distill.cog.tcm(matrix1=doc_term_matx_tcm()$doc_term_matx, # input TCM MAT
                      mattype = "Doc_Term_Tatx",
                      title = "COG from Doc_Term_Tatx Adjacency", # title for the net_graph
                      s=input$nodes,    # no. of central nodes
                      k1 = input$connection)  # No. of Connection with central Nodes
    }
    
    
  })
  
  # output$cog.tcm <- renderPlot({
  #   
  # distill.cog.tcm(matrix1=doc_term_matx_tcm()$tcm, # input TCM MAT,
  #                   mattype = "TCM",
  #                   title = "TCM from glove algorithm - Graph ", # title for the net_graph
  #                   s=input$nodes,    # no. of central nodes
  #                   k1 = input$connection)  # No. of Connection with central Nodes
  #   
  #       })
  
  
  # output$doc_term_matxsummary  <- renderPrint({
  #   if (is.null(input$file)) {return(NULL)}
  #   else {
  #     sortedobj = doc_term_matx_tcm()$doc_term_matx[,order(wordcounts(), decreasing = T)]
  #     (t(sortedobj[1:10,1:10]))
  #   }
  # })
  
  output$idf_size  <- renderPrint({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else {
      size = dim(t(doc_term_matx_idf()$doc_term_matx))
      doc_term_matx_size = paste("TF-IDF matrix size is ", size[1]," X ", size[2],". Below are the first 10 docs X top 10 tokens")
      return(doc_term_matx_size)
    }
  })
  
  
  output$doc_term_matxsize  <- renderPrint({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else {
      size = dim(t(doc_term_matx_tcm()$doc_term_matx))
      doc_term_matx_size = paste("Term Document Matrix (TDM) size is ", size[1]," X ", size[2],". Below are the first 10 docs X top 10 tokens")
      return(doc_term_matx_size)
    }
  })
  
  
  output$doc_term_matxsummary2  <- renderDataTable({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else {
      data.frame(score = idfwordcounts()[order(idfwordcounts(), decreasing = T)][1:input$max])
    }
  })
  
  
  
  output$doc_term_matxsummary1  <- renderDataTable({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else {
      data.frame(Counts = wordcounts()[order(wordcounts(), decreasing = T)][1:input$max])
    }
  })
  
  
  # This is your reactive element.
  df_reactive <- eventReactive(input$apply,{
    
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else{
      a0 = concordance.r(dataset()[,input$y],input$concord.word, input$window,input$regx)
      a0
      # a0 %>%
      #   # Filter if input is anywhere, even in other words.
      #   filter_all(any_vars(grepl(input$concord.word, ., T, T))) %>% 
      #   # Replace complete words with same in HTML.
      #   mutate_all(~ gsub(
      #     paste(c("\\x(", input$concord.word, ")\\x"), collapse = ""),
      #     "<span style='background-color:#6ECFEA;'>\\1</span>",
      #     .,
      #     TRUE,
      #     TRUE
      #   )
      #   )
    }
    
  })
  
  
  output$concordance = renderDataTable({
    
    # a0 = concordance.r(dataset()$Document,input$concord.word, input$window)
    # concordance = a0
    # datatable(concordance, escape = F, options = list(dom = "lt"))
    datatable(df_reactive(), escape = F, options = list(dom = "lt"))
  })
  
  
  bi_gram <- reactive({
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else{
      a1 = dataset()[,input$y] %>% split_by_punctuations(punctuations,.) #N-------------
      a2 = tibble(phrases = unlist(a1));
      a0 = bigram.collocation(a2)
      
    #  a0 = bigram.collocation(dataset()$Document)
      a0$coll.ratio <- round(a0$coll.ratio,2)
      a0 = a0[order(a0$n, decreasing = T),]
      if (nrow(a0) > 100){
        a1 = a0[1:100,]
      } else {
        a1 = a0
      }
      a1
    }
    
    
  })
  
  
  output$bi.grams = renderDataTable({
    bi_gram()
    
  })
  
  output$bi_word_cloud <- renderPlot({
    
    if (is.null(input$file)|input$apply==0) {return(NULL)}
    else{
      wordcloud(bi_gram()$bigram_united, bi_gram()$n,     # words, their freqs 
                scale = c(4, 1),     # range of word sizes
                min.freq = .01,                     # min.freq of words to consider
                max.words = input$max,       # max #words
                colors = brewer.pal(8, "Dark2"))
    }
    
  })
  
  
  output$doc_term_matx_text <- renderText({
    size = dim(ordered_doc_term_matx())
    doc_term_matx_size = paste("Doc_Term_Tatx has  ", size[1],"(rows)"," X ", size[2],"(columns)","")
    
    
  })
  
  
  
  output$tfidf_text <- renderText({
    size = dim(ordered_doc_term_matx_idf())
    doc_term_matx_size = paste("TF-IDF has  ", size[1],"(rows)"," X ", size[2],"(columns)","")
    
    
  })
  
  
  output$bi_text <- renderText({
    size = dim(bigram_data())
    doc_term_matx_size = paste("Bi-gram corpus has  ", size[1],"(rows)"," X ", size[2],"(columns)","")
    
    
  })
  
  
  
  
  
  
  
  
  
  output$download_tfidf <- downloadHandler(
    filename = function() {paste(str_split(input$file$name,"\\.")[[1]][1],"_tfidf.csv",collapse = "") },
    content = function(file) {
      
      new_doc_term_matx <- ordered_doc_term_matx_idf()
      write.csv(new_doc_term_matx, file, row.names=T)
      
      
    }
  )
 
  output$download_doc_term_matx <- downloadHandler(
    filename = function() {paste(str_split(input$file$name,"\\.")[[1]][1],"_doc_term_matx.csv",collapse = "") },
    content = function(file) {
      
      new_doc_term_matx <- ordered_doc_term_matx()
      write.csv(new_doc_term_matx, file, row.names=T)
      
      
    }
  )
  
  
  bigram_data <- reactive({
                bigrammed_corpus = replace_bigram(dataset(),
                                                  min_freq = 2,
                                                  stopw_list=unlist(strsplit(input$stopw,","))
                                                  )
                return(bigrammed_corpus[,2:3])
                })
  
  
  
  
  
  
  
  
  output$download_bigram <- downloadHandler(
    filename = function() { paste(str_split(input$file$name,"\\.")[[1]][1],"_bigram_corpus.csv",collapse = " ") },
    content = function(file) {
      write.csv(bigram_data(), file,row.names=FALSE)
    }
  )
  
  
  
  
  output$downloadData1 <- downloadHandler(
    filename = function() { "20_Hotel_Reviews.txt" },
    content = function(file) {
      writeLines(readLines("data/20_Hotel_Reviews.txt"), file)
    }
  )
  
})