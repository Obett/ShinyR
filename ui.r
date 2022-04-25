#################################################
#               Basic Text Analysis             #
#################################################

library(shiny)
library(txtdata2vec)
library(tm)
library(tokenizers)
library(wordcloud)
library(slam)
library(stringi)
library(magrittr)
library(tidytext)
library(dplyr)
library(visNetwork)
library(tidyr)
library(DT)
library(stringr)
library(tools)

shinyUI(fluidPage(
  title = "Sentiment Analysis of 20 Hotel Reviews",
  titlePanel(title=div(img(src="UniLogo.PNG",align='right'),"Sentiment Analysis of 20 Hotel Reviews")),
  
  # Input in sidepanel:
  sidebarPanel(
    
    fileInput("file", "Upload text file"),
    uiOutput('id_var'),
    uiOutput("doc_var"),
    textInput("stopw", ("Enter stop words separated by comma(,)"), value = "is,above"),
    
    # selectInput("ws", "Weighing Scheme", 
    #             c("weightTf","weightTfIdf"), selected = "weightTf"), # weightTf, weightTfIdf, weightBin, and weightSMART.
    #
    htmlOutput("pre_proc1"),
    htmlOutput("pre_proc2"),
    sliderInput("freq", "Minimum Frequency in Wordcloud:", min = 0,  max = 100, value = 2),
    
    sliderInput("max",  "Maximum Number of Words in Wordcloud:", min = 1,  max = 300,  value = 50),  
    
    numericInput("nodes", "Number of Central Nodes in co-occurrence net_graph", 4),
    numericInput("connection", "Number of Max Connection with Central Node", 5),
    
    
    textInput("concord.word",('Enter word for which you want to find concordance'),value = 'good'),
    checkboxInput("regx","Check for regex match"),
    sliderInput("window",'Concordance Window',min = 2,max = 100,5),
    
    
    actionButton(inputId = "apply",label = "Commit Changes", icon("refresh"))
    
  ),
  
  # Main Panel:
  mainPanel( 
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview of 20 Hotel Reviews Dataset",h4(p("Data preparation steps below")),
                         
                         p("Manually, I filtered 20 hotels in the excel sheet and extracted data of only Review column into a textpad and saved it as 20_Hotel_Reviews because I built this app in a way that it accepts only a document in txt file format. Again, I ensured that each row of the reviews was separated from one another with a new line of character. Also, I have already attached my prepared dataset to be used below. Kind download it under Download text file and then, 
                           on Browse in left side bar panel and upload the downloaded txt file which has been prepared. immediately the file has been uploaded, it will carry out the data processing in 
                            backend with default inputs and show the results in different tabs.", align = "justify"),
                         p("If you wish to modify the input, edit the input in left side bar panel and click on Commit Changes. The outputs in other tabs will be reprocessed.
                           ", align = "Justify"),
                         h5("Note"),
                         p("The changes in output takes effect few seconds after clicking on 'Commit Changes'. This is to allow all the data processing
                          in backend to complete before displaying the result",
                           align = "justify"),
                         #, height = 280, width = 400
                         br(),
                         h4(p("Download My Dataset")),
                         downloadButton('downloadData1', '20_Hotel_Reviews reviews txt file'),br(),br(),
                         p("Kindly note that download is not supported with RStudio interface.  Thus use a web-browser to open this App and then download the 20 Hotel Reviews file. Please click on Open in Browser as shown at top left of the browser to open this App."),
                         img(src = "UniLogo.PNG")
                )
                ,
              
                tabPanel("Doc_Term_Tatx",
                         verbatimTextOutput("doc_term_matxsize"),
                         h4("Sample Doc_Term_Tatx (Document Term Matrix) "),
                         DT::dataTableOutput("doc_term_matx_table"),br(), 
                         h4("Word Cloud"),
                         plotOutput("wordcloud",height = 700, width = 700),br(),
                         #textInput("in",label = "text"),
                         h4("Weights Distribution of Wordcloud"),
                         DT::dataTableOutput("doc_term_matxsummary1")),
                
                
                tabPanel("TF-IDF", 
                         verbatimTextOutput("idf_size"),
                         h4("Sample TF-IDF (Term Frequency-Inverse Document Frequency) "),
                         DT::dataTableOutput("idf_table"),br(), 
                         h4("Word Cloud"),
                         plotOutput("idf_wordcloud",height = 700, width = 700),br(),
                         #textInput("in",label = "text"),
                         h4("Weights Distribution of Wordcloud"),
                         DT::dataTableOutput("doc_term_matxsummary2")),
                tabPanel("Term Co-occurrence",
                         h4("Doc_Term_Tatx Co-occurrence"),
                         visNetworkOutput("cog.doc_term_matx",height = 700, width = 700),
                         h4("TF-IDF Co-occurrence"),
                         visNetworkOutput("cog.idf",height = 700, width = 700)
                ),
                tabPanel("Bigram",
                         h4('Collocations Bigrams'),
                         p('If a corpus has n word tokens, then it can have at most (n-1) bigrams. However, most of
                                    these bigram are uninteresting. The interesting ones - termed collocations bigrams - comprise
                                    those bigrams whose occurrence in the corpus is way more likely than would be true if the 
                                    constituent words in the bigram randomly came together. Below is the list of all collocations 
                                    bigrams (top 100, if collocations bigrams are above 100) from the corpus you uploaded on 
                                    this App',align = "Justify"),
                         DT::dataTableOutput("bi.grams"),
                         h4("Bigram wordcloud"),
                         plotOutput("bi_word_cloud",height=700,width=700),
                         
                ),
                tabPanel("Concordance",
                         h4('Concordance'),
                         p('Concordance allows you to see the local context around a word of interest. It does so by building a moving window of words before and after the focal word\'s every instance in the corpus. Below is the list of all instances of concordance in the corpus for your word of interest entered in the left side bar panel of this app. You can change the concordance window or word of interest in the left side bar panel.',align = "Justify"),
                         #verbatimTextOutput("concordance"))
                         DT::dataTableOutput("concordance")),
                tabPanel("Downloads",
                         h4("Download Doc_Term_Tatx"),
                         #h3("-------------"),
                         verbatimTextOutput("doc_term_matx_text"),
                         downloadButton('download_doc_term_matx', 'Download Doc_Term_Tatx'),br(),
                        
                         
                         h3("-----------------------------------------------------"),
                         h4("Download TF-IDF"),
                         verbatimTextOutput("tfidf_text"),
                         downloadButton('download_tfidf', 'Download TF-IDF'),br(),
                         
                         
                         h3("-----------------------------------------------------"),
                         h4("Download Bigram Corpus"),
                         verbatimTextOutput("bi_text"),
                         downloadButton("download_bigram","Download Bigram Corpus"))
                          
          
                
                
    )
  )
)
)