suppressWarnings(library(tm))
suppressWarnings(library(stringr))
suppressWarnings(library(shiny))

# load n-grams

unigram <- readRDS("unigram.RData");
bigram <- readRDS("bigram.RData");
trigram <- readRDS("trigram.RData");
quadgram <- readRDS("quadgram.RData");
msg <<- ""

# Cleaning of user input before predicting the next word

predict <- function(x.word) {
    x.clean <- removeNumbers(removePunctuation(tolower(x.word)))
    x.string <- strsplit(x.clean, " ")[[1]]
    
    # Logic model for word prediction
    # 1. Quadgram is first used (first three words of quadgram are matched to
    # last three words of user input)
    # 2. If no quadgram is found, trigram is used next i.e. match first two words
    # to last two words of user input
    # 3. If no trigram is found, bigram used next in the same principle
    # 4. If no bigram is found, unigram is referenced and highest frequency term
    # is returned
    
    if (length(x.string) >= 3) {
        x.string <- tail(x.string,3)
        if (identical(character(0),
                      head(quadgram[quadgram$unigram == x.string[1] & 
                                    quadgram$bigram == x.string[2] & 
                                    quadgram$trigram == x.string[3], 4],1))){
            
            predict(paste(x.string[2], x.string[3], sep=" "))
        
        }
        
        else {msg <<- "Next word is predicted using 4-gram.";
        head(quadgram[quadgram$unigram == x.string[1] & 
                          quadgram$bigram == x.string[2] & 
                          quadgram$trigram == x.string[3], 4],1)}
    
    }
    
    else if (length(x.string) == 2){
        x.string <- tail(x.string,2)
        if (identical(character(0),
                      head(trigram[trigram$unigram == x.string[1] & 
                                   trigram$bigram == x.string[2], 3],1))){
            
            predict(x.string[2])
        
        }
        
        else {msg <<- "Next word is predicted using 3-gram."; 
        head(trigram[trigram$unigram == x.string[1] &
                         trigram$bigram == x.string[2], 3],1)}
    
    }
    
    else if (length(x.string) == 1){
        x.string <- tail(x.string,1)
        if (identical(character(0),
                      head(bigram[bigram$unigram == x.string[1], 2],1)))
        {msg <<- "No match found. Most common single term will be returned.";
            head(unigram[sample(nrow(unigram),1),1])}
        else {msg <<- "Next word is predicted using 2-gram."; 
        head(bigram[bigram$unigram == x.string[1],2],1)}
    }
}

shinyServer(function(input, output) {
    output$prediction <- renderPrint({
        result <- predict(input$inputString)
        output$text2 <- renderText({msg})
        result
    });
    
    output$text1 <- renderText({
        input$inputString});
    
}
)
