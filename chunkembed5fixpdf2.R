api_key <- Sys.getenv("OPENAI_API_KEY")
library(httr)
library(jsonlite)
library(pdftools)
library(stringr)
library(openai)  # Make sure you install this package
load("C:/Users/Public/OneDrive - UGent/CROPDIVA_SL/App3/test.RData")

# OpenAI API setup (Make sure you have your API key)
api_key <- Sys.getenv("OPENAI_API_KEY")
openai_url <- "https://api.openai.com/v1/embeddings"
pdf_directory <- "C:/Users/Public/OneDrive - UGent/CROPDIVA_SL/App3/pdf_update16012026/"
#pdf_directory <- "C:/Users/Public/OneDrive - UGent/CROPDIVA_SL/App3/pdfs2/"

# List of PDF files to be processed
pdf_files <- paste(pdf_directory, list.files(pdf_directory), sep="")

# Function to read PDFs and extract text
extract_text_from_pdf <- function(pdf_file) {
  text <- pdf_text(pdf_file)
  return(paste(text, collapse = "\n"))  # Combine all pages into one string
}

# Function to split text into chunks (e.g., 7000 tokens per chunk)
chunk_text <- function(text, chunk_size = 4200) {
  words <- unlist(strsplit(text, " "))
  l=which(words=="")
  words=words[-l]
  chunks <- split(words, ceiling(seq_along(words) / chunk_size))
  chunked_text <- sapply(chunks, function(chunk) paste(chunk, collapse = " "))
  return(chunked_text)
}

# Function to get embeddings for chunks using OpenAI's embedding model
get_embeddings <- function(text_chunks) {
  embeddings <- list()
    for (chunk in text_chunks) {
    response <- openai::create_embedding(
      model = "text-embedding-ada-002",
      input = chunk,
      openai_api_key =api_key
    )
        embeddings <- c(embeddings, list(response$data$embedding))
  }
  
  return(embeddings)
}
# Process multiple PDFs
all_text_chunks2 <- list()
all_embeddings2 <- list()

for (pdf_file in pdf_files) {
  pdf_text <- extract_text_from_pdf(pdf_file)
  chunked_text <- chunk_text(pdf_text)
  embeddings <- get_embeddings(chunked_text)
  
  all_text_chunks2 <- c(all_text_chunks2, chunked_text)
  all_embeddings2 <- c(all_embeddings2, embeddings)
}

all_text_chunks=c(all_text_chunks,all_text_chunks2)
all_embeddings=c(all_embeddings,all_embeddings2)
save(all_text_chunks, all_embeddings, file  ="C:/Users/Public/OneDrive - UGent/CROPDIVA_SL/App3/test2.RData")


