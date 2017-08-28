#' Locate and extract taxonomic names from given input files
#'
#' \code{find_taxoname} locates and extracts taxonomic names from txt, docx, or pdf
#' files and reorganize the taxonomy names into standard order: genus, species, subspecies,
#' author&year, distribution. The function can output the result to a txt file and each row
#' of the file is one entry of a taxonomic name. The result txt file of this function can be
#' further processed into a tabular format in csv which contains more detailed information
#' using function \code{parse_taxolist}.
#'
#' @import pdftools
#' @import qdapTools
#' @import stringr
#'
#' @param filepath Required. The path of the file which the data is to be read from.
#' If it does not contain an absolute path, the file name is relative to the current
#' working directory.
#' @param filename Required. The name of the file which the data is to be read from.
#' @param type Required. Currently accept 'txt', 'docx', and 'pdf' format files.
#' @param encoding Optional. The encoding method of the input file. Default value is
#' 'unknown'.
#' @param output_name Required. The path and name of the file for writing. If it does not
#' contain an absolute path, the file name is relative to the current working directory.
#'
#' @return A data frame containing the result of finding and reorganizing taxonomic names
#' in the input file into standard format.
#'
#' A TXT file written from the above data frame and each line of this file contains one
#' entry of taxonomic names.
#'
#' @examples  \dontrun{
#' df <- find_taxoname(filepath = "D:/code/Parser/Parser_for_Biodiversity_Checklists/Examples/input_data",
#'                     filename = "taxo01.txt",
#'                     type = "txt",
#'                     output_name = "D:/code/Parser/Parser_for_Biodiversity_Checklists/Examples/output_data/taxo01_output")
#'}
#'
#' @export
find_taxoname <- function(filepath, filename, type, encoding = "unknown", output_name = "FALSE"){
  content <- read_file(filepath, filename, type, encoding)
  wordlist <- build_words_list(content, type)
  wordlist <- remove_na_value(wordlist)
  wordlist <- c(wordlist, "9999", "stop", "stop", "stop")
  index_list <- locate_genus(wordlist)
  df <- build_df(wordlist, index_list)
  if (output_name != "FALSE"){
    output_name = paste(output_name, "txt", sep = ".")
    write.table(df, output_name, row.names = F)
  }
  return(df)
}

read_file <- function(filepath, filename, type, encoding = "unknown"){
  if (tolower(type) == "txt") {
    if (filepath != "") {
      file_path_name <- paste(filepath, "/", filename, sep="")
      file <- readLines(file_path_name, encoding=encoding)
    } else {
      file_path_name <- filename
      file <- readLines(file_path_name, encoding)
    }
  } else if (tolower(type) == "docx") {
    if (filepath != ""){
      file_path_name <- paste(filepath, "/", filename, sep="")
      file <- read_docx(file_path_name)
    } else {
      file_path_name <- filename
      file <- read_docx(file_path_name)
    }
  } else if (tolower(type) == 'pdf') {
    pdf_file <- file.path(filepath, filename)
    context <- pdf_text(pdf_file)
    file <- context
  } else if (tolower(type) == 'html') {
    d = debugGatherer()
    temp <- getURL(filename, debugfunction=d$update, verbose = TRUE, .encoding = 'UTF-8')
    temp <- str_replace_all(temp, "<[^>]*>","")
    temp <- str_replace_all(temp, "\\n","")
    file <- str_replace_all(temp, "&amp","")
  } else {
    stop("'type' should be one of 'txt', 'docx', 'pdf'.")
  }
  return(file)
}


# this function accepts the output from read_file function
build_words_list <- function(content, html = "F"){
  if (html == "html"){
    word_list <- ""
    word_list <- c(word_list,strsplit(content,"[^A-z0-9]"))
  } else {
    a <- length(content)
    word_list <- ""
    for (i in 1:a){
      each_row <- strsplit(content[i], "[^A-z0-9]")[[1]]
      word_list <- c(word_list, each_row)
    }
  }
  return(word_list[-1])
}

remove_na_value <- function(wordlist){
  new_word_list <- " "
  for (j in 1:length(wordlist)){
    if (wordlist[j] != ""){
      new_word_list <- c(new_word_list, wordlist[j])
    }
  }
  return(new_word_list[-1])
}

locate_genus <- function(wordlist){
  dic <- genus_dic
  dic$genus <- tolower(dic$genus)
  len <- length(wordlist)
  index_list <- ""
  for (i in 1:len){
    if (tolower(wordlist[i]) %in% dic$genus & !str_detect(wordlist[i+1],"^[A-Z]{1}")){
      #print(wordlist[i])
      index_list <- c(index_list,i)
    }
  }
  return(index_list[-1])
}



build_df <- function(wordlist,index_list){
  place_name <- all_place_name
  place_name$x <- tolower(iconv(place_name$x,"WINDOWS-1252","UTF-8"))

  df <- data.frame(entry = c(0))

  for (i in 1:length(index_list)){
    new_entry <- wordlist[as.integer(index_list[i])]
    j = 1
    while ( !str_detect(wordlist[as.integer(index_list[i])+j],"[0-9]{4}") & j < 10){
      new_entry <- paste(new_entry,wordlist[as.integer(index_list[i])+j],sep =" ")
      j = j + 1
    }

    while (str_detect(wordlist[as.integer(index_list[i])+j],"[0-9]{4}") |
           tolower(wordlist[as.integer(index_list[i])+j]) %in% place_name$x |
           (str_detect(wordlist[as.integer(index_list[i]) + j + 1],"[0-9]{4}")) |
           tolower(wordlist[as.integer(index_list[i])+j]) == "distribution" |
           wordlist[as.integer(index_list[i])+j] == "and" |
           str_detect(wordlist[as.integer(index_list[i])+j+1],"[0-9]{4}")|
           tolower(wordlist[as.integer(index_list[i])+j+1]) %in% place_name$x |
           (str_detect(wordlist[as.integer(index_list[i]) + j + 2],"[0-9]{4}"))|
           tolower(wordlist[as.integer(index_list[i]) + j + 1]) == "distribution" |
           wordlist[as.integer(index_list[i]) + j + 1] == "and")
    {
      new_entry <- paste(new_entry,wordlist[as.integer(index_list[i]) + j],sep = " ")
      j = j + 1
    }
    df <- rbind(df,new_entry)
  }
  df <- df[-1,]
  df <- str_replace_all(df, "9999", "")
  selected_list <- ""

  for (i in 1:length(df)){
    if (str_detect(df[i],"[1-2]{1}[0-9]{3}") & !str_detect(df[i],"[0-9]{5}")){
      if (length(str_extract_all(df[i],"[1-2]{1}[0-9]{3}")[[1]]) == length(str_extract_all(df[i],"[0-9]+")[[1]])){
        str_entry <- str_split(df[i],"\\s")
        if (! tolower(str_entry[[1]][1]) %in% place_name$x){
          n <- length(str_entry[[1]])
          final_entry <- str_entry[[1]][1]
          for (j in 2:n){
            if (! tolower(str_entry[[1]][j]) %in% place_name$x &
                str_entry[[1]][j] != "and" &
                !tolower(str_entry[[1]][j-1]) %in% place_name$x){
              final_entry <- paste(final_entry,str_entry[[1]][j],sep = " ")
            }
          }
          if(!"distribution" %in% tolower(str_entry[[1]])){
            final_entry <- paste(final_entry, "distribution:", sep = " ")
          }
          for (j in 2:n){
            if (tolower(str_entry[[1]][j]) %in% place_name$x |
                (!str_detect(str_entry[[1]][j],"[1-2]{1}[0-9]{3}") &
                 tolower(str_entry[[1]][j-1]) %in% place_name$x &
                 (tolower(str_entry[[1]][j+1]) %in% place_name$x |
                  is.na(str_entry[[1]][j+1])))
            )
            {
              final_entry <- paste(final_entry,str_entry[[1]][j],sep = " ")
            }
          }
          df[i] = final_entry
          selected_list <- c(selected_list,i)
        }
      }
    }
    else if(!str_detect(df[i],"[0-9]{5}")){
      str_entry <- str_split(df[i],"\\s")
      if (! tolower(str_entry[[1]][1]) %in% place_name$x){
        count = 0
        for (k in 3:length(str_entry[[1]])){
          if (tolower(str_entry[[1]][k]) %in% place_name$x | str_entry[[1]][k] == "and"){
            count = count + 1
          }
        }
        if (count > 3){
          n <- length(str_entry[[1]])
          final_entry <- str_entry[[1]][1]
          for (j in 2:n){
            if (! tolower(str_entry[[1]][j]) %in% place_name$x &
                str_entry[[1]][j] != "and" &
                !tolower(str_entry[[1]][j-1]) %in% place_name$x ){
              final_entry <- paste(final_entry,str_entry[[1]][j],sep = " ")
            }
          }
          if(!"distribution" %in% tolower(str_entry[[1]])){
            final_entry <- paste(final_entry, "distribution:", sep = " ")
          }
          for (j in 2:n){
            if ( tolower(str_entry[[1]][j]) %in% place_name$x |
                 (!str_detect(str_entry[[1]][j],"[0-9]{4}") &
                  tolower(str_entry[[1]][j-1]) %in% place_name$x &
                  (tolower(str_entry[[1]][j+1]) %in% place_name$x |
                   is.na(str_entry[[1]][j+1]))))
            {
              if(tolower(str_entry[[1]][j]) == "distribution"){
                continue
              }
              final_entry <- paste(final_entry, str_entry[[1]][j], sep = " ")
            }
          }
          df[i] = final_entry
          selected_list <- c(selected_list,i)
        }
      }
    }
  }
  selected_list <- selected_list[-1]
  selected_list <- as.integer(selected_list)
  df <- df[selected_list]
  return(df)
}


