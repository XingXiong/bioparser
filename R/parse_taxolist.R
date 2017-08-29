#' Parse and extract taxonomic names from txt files
#'
#' \code{parse_taxolist} reads and parses all text lines from a file which contains
#' taxonomic names, authors and distribution in each row and writes the tabular output
#' to a csv file automatically or based on the configuration \code {config} specified
#' by the users.
#'
#' @import RCurl
#' @import plyr
#' @import stringr
#'
#' @param input_file Required. The path and name of the file which the data is to be
#' read from. If it does not contain an absolute path, the file name is relative to the
#' current working directory.
#' @param output_file Required. The path and name of the file for writing. If it does
#' not contain an absolute path, the file name is relative to the current working
#' directory.
#' @param location_detail Optional.A logical value indicating whether the detailed
#' information including longitude, latitude and detail location names of distributions
#' is to be exported. Defaults to TRUE.
#' @param language Optional.The language of detailed distribution information which
#' must be one of "English" or "Chinese". Defaults to "English".
#' @param evaluation Optional. A logical value indicating whether the evaluation of the
#' parsing result is to be exported. Defaults to TRUE.
#' @param config Optional. If it is not specified by users, the output will be generated
#' automatically based on the structure of input texts. If it is indicated explicitly by
#' users, the function will parse the input texts based on the rules specified in the
#' \code{config}. Some examples of config are provided in the "Examples" part. Note that:
#' Author_year should be regarded as a whole part; The separator between author_year part
#' and distribution part should be stated clearly; If '\n' exsits, it can only appear
#' right after the \code{genus} part.
#'
#' @return A data frame containing the result of parsing taxonomic names in the input
#' file and detailed distribution information about species. For those taxonomic names
#' which have more than one distribution, if \code{location_detail} is \code{TRUE}, each
#' row in the data frame will only contain one distribution. If \code{location_detail}
#' is \code{FALSE}, all distributions for a single species will be written in one row.
#'
#' A CSV file written from the above data frame.
#'
#' @examples  \dontrun{
#' # example1:
#' parse_taxolist(input_file = "./Examples/input_data/test_example.txt",
#'                output_file = "./Examples/output_data/test_example_output.csv",
#'                location_detail = TRUE,
#'                language = "English",
#'                evaluation = TRUE,
#'                config = "")
#'
#' input example:
#' Charmus indicus Chiang, Chen & Zhang 1996.Distribution: Andhra Pradesh, Kerala,Pondicherry and Tamil Nadu.
#' Isometrus maculatus De Geer, 1778.Distribution: Kerala, Andhra Pradesh, Madhya Pradesh, Karnataka,Maharashtra, Meghalaya and Tamil Nadu.
#' Lychas hendersoni Pocock, 1897) Distribution: Kerala and Tamil Nadu.
#'
#'
#' # example2:
#' parse_taxolist(input_file = "./Examples/input_data/test_example_config_1.txt",
#'                output_file = "./Examples/output_data/test_example_output_config_1.csv",
#'                location_detail = FALSE,
#'                language = "English",
#'                evaluation = TRUE,
#'                config = "Genus, Species, Author_Year, 'Distribution:', distribution")
#'
#' input example:
#' Pachliopta pandiyana Moore, 1881 Distribution: Goa, Karnataka, Kerala
#'
#'
#' # example3:
#' parse_taxolist(input_file = "./Examples/input_data/test_example_config_2.txt",
#'                output_file = "./Examples/output_data/test_example_output_config_2.csv",
#'                location_detail = FALSE,
#'                language = "English",
#'                evaluation = FALSE,
#'                config = "Genus, Species, Author_Year, ':', distribution")
#'
#' input example:
#' Pachliopta pandiyana Moore, 1881 : Goa, Karnataka, Kerala
#'
#'
#' # example4:
#' parse_taxolist(input_file = "./Examples/input_data/test_example_config_3.txt",
#'                output_file = "./Examples/output_data/test_example_output_config_3.csv",
#'                location_detail = TRUE,
#'                language ="English",
#'                evaluation = FALSE,
#'                config = "Genus, '\n', Species, Author_Year, ':',distribution")
#'
#' input example:
#' Pachliopta
#' pandiyana Moore, 1881 : Goa, Karnataka, Kerala
#' aristolochiae Fabricius, 1775  : Meghalaya, Paschimbanga, Kerala, Karnataka, Arunachal Pradesh, Telangana, Andhra Pradesh, Maharashtra, Gujarat, Odisha, Chhattisgarh
#'}
#'
#' @export
parse_taxolist <- function(input_file, output_file, location_detail, language, evaluation, config){
  lines <- readLines(input_file, warn = FALSE)
  final_result <- c()
  if(config == ""){
    for (i in 1:length(lines)){
      if (lines[i] != ""){
        cur_sci_name <- get_full_sciname_one_line(lines[i])
        cur_result <- get_tabular_output(cur_sci_name, location_detail, language, evaluation)
        final_result <- rbind(final_result, cur_result)
      }
    }
    write.csv(final_result, file = output_file, row.names = F)
  } else {
    config_list <- as.list(strsplit(config, ",")[[1]])
    config_list <- str_trim(config_list)
    line_break_num <- length(str_locate_all(config, "\n")[[1]][,"start"])
    if(line_break_num == 0){
      for (i in 1:length(lines)){
        if (lines[i] != ""){
          cur_sci_name <- get_sciname_by_config_one_line(lines[i], config)
          cur_result <- get_tabular_output(cur_sci_name, location_detail, language, evaluation)
          final_result <- rbind(final_result, cur_result)
        }
      }
      write.csv(final_result, file = output_file, row.names = F)
    } else if(line_break_num == 1){
      output_sciname_by_config_multi_line(config, lines, output_file, location_detail, language, evaluation)
    } else{
      Stop("You can't have more than one line break symbols in config.")
    }
  }
  return(final_result)
}

getGenus <- function(science_name){
  # if the first character is capitalized, then the first word must be Genus
  # quality: score to evaluate results (1: good, 2: uncertainty in some part, 3: possibly error in input)
  if (str_detect(science_name, "^[A-Z]")){
    capital_index <- str_locate_all(science_name,"[[:blank:]]?[A-Z]+")
    genus_species_sub <- str_sub(science_name, capital_index[[1]][1], capital_index[[1]][2]-1)
    split <- str_split(genus_species_sub, " ")
    split <- unlist(split)
    if (length(split) == 1){ # if there's only one word
      genus <- split[1]
      species <- 'NA'
      subspecies <- 'NA'
      score <- 1
      evaluation <- "High credibility."
    } else if (length(split) == 2){ # if there are two words
      genus <- split[1]
      species <- split[2]
      subspecies <- 'NA'
      score <- 1
      evaluation <- "High credibility."
    } else if (length(split) == 3){ # if there are three words
      genus <- split[1]
      species <- split[2]
      subspecies <- split[3]
      score <- 1
      evaluation <- "High credibility."
    } else { # if there are more than three words in the fist part
      genus <- split[1]
      species <- split[2]
      subspecies <- split[3]
      score <- 3
      evaluation <- "Possibly error or confusion in the input."
    }
    # if the first character is not capitalized, then this entry doesn't include Genus
  } else {
    capital_index <- str_locate_all(science_name,"[[:blank:]]?[A-Z]+")
    genus_species_sub <- str_sub(science_name, 1, capital_index[[1]][1]-1)
    split <- str_split(genus_species_sub, " ")
    split <- unlist(split)
    if (length(split) == 1){
      # to be continued (how to distinguish between species and subspecies? )
      genus <- 'NA'
      species <- split[1]
      subspecies <- 'NA'
      score <- 2
      evaluation <- "Cannot distinguish species or subspecies."
    } else if (length(split) == 2) {
      genus <- 'NA'
      species <- split[1]
      subspecies <- split[2]
      score <- 1
      evaluation <- "High credibility."
    } else {
      genus <- 'NA'
      species <- split[1]
      subspecies <- split[2]
      score <- 3
      evaluation <- "Possibly error or confusion in the input."
    }
  }
  return (list(genus, species, subspecies, score, evaluation))
}

getAuthorYear <- function(science_name){
  if(str_detect(science_name, "[0-9]{4}.?")){
    year_list <- str_locate_all(science_name, "[0-9]{4}.?")[[1]]
    # end_index is the position of the last character of the last year
    end_index <- year_list[, "end"][length(year_list[, "end"])]
    capital_index <- str_locate_all(science_name,"[[:blank:]]?[A-Z]+")
    if(str_detect(science_name, "^[A-Z]")){
      # if scientific name starts with a capital letter (genus), then the second capital letter is the start of author_year part
      start_index <- capital_index[[1]][2]
    } else {
      # if not, then the first capital letter is the start of author_year part
      start_index <- capital_index[[1]][1]
    }
    author_year_list <- str_sub(science_name, start_index + 1, end_index - 1)
    score <- 1
    evaluation <- "High credibility."
  } else {
    author_year_list <- 'NA'
    score <- 1
    evaluation <- "There's no author info in this entry."
  }
  return(c(author_year_list, score, evaluation))
}


getDistribution <- function(science_name){
  # if there is year part in the scientific name, the distribution info is right after the last year.
  if(str_detect(science_name, "[0-9]{4}.?")){
    year_list <- str_locate_all(science_name, "[0-9]{4}.?")[[1]]
    # start_index is the position of the last character of the last year
    start_index <- year_list[, "end"][length(year_list[, "end"])]
    # if there's no author_year part, the distribution info starts with the second (or first) capital letter in the scientific name
  } else {
    capital_index <- str_locate_all(science_name,"[[:blank:]]?[A-Z]+")
    if(str_detect(science_name, "^[A-Z]")){
      start_index <- capital_index[[1]][2]
    } else{
      start_index <- capital_index[[1]][1]
    }
  }
  distribution_list <- substring(science_name, start_index + 1)
  clean_list <- gsub("\\sand\\s", ",", distribution_list)
  clean_list <-  gsub("Distribution:", "", clean_list)
  clean_list <-  str_trim(gsub("[:punct:]", "", clean_list))
  if(str_detect(science_name, ",") == FALSE & length(str_split(clean_list, " ")[[1]]) != 1){
    distribution <- str_split(clean_list, " ")
  } else {
    distribution <- str_split(clean_list, ",")
  }
  score <- 1
  evaluation <- "High credibility."
  # if there's no distribution info, to be continued
  return(c(distribution, score, evaluation))
}

# connect with google map api to get the longitude, altitude and detailed information about the distribution
url <- function(address, language, return.call = "json", sensor = "false") {
  if (language == "English"){
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))
  } else {
    root <- "http://maps.google.cn/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))
  }
}


geoCode <- function(address, language, verbose = FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address, language)
  doc <- getURL(u)
  x <- fromJSON(doc, simplify = FALSE)
  if(x$status == "OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA, NA, NA, NA))
  }
}

evaluation_output <- function(sci_name){
  score <- max(sci_name$genus_score,sci_name$author_year_score,sci_name$distribution_score)
  evaluation <- paste("genus:", sci_name$genus_evaluation,
                      "author:", sci_name$author_year_evaluation,
                      "distribution:", sci_name$distribution_evaluation,
                      sep = " ")
  return(c(score,evaluation))
}


get_full_sciname_one_line <- function(line){
  sci_name <-c()
  sci_name$genus <- 'NA'
  sci_name$species <- 'NA'
  sci_name$subspecies <- 'NA'
  sci_name$author_year <- 'NA'
  sci_name$distribution <- 'NA'

  sci_name$genus_score <- -1
  sci_name$genus_evaluation <- ''
  sci_name$author_year_score <- -1
  sci_name$author_year_evaluation <- ''
  sci_name$distribution_score <- -1
  sci_name$distribution_evaluation <- ''

  # get genus, species, subspecies and its evaluation
  genus_species_sub <- getGenus(line)
  sci_name$genus <- unlist(genus_species_sub)[1]
  sci_name$species <- unlist(genus_species_sub)[2]
  sci_name$subspecies <- unlist(genus_species_sub)[3]
  sci_name$genus_score <- unlist(genus_species_sub)[4]
  sci_name$genus_evaluation <- unlist(genus_species_sub)[5]

  # get author year info and the evaluation
  author_year_result <- getAuthorYear(line)
  sci_name$author_year <- unlist(author_year_result)[1]
  sci_name$author_year_score <- unlist(author_year_result)[2]
  sci_name$author_year_evaluation <- unlist(author_year_result)[3]

  # get basic distribution
  distribution_result <- getDistribution(line)
  distribution_list <- distribution_result[1]
  sci_name$distribution_score <- distribution_result[2][[1]]
  sci_name$distribution_evaluation <- distribution_result[3][[1]]
  sci_name$distribution <- paste(distribution_result[[1]], collapse = ",")

  return(sci_name)
}

get_sciname_by_config_one_line <- function(line, config){
  sci_name <-c()
  sci_name$genus <- 'NA'
  sci_name$species <- 'NA'
  sci_name$subspecies <- 'NA'
  sci_name$author_year <- 'NA'
  sci_name$distribution <- 'NA'

  sci_name$genus_score <- -1
  sci_name$genus_evaluation <- ''
  sci_name$author_year_score <- -1
  sci_name$author_year_evaluation <- ''
  sci_name$distribution_score <- -1
  sci_name$distribution_evaluation <- ''

  config_list <- as.list(strsplit(config, ",")[[1]])
  config_list <- str_trim(config_list)
  len = length(config_list)
  if("Genus" %in% str_trim(config_list)){
    genus_pos <- get_config_position(config_list, "Genus")
    sci_name$genus <- str_split(line, " ")[[1]][genus_pos]
    rest_part <- str_trim(gsub(sci_name$genus, "", line))
    sci_name$genus_score <- 1
    sci_name$genus_evaluation <- "High credibility."
  } else {
    sci_name$genus_score <- 1
    sci_name$genus_evaluation <- "Missing 'genus' in the scientific name."
  }
  if("Species" %in% str_trim(config_list)){
    species_pos <- get_config_position(config_list, "Species")
    sci_name$species <- str_split(line, " ")[[1]][species_pos]
    rest_part <- str_trim(gsub(sci_name$species, "", rest_part))
  } else {
    if (sci_name$genus_evaluation == "High credibility."){
      sci_name$genus_score <- 1
      sci_name$genus_evaluation <- "Missing 'species' in the scientific name."
    } else {
      sci_name$genus_score <- 1
      sci_name$genus_evaluation <- paste(sci_name$genus_evaluation, "Missing 'species' in the scientific name.")
    }
  }
  if("Subspecies" %in% str_trim(config_list)){
    subspecies_pos <- get_config_position(config_list, "Subspecies")
    sci_name$subspecies <- str_split(line, " ")[[1]][subspecies_pos]
    rest_part <- str_trim(gsub(sci_name$subspecies, "", rest_part))
  } else {
    if (sci_name$genus_evaluation == "High credibility."){
      sci_name$genus_score <- 1
      sci_name$genus_evaluation <- "Missing 'subspecies' in the scientific name."
    } else {
      sci_name$genus_score <- 1
      sci_name$genus_evaluation <- paste(sci_name$genus_evaluation, "Missing 'subspecies' in the scientific name.")
    }
  }
  # if config list ends with distribution
  if(str_trim(config_list[len]) == 'distribution'){
    # extract distribution part
    if (str_detect(config_list[len-1], "'*'")){
      split_word <- str_split(config_list[len-1], "'*'")[[1]][2]
      sci_name$distribution  <- str_trim(str_split(rest_part, split_word)[[1]][2])
      rest_part <- str_trim(gsub(split_word, "", rest_part))
      sci_name$author_year <- str_trim(gsub(sci_name$distribution , "", rest_part))
      sci_name$distribution_score <- 1
      sci_name$distribution_evaluation  <- "High credibility."
      if (sci_name$author_year != ""){
        sci_name$author_year_score <- 1
        sci_name$author_year_evaluation <- "High credibility."
      } else {
        sci_name$author_year_score <- 1
        sci_name$author_year_evaluation <- "Missing author and year part in the scientific name."
      }
    }
    # if config list ends with author and year part
    else if (str_trim(config_list[len-1]) == 'Author_Year'){
      year_list <- str_locate_all(rest_part, "[0-9]{4}.?")[[1]]
      # start_index is the position of the last character of the last year
      start_index <- year_list[, "end"][length(year_list[, "end"])]
      sci_name$distribution <- str_trim(substring(rest_part, start_index + 1))
      sci_name$author_year <- str_trim(gsub(sci_name$distribution, "", rest_part))
      sci_name$author_year_score <- 1
      sci_name$author_year_evaluation <- "High credibility."
      sci_name$distribution_score <- 1
      sci_name$distribution_evaluation  <- "Missing distribution part in the scientific name."
    }
    # if config list does not contain author_year and distribution
    else {
      sci_name$distribution <- rest_part
      sci_name$distribution_score <- 1
      sci_name$distribution_evaluation  <- "High credibility."
      sci_name$author_year_score <- 1
      sci_name$author_year_evaluation <- "Missing author and year part in the scientific name."
    }
  } else if (str_trim(config_list[len]) == 'Author_Year'){
    sci_name$author_year <- rest_part
    sci_name$distribution_score <- 1
    sci_name$distribution_evaluation <- "Missing distribution part in the scientific name."
  } else{
    sci_name$author_year_score <- 1
    sci_name$author_year_evaluation <- "Missing author and year part in the scientific name."
    sci_name$distribution_score <- 1
    sci_name$distribution_evaluation <- "Missing distribution part in the scientific name."
  }
  return(sci_name)
}


output_sciname_by_config_multi_line <- function(config, lines, output_file, location_detail, language, evaluation){
  sci_name <-c()
  sci_name$genus <- 'NA'
  sci_name$species <- 'NA'
  sci_name$subspecies <- 'NA'
  sci_name$author_year <- 'NA'
  sci_name$distribution <- 'NA'

  sci_name$genus_score <- -1
  sci_name$genus_evaluation <- ''
  sci_name$author_year_score <- -1
  sci_name$author_year_evaluation <- ''
  sci_name$distribution_score <- -1
  sci_name$distribution_evaluation <- ''

  final_result <- c()
  config_list <- as.list(strsplit(config, ",")[[1]])
  config_list <- str_trim(config_list)
  line_break_pos <- get_config_position(config_list, "'\n'")
  line_break_num <- length(str_locate_all(config, "\n")[[1]][,"start"])
  if(line_break_num == 1 & line_break_pos ==2){
    len <- length(lines)
    for(i in 1:len){
      if(lines[i] == ""){
        continue
      }
      line_split <- str_split(str_trim(lines[i]), " ")[[1]]
      if(length(line_split) == 1){
        cur_line_index <- i
        sci_name$genus <- str_trim(lines[i])
      } else {
        cur_config_list <- config_list[-1]
        cur_config_list <- cur_config_list[-1]
        cur_config_len <- length(cur_config_list)
        if("Species" %in% str_trim(cur_config_list)){
          species_pos <- get_config_position(cur_config_list, "Species")
          sci_name$species <- str_split(lines[i], " ")[[1]][species_pos]
          lines[i] <- str_trim(gsub(sci_name$species, "", lines[i]))
        } else {
          sci_name$genus_score <- 1
          sci_name$genus_evaluation <- "Missing 'species' in the scientific name."
        }
        if("Subspecies" %in% str_trim(cur_config_list)){
          subspecies_pos <- get_config_position(cur_config_list, "Subspecies")
          sci_name$subspecies <- str_split(lines[i], " ")[[1]][subspecies_pos]
          lines[i] <- str_trim(gsub(sci_name$subspecies, "", lines[i]))
        } else {
          if (sci_name$genus_evaluation == "High credibility."){
            sci_name$genus_score <- 1
            sci_name$genus_evaluation <- "Missing 'subspecies' in the scientific name."
          } else {
            sci_name$genus_score <- 1
            sci_name$genus_evaluation <- paste(sci_name$genus_evaluation, "Missing 'subspecies' in the scientific name.")
          }
        }
        if(str_trim(cur_config_list[cur_config_len]) == 'distribution'){
          # extract distribution part
          if (str_detect(cur_config_list[cur_config_len-1], "'*'")){
            split_word <- str_split(cur_config_list[cur_config_len-1], "'*'")[[1]][2]
            sci_name$distribution  <- str_trim(str_split(lines[i], split_word)[[1]][2])
            lines[i] <- str_trim(gsub(split_word, "", lines[i]))
            sci_name$author_year <- str_trim(gsub(sci_name$distribution , "", lines[i]))
            sci_name$distribution_score <- 1
            sci_name$distribution_evaluation  <- "High credibility."
            if (sci_name$author_year != ""){
              sci_name$author_year_score <- 1
              sci_name$author_year_evaluation <- "High credibility."
            } else {
              sci_name$author_year_score <- 1
              sci_name$author_year_evaluation <- "Missing author and year part in the scientific name."
            }
          } else if (str_trim(cur_config_list[cur_config_len-1]) == 'Author_Year'){
            year_list <- str_locate_all(lines[i], "[0-9]{4}.?")[[1]]
            # start_index is the position of the last character of the last year
            start_index <- year_list[, "end"][length(year_list[, "end"])]
            sci_name$distribution <- str_trim(substring(lines[i], start_index + 1))
            sci_name$author_year <- str_trim(gsub(sci_name$distribution, "", lines[i]))
            sci_name$author_year_score <- 1
            sci_name$author_year_evaluation <- "High credibility."
            sci_name$distribution_score <- 1
            sci_name$distribution_evaluation  <- "Missing distribution part in the scientific name."
          } else {
            sci_name$distribution <- lines[i]
            sci_name$distribution_score <- 1
            sci_name$distribution_evaluation  <- "High credibility."
            sci_name$author_year_score <- 1
            sci_name$author_year_evaluation <- "Missing author and year part in the scientific name."
          }
        } else if (str_trim(cur_config_list[cur_config_len]) == 'Author_Year'){
          sci_name$author_year <- lines[i]
          sci_name$distribution_score <- 1
          sci_name$distribution_evaluation <- "Missing distribution part in the scientific name."
        } else {
          sci_name$author_year_score <- 1
          sci_name$author_year_evaluation <- "Missing author and year part in the scientific name."
          sci_name$distribution_score <- 1
          sci_name$distribution_evaluation <- "Missing distribution part in the scientific name."
        }
        # write the current line to tabular output
        cur_result <- get_tabular_output(sci_name, location_detail, language, evaluation)
        final_result <- rbind(final_result, cur_result)
      }
    }
    write.csv(final_result, file = output_file, row.names = F)
  } else {
    stop("Can't support current config.")
  }
}

get_tabular_output <- function(sci_name, location_detail, language, evaluation){
  # create a new dataframe
  result <- data.frame(genus = c(0), species = c(0), subspecies = c(0), author_year = c(0), distribution = c(0),
                       latitude = c(0), longitude = c(0), detail = c(0))
  eva_df <- data.frame(score = c(0), evaluation = c(0))
  if (location_detail == "TRUE"){
    distribution_list <- str_split(sci_name$distribution, ",")[[1]]
    for (j in 1:length(distribution_list)){
      # To make the table clear,the table will show information about genus,author and etc. for only one time
      # for different locations from the same entry.
      if (j == 1) {
        distribution_list[j] <- gsub("[[:punct:]]", "", distribution_list[j])
        address <- geoCode(distribution_list[j], language)
        new_row <- c(sci_name$genus, sci_name$species, sci_name$subspecies, sci_name$author_year, distribution_list[j], address[1], address[2], address[4])
        result <- rbind(result, new_row)
        if (evaluation == "TRUE"){
          eva_result <- evaluation_output(sci_name)
          eva_df <- rbind(eva_df, eva_result)
        }
      } else {
        address <-geoCode(distribution_list[j], language)
        new_row <- c(" ", " ", " ", " ", distribution_list[j], address[1], address[2], address[4])
        result <- rbind(result, new_row)
        if (evaluation == "TRUE"){
          eva_df <- rbind(eva_df,c(" "," "))}
      }
    }
    # The server of api will reject too frequent access,so set a stop after each loop.
    Sys.sleep(1)
  } else{
    if (evaluation == "TRUE"){
      new_row <- c(sci_name$genus, sci_name$species, sci_name$subspecies, sci_name$author_year, sci_name$distribution)
      result <- rbind(result[,1:5], new_row)
      eva_result <- evaluation_output(sci_name)
      eva_df <- rbind(eva_df, eva_result)
    } else {
      new_row <- c(sci_name$genus, sci_name$species, sci_name$subspecies, sci_name$author_year, sci_name$distribution)
      result <- rbind(result[,1:5], new_row)}
  }
  if (evaluation == "TRUE"){
    result = cbind(result[-1,],eva_df[-1,])
  } else {
    result = result[-1,]
  }
  return(result)
}


parse_taxoname <- function(input_str, location_detail, language, evaluation, config){
  lines <- str_split(input_str, "\n")[[1]]
  if(config == ""){
    cur_sci_name <- get_full_sciname_one_line(input_str)
  } else {
    config_list <- as.list(strsplit(config, ",")[[1]])
    config_list <- str_trim(config_list)
    line_break_num <- length(str_locate_all(config, "\n")[[1]][,"start"])
    if(line_break_num == 0){
      cur_sci_name <- get_sciname_by_config_one_line(input_str, config)
    } else{
      Stop("You can't have more than one line break symbols in config.")
    }
  }
  return(cur_sci_name)
}

