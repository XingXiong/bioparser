#' Crawl and extract full taxonomic names from iterative webpages according to given html
#' nodes of the contents on each page
#'
#' \code{recursive_crawler} crawls, locates and extracts full taxonomic names including order,
#' suborder, family, subfamily, tribe, subtribe, genus, subgenus, species, subspecies, if
#' given. Users need to clarify a full structure of contents for crawling by indicating the
#' corresponding html nodes on different pages. Html nodes which contain urls that lead to
#' the child pages should also be passed into the functions. Besides, users should also specify
#' the prefixes or postfixes of child pages if they have one.
#'
#' @import RCurl
#' @import stringr
#' @import XML
#' @import rvest
#' @import hash
#' @import tidyverse
#' @import gtools
#' @import xml2
#'
#' @param start_url Required. The starting webpage which needs to be processed and can lead to
#' child webpages via hyperlinks.
#' @param crawl_contents Required. A full structure indicating the crawling format. It should
#' specify the html nodes on each layer of websites. The html nodes can be obtained by using
#' selectorgadget and the html nodes should point to exact contents which need to be extracted.
#' Only accepts \code{order_node}, \code{suborder_node}, \code{family_node}, \code{subfamily_node},
#' \code{tribe_node}, \code{subtribe_node}, \code{genus_node}, \code{subgenus_node}, \code{species},
#' \code{subspecies} are accepted. The format should be as follows:
#' crawl_contents <- list(first_page = list(order_node = '',
#'                                         suborder_node = '',
#'                                         family_node = ''),
#'                        sec_page = list(subfamily_node = ''),
#'                        third_page = list(genus_node = ''),
#'                        fourth_page = list(species_node = ''))
#' Note that the number of pages is not restricted and users can name the page as they like.
#' @param link_urls Required. The html nodes which contain urls that can lead to the child pages of
#' each parent page. The format should be as follows:
#' link_urls <- list(first_page = '',
#'                   sec_page = '',
#'                   third_page ='' ,
#'                   fourth_page = '',
#'                   fifth_page = '')
#' Note that one page should have one and only one html node, and the last page should be defined as
#' '' since it does not have a child page.
#' @param pre_postfix_list Required. The constant part of child page urls which are not identical with
#' the parent webpage. For example, suppose the parent page is "https://species.wikimedia.org/wiki/Belenois"
#' and the child page is "https://species.wikimedia.org/wiki/Belenois_aurota", and the href part captured
#' from the source code is "/wiki/Belenois_aurota". Because the subpage url can't be obtained by
#' concatenating "https://species.wikimedia.org/wiki/Belenois" and "/wiki/Belenois_aurota", the user needs
#' to specify the \code{prefix} to be "https://species.wikimedia.org" and \code{postfix} to be "". The
#' standard format of passing this parameter is as follows:
#' pre_postfix_list <- list(first_page = c(prefix = "", postfix = ""),
#'                          sec_page = c(prefix = "", postfix = ""),
#'                          third_page = c(prefix = "", postfix = ""),
#'                          fourth_page = c(prefix = "", postfix = ""))
#' Note that the names of the pages can be named as you like, but \code{prefix} and \code{postfix} can't
#' be changed.
#' @param output_file Required. The path and name of the file for writing. If it does
#' not contain an absolute path, the file name is relative to the current working
#' directory.
#'
#' @return A data frame containing the result.
#'
#' A TXT file written from the above data frame.
#'
#' @examples  \dontrun{
#' example#1:
#' start_url = "http://www.nic.funet.fi/pub/sci/bio/life/insecta/coleoptera/"
#' crawl_format = list(first_page = list(order_node = '#Coleoptera i',
#'                                       suborder_node = '.TN .TN b',
#'                                       family_node = '.LIST .TN'),
#'                     sec_page = list(subfamily_node = '.LIST .TN'),
#'                     third_page = list(genus_node = '.LIST .TN'),
#'                     fourth_page = list(species_node = '.SP .TN .TN i'))
#' link_urls = list(first_page = '.LIST .TN',
#'                  sec_page = '.LIST .TN',
#'                  third_page = '.LIST .TN',
#'                  fourth_page = '')
#' pre_postfix_list = list(first_page = c(prefix = "", postfix = ""),
#'                         sec_page = c(prefix = "", postfix = ""),
#'                         third_page = c(prefix = "", postfix = ""),
#'                         fourth_page = c(prefix = "", postfix = ""))
#' output_file = './Examples/output_data/recursive_crawler_result1.csv'
#' df_result = recursive_crawler(start_url, crawl_format, link_urls, pre_postfix_list, output_file)
#'
#' example#2:
#' start_url <- "https://species.wikimedia.org/wiki/Pierini"
#' crawl_contents <- list(first_page = list(family_node = '.mw-collapsed+ p a:nth-child(1)',
#'                                          subfamily_node = '#mw-content-text a:nth-child(3)',
#'                                          tribe_node = '.selflink'),
#'                        sec_page = list(subtribe_node = '.selflink'),
#'                        third_page = list(genus_node = '.selflink'),
#'                        fourth_page = list(species_node = '.selflink'),
#'                        fifth_page = list(subspecies_node = 'h2+ p'))
#' link_urls <- list(first_page = '.selflink~ a',
#'                   sec_page = 'i a',
#'                   third_page ='i~ i a' ,
#'                   fourth_page = 'i+ i a , i:nth-child(13) a',
#'                   fifth_page = '')
#' pre_postfix_list <- list(first_page = c(prefix = "https://species.wikimedia.org", postfix = " "),
#'                          sec_page = c(prefix = "https://species.wikimedia.org", postfix = ""),
#'                          third_page = c(prefix = "https://species.wikimedia.org", postfix = ""),
#'                          fourth_page = c(prefix = "https://species.wikimedia.org", postfix = ""),
#'                          fifth_page = c(prefix = "https://species.wikimedia.org", postfix = ""))
#' output_file <- './Examples/output_data/recursive_crawler_result2.csv'
#' df_result <- recursive_crawler(start_url, crawl_contents, link_urls, pre_postfix_list, output_file)
#'
#' }
#'
#' @export
recursive_crawler <- function(start_url, crawl_contents, link_urls, pre_postfix_list, output_file){
  start_page_df <- get_sciname_structure(start_url, crawl_contents[[1]], link_urls[[1]], pre_postfix_list[[1]])
  df_list <- list()
  df_list[[1]] <- start_page_df
  #print(start_page_df)
  for (i in 2:length(crawl_contents)){
    join_data <- data.frame()
    print(paste("Currently crawling on page:", i))
    for(j in 1:nrow(df_list[[i-1]])){
      cur_url <- df_list[[i-1]][j,]$sub_page_url
      print(paste("Current url:", cur_url))
      if(!is.na(cur_url)){
        cur_df <- get_sciname_structure(cur_url, crawl_contents[[i]], link_urls[[i]], pre_postfix_list[[i]])
        if (length(cur_df)>0){
          url <- c(rep(cur_url, nrow(cur_df)))
          cur_df$join_url <- url
          join_data <- smartbind(join_data, cur_df)}
      }
    }
    df_list[[i]] = join_data
  }

  final_df <- df_list[[1]]
  for (i in 2:length(df_list)){
    final_df <- left_join(df_list[[i]], final_df, by = c("join_url" = "sub_page_url"))
  }

  delete_col <- ""
  for (i in 1:length(names(final_df))){
    if (str_detect(names(final_df)[i], ".*url.*"))
    {delete_col <- c(delete_col,i)}
  }
  delete_col <- delete_col[-1]
  delete_col <- as.integer(delete_col)
  final_df <- final_df[-delete_col]
  len <- nrow(final_df)
  df_inorder <- data.frame(fcol <- c(rep(0,len)))
  names_col <- names(final_df)
  key <- c('order', 'suborder', 'family', 'subfamily',
           'tribe', 'subtribe', 'genus', 'subgenus',
           'species', 'subspecies')
  if (key[1] %in% names_col)
  {df_inorder <- cbind(df_inorder, final_df$order)}
  if (key[2] %in% names_col)
  {df_inorder <- cbind(df_inorder, final_df$suborder)}
  if (key[3] %in% names_col)
  {df_inorder <- cbind(df_inorder, final_df$family)}
  if (key[4] %in% names_col)
  {df_inorder <- cbind(df_inorder, final_df$subfamily)}
  if (key[5] %in% names_col)
  {df_inorder <- cbind(df_inorder, final_df$tribe)}
  if (key[6] %in% names_col)
  {df_inorder <- cbind(df_inorder, final_df$subtribe)}
  if (key[7] %in% names_col)
  {df_inorder <- cbind(df_inorder, final_df$genus)}
  if (key[8] %in% names_col)
  {df_inorder <- cbind(df_inorder, final_df$subgenus)}
  if (key[9] %in% names_col)
  {df_inorder <- cbind(df_inorder, final_df$species)}
  if (key[10] %in% names_col)
  {df_inorder <- cbind(df_inorder, final_df$subspecies)}
  final_df <- df_inorder[,-1]
  newnames <- names(final_df)
  newnames <- str_replace_all(newnames,"final_df\\$","")
  colnames(final_df) <- newnames
  write.csv(unique(final_df), file = output_file, row.names = F)
  return(final_df)
}


get_href_list_by_node <- function(start_url, html_node, pre_postfix_list){
  tryCatch({href_source <- start_url %>%
    read_html() %>%
    html_nodes(html_node)},
    error = function(e){""})
  #print(pre_postfix_list)
  href_list <- clean_href_source(href_source)
  child_href_list <-list()
  if(length(href_list) != 0){
    for(i in 1:length(href_list)){
      if(identical(href_list[i], character(0))){
        child_href_list[i] <- NULL
      } else {
        if (pre_postfix_list[1] == "" & pre_postfix_list[2] == ""){
          child_href_list[i] <- str_trim(paste(start_url, href_list[i], sep=""))
        }
        else{
          #print(pre_postfix_list[[1]])
          child_href_list[i] <- str_trim(paste(pre_postfix_list[1], href_list[i], pre_postfix_list[2], sep=""))
          #print(child_href_list[i])
        }
      }
    }
    return(unlist(child_href_list))
  }
}


grab_href_context <- function(start_url, html_node){
  href_source <- start_url %>%
    read_html() %>%
    html_nodes(html_node) %>%
    html_text()
  return(unique(href_source))
}


clean_href_source <-function(href_source){
  href_list <- str_extract_all(href_source, "href=\"[^\"]*\"")
  href_list <- unlist(href_list)
  href_list <- str_extract_all(href_list,"\".*\"")
  href_list <- str_replace_all(href_list,"\"","")
  href_list <- str_replace_all(href_list,"&amp;","&")
  return(href_list)
}


get_sciname_structure <- function(start_url, crawl_format_by_page, link_urls, pre_postfix_list){
  cur_page_nodes_name <- names(crawl_format_by_page)
  exist_nodes <- c()
  exist_lists <- list()
  if('order_node' %in% cur_page_nodes_name){
    order_node <- crawl_format_by_page$order_node
    order_list <- grab_href_context(start_url, order_node)
    exist_nodes <- c(exist_nodes, order_node)
    exist_lists[['order_list']] <- order_list
  } else {
    order_list <- character()
  }
  if('suborder_node' %in% cur_page_nodes_name){
    suborder_node <- crawl_format_by_page$suborder_node
    suborder_list <- grab_href_context(start_url, suborder_node)
    exist_nodes <- c(exist_nodes, suborder_node)
    exist_lists[['suborder_list']] <- suborder_list
  } else {
    suborder_list <- character()
  }
  if('family_node' %in% cur_page_nodes_name){
    family_node <- crawl_format_by_page$family_node
    family_list <- grab_href_context(start_url, family_node)
    exist_nodes <- c(exist_nodes, family_node)
    exist_lists[['family_list']] <- family_list
  } else {
    family_list <- character()
  }
  if('subfamily_node' %in% cur_page_nodes_name){
    subfamily_node <- crawl_format_by_page$subfamily_node
    subfamily_list <- grab_href_context(start_url, subfamily_node)
    exist_nodes <- c(exist_nodes, subfamily_node)
    exist_lists[['subfamily_list']] <- subfamily_list
  } else {
    subfamily_list <- character()
  }
  if('tribe_node' %in% cur_page_nodes_name){
    tribe_node <- crawl_format_by_page$tribe_node
    tribe_list <- grab_href_context(start_url, tribe_node)
    exist_nodes <- c(exist_nodes, tribe_node)
    exist_lists[['tribe_list']] <- tribe_list
  } else {
    tribe_list <- character()
  }
  if('subtribe_node' %in% cur_page_nodes_name){
    subtribe_node <- crawl_format_by_page$subtribe_node
    subtribe_list <- grab_href_context(start_url, subtribe_node)
    exist_nodes <- c(exist_nodes, subtribe_node)
    exist_lists[['subtribe_list']] <- subtribe_list
  } else {
    subtribe_list <- character()
  }
  if('genus_node' %in% cur_page_nodes_name){
    genus_node <- crawl_format_by_page$genus_node
    genus_list <- grab_href_context(start_url, genus_node)
    exist_nodes <- c(exist_nodes, genus_node)
    exist_lists[['genus_list']] <- genus_list
  } else {
    genus_list <- character()
  }
  if('subgenus_node' %in% cur_page_nodes_name){
    subgenus_node <- crawl_format_by_page$subgenus_node
    subgenus_list <- grab_href_context(start_url, subgenus_node)
    exist_nodes <- c(exist_nodes, subgenus_node)
    exist_lists[['subgenus_list']] <- subgenus_list
  } else {
    subgenus_list <- character()
  }
  if('species_node' %in% cur_page_nodes_name){
    species_node <- crawl_format_by_page$species_node
    species_list <- grab_href_context(start_url, species_node)
    exist_nodes <- c(exist_nodes, species_node)
    exist_lists[['species_list']] <- species_list
  } else {
    species_list <- character()
  }
  if('subspecies_node' %in% cur_page_nodes_name){
    subspecies_node <- crawl_format_by_page$subspecies_node
    subspecies_list <- grab_href_context(start_url, subspecies_node)
    exist_nodes <- c(exist_nodes, subspecies_node)
    exist_lists[['subspecies_list']] <- subspecies_list
  } else {
    subspecies_list <- character()
  }
  # get the whole list including all the nodes in their correct order
  general_node <- paste(exist_nodes, collapse =",")
  general_list <- grab_href_context(start_url, general_node)
  h <- hash(keys = c('order_list', 'suborder_list', 'family_list', 'subfamily_list',
                     'tribe_list', 'subtribe_list', 'genus_list', 'subgenus_list',
                     'species_list', 'subspecies_list'),
            values = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

  num_list <- character()
  if(length(general_list) != 0){
    for(i in 1:length(general_list)) {
      if(general_list[i] %in% order_list){
        num_list[i] <- h$'order_list'
      }
      if(general_list[i] %in% suborder_list){
        num_list[i] <- h$'suborder_list'
      }
      if(general_list[i] %in% family_list){
        num_list[i] <- h$'family_list'
      }
      if(general_list[i] %in% subfamily_list){
        num_list[i] <- h$'subfamily_list'
      }
      if(general_list[i] %in% tribe_list){
        num_list[i] <- h$'tribe_list'
      }
      if(general_list[i] %in% subtribe_list){
        num_list[i] <- h$'subtribe_list'
      }
      if(general_list[i] %in% genus_list){
        num_list[i] <- h$'genus_list'
      }
      if(general_list[i] %in% subgenus_list){
        num_list[i] <- h$'subgenus_list'
      }
      if(general_list[i] %in% species_list){
        num_list[i] <- h$'species_list'
      }
      if(general_list[i] %in% subspecies_list){
        num_list[i] <- h$'subspecies_list'
      }
    }

    general_hash_list <- data.frame(general_list, num_list)

    result_df <- data.frame(order=character(), suborder=character(),
                            family=character(), subfamily=character(),
                            tribe=character(), subtribe=character(),
                            genus=character(), subgenus=character(),
                            species=character(), subspecies=character())

    cur_order <- ''
    cur_suborder <- ''
    cur_family <- ''
    cur_subfamily <- ''
    cur_tribe <- ''
    cur_subtribe <- ''
    cur_genus <- ''
    cur_subgenus <- ''
    cur_species <- ''
    cur_subspecies <- ''

    cur_nums <- levels(general_hash_list$num_list)
    max_num <- max(cur_nums)
    min_num <- min(cur_nums)
    # order:1, suborder:2, family:3, subfamily:4, tribe:5, subtribe:6, genus:7, subgenus:8, species:9, subspecies:10

    for(i in 1:length(general_hash_list[[1]])){
      if(general_hash_list[i,][2] == 1){
        cur_order <- as.character(general_hash_list$general_list[i])
      }
      if(general_hash_list[i,][2] == 2){
        cur_suborder <- as.character(general_hash_list$general_list[i])
      }
      if(general_hash_list[i,][2] == 3){
        cur_family <- as.character(general_hash_list$general_list[i])
      }
      if(general_hash_list[i,][2] == 4){
        cur_subfamily <- as.character(general_hash_list$general_list[i])
      }
      if(general_hash_list[i,][2] == 5){
        cur_tribe <- as.character(general_hash_list$general_list[i])
      }
      if(general_hash_list[i,][2] == 6){
        cur_subtribe <- as.character(general_hash_list$general_list[i])
      }
      if(general_hash_list[i,][2] == 7){
        cur_genus <- as.character(general_hash_list$general_list[i])
      }
      if(general_hash_list[i,][2] == 8){
        cur_subgenus <- as.character(general_hash_list$general_list[i])
      }
      if(general_hash_list[i,][2] == 9){
        cur_species <- as.character(general_hash_list$general_list[i])
      }
      if(general_hash_list[i,][2] == 10){
        cur_subspecies <- as.character(general_hash_list$general_list[i])
      }
      #print(cur_genus)
      if(general_hash_list[i,][2] == max_num){
        new_row <- data.frame(order=cur_order, suborder=cur_suborder,
                              family=cur_family, subfamily=cur_subfamily,
                              tribe=cur_tribe, subtribe=cur_subtribe,
                              genus=cur_genus, subgenus=cur_subgenus,
                              species=cur_species, subspecies=cur_subspecies)
        result_df <- rbind(result_df, new_row)
      }
    }
    #print(result_df)
    #print(names(result_df))
    #remove empty columns
    remain_cols <- c()
    for(i in 1:length(result_df)){
      if(!all(levels(result_df[[i]])=='')){
        remain_cols <- c(remain_cols, i)
      }
    }
    result_df <- result_df[, remain_cols, drop=FALSE]
    #print(result_df)
    if(!is.null(link_urls)){
      if(!is.null(get_href_list_by_node(start_url, link_urls, pre_postfix_list))){
        #print(get_href_list_by_node(start_url, link_urls, pre_postfix_list))
        sub_page_url <- get_href_list_by_node(start_url, link_urls, pre_postfix_list)
        if (length(sub_page_url) == nrow(result_df)){
          #print("all good")
          result_df['sub_page_url'] <- sub_page_url
        }
        else {if (nrow(result_df) > length(sub_page_url)){
          m <- 0
          k <- 1
          page_num_list <- c()
          for (p in 1:length(sub_page_url)){
            page_num_list <- c(page_num_list,p)
          }
          while (m < (nrow(result_df) - length(sub_page_url))){
            if (k < nrow(result_df)){
              test_part <- str_split(result_df[k+1,1], "[^A-z0-9]|\\[|\\]")
              test_dup_result <- c()


              if (length(test_part) > 0){
                for (i in 1:length(test_part[[1]])){
                  test_dup_result <- c(test_dup_result, str_detect(result_df[k,1], test_part[[1]][i]))
                }
              }
              if (sum(test_dup_result) == length(test_part[[1]])){
                page_num_list <- c(page_num_list, k-m)
                m = m + 1

              }
              k = k + 1
              #print(k)
            }
            else{
              m = nrow(result_df) - length(sub_page_url)
              k = 9999
            }
          }
          if (k == 9999){
            for (j in 1:m){
              sub_page_url <- c(sub_page_url, " ")
            }
            result_df['sub_page_url'] <- sub_page_url
          }
          else{
            #print(99999999)
            page_num_list <- sort(page_num_list)
            #print(page_num_list)
            new_sub_page_url <- c()
            for (q in 1:length(page_num_list)){
              new_sub_page_url <- c(new_sub_page_url, sub_page_url[page_num_list[q]])
            }
            result_df['sub_page_url'] <- new_sub_page_url
          }
        }
          else {
            #print("more url")
            #print(result_df)
            sub_page_url <- unique(sub_page_url)
            if (length(sub_page_url) == nrow(result_df)){
              result_df['sub_page_url'] <- sub_page_url
            }
            if (length(sub_page_url) < nrow(result_df)){
              for (j in 1:(nrow(result_df) - length(sub_page_url))){
                sub_page_url <- c(sub_page_url," ")
              }
              result_df['sub_page_url'] <- sub_page_url
            }
            if (length(sub_page_url) > nrow(result_df)){
              result_df1 <- result_df
              for (i in 1:(length(sub_page_url) - nrow(result_df))){
                result_df <- rbind(result_df,result_df1)
              }

              result_df['sub_page_url'] <- sub_page_url
            }
          }

        }
      }
    }
    return(result_df)
  }
}


normalize_url <- function(url){
  url <- str_replace_all(url, "\\.{2,}","")
  url <- str_replace_all(url, "[^:]\\//","\\/")
  return(url)
}
