#' Extract information starting from a given url
#'
#' \code{web_crawler} is a supplementary function to `recursive_crawler`. It's more flexible when users
#' need to extract additional contents apart from order, suborder, family, subfamily, tribe, subtribe,
#' genus, subgenus, species, subspecies if specified. The function handles some unusual situations when
#' certain html nodes of identical contents on different pages change unexpectly. For example, html nodes
#' of parent and child webpages which refer to exactly the same contents may change from "p:nth-child(4)"
#' to "p:nth-child(6)" among pages by varying the number in a certain range. In general, this function
#' starts from the top layer page, follows available url links to lower level pages and can only crawl
#' contents on the lowest layer pages, which is the main difference to `recursive_crawler`. Since some
#' useless information may also be grabbed due the changing html node scenarios, further data cleaning
#' by users is strongly recommended.
#'
#'
#' @import rvest
#' @import RCurl
#' @import stringr
#'
#' @param start_url Required. The starting webpage which needs to be processed and can lead to
#' child webpages via hyperlinks.
#' @param crawl_format Required. The html nodes which contain urls that can lead to the child pages of
#' each parent page. The format should be as follows:
#' crawl_format <- list(first_page = '',
#'                   sec_page = '',
#'                   third_page ='' ,
#'                   fourth_page = '',
#'                   fifth_page = '')
#' The last page should be defined as '' since it does not have a child page.
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
#' @param colnames Optional. Set names of columns in advance to avoid confusions. Default is system
#' default column names.
#' @param search_range Optional. The range to change the original number in nodes.Pay attention that the actual range is
#' double.For example,the original number in the node is 4 and the range is 2,there will be a for loop
#' from (4-2) to (4+2).Default is 5.
#'
#' @return A data frame containing the result of crawler.
#'
#'
#' @examples  \dontrun{
#' start_url <- "https://species.wikimedia.org/wiki/Pieridae"
#' crawl_format <- list(first_page = "p:nth-child(5)",
#'                      second_page = "p:nth-child(4)",
#'                      third_page = "p:nth-child(6)",
#'                      forth_page = " i:nth-child(11) a, i:nth-child(10) a, i:nth-child(9) a",
#'                      fifth_page = "div:nth-child(8) , p:nth-child(6)")
#'
#' pre_postfix_list <- list(first_page = c(prefix = "https://species.wikimedia.org", postfix = ""),
#'                          sec_page = c(prefix = "https://species.wikimedia.org", postfix = ""),
#'                          third_page = c(prefix = "https://species.wikimedia.org", postfix = ""),
#'                          fourth_page = c(prefix = "https://species.wikimedia.org", postfix = ""))
#'
#' colnames <- c("sciname", "vernacular_name")
#' df <- web_crawler(start_url, crawl_format, pre_postfix_list, colnames)
#'}
#'
#' @export
web_crawler <- function(starturl, crawl_format, pre_postfix_list, colnames = "", search_range = 5){
  href_nodes <- ""
  for (i in 1:length(crawl_format)){
    href_nodes <- c(href_nodes, crawl_format[[i]])
  }
  href_nodes <- href_nodes[-1]

  prefix = ""
  postfix = ""
  for (i in 1:length(pre_postfix_list)){
    prefix <- c(prefix, pre_postfix_list[[i]][1])
    postfix <- c(postfix, pre_postfix_list[[i]][2])
  }
  prefix <- prefix[-1]
  postfix <- postfix[-1]

  len <- length(href_nodes)
  href <- list(starturl)
  for (i in 1:(len-1)){
    href[[i+1]] <- get_href(prefix[i-1], postfix[i-1], href[[i]], href_nodes[i], search_range)
  }


  len1 <- length(href[[len]])
  len2 <- length(str_split(href_nodes[[len]], ",")[[1]]) + 1
  df <- matrix(c(rep(0, len1*len2)), nrow = len1)
  df <- data.frame(df)
  df_delete <- 0

  for (i in 1:len1){
    new_url <- paste(prefix[len-1], href[[len]][i], postfix[len-1], sep = "")
    new_url <- str_replace_all(new_url, "&amp;", "&")
    #print(new_url)

    content_nodes_list <- str_split(href_nodes[[len]], ",")[[1]]
    content_list <- ""
    for (k in 1:length(content_nodes_list)){
      tryCatch({content <- new_url %>%
        read_html()%>%
        html_nodes(content_nodes_list[k])},
        error = function(e){""})
      content <- str_replace_all(content, "<[^>]*>", "")
      #print(content)
      if (length(content) == 0){
        if (str_detect(content_nodes_list[k], "[0-9]") & search_range > 0){
          content <- search_nodes(new_url, content_nodes_list[k], search_range)
          content_list <- c(content_list, content)
        }
        else {content_list <- c(content_list, " ")}
      }
      else {content_list <- c(content_list, content)}
    }
    content_list <- content_list[-1]

    all_content <- content_list

    if (sum(all_content == " "|all_content == "") != length(all_content)){
      all_content <- c(all_content, new_url)
      #print(all_content)
      if (length(all_content) == (length(content_list)+1)){
        all_content <- str_replace_all(all_content, "^\\s+","")
        all_content <- str_replace_all(all_content, "\\s+$","")
        all_content <- str_replace_all(all_content, "\\s{4,}","")
        df[i,] <- all_content
      }}
    else {df_delete <- c(df_delete, i)}
  }
  if (colnames != ""){
  colnames <- c(colnames, "URL")
  colnames(df) <-  colnames}
  df <- df[-df_delete,]
  return(df)
}


get_href <- function(prefix, postfix, href_list, nodes, search_range = 5){
  url <- paste(prefix, href_list[1], postfix,sep="")
  href <- url %>%
    read_html()%>%
    html_nodes(nodes)
  href <- str_extract_all(href, "href=\"[^\"]*\"")
  href <- unlist(href)
  href <- str_extract_all(href, "\".*\"")
  href <- str_replace_all(href, "\"","")
  href <- str_replace_all(href, "&amp;","&")
  if (length(href_list)>1){
    for (i in 2:length(href_list)){
      url <- paste(prefix, href_list[i], postfix, sep="")

      charac <- str_split(nodes, ",")
      charac <- unlist(charac)
      charac <- str_replace_all(charac, "^\\s+","")
      charac <- str_replace_all(charac, "\\s+$","")
      num <- str_extract_all(charac, "[0-9]+")
      num <- unlist(num)
      charac_nonum <- str_replace_all(charac, "[0-9]+",">")
      unique_clist <- unique(charac_nonum)
      dup_length <- get_duplicated_time(charac_nonum, num)
      nodes_list <- list(charac[1:dup_length[1]])
      charac <- charac[-1:-dup_length[1]]
      if (length(dup_length) > 1){
        for (i in 2:length(dup_length)){
          nodes_list[[i]] <- charac[1:dup_length[i]]
          charac <- charac[-1:-dup_length[i]]
        }}

      num_list <- list(num[1:dup_length[1]])
      num <- num[-1:-dup_length[1]]
      if (length(dup_length) > 1){
        for (i in 2:length(dup_length)){
          num_list[[i]] <- num[1:dup_length[i]]
          num <- num[-1:-dup_length[i]]
        }}

      for (k in 1:length(nodes_list)){
        new_nodes <- list_to_string(nodes_list[[k]])
        #print(new_nodes)
        tryCatch({href_link <- url %>%
          read_html()%>%
          html_nodes(new_nodes)}, error = function(e){""})
        if (length(href_link) == length(nodes_list[[k]])){
          href_link <- str_extract_all(href_link, "href=\"[^\"]*\"")
          href_link <- unlist(href_link)
          href_link <- str_extract_all(href_link, "\".*\"")
          href_link <- str_replace_all(href_link, "\"","")
          href_link <- str_replace_all(href_link, "&amp;","&")
          if (length(href_link != 0)){
            if (sum(str_detect(href_link, ",")) == 0){
              href <- c(href, href_link)
            }}}

        else {
          lower = max(1, (min(as.integer(num_list[[k]])) - search_range))
          upper = max(as.integer(num_list[[1]])) + search_range
          for (m in lower:upper){
            nodes_list[[k]] <- c(nodes_list[[k]], str_replace(unique_clist[k], ">", as.character(m)))
          }
          nodes_list[[k]] <- unique(nodes_list[[k]])
          new_nodes1 <- list_to_string(nodes_list[[k]])
          tryCatch({href_link <- url %>%
            read_html()%>%
            html_nodes(new_nodes1)}, error = function(e){""})
          href_link <- str_extract_all(href_link, "href=\"[^\"]*\"")
          href_link <- unlist(href_link)
          href_link <- str_extract_all(href_link, "\".*\"")
          href_link <- str_replace_all(href_link, "\"","")
          href_link <- str_replace_all(href_link, "&amp;","&")
          if (length(href_link != 0)){
            if (sum(str_detect(href_link, ",")) == 0){
              href <- c(href, href_link)
            }
          }
        }
      }
    }
  }
  href <- unique(href)
  delete_list <- 0

  if (length(prefix) != 0){
    if (str_detect(prefix, "http")){
      for (i in 1:length(href)){
        if (!str_detect(href[i], "[A-z0-9]+") |
            length(str_extract_all(href[i],"https://|http://")[[1]]) > 0){
          delete_list <- c(delete_list, i)
        }
      }
    }

    if (str_detect(prefix, "www")){
      for (i in 1:length(href)){
        if (!str_detect(href[i],"[A-z0-9]+")|
            length(str_extract_all(href[i], "www.")[[1]]) > 0){
          delete_list <- c(delete_list,i)
        }
      }
    }
  }

  if (length(postfix) != 0){
    if (str_detect(postfix, "com")){
      for (i in 1:length(href)){
        if (! str_detect(href[i], "[A-z0-9]+") |
            length(str_extract_all(href[i], ".com")[[1]]) > 0){
          delete_list <- c(delete_list, i)
        }
      }
    }
  }

  if (length(postfix) != 0 & length(prefix) != 0){
    if (!str_detect(postfix, "com") & !str_detect(prefix, "www") & !str_detect(prefix, "http")){
      for (i in 1:length(href)){
        if (!str_detect(href[i], "[A-z0-9]+") |
            length(str_extract_all(href[i], ".com")[[1]]) > 1 |
            length(str_extract_all(href[i], "www.")[[1]]) > 1 |
            length(str_extract_all(href[i], "https://|http://")[[1]]) > 1){
          delete_list <- c(delete_list, i)
        }
      }
    }
  }
  else {
    for (i in 1:length(href)){
      if (! str_detect(href[i], "[A-z0-9]+") |
          length(str_extract_all(href[i], ".com")[[1]]) > 1 |
          length(str_extract_all(href[i], "www.")[[1]]) > 1 |
          length(str_extract_all(href[i], "https://|http://")[[1]]) > 1){
        delete_list <- c(delete_list, i)
      }
    }
  }


  if (length(delete_list) > 1){
    href <- href[-delete_list]
  }
  return(href)
}

list_to_string <- function(x){
  str <- x[1]
  if (length(x) > 1){
    for (i in 2:length(x)){
      str <- paste(str, x[i], sep = ",")
    }
  }
  return(str)
}

search_nodes <- function(url, node, range){
  number_list <- 0
  for (i in 1:range){
    number_list <- c(number_list, i)
    number_list <- c(number_list, -i)
  }
  number_list <- number_list[-1]
  i = 1
  content <- character(0)
  real_content <- " "
  while (i <= (2*range) & length(content) == 0){
    node1 <- str_replace(node, str_extract(node,"[0-9]"),
                         as.character(as.integer(str_extract(node, "[0-9]")) + number_list[i]))
  tryCatch({
    content <- url %>%
      read_html() %>%
      html_nodes(node1)},
    error = function(e){""})
    content <- str_replace_all(content, "<[^>]*>", "")
    if (length(content) != 0){
      real_content <- content
    }
    i = i + 1
  }
  return(real_content)
}

get_duplicated_time <- function(charac_nonum, num){
  unique_clist <- unique(charac_nonum)
  #print(charac_nonum)
  #print(unique_clist)
  #print(num)
  len_a = length(unique_clist)
  dup_length <- c(rep(0, len_a))
  for (j in 1:len_a){
    for (i in 1:length(num)){
      if (charac_nonum[i] == unique_clist[j]){
        dup_length[j] <- i
      }
    }
  }
  if (length(dup_length) > 1){
    for (i in length(dup_length):2){
      dup_length[i] <- dup_length[i] - dup_length[i-1]
    }
  }
  return(dup_length)
}




