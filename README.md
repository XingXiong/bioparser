# Parser and Crawler for Biodiversity Checklists

`bioparser` is an R package that implements locating, extracting, parsing and crawling biodiversity related scientific names and gets an organized structured results.

The package started as a [Google Summer of Code project (2017)](https://summerofcode.withgoogle.com/projects/#5664877317193728) and was mentored by Thomas Vattakaven, Narayani Barve, Vijay Barve, and Rohit George.

## Background
Compiling taxonomic checklists from varied sources of data is a common task that biodiversity informaticians encounter. Data for checklists usually occur within textual formats and significant manual effort is required to extract taxonomic names from a given text into a tabular format. Textual data in sources such as research publications and websites, frequently also contain additional attributes like synonyms, common names, higher taxonomy, and distribution. A facility to quickly extract textual data into tabular lists will facilitate easy aggregation of biodiversity data in a structured format that can be used for further processing and upload onto data aggregation initiatives and help in compiling biodiversity data.

## Installation

This package can be installed from the current GitHub repository using `devtools` in the following manner:

`install.packages("devtools")  # if not installed`

`devtools::install_github("qingyuexu/bioparser")`

## Functions

Brief introductions to the functions are as followed. (For detailed description, usage or examples, please refer to the function documentation in this package.)

- `parse_taxolist(input_file, output_file, location_detail, language, evaluation, config)`:

This function aims to read and parse all text lines from a given file which contains taxonomic names, authors and distribution in each row and write out the tabular output to a csv file. The parsing process can be done automatically or based on the configuration specified by the users. The function can obtain detailed distribution information if 'location_detail' is specified. An evaluation of the parsing result for every entry can be accessed if 'evaluation' is specified.


- `find_taxoname(filepath, filename, type, encoding, output_name)`:

This function locates and extracts taxonomic names from txt, docx, pdf or html files and reorganizes the taxonomy names into a standard order according to scientific naming rules: genus, species, subspecies, author&year, distribution. The function will output the result to a txt file and each row of the file will be one entry of a taxonomic name. The result txt file of this function can be further processed into a tabular format in csv which contains more detailed information using function `parse_taxolist`.


- `recursive_crawler(start_url, crawl_contents, link_urls, pre_postfix_list, output_file)`:

This function crawls, locates and extracts full taxonomic names including order, suborder, family, subfamily, tribe, subtribe, genus, subgenus, species, subspecies, if given. Users need to clarify a full structure of contents for crawling by indicating the corresponding html nodes on different pages. Html nodes which contain urls that lead to the child pages should also be passed into the functions. It can be obtained by using [selectorgadget](http://selectorgadget.com/). Besides, users should also specify the prefixes or postfixes of child pages which are the constant part of child page urls that are not identical with the parent webpage. Note that this function can never be a universal solution for all websites due to the nature of various website structures and contents. Only the ones which apply to the proper structure can be processed with this function. 


- `web_crawler(starturl, crawl_format, pre_postfix_list, colnames, search_range)`:

This function is a supplementary function to `recursive_crawler`. It's more flexible when users need to extract additional contents apart from order, suborder, family, subfamily, tribe, subtribe, genus, subgenus, species, subspecies if specified. The function handles some unusual situations when certain html nodes of identical contents on different pages change unexpectly. For example, html nodes of parent and child webpages which refer to exactly the same contents may change from "p:nth-child(4)" to "p:nth-child(6)" among pages by varying the number in a certain range. In general, this function starts from the top layer page, follows available url links to lower level pages and can only crawl contents on the lowest layer pages, which is the main difference to `recursive_crawler`. Since some useless information may also be grabbed due the changing html node scenarios, further data cleaning by users is strongly recommended.

## Dictionary Sources
In the `find_taxoname' function, the following dictionaries are used to locate taxonomy names and distribution info:
- [GBIF Backbone Taxonomy](https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c)
- [Geographic Names for Geopolitical Areas from GNS (ISO/IEC 10646 Compliant as of 18 July 2002)](http://geonames.nga.mil/gns/html/namefiles.html) (Note: only part of countries with large species number are used in consideration of look-up efficiency. )
