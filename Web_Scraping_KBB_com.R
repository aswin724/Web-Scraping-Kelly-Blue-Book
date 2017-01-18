#-------------------------------------------------------------------------------------------
#  WEB SCRAPPING
#  Gather data on Options,Packages from KBB.com
#__________________________________________________________________________________________
#                   
#     Authors:         Aswin.P
#     Written on:     10.10.2016
#     Modified on:    1.4.2017
#   
#------------------------------------------------------------------------------------------
# What does this code accomplish?

# Pulls the options and packages data from kbb.com and convert it into a consumable format
# This goes as an input to the shiney dashboard which we have created along with this code

#------------------------------------------------------------------------------------------
# Updates:

# 1. 4th Jan - 
# - added Regardless of what the sequence of options/pacakages is we can pull the required data
# - added package description
# 2. 
#------------------------------------------------------------------------------------------
# packages required:
#__________________________________________________________________________________________
#install.packages("XML") ##### XML package - extract from parsed XMl content
#install.packages("stringr")
#install.packages("httr")
#__________________________________________________________________________________________
# Additional code blocks
#   options(java.parameters = "-Xms50g") --- for RAM allocation
#   rm(list=ls(all=TRUE))
#__________________________________________________________________________________________

start.time <- Sys.time()

library(XML)
library(stringr)
library(httr)

options_data_master<-NULL
package_data_master<-NULL

## level 1 - Find the categories - sedan/suv...

parent_url="http://www.kbb.com/"
newrep.doc=htmlParse(rawToChar(GET(parent_url)$content))
links=xpathSApply(newrep.doc,"//*/ul[@class='by-category']/li/a",xmlAttrs)

cat("Child url string for SUV category : " ,links[2]) # should throw the link for SUV

## level 2  - Find the make - bmw, chevvy ...
# Testing it for one Category - Suv - {links[2] }

#for (make in 1:length(links))
#{

#make_url =paste0("http://www.kbb.com",links[make])

make_url =paste0("http://www.kbb.com",links[4])
newrep.doc1=htmlParse(rawToChar(GET(make_url)$content))

links_make_2 =xpathSApply(newrep.doc1,"//*/ul[@class='contentlist make-list no-bull left']/li/a",xmlGetAttr, 'href')

brands_available<-xpathSApply(newrep.doc1,"//*/ul[@class='contentlist make-list no-bull left']/li/a/text()",xmlValue)

print(brands_available)

links_make_3<-links_make_2
## level 3 - Find the model - Cruze, Impala ... 
# Testing it for one make - Chevvy - 

#for (company in 1:length(links_make_3))
#{
  

//*[@id="pageBottom"]/div[1]/div/div[1]/div[2]/a[11]

 # model_url=paste0("http://www.kbb.com",links_make_3[company])
model_url=paste0("http://www.kbb.com",links_make_3[26])
  newrep.doc2=htmlParse(rawToChar(GET(model_url)$content))
  links_model=xpathSApply(newrep.doc2,"//*/div[@class='collapse']/div/div/div/div/a",xmlGetAttr, 'href')
  if (links_model == NULL)
  {
    
    #pageBottom > div.grid-fill.bg-a > div > div.grid-12.bg-a > div:nth-child(2) > a.clearfix.make-page-model.visible
    
    # ----- ISSSSUUUUUUEEEEEEEEEEEEE---------
    #issue
    links_model <- xpathSApply(newrep.doc2,"//*[@id='pageBottom']/div/div/div/div/a[@class='clearfix make-page-model visible']",xmlGetAttr, 'href') 
    links_model <- xpathSApply(newrep.doc2,"//*[@id='pageBottom']/div[1]/div/div[1]/div[@class='with-box']/a[@class='clearfix make-page-model visible']",xmlGetAttr, 'href') 
    links_model <- xpathSApply(newrep.doc2,"//*[@id='pageBottom']/div/div/div/div/a[11]",xmlGetAttr, 'href') 
    links_model <- xpathSApply(newrep.doc2,"//*[@id='pageBottom']/div[1]/div/div[1]/div[@class='with-box']/a[@class='clearfix make-page-model visible']/@href",xmlGetAttr, 'href') 
    }
  # ----- ISSSSUUUUUUEEEEEEEEEEEEE---------  
  # Observed a couple of repeated entries - there are two ways in which you can land on next page
  # so remove duplicates
  links_model<-unique(links_model)
  #print(links_model)
  
  ## level 4 - Find the year version - 2016, 2017 ... 
  # Testing it for one model - Suburban - links_model[2]
  
  for (model in 1:length(links_model))
  {
    
    year_version_url=paste0("http://www.kbb.com",links_model[model])
    newrep.doc3=htmlParse(rawToChar(GET(year_version_url)$content))
    links_year_version=xpathSApply(newrep.doc3,"//*/div[@class='mod-inv']/div/div/a",xmlGetAttr, 'href')
    links_year_version<-unique(links_year_version)
    links_year_version<-links_year_version[links_year_version!="javascript:void(0)"]
    
    
    ## level 5 - Find the style - L,LS,Premier ... 
    # Testing it for one year - 2016 - In links_year_version[2]

    year_counter <- length(links_year_version)
    if (length(links_year_version)==0)
    {
      year_counter<-1
    }
    
    
    for (year in 1:year_counter)
    {
    
      # -- -Sometimes category selection happens (coupe or convertible)  even at this point.....
      
      category2_url=paste0("http://www.kbb.com",links_year_version[year])
      
      if (length(links_year_version)==0)
      {
        category2_url=year_version_url
      }
      
      newrep.doc4.1=htmlParse(rawToChar(GET(category2_url)$content))
      links_category2<-xpathSApply(newrep.doc4.1,"//*/div[@class='mod-single']/div/div/a",xmlGetAttr, 'href')
      links_category2<-unique(links_category2)
      #print(links_style)
      
      links_category2<-links_category2[links_category2!="javascript:void(0)"]
      #-----------------------------------------------------------------
    
      category2_counter<-length(links_category2)
      if (length(links_category2)==0)
      {
        category2_counter<-1
      }
      
        
      for (category_2 in 1: category2_counter)
      {
      
        style_url=paste0("http://www.kbb.com",links_category2[category_2])
        if (length(links_category2)==0)
        {
          if (length(links_year_version)==0)
          {
            style_url=year_version_url
            }
          style_url=category2_url
        }
        
        newrep.doc4=htmlParse(rawToChar(GET(style_url)$content))
        links_style=xpathSApply(newrep.doc4,"//*/div[@class='mod-content expanded-content']/div/div/a",xmlGetAttr, 'href')
        
        links_style<-unique(links_style)
        #print(links_style)
        links_style<-links_style[links_style!="javascript:void(0)"]
        
        #--- THis link has compare charts as an option which we need to get rid of ---
        remove_compare_charts<-(links_style[ grep("compare",links_style)])
        if (length(remove_compare_charts<1)){
          links_style<-links_style[links_style!= remove_compare_charts]
        }
        
        #print("links of style available")
        #print(links_style)
        
        for (trims in 1:length(links_style))
        {
          
          #for options and packages
          target_url=paste0("http://www.kbb.com",links_style[trims])
          newrep.doc5=htmlParse(rawToChar(GET(target_url)$content))
          
          #Facilatating dynamic sequence detection of options and packages
          
          list_of_selection=xpathSApply(newrep.doc5,"//*/div[@class='options-container mod-single']/*/div[1]/span[2]/text()",xmlValue)
          
          sequence_of_options<-grep("options",list_of_selection,ignore.case = T)
          sequence_of_packages<-grep("packages",list_of_selection,ignore.case = T)
          
          #----------------------------------------------------------------------------------------
          #  What - brand, model, year, trim...etc 
          #----------------------------------------------------------------------------------------
          model_year_trim<-paste(strsplit(as.character(links_style[trims]),"/")[[1]][2],strsplit(as.character(links_style[trims]),"/")[[1]][3],strsplit(as.character(links_style[trims]),"/")[[1]][4],strsplit(as.character(links_style[trims]),"/")[[1]][5],sep="_")
          
          make<-strsplit(as.character(links_style[trims]),"/")[[1]][2]
          model_name<-strsplit(as.character(links_style[trims]),"/")[[1]][3]
          model_year<-strsplit(as.character(links_style[trims]),"/")[[1]][4]
          trim<-strsplit(as.character(links_style[trims]),"/")[[1]][5]
          #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          
          #________________________________________________________________________________________
          #----------------------------------------------------------------------------------------
          ##  1. For options
          #----------------------------------------------------------------------------------------
          label=xpathSApply(newrep.doc5,"//*/div[@class='options-container mod-single']/div['sequence_of_options']/div[@class='mod-content expanded-content options-list']/div/span[@class='type']/label",xmlValue)
          mfg=xpathSApply(newrep.doc5,"//*/div[@class='options-container mod-single']/div['sequence_of_options']/div[@class='mod-content expanded-content options-list']/div/span[@class='mfg']",xmlValue)
          invoice=xpathSApply(newrep.doc5,"//*/div[@class='options-container mod-single']/div['sequence_of_options']/div[@class='mod-content expanded-content options-list']/div/div[@class='invoice']/span",xmlValue)
          msrp=xpathSApply(newrep.doc5,"//*/div[@class='options-container mod-single']/div['sequence_of_options']/div[@class='mod-content expanded-content options-list']/div/div[@class='msrp']/span",xmlValue)
          description <- "-"
          option_or_package<-"option"
          
          # to data frame
          options_data=data.frame(model_year_trim,make,model_name,model_year,trim,option_or_package,label,invoice,msrp,description)
          
          options_data_master<-rbind(options_data_master,options_data)
          
          #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          
          #________________________________________________________________________________________
          #----------------------------------------------------------------------------------------
          ##   2. For packages 
          #----------------------------------------------------------------------------------------
          label=xpathSApply(newrep.doc5,"//*/div[@class='options-container mod-single']/div['sequence_of_packages']/div[@class='mod-content expanded-content options-list']/div/span[@class='type']/label",xmlValue)
          mfg=xpathSApply(newrep.doc5,"//*/div[@class='options-container mod-single']/div['sequence_of_packages']/div[@class='mod-content expanded-content options-list']/div/span[@class='mfg']",xmlValue)
          invoice=xpathSApply(newrep.doc5,"//*/div[@class='options-container mod-single']/div['sequence_of_packages']/div[@class='mod-content expanded-content options-list']/div/div[@class='invoice']/span",xmlValue)
          msrp=xpathSApply(newrep.doc5,"//*/div[@class='options-container mod-single']/div['sequence_of_packages']/div[@class='mod-content expanded-content options-list']/div/div[@class='msrp']/span",xmlValue)
          description<-xpathSApply(newrep.doc5,"//*/div[@class='options-container mod-single']/div['sequence_of_packages']/div[@class='mod-content expanded-content options-list']/div/div[@class='more-content']",xmlValue) 
          
          option_or_package<-"package"
          
          # to dataframe
          package_data=data.frame(model_year_trim,make,model_name,model_year,trim,option_or_package,label,invoice,msrp,description)
          
          package_data_master<-rbind(package_data_master,package_data)
        }  
        
      }
    }
  }
#}

#}
options_data_master_1<-options_data_master[c(grep("Wheel",options_data_master$label)),]
options_data_master_2<-options_data_master_1[options_data_master_1$invoice != "no charge", ]
options_data_master_2<-unique(options_data_master_2)


package_data_master_1<-(package_data_master[c(grep("Wheel",package_data_master$description)),])
package_data_master_2<-package_data_master_1[package_data_master_1$invoice != "no charge", ]
package_data_master_2<-unique(package_data_master_2)
#package_data_master_1<-package_data_master


output<-rbind(options_data_master_2,package_data_master_2)

end.time <- Sys.time()
print(end.time-start.time)