
library (tidyr)
library (data.table)
library (highcharter)
library (dplyr)
library(stringr)


population=unhcrdatapackage::end_year_population_totals
code=unhcrdatapackage::reference%>%
  select(iso_3,UNHCRBureau)
population=merge(population,code,by.x = "CountryAsylumCode",by.y = "iso_3",all.x = T)

# Let's shorten long country names in all columns
population[population == "United Kingdom of Great Britain and Northern Ireland"] <- "UK"
population[population == "Iran (Islamic Republic of)"] <- "Iran"
population[population == "United Republic of Tanzania"] <- "Tanzania"
population[population == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
population[population == "Republic of North Macedonia"] <- "North Macedonia"
population[population == "China, Hong Kong Special Administrative Region"] <- "China"
population[population == "Republic of Moldova"] <- "Moldova"
population[population =="Democratic Republic of the Congo"]<-"Congo DR"
population[population =="Syrian Arab Republic"]<-"Syria"
population[population =="United States of America"]<-"USA"
population[population =="Central African Republic"]<-"CAR"

population$CountryOriginName<-as.factor(population$CountryOriginName)
population$CountryOriginCode<-as.factor(population$CountryOriginCode)
population$CountryAsylumName<-as.factor(population$CountryAsylumName)
population$CountryAsylumCode<-as.factor(population$CountryAsylumCode)

# choose the latest year data 
population<-population%>%filter(Year==max(Year))


#First top level

Level1df=population%>%
  group_by(UNHCRBureau)%>%
  summarise(y=sum(REF+ASY+OIP+IDP+STA+OOC))%>%
  mutate(drilldown=toupper(UNHCRBureau))%>%
  rename(name=UNHCRBureau)%>%
  arrange(desc(y))%>%
  mutate(p=paste0((round(prop.table(y)*100,2)),'%')) # get the percentage of the total for column
  
# Second level 
#Lets prepare our dataframe and transpose 
pop=population%>%
  group_by(UNHCRBureau,CountryAsylumName,CountryOriginName)%>%
  summarise(REF=sum(REF),ASY=sum(ASY),OIP=sum(OIP),IDP=sum(IDP),STA=sum(STA),OOC=sum(OOC))

pop2=pop%>%
  gather(type,y ,-UNHCRBureau,-CountryAsylumName,-CountryOriginName)%>%
  rename(name=UNHCRBureau)%>%
  mutate(drilldown=toupper(name), name=toupper(name))

# let create blank list to store Level two drill down data frame
Level2df=list()
for (i in unique(pop2$name)){
  data1=pop2%>%
    filter(name==i) # select each bureau dataset on iteration
  poctype=data1%>%
    group_by(name,type)%>%
    summarise(Total=sum(y))%>%
    filter(Total>0)%>%
    arrange(desc(Total))%>%
    mutate(p=paste0((round(prop.table(Total)*100,2)),'%'))%>%
    top_n(10)
  
  Level2df=append(
    Level2df,list(list(id=(toupper(i)),type="bar",
                          dataLabels = list(enabled = TRUE, format='{point.p}'),
                          data = list_parse(
                                    tibble(
                                      name = poctype$type, y = poctype$Total,p=poctype$p,
                                      drilldown = toupper(paste(i, name, sep = "/"))
                                    )
                                  )
    ))
  )
}


Level3df=list()
for (i in unique(pop2$name)){
  
  data2=pop2%>%
    filter(name==i)
  
  for(j in unique(data2$type)){
    coa=data2%>%
      filter(type==j)%>%
      group_by(CountryAsylumName)%>%
      summarise(Total=sum(y))%>%
      arrange(desc(Total))%>%filter(Total>0) %>%
      mutate(p=paste0((round(prop.table(Total)*100,2)),'%'))%>% 
      top_n(10)
    Level3df=append(
      Level3df,list(list(id = toupper(paste(i, j, sep = "/")), 
                            type = "bar",
                            dataLabels = list(enabled = TRUE, format='{point.p}'),
                                    data = list_parse(
                                      tibble(name = coa$CountryAsylumName,y = coa$Total,p=coa$p,
                                      drilldown = toupper(paste(i,j, name, sep = "/"))
                                    )
      )
      )
)
    )
  }
  
}

#----------------------------------------------------------------------------
# The for loop seems taking longer and got complicated at this moment . lapply came to rescue
#Fourth level drill down
Level4df <- lapply(unique(pop2$name), function(level_A) {
  pop_level_A=pop2%>%
    filter(name==level_A)
  
  lapply(unique(pop_level_A$type), function(level_B) {
    pop_level_A_B=pop_level_A%>%
      filter(type==level_B & type!='IDP',type!='STA') # take out the IDP as the drill down to IDP is the same; County of origin and asylum is the same
    
    lapply(unique(pop_level_A_B$CountryAsylumName), function(level_C) {
      pop_level_A_B_C=pop_level_A_B%>%filter(CountryAsylumName==level_C)%>%
        group_by(CountryOriginName) %>%
        summarize(Total = sum(y))%>%
        arrange(desc(Total))%>%
        mutate(p=paste0((round(prop.table(Total)*100,2)),'%'))%>%
        filter(Total>0)%>%
        top_n(10) # this made the code to run for long while fining the top 10? Any solution?
      
      list(id = toupper(paste(level_A, level_B, level_C, sep = "/")),
           type = "column",
           dataLabels = list(enabled = TRUE, format='{point.p}'),
           data = list_parse(
             tibble(name = pop_level_A_B_C$CountryOriginName,y = pop_level_A_B_C$Total, p = pop_level_A_B_C$p)
             )
           )
    })
  })%>% unlist(recursive = FALSE)
}) %>% unlist(recursive = FALSE)


#Create the chart


highchart() %>%
  hc_chart(
    type = "bar", renderTo= 'container',
    events = list(
    load = JS("function() {console.log(this)}"),
    drilldown = JS("function(e) {this.update({title: {text: e.seriesOptions.id}})}"),
    drillup = JS("function(e) {this.update({title: {text: e.seriesOptions.id}})}"))
    #dataLabels = list(enabled = TRUE, format='{point.p}')
   ) %>%
  hc_xAxis(type = "category") %>%
  hc_add_series(Level1df, "pie",
                hcaes(x = name, y = y),name="UNHCR")%>%
  hc_title(text = "UNHCR") %>%
  hc_subtitle(text= 'Click on slices to drill-down. You will find four level to all except IDP and STA (3 level)')%>%
  hc_plotOptions(pie =list(dataLabels =
                             list(enabled = TRUE,borderRadius= 5,distance= "-30%",
                                  format= "<b>{point.name}</b>: {point.p}"
                             )
                           )
                 )%>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = c(Level2df,
               Level3df,
               Level4df)
    )%>% 
  hc_tooltip(
    pointFormat = "{point.y}",
    useHTML = TRUE,
    valueDecimals = 0
    )%>%
  hc_legend(enabled=FALSE)
