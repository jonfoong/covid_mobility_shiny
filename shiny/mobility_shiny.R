library(tidyverse)
library(plotly)
library(shiny)
library(shinythemes)

dl<-'https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip'

temp <- tempfile()
download.file(dl,temp)

sg<-rbind(read_csv(unz(temp,'2020_SG_Region_Mobility_Report.csv')) %>% .[,9:15],
          read_csv(unz(temp,'2021_SG_Region_Mobility_Report.csv')) %>% .[,9:15]) %>%
  pivot_longer(!date,names_to='type',values_to='change')
sg$type<-sg$type %>% str_replace_all('_percent_change_from_baseline','')

yg<-rbind(read_csv(unz(temp,'2020_MM_Region_Mobility_Report.csv')) %>%
  filter(metro_area=='Yangon Metropolitan Area') %>% .[,9:15],
  read_csv(unz(temp,'2021_MM_Region_Mobility_Report.csv')) %>%
    filter(metro_area=='Yangon Metropolitan Area') %>% .[,9:15]) %>%
  pivot_longer(!date,names_to='type',values_to='change')
yg$type<-yg$type %>% str_replace_all('_percent_change_from_baseline','')

lon<-rbind(read_csv(unz(temp,'2020_GB_Region_Mobility_Report.csv'),col_types = 'ccccccccDiiiiii') %>%
  filter(sub_region_1=='Greater London'),
  read_csv(unz(temp,'2021_GB_Region_Mobility_Report.csv'),col_types = 'ccccccccDiiiiii') %>%
    filter(sub_region_1=='Greater London'))
lon<-lon[which(is.na(lon$sub_region_2)),9:15] %>% 
  pivot_longer(!date,names_to='type',values_to='change')
lon$type<-lon$type %>% str_replace_all('_percent_change_from_baseline','')

#specifying dates
#sg
date<-c('2020/04/07','2020/06/02','2020/06/19','2020/12/28')
measure<-c('Circuit<br>breaker','Phase 1','Phase 2','Phase 3')
sgdates<-cbind(date,measure) %>% as.data.frame()
sgdates$date<- sgdates$date%>% as.Date(format='%Y/%m/%d')

#yg
date<-c('2020/03/31','2020/04/18','2020/05/14','2020/09/01','2020/09/22','2021/02/01')
measure<-c('Universal<br>travel ban','Partial<br>lockdown','Lockdown eased','Partial<br>lockdown','Stay-at-home<br>measures','Military coup')
ygdates<-cbind(date,measure) %>% as.data.frame()
ygdates$date<- ygdates$date%>% as.Date(format='%Y/%m/%d')

#lon
date<-c('2020/03/26','2020/07/04','2020/09/14','2020/09/24','2020/11/05','2020/12/02','2020/12/20','2021/1/5')
measure<-c('Lockdown 1','Lockdown<br>eased','Rule-of-six','Restricted<br>biz-hours','Lockdown 2','Tier 2','Tier 4','Lockdown 3')
londates<-cbind(date,measure) %>% as.data.frame()
londates$date<- londates$date%>% as.Date(format='%Y/%m/%d')

## UI
ui<-navbarPage(theme = shinytheme("flatly"),
               "Covid Mobility",
               tabPanel("Singapore",
                        downloadButton("downloadData1", " Export"),
                        plotlyOutput("SG")),
               tabPanel("London",
                        downloadButton("downloadData2", " Export"),
                        plotlyOutput("LON")),
               tabPanel("Yangon",
                        downloadButton("downloadData3", " Export"),
                        plotlyOutput("YG")),
               tabPanel("Understanding the Data",
                       htmlOutput("text"))
)

server<-function(input,output){
  output$downloadData1 <- downloadHandler(
    filename = "sg_covid_mobility.csv",
    content = function(file) {
      write.csv(sg, file)
    }
  )
  output$SG<-renderPlotly({p<-sg %>% plot_ly(x=~date,y=~change,color=~type,type='scatter',mode='lines')%>%
    layout(hovermode="x unified",title = 'Singapore',
           yaxis = list(title = 'Change from baseline (%)',
                        zeroline = TRUE))%>%
    config(displayModeBar = F);
  for (i in 1:(nrow(sgdates))){
    p<-p %>% add_trace(x=sgdates[i,1],
                       type='scatter',mode='lines+text',
                       line = list(dash='dot',
                                   color = 'red',
                                   width=0.5),name='',showlegend=F,hoverinfo='none') %>%
      add_annotations(x=sgdates[i,1],y=max(sg$change)+10,text=sgdates[i,2],showarrow=F,align='center',textangle=270,bgcolor='white')
  };
  p
  }
  )
  output$downloadData2 <- downloadHandler(
    filename = "lon_covid_mobility.csv",
    content = function(file) {
      write.csv(lon, file)
    }
  )
  output$LON<-renderPlotly({p<-lon %>% plot_ly(x=~date,y=~change,color=~type,type='scatter',mode='lines')%>%
    layout(hovermode="x unified",title = 'London',
           yaxis = list(title = 'Change from baseline (%)',
                        zeroline = TRUE))%>%
    config(displayModeBar = F);
  for (i in 1:(nrow(londates))){
    p<-p %>% add_trace(x=londates[i,1],
                       type='scatter',mode='lines+text',
                       line = list(dash='dot',
                                   color = 'red',
                                   width=0.5),name='',showlegend=F,hoverinfo='none') %>%
      add_annotations(x=londates[i,1],y=max(lon$change),text=londates[i,2],showarrow=F,align='center',textangle=270,bgcolor='white')
  };
  p %>% add_trace(x='2020/12/16'%>%as.Date(format='%Y/%m/%d'),
                  type='scatter',mode='lines+text',
                  line = list(dash='dot',
                              color = 'red',
                              width=0.5),name='',showlegend=F,hoverinfo='none') %>%
    add_annotations(x='2020/12/16'%>%as.Date(format='%Y/%m/%d'),y=max(lon$change)-30,text='Tier 3',showarrow=F,align='center',textangle=270,bgcolor='white')
  }
  )
  output$downloadData3 <- downloadHandler(
    filename = "yg_covid_mobility.csv",
    content = function(file) {
      write.csv(yg, file)
    }
  )
  output$YG<-renderPlotly({p<-yg %>% plot_ly(x=~date,y=~change,color=~type,type='scatter',mode='lines')%>%
    layout(hovermode="x unified",title = 'Yangon',
           yaxis = list(title = 'Change from baseline (%)',
                        zeroline = TRUE))%>%
    config(displayModeBar = F);
  for (i in 1:(nrow(ygdates))){
    p<-p %>% add_trace(x=ygdates[i,1],
                       type='scatter',mode='lines+text',
                       line = list(dash='dot',
                                   color = 'red',
                                   width=0.5),name='',showlegend=F,hoverinfo='none') %>%
      add_annotations(x=ygdates[i,1],y=max(yg$change)+30,text=ygdates[i,2],showarrow=F,align='center',textangle=270,bgcolor='white')
  };
  p
  }
  )
  output$text<-renderText(sprintf("The data shows how visitors to (or time spent in) categorized places change compared to Google's baseline days. A baseline day represents a normal value for that day of the week. The baseline day is the median value from the 5 week period Jan 3-Feb 6, 2020. Google will be updating these reports for an unspecified but limited period. As this is a live scraper, the link will stop functioning when Google takes its data offline. Reference dates are supplementary.<br><br><b>For more info on the dataset please visit <a href='https://support.google.com/covid19-mobility/answer/9825414?hl=en&ref_topic=9822927'>this link.</b></a><br><br>Visit the <a href=https://www.linkedin.com/in/jonahfoong/> dashboard creator.</a><br>Link to <a href='https://github.com/jonfoong/covid_mobility_shiny'> Github repo.</a><br><br>Google LLC <i>'Google COVID-19 Community Mobility Reports'</i>.<br>https://www.google.com/covid19/mobility/ Accessed: %s.",Sys.Date()))
}
shinyApp(ui, server)

