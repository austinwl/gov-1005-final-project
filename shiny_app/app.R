library(shiny)
library(plotly)
library(ggthemes)
library(tidyverse)
library(shinythemes)
library(skimr)
library(patchwork)
library(shinycssloaders)
library(cowplot)
library(gifski)
library(png)
library(gt)

# READING IN DATA FILES

#Data we cleaned and collected:

official_housing <- readRDS("official_housing.RDS") %>%
  mutate(quad = ifelse(house %in% c("Pforzheimer", "Cabot", "Currier"), "Quad", "River"))

#Pivoted dataset that excludes data and assigned for the purpose of saving space.

simplified <- readRDS("simplified.RDS")

#Actual data we collected.

base_data <- readRDS("base_data.RDS") %>% ungroup()

#And that data pivoted.

base_data_pivoted <- base_data %>% 
  pivot_longer(-data, names_to = "community", values_to = "demographics")

#Reading in the files for crimson comparison:

crim_athletes <- readRDS("crimson_comparison/crim_athletes")
crim_ethnicity <- readRDS("crimson_comparison/crim_ethnicity")
crim_finaid <- readRDS("crimson_comparison/crim_finaid")
crim_gender <- readRDS("crimson_comparison/crim_gender")
crim_international <- readRDS("crimson_comparison/crim_international")
crim_legacy <- readRDS("crimson_comparison/crim_legacy")
our_athlete <- readRDS("crimson_comparison/our_athletes")
our_ethnicity <- readRDS("crimson_comparison/our_ethnicity")
our_finaid <- readRDS("crimson_comparison/our_finaid")
our_gender <- readRDS("crimson_comparison/our_gender")
our_international <- readRDS("crimson_comparison/our_international")
our_legacy <- readRDS("crimson_comparison/our_legacy")

#CREATING COMMUNITIES THAT ARE FILTERED BY HOUSE TO MAKE THE PROCESS QUICKER

pfoho <- simplified %>% filter(community == "pfoho") %>% unnest(demographics) %>% ungroup()
currier <- simplified %>% filter(community == "currier") %>% unnest(demographics) %>% ungroup()
cabot <- simplified %>% filter(community == "cabot") %>% unnest(demographics) %>% ungroup()
mather <- simplified %>% filter(community == "mather") %>% unnest(demographics) %>% ungroup()
dunster <- simplified %>% filter(community == "dunster") %>% unnest(demographics) %>% ungroup()
leverett <- simplified %>% filter(community == "leverett") %>% unnest(demographics) %>% ungroup()
quincy <- simplified %>% filter(community == "quincy") %>% unnest(demographics) %>% ungroup()
adams <- simplified %>% filter(community == "adams") %>% unnest(demographics) %>% ungroup()
lowell <- simplified %>% filter(community == "lowell") %>% unnest(demographics) %>% ungroup()
eliot <- simplified %>% filter(community == "eliot") %>% unnest(demographics) %>% ungroup()
kirkland <- simplified %>% filter(community == "kirkland") %>% unnest(demographics) %>% ungroup()
winthrop <- simplified %>% filter(community == "winthrop") %>% unnest(demographics) %>% ungroup()

base_pfoho <- base_data %>% select(pforzheimer) %>% unnest(pforzheimer)
base_currier <- base_data %>% select(currier) %>% unnest(currier)
base_cabot <- base_data %>% select(cabot) %>% unnest(cabot)
base_mather <- base_data %>% select(mather) %>% unnest(mather)
base_dunster <- base_data %>% select(dunster) %>% unnest(dunster)
base_leverett <- base_data %>% select(leverett) %>% unnest(leverett)
base_quincy <- base_data %>% select(quincy) %>% unnest(quincy)
base_adams <- base_data %>% select(adams) %>% unnest(adams)
base_lowell <- base_data %>% select(lowell) %>% unnest(lowell)
base_eliot <- base_data %>% select(eliot) %>% unnest(eliot)
base_kirkland <- base_data %>% select(kirkland) %>% unnest(kirkland)
base_winthrop <- base_data %>% select(winthrop) %>% unnest(winthrop)

#For investigation into gender and ethnicity

ethnicities <- readRDS("ethnicity_results.RDS")
gender <- readRDS("gender_results.RDS")
orientations <- readRDS("orientation_results.RDS")

#Staying with suite mates against size of frosh dorm

suitemate_size_relationship <- readRDS("suitemate_size_relationship.RDS")
varsity_per_block <- readRDS("varsity_per_block.RDS")

#Confidence interval function that accomodates for pivoted data

confidence_interval_pivoted <- function(section, lower_percentile = 0.025, median = 0.5, upper_percentile = 0.975){
  
  selected <- simplified %>% filter(community == section)
  
  percentiles <- tibble(
    percentile = c(lower_percentile, median, upper_percentile),
    
    prop_international = quantile(selected %>% ungroup() %>% select(demographics) %>% unnest(demographics) %>% pull(prop_international),
                                  c(lower_percentile, median, upper_percentile), na.rm = TRUE),
    prop_varsity = quantile(selected %>% ungroup() %>% select(demographics) %>% unnest(demographics) %>% pull(prop_varsity),
                            c(lower_percentile, median, upper_percentile), na.rm = TRUE),   
    prop_legacy = quantile(selected %>% ungroup() %>% select(demographics) %>% unnest(demographics) %>% pull(prop_legacy),
                           c(lower_percentile, median, upper_percentile), na.rm = TRUE),
    prop_financial_aid = quantile(selected %>% ungroup() %>% select(demographics) %>% unnest(demographics) %>% pull(prop_financial_aid),
                                  c(lower_percentile, median, upper_percentile), na.rm = TRUE),
    prop_group_size = quantile(selected %>% ungroup() %>% select(demographics) %>% unnest(demographics) %>% pull(prop_group_size),
                               c(lower_percentile, median, upper_percentile), na.rm = TRUE)
  )
  
  percentiles
  
}

#To speed things up with inputSelect

pull_desired <- function(data, variable){
  
  pull(data, case_when(
    variable == "prop_international" ~ prop_international,
    variable == "prop_varsity" ~ prop_varsity,
    variable == "prop_legacy" ~ prop_legacy,
    variable == "prop_financial_aid" ~ prop_financial_aid,
    variable == "prop_group_size" ~ prop_group_size))
  
}

#For pie chart
prop <- function(list1,list2) {
  length(list1)/length(list2)
}

create_pie <- function(data, title) {
  data %>%
    arrange(desc(Type)) %>%
    mutate(lab.ypos = cumsum(Percentage) - 0.5*Percentage) %>%
    ggplot(aes(x = 2, y = Percentage, fill = Type)) +
    geom_bar(stat="identity", color = "white") +
    coord_polar(theta = "y", start = 0)+
    geom_text(aes( y = lab.ypos, label = paste(Percentage, "%")), color = "Black", family = "Avenir")+
    theme_void()+
    labs(title = title,
         fill = NULL) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text = element_text(family = "Avenir")) +
    theme(title = element_text(family = "Avenir")) + 
    xlim(.5, 2.5)
}


ethnicity <- function(community) {
  w <- which(grepl("White", community$ethnicity))
  a <- which(grepl("Asian", community$ethnicity))
  b <- which(grepl("Black", community$ethnicity))
  hl <- which(grepl("Hispanic/Latinx", community$ethnicity))
  mena <- which(grepl("Middle Eastern/North African", community$ethnicity))
  indna <- which(grepl("Indigenous/Native American", community$ethnicity))
  eth_pref <- which(grepl("Prefer not to say", community$ethnicity))
  total_ethnicity = c(w, a, b, hl, mena, indna, eth_pref)
  
  prop_ethnicity_tibble <- tibble(
    prop_white = prop(w, total_ethnicity),
    prop_asian = prop(a, total_ethnicity),
    prop_black = prop(b, total_ethnicity),
    prop_hl = prop(hl, total_ethnicity),
    prop_mena = prop(mena, total_ethnicity),
    prop_indna = prop(indna, total_ethnicity),
    prop_eth_pref = prop(eth_pref, total_ethnicity)
  )
  
  prop_ethnicity_tibble
}


# for chi_sq tests

chisq_houses <- readRDS("chisq_houses.RDS") 
chisq_neighborhoods <- readRDS("chisq_neighborhoods.RDS")
  

ui <- navbarPage(fluid = T,
                 theme = shinytheme("simplex"),
                 "Blocking Project",
                 tabPanel("Data Validation",
                          titlePanel("Data Validation"),
                          p("To gauge how representative of the class of 2023 our collected data was, we compared the demographics of our collected data to the official demographics that the Harvard Crimson tabulates annually. The Crimson's survey is sent to all Harvard First Years, so if our demographics match up with the Crimson's results, we can be more confident that our data is representative of the class. All in all, our data shows very similar demographic distributions. For Gender, International students, and Financial Aid distributions, percentages differ by less than 2%. With ethnicity, we collected Middle Eastern/North African, which the Crimson did not, and the Crimson collected South Asian, which we did not. However, when South Asian was combined with Asian, and White was combined with Middle Eastern/North African, we saw that our numbers  differed by less than 1%. Our numbers differed significantly with Crimson data in the demographics of legacy students and athletes. However, this is easily explained when considering that the Crimson counted only recruited student athletes, whereas we counted all student athletes, and the Crimson counted anyobody with any related Harvard Alumni as legacy, whereas we did not provide a definition for legacy."),
                          
                          p("Click the dropdown menu to see how our data stacks up against the Crimson's for international students, ethnicity, financial aid, gender, legacy students, and varsity athlete composition of the class of 2023."),
                          
                          p("Side-by-side comparison of our data's demographics vs The Crimson emographics"),
                          selectInput("type",
                                      label = "Select a distribution to compare:",
                                      choices = c("International Students" = "International",
                                                  "Athletes" = "Athlete",
                                                  "Legacy Students" = "Legacy",
                                                  "Financial Aid Students" = "Financial Aid",
                                                  "Ethnicity Distributions" = "Ethnicity",
                                                  "Gender Distributions" = "Gender")
                          ),                          
                          mainPanel(
                            plotOutput("ValGraphs", width = "140%")
                          )),
                 tabPanel("Comparisons",
                          navlistPanel(
                            tabPanel("Comparisons Across Neighborhoods",
                                     selectInput("neighborhood_1", 
                                                 label = "Left-hand Graph",
                                                 choices = c("River West" = "river_west",
                                                             "River Central" = "river_central",
                                                             "River East" = "river_east",
                                                             "River" = "river",
                                                             "Quad" = "quad"),
                                                 selected = "river_east",
                                     ),
                                     selectInput("neighborhood_2",
                                                 label = "Right-hand Graph",
                                                 choices = c("River West" = "river_west",
                                                             "River Central" = "river_central",
                                                             "River East" = "river_east",
                                                             "River" = "river",
                                                             "Quad" = "quad")),
                                     selectInput("variable", 
                                                 label = "Variable Displayed",
                                                 choices = c("International Students" = "prop_international",
                                                             "Varsity Students" = "prop_varsity",
                                                             "Legacy Students" = "prop_legacy",
                                                             "Financial Aid Students" = "prop_financial_aid",
                                                             "Blocking Group Sizes" = "prop_group_size")),
                                     mainPanel(
                                       p("Pick two neighborhoods (we added the River as a section for your convenience) and a variable to view a side-by-side comparison! Turquoise bars represent the actual values we calculated through data collection, while the red bars represent 95% confidence intervals we calculated by running 500 replicates of a randomized housing day."),
                                       plotOutput("graphsTogether", width = "150%") %>%
                                         withSpinner(color="#0dc5c1"),
                                       br(),
                                       br()
                                     )),
                            tabPanel("Comparisons Across Houses",
                                     selectInput("variable2",
                                                 label = "Variable Displayed",
                                                 choices = c("International Students" = "prop_international",
                                                             "Varsity Athletes" = "prop_varsity",
                                                             "Students with Legacy" = "prop_legacy",
                                                             "Students on Financial Aid" = "prop_financial_aid",
                                                             "Blocking Group Sizes" = "prop_group_size")
                                     ),
                                     p("Select a variable to see its distributions across all 12 upperclassmen houses! Turquoise bars represent the actual values we calculated through data collection, while the red bars represent 95% confidence intervals we calculated by running 500 replicates of a randomized housing day."),
                                     mainPanel(
                                       plotOutput("allHouses", width = "160%", height = "500px") %>%
                                         withSpinner(color="#0dc5c1"),
                                       br(),
                                       br()
                                     ))
                            )),
                tabPanel("Other Trends",
                         navlistPanel(
                           tabPanel("Self-Segregation",
                         titlePanel("Self-Segregation by Race"),
                         p("We wanted to investigate whether students self-segregated during the blocking process. We only showed analysis for variables and groups that contained enough data for meaningful conclusions."),
                         p("Our first analysis, conducted below, shows that there is some degree of self-segregation. Of all the blocking groups that contained at least one Asian student, more than twenty percent of them were comprised entirely of Asian students. On the other hand, less than ten percent of the blocking groups that contained white students were entirely white."),
                         plotOutput("segregationGraphs", width = "110%") %>%
                  withSpinner(color="#0dc5c1"),
                  titlePanel("Self-Segregation by Sexual Orientation"),
                  p("We were also curious about self-segregation among students of different sexual orientations. There was a striking divide between blocking groups that contained homosexual students and those that contained heterosexual students. The majority of groups containing at least one heterosexual member were comprised entirely of heterosexual students, while the majority of blocking groups with at least one homosexual member were less than 50% composed of homosexual members."),
                  plotOutput("sexualOrientationGraphs", width = "110%") %>%
                    withSpinner(color="#0dc5c1"),
                  titlePanel("Self-Segregation by Gender"),
                  p("
                    Finally, we also investigated gender distribution across blocking groups and found segregation occured in that realm also. 40 percent of blocking groups that contained a member of one gender were comprised entirely of that gender, a trend that was found in both the male and female genders."),
                  plotOutput("genderGraphs", width = "110%") %>%
                    withSpinner(color="#0dc5c1"),
                  br(),
                  br()
                  ),
                  tabPanel("Correlations",
                             titlePanel("Blocking with your Suitemates"),
                                      p("We wanted to see if the size of a given freshman dorm impacted whether suitemates from the dorm decided to block together. The data shows a moderately strong negative correlation (r = -0.5324) between dorm size and the percentage of suitemates from each dorm that blocked together; students were more likely to block with their suitemate/multiple suitemates if they lived in a smaller freshman dorm."),
                                     plotOutput("suitemateSizeRelationship") %>% 
                                       withSpinner(color="#0dc5c1")),
                  tabPanel("Miscellaneous",
                           titlePanel("Freshman Dorm"),
                           p("In the visualization below, we have house placements arranged by freshman dorm. 100 percent of Massachusetts Hall residents were placed into a river house, but this number shouldn't be weighed too heavily due to the small population size (14 members). Massachusetts Hall was followed by Apley Court, which had roughly 86 percent of its members placed into a river house. Greenough had the most of its students quadded, with about forty percent of its members heading for the quad next year."),
                           plotOutput("whereDoTheyGo") %>%
                             withSpinner(color="#0dc5c1"),
                           titlePanel("Varsity Athletes per Blocking Group"),
                           p("Perhaps the most popular housing day theory is that athletes are most likely to be placed in a river house. However, this year, Currier House had the highest number, on average, of varsity athletes per blocking group placed into that house."),
                  plotOutput("varsityPerBlock") %>% withSpinner(color="#0dc5c1"),
                  br(),
                  br()))),
                
                 tabPanel("Discussion",
                          titlePanel("Data Collection"),
                          p("To collect our data, we sent an email three times to all first-years at the College. We used a total of 16 variables from the survey: 3 unique identifiers (name, blocking group name, blocking group leader), 9 individual variables (freshman dorm, gender, ethnicity, religion, sexual orientation, international student, varsity status, legacy, financial aid), and 4 group variables (blocking group size, blocking group members, house placement, whether students blocked with suitemates)."),
                          p("We collected 583 (35.6% of the 1637 first-years) responses from the survey. Respondents provided us with the names of their blocking group members, so we were able to match 1180 students (72.1%) to their blocking groups. Additionally, we scraped information from athletic roasters so we were able to collect information on everyone at the college. Although we planned to collect responses in first-year common spaces such as the dining hall, we were unable to following campus evacuation (due to COVID-19). While this reduced the total number of responses we could collect, it still gave us a large enough dataset to produce meaningful analysis."),
                          p("We made sure there was no double counting in the list of 1180 students (there was potential for overlap between the 583 students that filled out the survey and the names of the blocking group members we pulled from each person's response). To do so, we matched every respondent and blocking group member to their result in the Harvard College Facebook. We then took out any duplicates once spelling was standardized. Due to the sensitive information collected in this survey like financial aid status and sexual orientation, we decided not to make the data publicly available."),
                          titlePanel("Randomization Process"),
                          p("To simulate the housing process, we [1] identified blocking groups, [2] selected blocking groups at random and assigned them to a house until the house was full (we referenced official house sizes to determine how many students to assign to the house), [3] repeated step 2 for each house. Once we completed the assignment process, we generated summary statistics for the house using the 9 individual variables and the group size. We repeated this process 500 times and created confidence intervals for each variable using the summary statistics for each house. We also generated summary statistics and confidence intervals for each neighborhood (the \"River\" as a whole was added as a single neighborhood - even though it technically consists of three neighborhoods: River West, River Central, and River East - to compare the River to the Quad more conveniently)."),
                          titlePanel("Goodness of Fit"),
                          p("Once we generated the confidence intervals and sample means from the simulated housing process, we compared the results to the official housing results. We generated summary statistics for each house and neighborhood with the same methodology used for the random housing process (we used the official housing assignments and collected the same summary statistics). To determine whether Harvard's housing process was \"fair,\" we checked if the variable distributions from the official housing assignments were comparable to the results of the 500 random simulations we ran. For this comparison we used a chi-squared test and confidence intervals."),
                          h4("Confidence Intervals"),
                          p("As can be seen on the \"Comparisons\" tab, there are very few variables that fall outside of the 95% confidence intervals we generated. The following exceptions were noted:"),
                          tags$ol(
                            tags$li("International Students low in Cabot: 2.6%"),
                            tags$li("Financial Aid low in Eliot: 44.6%"),
                            tags$li("Group Size large in Adams: 6.86"),
                            tags$li("Group Size large in Mather: 6.92"),
                            tags$li("Financial Aid low in River West: 51.4%")
                          ),
                          h4("Chi-squared Test"),
                          p("Since this project is heavy in raw data collection, there are possibilities for error from the respondents. Although we tried to incentivize respondents with gift cards, there might not have been strong enough of an incentive for respondents to provide accurate information, and in the rare case some might have even put in inaccurate information to blow the end result. Some survey questions regarding personal information were sensitive, and we were therefore unable to get a large sample of results that would be the perfect representation of the Class of 2023. Further, possible errors included misspellings of textual data when trying to match the names of individuals. It was common to have individual repsondents misspell the names of their block mates and their blocking group leader's names. Variations in spacing, punctuations and capitalization also required extensive data cleaning in order to provide the most accurate interpretation of the data possible."),
                          gt_output("chisq_houses"),
                          gt_output("chisq_neighborhoods")
                          ),
                
                
                 tabPanel("About",
                   fluidRow(
                     column(6,
                            h1("About")
                     )
                   ),
                   fluidRow(
                     column(12,
                            h3("Project Background and Motivations")
                     )
                   ),
                   fluidRow(
                     column(12,
                            p("Housing Day at Harvard College is one of the most thrilling and dramatic days of the school year, and we wanted to wield data as a tool for tackling some of the myths and stereotypes about housing day.
               This project aims to continue the work of the previous blocking group project with more rigorous data analytics and statistical computation, more intuitive graph design, and additional questions (including sexual orientation). It will hopefully build on the previous analysis attempting to find any discrepancies.
              

The project will attempt to replicate a truly random housing lottery to assess if Harvard’s housing appears to be random as well. The project will check variables such as legacy, race, religion, athletics, sex, freshman dorm, financial aid, international students, and blocking group size.

All Sensitive questions have a “prefer not to answer” option.")
                     )
                   ),
                   fluidRow(
                     column(3,
                            h3("About Us")
                     )
                   ),
                   fluidRow(
                     column(12,
                            p("We are a group of nine students who sought to continue the work of GOV 1005 students from last year. We are
             Jamal Nimer, Carina Peng, Ilyas Mardin, Shojeh Liu, Eliot Min, Lucy He, Angie Shin, Austin Li, and Sam Saba.
               ")
                     )
                   ),
                   fluidRow(
                     column(4,
                            imageOutput("Jamal")),
                     column(4,
                            imageOutput("Angie")),
                     column(4,
                            imageOutput("Austin"))
                   ),
                   fluidRow(
                     column(4,
                            p("Hi, I’m Jamal, a member of the class of 2023 from Chicago, IL! I'm interested in data science and its applications to international development. Besides R, I enjoy biking and playing card games.")
                     ),
                     column(4,
                            p("Hey, my name is Angie and I’m a first-year at the College! I like data analytics because it lets me study how people interact with each other in different environments. You can always find me grumbling about how cereal is indeed a soup or why Christmas songs should be played all year long!")
                     ),
                     column(4,
                            p("My name is Austin Li, and I am planning on studying Physics/Math with a secondary in Computer Science. I’m interested in Asian American civic engagement and outside of class, I enjoy writing for The Crimson, playing volleyball, and playing board games! Feel free to contact me at awli@college.harvard.edu")
                     )                     
                   ),
                   fluidRow(
                     column(4,
                            imageOutput("Carina"),
                            "Hello! I’m Carina, a rising sophomore from the best city Chicago and living in the best house Dunster. I was lucky to be working with a great team to investigate Harvard’s housing lottery system, and I hope to use data science in the future on issues of international development and public health. I love traveling and snacking on roasted coconut chips"
                     ),
                     column(4,
                            imageOutput("Ilyas"),
                            p("Hi I’m Ilyas, a member of the class of 2023! I hope to use my interest in data science to more effectively implement medicines and antibiotics in the future. Some of my favourite things are Lauv, tennis, and exploring new places to eat! Feel free to get in touch with me at ilyasmardin@college.harvard.edu.")
                     ),
                     column(4,
                            imageOutput("Lucy"),
                            p("Hi! I’m Lucy He, a current freshman at Harvard College thinking about concentrating in computer science & math. Besides data science, I’m also a big fan of musical theater, urban planning, and more ;) You can reach me at luxihe@college.harvard.edu")
                     )

                   ),
                   fluidRow(
                     column(4,
                            imageOutput("Sam"),
                            p("My name is Sam (Sam'aan) Saba, and I am a rising Palestinian-American sophomore from Detroit, MI intending to concentrate in Social Studies and Near Eastern Languages and Civilizations! Though I love spending time on data analysis and visualizations, I also work with the Arab Conference at Harvard, Harvard's Act on a Dream (the college's immigrant-advocacy organization), and the Society of Arab Students! Besides my extracurriculars, I love exploring languages (he's been working on his classical Arabic), eating white chocolate, and watching Avatar: The Last Airbender. Some of my interests include: reading religious texts, writing, learning about his culture, and spending countless hours on TikTok ;)!")
                     ),
                     column(4,
                            imageOutput("Eliot"),
                            p("Hello! My name is Eliot and I’m a member of the class of 2023 here at Harvard. I hope to use my newly learned data analyzing skills to investigate speculative claims and unsubstantiated theories, as we did with Harvard College’s housing day. Outside of data science, I’m an a cappella junkie, diehard New York Mets fan, and sushi addict.")
                     ),
                     column(4,
                            imageOutput("Shojeh"),
                            p("Hi, I’m Shojeh, a member of the class of 2023! I hope to use my data analysis skills in the future to study how humans work together. My favorite things are Settlers of Catan, Sushi, and the web serial Worm!")
                     )
                   

                          )))


server <- function(input, output) {
  
  
  output$ValGraphs <- renderPlot({
    
    type <- case_when(
      input$type == "International" ~ 1,
      input$type == "Athlete" ~ 2,
      input$type == "Legacy" ~ 3,
      input$type == "Financial Aid" ~ 4,
      input$type == "Ethnicity" ~ 5,
      input$type == "Gender" ~ 6) 
    
    if(type == 1) {
      our_graph <- create_pie(our_international, "Our Data")
      crim_graph <- create_pie(crim_international, "Crimson Data")
    } else if (type == 2) {
      our_graph <- create_pie(our_athlete, "Our Data") +
        labs(caption = "  
 ")
      crim_graph <- create_pie(crim_athletes, "Crimson Data") +
        labs(caption = "Crimson Athlete data only includes recruited student athletes.* 
This likely causes the discrepancy seen here.")     
    } else if (type == 3) {
      our_graph <- create_pie(our_legacy, "Our Data") +
        labs(caption = "  
 ")
      crim_graph <- create_pie(crim_legacy, "CrimsonData") +
        labs(caption = "Crimson Legacy data includes all relatives, including siblings.* 
This likely causes the discrepancy seen here.")
    } else if (type == 4) {
      our_graph <- create_pie(our_finaid, "Our Data")
      crim_graph <- create_pie(crim_finaid, "Crimson Data")      
    } else if (type == 5) {
      our_graph <- create_pie(our_ethnicity, "Our Data") + scale_fill_manual(values=c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#7732a8", "#F564E3"))
      crim_graph <- create_pie(crim_ethnicity, "Crimson Data")      
    } else if (type == 6) {
      our_graph <- create_pie(our_gender, "Our Data")
      crim_graph <- create_pie(crim_gender, "Crimson Data")      
    }
    
    plot_grid(our_graph, crim_graph)  
    
  }) 
  
  
  output$graphsTogether <- renderPlot({
    
    xscale <- case_when(
      input$variable == "prop_international" ~ c(.05, .25),
      input$variable == "prop_varsity" ~ c(.10, .25),
      input$variable == "prop_legacy" ~ c(.1, .25),
      input$variable == "prop_financial_aid" ~ c(.4, .8),
      input$variable == "prop_group_size" ~ c(5, 7)
    )
    
    xlabel <- case_when(
      input$variable == "prop_international" ~ "Percentage of International Students",
      input$variable == "prop_varsity" ~ "Percentage of Varsity Students",
      input$variable == "prop_legacy" ~ "Percentage of Legacy Students",
      input$variable == "prop_financial_aid" ~ "Percentage of Students on Financial Aid",
      input$variable == "prop_group_size" ~ "Average blocking group size"
      )
    
    filtered1 <- simplified %>%
      filter(community == input$neighborhood_1) %>%
      mutate(prop = map(demographics, ~pull_desired(., input$variable))) %>%
      unnest(prop) %>%
      ungroup(replicate)
    
    filtered2 <- simplified %>%
      filter(community == input$neighborhood_2) %>%
      mutate(prop = map(demographics, ~pull_desired(., input$variable))) %>%
      unnest(prop) %>%
      ungroup(replicate)
    
    conf.int1 <- confidence_interval_pivoted(input$neighborhood_1) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable)
    
    conf.int2 <- confidence_interval_pivoted(input$neighborhood_2) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable)
    
    base_value1 <- base_data_pivoted %>% 
      filter(community == input$neighborhood_1) %>%
      unnest(demographics) %>%
      pull_desired(., input$variable)
    
    base_value2 <- base_data_pivoted %>% 
      filter(community == input$neighborhood_2) %>%
      unnest(demographics) %>%
      pull_desired(., input$variable)
    
    graph1 <- 
      ggplot(filtered1, aes(x = prop)) +
      geom_histogram(binwidth = case_when(
        input$variable != "prop_group_size" ~ .01,
        TRUE ~ .25)) +
      geom_vline(xintercept  = conf.int1[1], color = "#F8766D") + 
      geom_vline(xintercept  = conf.int1[2], color = "#F8766D") +
      geom_vline(xintercept = base_value1,
                 color = "#00BFC4") +
      labs(title = paste("Showing Data for", case_when(
        input$neighborhood_1 == "quad" ~ "The Quad",
        input$neighborhood_1 == "river" ~ "The River",
        input$neighborhood_1 == "river_east" ~ "River East",
        input$neighborhood_1 == "river_west" ~ "River West",
        input$neighborhood_1 == "river_central" ~ "River Central")
      ),
      x = xlabel,
      y = "Replicates",
      subtitle = "Red bars represent confidence intervals") + 
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    if(input$variable != "prop_group_size"){
      graph1 <- graph1 + scale_x_continuous(limits = xscale, labels = scales::percent)
    }
    else{
      graph1 <- graph1 + scale_x_continuous(limits = xscale)
    }
    
    
    graph2 <- ggplot(filtered2, aes(x = prop)) +
      geom_histogram(binwidth = case_when(
        input$variable != "prop_group_size" ~ .01,
        TRUE ~ .25)) +
      geom_vline(xintercept  = conf.int2[1], color = "#F8766D") + 
      geom_vline(xintercept  = conf.int2[2], color = "#F8766D") + 
      geom_vline(xintercept = base_value2,
                 color = "#00BFC4") +
      labs(title = paste("Showing Data for", case_when(
        input$neighborhood_2 == "quad" ~ "The Quad",
        input$neighborhood_2 == "river" ~ "The River",
        input$neighborhood_2 == "river_east" ~ "River East",
        input$neighborhood_2 == "river_west" ~ "River West",
        input$neighborhood_2 == "river_central" ~ "River Central")
      ),
      x = xlabel,
      y = "Replicates",
      subtitle = "Red bars represent confidence intervals") + 
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    if(input$variable != "prop_group_size"){
      graph2 <- graph2 + scale_x_continuous(limits = xscale, labels = scales::percent)
    }
    else{
      graph2 <- graph2 + scale_x_continuous(limits = xscale)
    }
    
    plot_grid(graph1, graph2)
    
  })
  
  
#CREATING A 4X3 GRID OF ALL THE HOUSES
  
  output$allHouses <- renderPlot({
  
    xlabel <- case_when(
      input$variable2 == "prop_international" ~ "Percentage of International Students",
      input$variable2 == "prop_varsity" ~ "Percentage of Varsity Students",
      input$variable2 == "prop_legacy" ~ "Percentage of Legacy Students",
      input$variable2 == "prop_financial_aid" ~ "Percentage of Students on Financial Aid",
      input$variable2 == "prop_group_size" ~ "Average Blocking Group Size"
    )
    
    ylabel <- "Replicates"
    
    xscale <- case_when(
      input$variable2 == "prop_international" ~ c(0, .3),
      input$variable2 == "prop_varsity" ~ c(0, .3),
      input$variable2 == "prop_legacy" ~ c(.05, .3),
      input$variable2 == "prop_financial_aid" ~ c(.4, .8),
      input$variable2 == "prop_group_size" ~ c(4, 8)
    )
    
    binvalue <- case_when(
      input$variable2 != "prop_group_size" ~ .01,
      TRUE ~ .25
      )
    
    pfoho_conf.int <- confidence_interval_pivoted("pfoho") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    pfoho_graph <- ggplot(pfoho, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
        geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel,
           y = ylabel,
           title = "Pfoho") + 
      geom_vline(xintercept = pfoho_conf.int[1], color = "#F8766D") + 
      geom_vline(xintercept = pfoho_conf.int[2], color = "#F8766D") +
      geom_vline(xintercept = pull_desired(base_pfoho, input$variable2), 
                 color = "#00BFC4") +
        theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    currier_conf.int <- confidence_interval_pivoted("currier") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    currier_graph <- ggplot(currier, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Currier") + 
      geom_vline(xintercept = currier_conf.int[1], color = "#F8766D") + 
      geom_vline(xintercept = currier_conf.int[2], color = "#F8766D") +
      geom_vline(xintercept = pull_desired(base_currier, input$variable2), 
                 color = "#00BFC4") +
        theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    cabot_conf.int <- confidence_interval_pivoted("cabot") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    cabot_graph <- ggplot(cabot, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Cabot") + 
      geom_vline(xintercept = cabot_conf.int[1], color = "#F8766D") + 
      geom_vline(xintercept = cabot_conf.int[2], color = "#F8766D") +
      geom_vline(xintercept = pull_desired(base_cabot, input$variable2), 
                 color = "#00BFC4") +
        theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    mather_conf.int <- confidence_interval_pivoted("mather") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    mather_graph <- ggplot(mather, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Mather") + 
      geom_vline(xintercept = mather_conf.int[1], color = "#F8766D") + 
      geom_vline(xintercept = mather_conf.int[2], color = "#F8766D") +
      geom_vline(xintercept = pull_desired(base_mather, input$variable2), 
                 color = "#00BFC4") +
        theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    leverett_conf.int <- confidence_interval_pivoted("leverett") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    leverett_graph <- ggplot(leverett, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Leverett") + 
      geom_vline(xintercept = leverett_conf.int[1], color = "#F8766D") + 
      geom_vline(xintercept = leverett_conf.int[2], color = "#F8766D") +
      geom_vline(xintercept = pull_desired(base_leverett, input$variable2), 
                 color = "#00BFC4") +
        theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
      
    dunster_conf.int <- confidence_interval_pivoted("dunster") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
   dunster_graph <- ggplot(dunster, aes(x = case_when(
     input$variable2 == "prop_international" ~ prop_international,
     input$variable2 == "prop_varsity" ~ prop_varsity,
     input$variable2 == "prop_legacy" ~ prop_legacy,
     input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
     input$variable2 == "prop_group_size" ~ prop_group_size))) +
     geom_histogram(binwidth = binvalue) + 
     labs(x = xlabel, 
          y = ylabel,
          title = "Dunster") + 
     geom_vline(xintercept = dunster_conf.int[1], color = "#F8766D") + 
     geom_vline(xintercept = dunster_conf.int[2], color = "#F8766D") +
     geom_vline(xintercept = pull_desired(base_dunster, input$variable2), 
                color = "#00BFC4") +
      theme_classic() + 
     theme(plot.title = element_text(hjust = 0.5)) +
     theme(text = element_text(family = "Avenir")) +
     theme(title = element_text(family = "Avenir")) 
    
    eliot_conf.int <- confidence_interval_pivoted("eliot") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    eliot_graph <- ggplot(eliot, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Eliot") + 
      geom_vline(xintercept = eliot_conf.int[1], color = "#F8766D") + 
      geom_vline(xintercept = eliot_conf.int[2], color = "#F8766D") +
      geom_vline(xintercept = pull_desired(base_eliot, input$variable2), 
                 color = "#00BFC4") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    kirkland_conf.int <- confidence_interval_pivoted("kirkland") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    kirkland_graph <- ggplot(kirkland, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Kirkland") + 
      geom_vline(xintercept = kirkland_conf.int[1], color = "#F8766D") + 
      geom_vline(xintercept = kirkland_conf.int[2], color = "#F8766D") + 
      geom_vline(xintercept = pull_desired(base_kirkland, input$variable2), 
                 color = "#00BFC4") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 

    winthrop_conf.int <- confidence_interval_pivoted("winthrop") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)

    winthrop_graph <- ggplot(winthrop, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Winthrop") +
      geom_vline(xintercept = winthrop_conf.int[1], color = "#F8766D") + 
      geom_vline(xintercept = winthrop_conf.int[2], color = "#F8766D") +
      geom_vline(xintercept = pull_desired(base_winthrop, input$variable2), 
                 color = "#00BFC4") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 

    adams_conf.int <- confidence_interval_pivoted("adams") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    adams_graph <- ggplot(adams, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Adams") + 
      geom_vline(xintercept = adams_conf.int[1], color = "#F8766D") + 
      geom_vline(xintercept = adams_conf.int[2], color = "#F8766D") +
      geom_vline(xintercept = pull_desired(base_adams, input$variable2), 
                 color = "#00BFC4") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    lowell_conf.int <- confidence_interval_pivoted("lowell") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    lowell_graph <- ggplot(lowell, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      geom_vline(xintercept = lowell_conf.int[1], color = "#F8766D") + 
      geom_vline(xintercept = lowell_conf.int[2], color = "#F8766D") +
      geom_vline(xintercept = pull_desired(base_lowell, input$variable2), 
                 color = "#00BFC4") +
      labs(x = xlabel, 
           y = ylabel,
           title = "Lowell") + 
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    
    quincy_conf.int <- confidence_interval_pivoted("quincy") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    
    quincy_graph <- ggplot(quincy, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel,
           y = ylabel,
           title = "Quincy") + 
      geom_vline(xintercept = quincy_conf.int[1], color = "#F8766D") + 
      geom_vline(xintercept = quincy_conf.int[2], color = "#F8766D") +
      geom_vline(xintercept = pull_desired(base_quincy, input$variable2), 
                 color = "#00BFC4") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    if(input$variable2 != "prop_group_size"){
      
      cabot_graph <- cabot_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      currier_graph <- currier_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      pfoho_graph <- pfoho_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      adams_graph <- adams_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      lowell_graph <- lowell_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      quincy_graph <- quincy_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      eliot_graph <- eliot_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      winthrop_graph <- winthrop_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      kirkland_graph <- kirkland_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      mather_graph <- mather_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      dunster_graph <- dunster_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      leverett_graph <- leverett_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      
    }
    
    else{
      cabot_graph <- cabot_graph + scale_x_continuous(limits = xscale)
      currier_graph <- currier_graph + scale_x_continuous(limits = xscale)
      pfoho_graph <- pfoho_graph + scale_x_continuous(limits = xscale)
      adams_graph <- adams_graph + scale_x_continuous(limits = xscale)
      lowell_graph <- lowell_graph + scale_x_continuous(limits = xscale)
      quincy_graph <- quincy_graph + scale_x_continuous(limits = xscale)
      eliot_graph <- eliot_graph + scale_x_continuous(limits = xscale)
      winthrop_graph <- winthrop_graph + scale_x_continuous(limits = xscale)
      kirkland_graph <- kirkland_graph + scale_x_continuous(limits = xscale)
      mather_graph <- mather_graph + scale_x_continuous(limits = xscale)
      dunster_graph <- dunster_graph + scale_x_continuous(limits = xscale)
      leverett_graph <- leverett_graph + scale_x_continuous(limits = xscale)
    }
    
    plot_grid(currier_graph, cabot_graph, pfoho_graph,
              eliot_graph, kirkland_graph, winthrop_graph,
              mather_graph, dunster_graph, leverett_graph,
              adams_graph, quincy_graph, lowell_graph, nrow = 4, ncol = 3)
    
  })
  
  output$segregationGraphs <- renderPlot({
    
    asians <- ggplot(ethnicities %>% filter(prop_asian > 0) %>% count(prop_asian), aes(x = prop_asian, y = n/46)) +
      geom_col(width = .05) +
      scale_x_continuous(limits = c(.1, 1.1), breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1), labels = scales::percent) +
      scale_y_continuous(limits = c(0, .25), labels = scales::percent) +
      labs(x = "Percentage of Asian students within blocking group", 
           y = "Percentage of Blocking Groups",
           title = "Composition of Blocking Groups containing Asian students",
           subtitle = "46 blocking groups contained at least one Asian student") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    whites <- ggplot(ethnicities%>%filter(prop_white > 0) %>% count(prop_white), aes(x = prop_white, y = n/57)) +
      geom_col(width = .05) +
      scale_x_continuous(limits = c(.1, 1.1), breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1), labels = scales::percent) +
      scale_y_continuous(limits = c(0, .25), labels = scales::percent) +
      labs(x = "Percentage of White students within blocking group", 
           y = "Percentage of Blocking Groups",
           title = "Composition of Blocking Groups containing White students",
           subtitle = "57 blocking groups contained at least one White student") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    plot_grid(asians, whites)
    
    
  })
  
  output$genderGraphs <- renderPlot({
    
    females <- ggplot(gender %>% filter(prop_female > 0) %>% count(prop_female), aes(x=prop_female, y = n/55)) + 
      geom_col(width = .05) + 
      scale_x_continuous(limits = c(.1, 1.1), breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1), labels = scales::percent) +
      scale_y_continuous(limits = c(0, .5), labels = scales::percent) +
      labs(x = "Percentage of female students within blocking group", 
           y = "Percentage of blocking groups",
           title = "Composition of blocking groups containing female students",
           subtitle = "55 blocking groups contained at least one female student") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    males <- ggplot(gender %>% filter(prop_male > 0) %>% count(prop_male), aes(x=prop_male, y = n/49)) + 
      geom_col(width = .05) + 
      scale_x_continuous(limits = c(.1, 1.1), breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1), labels = scales::percent) +
      scale_y_continuous(limits = c(0, .5), labels = scales::percent) +
      labs(x = "Percentage of male students within blocking group", 
           y = "Percentage of blocking groups",
           title = "Composition of blocking groups containing male students",
           subtitle = "49 blocking groups contained at least one male student") +
      theme_classic()  + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    plot_grid(females, males)
                    
  })
  
  output$sexualOrientationGraphs <- renderPlot({
    
    heterosexuals <- ggplot(orientations %>% filter(prop_heterosexual > 0) %>% count(prop_heterosexual), aes(x=prop_heterosexual, y = n/70)) + 
      geom_col(width = .05) + 
      scale_x_continuous(limits = c(.1, 1.1), breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1), labels = scales::percent) +
      scale_y_continuous(limits = c(0, .6), labels = scales::percent) +
      labs(x = "Percentage of heterosexual students within blocking group", 
           y = "Percentage of blocking groups",
           title = "Composition of blocking groups containing heterosexual students",
           subtitle = "70 blocking groups contained at least one heterosexual student") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    homosexuals <- ggplot(orientations %>% filter(prop_homosexual > 0) %>% count(prop_homosexual), aes(x=prop_homosexual, y = n/20)) + 
      geom_col(width = .05) + 
      scale_x_continuous(limits = c(.1, 1.1), breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1), labels = scales::percent) +
      scale_y_continuous(limits = c(0, .6), labels = scales::percent) +
      labs(x = "Percentage of homosexual students within blocking group", 
           y = "Percentage of blocking groups",
           title = "Composition of blocking groups containing homosexual students",
           subtitle = "20 blocking groups contained at least one homosexual student") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
    plot_grid(homosexuals, heterosexuals)
    
  })
  
  output$suitemateSizeRelationship <- renderPlot({
    
    
    suitemate_size_relationship %>%
      ggplot(aes(x = size, y = perc_blockwithsuite))+geom_point() +
      geom_smooth(method = "lm", se = F, color = "#00BFC4") + 
      labs(x = "Size of Freshman Dorm",
           y = "Percentage of blocking groups containing 2+ suitemates from dorm",
           title = "Size of Freshman Dorm against Rooming with Suitemates") +
      scale_y_continuous(labels = scales::percent) +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
  })
  
  output$whereDoTheyGo <- renderPlot({
    
    totals <- official_housing %>%
       count(dorm) %>%
      rename(total = n)
      
    selected <- official_housing %>%
    select(dorm, house, quad) %>%
      count(dorm, quad)
  
    combined <- full_join(selected, totals, by = "dorm") %>%
      mutate(pct = n/total,
             pctriver = ifelse(quad == "River", pct, 0))
      
    
    ggplot(combined, aes(x = fct_reorder(dorm, (pctriver)), y = pct, fill = quad)) + 
    geom_bar(stat = "identity") +
      scale_y_continuous(labels = scales::percent) +
      coord_flip() +
      labs(title = "House Placements by Freshman Dorm",
           x = "Freshman Dorm", 
           y = "Percentage of Students",
           fill = "Neighborhood") +
      theme_classic() +  
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
    
  })

  
  output$varsityPerBlock <- renderPlot({
    
    ggplot(varsity_per_block, aes(x = fct_reorder(house, (average_varsity)), y = average_varsity)) +
      geom_col() + 
      labs(x = "House Placement",
           y = "Average Varsity athletes per blocking group",
           title = "Varsity Athletes per Blocking Group") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(family = "Avenir")) +
      theme(title = element_text(family = "Avenir")) 
  })
  
  output$chisq_houses <- render_gt({
    chisq_houses 
    
  })
  
  output$chisq_neighborhoods <- render_gt({
    chisq_neighborhoods
  })
  
  output$Jamal <- renderImage({list(src = './www/Jamal.JPG',
                                    width = 300,
                                    height = 400)
    
  }, deleteFile = FALSE)
  
  output$Austin <- renderImage({list(src = './www/Austin.JPG',
                                    width = 300,
                                    height = 400)
    
  }, deleteFile = FALSE)
  
  output$Carina <- renderImage({list(src = './www/Carina.JPG',
                                    width = 300,
                                    height = 400)
    
  }, deleteFile = FALSE)
  
  output$Shojeh <- renderImage({list(src = './www/Shojeh.JPG',
                                    width = 300,
                                    height = 400)
    
  }, deleteFile = FALSE)
  
  output$Lucy <- renderImage({list(src = './www/Lucy.JPG',
                                    width = 300,
                                    height = 400)
    
  }, deleteFile = FALSE)
  
  output$Sam <- renderImage({list(src = './www/Sam.JPG',
                                   width = 300,
                                   height = 400)
    
  }, deleteFile = FALSE)
  
  output$Angie <- renderImage({list(src = './www/Angie.JPG',
                                   width = 300,
                                   height = 400)
    
  }, deleteFile = FALSE)
  
  output$Ilyas <- renderImage({list(src = './www/Ilyas.JPG',
                                    width = 300,
                                    height = 400)
    
  }, deleteFile = FALSE)
  
  output$Eliot <- renderImage({list(src = './www/Eliot.JPG',
                                    width = 300,
                                    height = 400)
    
  }, deleteFile = FALSE)
}
#   
# animate(plot, renderer = ffmpeg_renderer())
  
  

shinyApp(ui, server)

