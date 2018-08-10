A Case study on Spark Funds Investment
======================================

------------------------------------------------------------------------

**Business Understanding**

Spark Funds, an asset management company wants to make investments in a few companies. The CEO of Spark Funds wants to understand the global trends in investments so that the investment decisions can be taken effectively.

Spark Funds has two minor constraints for investments:

-   It wants to invest between 5 to 15 million USD per round of investment
-   It wants to invest only in English-speaking countries because of the ease of communication with the companies it would invest in

**Objective**

To identify the best sectors, countries, and a suitable investment type for making investments. The overall strategy is to invest where others are investing, implying that the best sectors and countries are the ones where most investments are happening.

------------------------------------------------------------------------

*Include the required libraries*

``` r
library(tidyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
```

Data Understanding
------------------

Data is investment data from crunchbase.com. We have the following files:

-   Company details(companies.txt)

    -   Permalink - Unique ID of company
    -   name - Company name
    -   homepage\_url - Website URL
    -   category\_list - Category/categories to which a company belongs
    -   status - Operational status
    -   country\_code - Country Code
    -   state\_code - State

-   Funding round details(rounds2.csv)

    -   company\_permalink - Unique ID of company
    -   funding\_round\_permalink - Unique ID of funding round
    -   funding\_round\_type - Type of funding - venture, angel, private equity etc.
    -   funding\_round\_code - Round of venture funding (round A, B etc.)
    -   funded\_at - Date of funding
    -   raised\_amount\_usd - Money raised in funding (USD)

-   Sector Classification:(mapping.csv)

This file maps the numerous category names in the companies table (such 3D printing, aerospace, agriculture, etc.) to eight broad sector names. The purpose is to simplify the analysis into eight sector buckets, rather than trying to analyse hundreds of them.

*Load the companies and rounds data*

``` r
companies <- read.delim("companies.txt", stringsAsFactors = F)
rounds2   <- read.csv("rounds2.csv ", stringsAsFactors = F)
```

Check the data

``` r
str(companies)
```

    ## 'data.frame':    66368 obs. of  10 variables:
    ##  $ permalink    : chr  "/Organization/-Fame" "/Organization/-Qounter" "/Organization/-The-One-Of-Them-Inc-" "/Organization/0-6-Com" ...
    ##  $ name         : chr  "#fame" ":Qounter" "(THE) ONE of THEM,Inc." "0-6.com" ...
    ##  $ homepage_url : chr  "http://livfame.com" "http://www.qounter.com" "http://oneofthem.jp" "http://www.0-6.com" ...
    ##  $ category_list: chr  "Media" "Application Platforms|Real Time|Social Network Media" "Apps|Games|Mobile" "Curated Web" ...
    ##  $ status       : chr  "operating" "operating" "operating" "operating" ...
    ##  $ country_code : chr  "IND" "USA" "" "CHN" ...
    ##  $ state_code   : chr  "16" "DE" "" "22" ...
    ##  $ region       : chr  "Mumbai" "DE - Other" "" "Beijing" ...
    ##  $ city         : chr  "Mumbai" "Delaware City" "" "Beijing" ...
    ##  $ founded_at   : chr  "" "4/9/2014" "" "1/1/2007" ...

``` r
str(rounds2)
```

    ## 'data.frame':    114949 obs. of  6 variables:
    ##  $ company_permalink      : chr  "/organization/-fame" "/ORGANIZATION/-QOUNTER" "/organization/-qounter" "/ORGANIZATION/-THE-ONE-OF-THEM-INC-" ...
    ##  $ funding_round_permalink: chr  "/funding-round/9a01d05418af9f794eebff7ace91f638" "/funding-round/22dacff496eb7acb2b901dec1dfe5633" "/funding-round/b44fbb94153f6cdef13083530bb48030" "/funding-round/650b8f704416801069bb178a1418776b" ...
    ##  $ funding_round_type     : chr  "venture" "venture" "seed" "venture" ...
    ##  $ funding_round_code     : chr  "B" "A" "" "B" ...
    ##  $ funded_at              : chr  "05-01-2015" "14-10-2014" "01-03-2014" "30-01-2014" ...
    ##  $ raised_amount_usd      : num  10000000 NA 700000 3406878 2000000 ...

``` r
head(companies)
```

    ##                             permalink                   name
    ## 1                 /Organization/-Fame                  #fame
    ## 2              /Organization/-Qounter               :Qounter
    ## 3 /Organization/-The-One-Of-Them-Inc- (THE) ONE of THEM,Inc.
    ## 4               /Organization/0-6-Com                0-6.com
    ## 5      /Organization/004-Technologies       004 Technologies
    ## 6    /Organization/01Games-Technology     01Games Technology
    ##                        homepage_url
    ## 1                http://livfame.com
    ## 2            http://www.qounter.com
    ## 3               http://oneofthem.jp
    ## 4                http://www.0-6.com
    ## 5 http://004gmbh.de/en/004-interact
    ## 6            http://www.01games.hk/
    ##                                          category_list    status
    ## 1                                                Media operating
    ## 2 Application Platforms|Real Time|Social Network Media operating
    ## 3                                    Apps|Games|Mobile operating
    ## 4                                          Curated Web operating
    ## 5                                             Software operating
    ## 6                                                Games operating
    ##   country_code state_code                region          city founded_at
    ## 1          IND         16                Mumbai        Mumbai           
    ## 2          USA         DE            DE - Other Delaware City   4/9/2014
    ## 3                                                                       
    ## 4          CHN         22               Beijing       Beijing   1/1/2007
    ## 5          USA         IL Springfield, Illinois     Champaign   1/1/2010
    ## 6          HKG                        Hong Kong     Hong Kong

``` r
head(rounds2)
```

    ##                     company_permalink
    ## 1                 /organization/-fame
    ## 2              /ORGANIZATION/-QOUNTER
    ## 3              /organization/-qounter
    ## 4 /ORGANIZATION/-THE-ONE-OF-THEM-INC-
    ## 5               /organization/0-6-com
    ## 6      /ORGANIZATION/004-TECHNOLOGIES
    ##                           funding_round_permalink funding_round_type
    ## 1 /funding-round/9a01d05418af9f794eebff7ace91f638            venture
    ## 2 /funding-round/22dacff496eb7acb2b901dec1dfe5633            venture
    ## 3 /funding-round/b44fbb94153f6cdef13083530bb48030               seed
    ## 4 /funding-round/650b8f704416801069bb178a1418776b            venture
    ## 5 /funding-round/5727accaeaa57461bd22a9bdd945382d            venture
    ## 6 /funding-round/1278dd4e6a37fa4b7d7e06c21b3c1830            venture
    ##   funding_round_code  funded_at raised_amount_usd
    ## 1                  B 05-01-2015          10000000
    ## 2                  A 14-10-2014                NA
    ## 3                    01-03-2014            700000
    ## 4                  B 30-01-2014           3406878
    ## 5                  A 19-03-2008           2000000
    ## 6                    24-07-2014                NA

Change the keys permalink to lower case so that both the the dataframes can be merged using the permalink primary key

``` r
companies$permalink       <- str_to_lower(companies$permalink)
rounds2$company_permalink <- str_to_lower(rounds2$company_permalink)
```

How many unique companies are present in rounds2?

``` r
length(unique(rounds2$company_permalink))
```

    ## [1] 66368

How many unique companies are present in companies?

``` r
length(unique(companies$permalink))
```

    ## [1] 66368

Merge the two data frames so that all variables in the companies frame are added to the rounds2 data frame.\#Name the merged frame master\_frame.

``` r
master.frame <- merge(rounds2, companies,by.x = "company_permalink",
                      by.y = "permalink")
head(master.frame)
```

    ##                     company_permalink
    ## 1                 /organization/-fame
    ## 2              /organization/-qounter
    ## 3              /organization/-qounter
    ## 4 /organization/-the-one-of-them-inc-
    ## 5               /organization/0-6-com
    ## 6      /organization/004-technologies
    ##                           funding_round_permalink funding_round_type
    ## 1 /funding-round/9a01d05418af9f794eebff7ace91f638            venture
    ## 2 /funding-round/22dacff496eb7acb2b901dec1dfe5633            venture
    ## 3 /funding-round/b44fbb94153f6cdef13083530bb48030               seed
    ## 4 /funding-round/650b8f704416801069bb178a1418776b            venture
    ## 5 /funding-round/5727accaeaa57461bd22a9bdd945382d            venture
    ## 6 /funding-round/1278dd4e6a37fa4b7d7e06c21b3c1830            venture
    ##   funding_round_code  funded_at raised_amount_usd                   name
    ## 1                  B 05-01-2015          10000000                  #fame
    ## 2                  A 14-10-2014                NA               :Qounter
    ## 3                    01-03-2014            700000               :Qounter
    ## 4                  B 30-01-2014           3406878 (THE) ONE of THEM,Inc.
    ## 5                  A 19-03-2008           2000000                0-6.com
    ## 6                    24-07-2014                NA       004 Technologies
    ##                        homepage_url
    ## 1                http://livfame.com
    ## 2            http://www.qounter.com
    ## 3            http://www.qounter.com
    ## 4               http://oneofthem.jp
    ## 5                http://www.0-6.com
    ## 6 http://004gmbh.de/en/004-interact
    ##                                          category_list    status
    ## 1                                                Media operating
    ## 2 Application Platforms|Real Time|Social Network Media operating
    ## 3 Application Platforms|Real Time|Social Network Media operating
    ## 4                                    Apps|Games|Mobile operating
    ## 5                                          Curated Web operating
    ## 6                                             Software operating
    ##   country_code state_code                region          city founded_at
    ## 1          IND         16                Mumbai        Mumbai           
    ## 2          USA         DE            DE - Other Delaware City   4/9/2014
    ## 3          USA         DE            DE - Other Delaware City   4/9/2014
    ## 4                                                                       
    ## 5          CHN         22               Beijing       Beijing   1/1/2007
    ## 6          USA         IL Springfield, Illinois     Champaign   1/1/2010

``` r
str(master.frame)
```

    ## 'data.frame':    114949 obs. of  15 variables:
    ##  $ company_permalink      : chr  "/organization/-fame" "/organization/-qounter" "/organization/-qounter" "/organization/-the-one-of-them-inc-" ...
    ##  $ funding_round_permalink: chr  "/funding-round/9a01d05418af9f794eebff7ace91f638" "/funding-round/22dacff496eb7acb2b901dec1dfe5633" "/funding-round/b44fbb94153f6cdef13083530bb48030" "/funding-round/650b8f704416801069bb178a1418776b" ...
    ##  $ funding_round_type     : chr  "venture" "venture" "seed" "venture" ...
    ##  $ funding_round_code     : chr  "B" "A" "" "B" ...
    ##  $ funded_at              : chr  "05-01-2015" "14-10-2014" "01-03-2014" "30-01-2014" ...
    ##  $ raised_amount_usd      : num  10000000 NA 700000 3406878 2000000 ...
    ##  $ name                   : chr  "#fame" ":Qounter" ":Qounter" "(THE) ONE of THEM,Inc." ...
    ##  $ homepage_url           : chr  "http://livfame.com" "http://www.qounter.com" "http://www.qounter.com" "http://oneofthem.jp" ...
    ##  $ category_list          : chr  "Media" "Application Platforms|Real Time|Social Network Media" "Application Platforms|Real Time|Social Network Media" "Apps|Games|Mobile" ...
    ##  $ status                 : chr  "operating" "operating" "operating" "operating" ...
    ##  $ country_code           : chr  "IND" "USA" "USA" "" ...
    ##  $ state_code             : chr  "16" "DE" "DE" "" ...
    ##  $ region                 : chr  "Mumbai" "DE - Other" "DE - Other" "" ...
    ##  $ city                   : chr  "Mumbai" "Delaware City" "Delaware City" "" ...
    ##  $ founded_at             : chr  "" "4/9/2014" "4/9/2014" "" ...

How many observations are present in master\_frame?

``` r
nrow(master.frame)
```

    ## [1] 114949

Are there any companies in the rounds2 file which are not present in companies?

``` r
setdiff(master.frame$company_permalink, companies$permalink)
```

    ## character(0)

**Additional Cleanup** The companies that are closed can be filtered out from the master.frame as there is no point in analysing to invest in the Closed Companies summary(as.factor(master.frame$status))

``` r
master.frame <- filter(master.frame, status != 'closed')
```

Funding Type Analysis
---------------------

Group by funding type on average investment amount

``` r
inv.by.fndtyp  <- master.frame %>%
  group_by(funding_round_type) %>%
  summarise(mean(raised_amount_usd,na.rm = T)) %>%
  setNames(c("Funding_Type", "Avg_Investment_Amount"))
```

Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for it?

``` r
((inv.by.fndtyp  %>% 
         filter(Avg_Investment_Amount >= 5000000 & 
                  Avg_Investment_Amount <= 15000000))$Funding_Type)
```

    ## [1] "venture"

Country Analysis
----------------

1.  Filter funding type as 'Venture' since that is the most suitable investment type for Spark funds.Additional Clean up: Filter out the countries that do not have any any values
2.  Group by country on total investment amount

``` r
inv.by.cntry <- master.frame %>%
  filter(funding_round_type == 'venture' & country_code != "") %>%
  group_by(country_code) %>%
  summarise(sum(raised_amount_usd,na.rm = T)) %>%
  setNames(c("country", "investment")) %>%
  arrange(desc(investment))

(top9 <- head((inv.by.cntry), 9))
```

    ## # A tibble: 9 x 2
    ##   country    investment
    ##   <chr>           <dbl>
    ## 1 USA     399935149987.
    ## 2 CHN      39101773502.
    ## 3 GBR      18241343570.
    ## 4 IND      14050603718.
    ## 5 CAN       8825487216.
    ## 6 FRA       6775044017.
    ## 7 ISR       6503020916.
    ## 8 DEU       6061140919.
    ## 9 JPN       3242031955.

Top 3 Countries for investment: USA, GBR and IND

Sector Analysis 1
-----------------

Merged data frame with each primary sector mapped to its main sector. Load the mapping data from excel into data frame

``` r
cat.sect.wide <- read.csv("mapping.csv", stringsAsFactors = F)
```

convert wide format columns into long format (category,sector)

``` r
cat.sect.long <- cat.sect.wide %>%
  gather(category, sector, 
         "Automotive...Sports":"Social..Finance..Analytics..Advertising") %>%
  filter(!(sector == 0)) %>%   #Omit the rows with 0 values in the sector 
  select(c(-3)) %>%       #Omit the last column with all 1's from the 
                          # data frame as it is no longer needed. 
  setNames(c("category", "sector"))
```

Split out column category\_list in master\_frame into primary and secondary Use escape sequence \\ to escape '|' symbol

``` r
cat.split <- master.frame %>%
  separate(category_list, into = c("primary", "secondary"), "\\|",
           extra = "drop", fill = "right") 
```

Merge the data frames master\_frame\_separated and category\_long

``` r
master.frame.with.sect <- merge(cat.split, cat.sect.long, 
                                by.x = "primary", by.y = "category")
```

Check if there are any rows that are dropped when master\_frame\_with\_sector is created From category\_separated

``` r
nrow(cat.split) - nrow(master.frame.with.sect)
```

    ## [1] 7730

Check for any categories that got dropped due to mismatch between category DF in long format and master\_frame DF with categories separated

``` r
(mismatch.drops <- length(setdiff(cat.sect.long$category, 
                                 master.frame.with.sect$primary)))
```

    ## [1] 56

Finding out why there was a mismatch

``` r
setdiff(cat.sect.long$category, master.frame.with.sect$primary)
```

    ##  [1] "Energy Ma0gement"             "0notechnology"               
    ##  [3] "0tural Language Processing"   "0tural Resources"            
    ##  [5] "Waste Ma0gement"              "Digital Rights Ma0gement"    
    ##  [7] "Digital Sig0ge"               "Educatio0l Games"            
    ##  [9] "Event Ma0gement"              "Musicians"                   
    ## [11] "Alter0tive Medicine"          "Can0bis"                     
    ## [13] "Medical Professio0ls"         "Perso0l Health"              
    ## [15] "Senior Citizens"              "Speech Recognition"          
    ## [17] "Veteri0ry"                    "Chi0 Internet"               
    ## [19] "Cloud Ma0gement"              "Jour0lism"                   
    ## [21] "0vigation"                    "Professio0l Networking"      
    ## [23] "Document Ma0gement"           "Fleet Ma0gement"             
    ## [25] "Gover0nce"                    "Intellectual Asset Ma0gement"
    ## [27] "IT Ma0gement"                 "Knowledge Ma0gement"         
    ## [29] "Lead Ma0gement"               "Perso0l Data"                
    ## [31] "Perso0lization"               "Professio0l Services"        
    ## [33] "Project Ma0gement"            "Smart Grid"                  
    ## [35] "Supply Chain Ma0gement"       "Task Ma0gement"              
    ## [37] "A0lytics"                     "Big Data A0lytics"           
    ## [39] "Business A0lytics"            "Career Ma0gement"            
    ## [41] "Contact Ma0gement"            "Fi0nce"                      
    ## [43] "Fi0nce Technology"            "Fi0ncial Exchanges"          
    ## [45] "Fi0ncial Services"            "Identity Ma0gement"          
    ## [47] "Innovation Ma0gement"         "Investment Ma0gement"        
    ## [49] "Mobile A0lytics"              "Perso0l Branding"            
    ## [51] "Perso0l Fi0nce"               "Predictive A0lytics"         
    ## [53] "Promotio0l"                   "Property Ma0gement"          
    ## [55] "Risk Ma0gement"               "Social Media Ma0gement"

*By looking at the result we could see that there are some categories like 'Energy Ma0gement', '0notechnology' and so on which did not match because of the pattern '0' instead of 'na'. This needs a cleanup*

``` r
cat.sect.long$category <- str_replace_all(cat.sect.long$category, 
                                          pattern = "0","na")
```

*One category 'Enterprise 2.0' gets replaced with '2.na'. The second replace statement ensures it is restored back to its original value 'Enterprise 2.0'*

``` r
cat.sect.long$category <- str_replace_all(cat.sect.long$category, 
                                          pattern = "\\.na",".0")
```

Since we added "na" (some in the front, some in the middle of the columns), inorder to make it uniform, let's convert category columns to Title case in both the DFs so that they match

``` r
cat.sect.long$category <- str_to_title(cat.sect.long$category)
cat.split$primary      <- str_to_title(cat.split$primary)
```

Merge the data frames master\_frame\_separated and category\_long again

``` r
master.frame.with.sect <- merge(cat.split, cat.sect.long, 
                                by.x = "primary", by.y = "category")
```

Check for the records that were dropped

``` r
mismatch.drops <- length(setdiff(cat.sect.long$category, 
                                 master.frame.with.sect$primary))
```

*Since mismatch drops is 0, all matched records from sector mapping file is now added to master\_frame\_with\_sector.*

Now we are good to go to the next step.

``` r
head(master.frame.with.sect)
```

    ##   primary              company_permalink
    ## 1         /organization/light-extraction
    ## 2              /organization/yellow-chip
    ## 3                 /organization/whiphand
    ## 4              /organization/autodisplay
    ## 5                 /organization/freebird
    ## 6                   /organization/omni3d
    ##                           funding_round_permalink funding_round_type
    ## 1 /funding-round/89583581043acce0eae4ce3c36264139        undisclosed
    ## 2 /funding-round/f916337918463d6396ceb1da52d61da2               seed
    ## 3 /funding-round/18a41a38e091e8e0a17eda4ef7cb1205               seed
    ## 4 /funding-round/233d4fecd0d93ae9862929822798adc9        undisclosed
    ## 5 /funding-round/6e2fcf9a3821f4d4d358173525401c0c               seed
    ## 6 /funding-round/9ee502c33d7e7f3664415c635fa7e33e               seed
    ##   funding_round_code  funded_at raised_amount_usd             name
    ## 1                    28-12-2012                NA Light Extraction
    ## 2                    01-01-2013                NA      Yellow Chip
    ## 3                    30-04-2013                NA         Whiphand
    ## 4                    01-01-2010                NA      Autodisplay
    ## 5                    19-11-2015           3500000   Freebird, Inc.
    ## 6                    01-01-2013                NA           Omni3D
    ##                          homepage_url secondary    status country_code
    ## 1          http://lightextraction.com      <NA> operating          DNK
    ## 2           http://www.yellowchip.it/      <NA> operating             
    ## 3    http://www.whiphandcosmetics.com      <NA> operating          USA
    ## 4 http://www.autodisplay-biotech.com/      <NA> operating          DEU
    ## 5        https://www.getfreebird.com/      <NA> operating          USA
    ## 6                   http://omni3d.net      <NA> operating          POL
    ##   state_code      region           city founded_at sector
    ## 1         17 DNK - Other Kongens Lyngby   1/1/2013 Blanks
    ## 2                                                  Blanks
    ## 3         MI     Detroit        Detroit            Blanks
    ## 4          7  Dusseldrof    DÃ¼sseldorf            Blanks
    ## 5         MA      Boston      Cambridge   1/1/2015 Blanks
    ## 6         86      Poznan         Poznan            Blanks

``` r
tail(master.frame.with.sect)
```

    ##             primary                 company_permalink
    ## 106150     Wireless     /organization/nimble-wireless
    ## 106151     Wireless  /organization/pacific-datavision
    ## 106152     Wireless   /organization/legra-systems-inc
    ## 106153     Wireless         /organization/mobileway-2
    ## 106154        Women             /organization/shenami
    ## 106155 Young Adults /organization/society-of-grownups
    ##                                funding_round_permalink funding_round_type
    ## 106150 /funding-round/793c757c48716ea853c5136fa26e39c1               seed
    ## 106151 /funding-round/3b17a2be3ab0b0b4d81e84ce58500168            venture
    ## 106152 /funding-round/1a38fd033526213e599c7112bac06f71            venture
    ## 106153 /funding-round/edd04b938d205f6239a77f2530417462            venture
    ## 106154 /funding-round/994c9dcdb5492172083910144200ab9b               seed
    ## 106155 /funding-round/83237a23ca374ea1a5ee099938e00df6            venture
    ##        funding_round_code  funded_at raised_amount_usd                name
    ## 106150                    11-02-2015          5.00e+05     Nimble Wireless
    ## 106151                    18-06-2014          3.89e+07         pdvWireless
    ## 106152                  B 01-12-2003          1.20e+07       Legra Systems
    ## 106153                  D 08-04-2004          2.30e+07           Mobileway
    ## 106154                    04-02-2014          2.50e+04             Shenami
    ## 106155                    15-10-2015          1.00e+08 Society of Grownups
    ##                         homepage_url secondary    status country_code
    ## 106150    http://nimblewireless.com/      <NA> operating          IND
    ## 106151    http://www.pdvwireless.com      <NA> operating          USA
    ## 106152          http://www.legra.com      <NA>  acquired          USA
    ## 106153                                    <NA> operating          USA
    ## 106154        http://www.shenami.com      <NA> operating             
    ## 106155 http://societyofgrownups.com/      <NA> operating          USA
    ##        state_code      region       city founded_at        sector
    ## 106150         25     Chennai    Chennai            Entertainment
    ## 106151         NJ                                   Entertainment
    ## 106152         MA      Boston Burlington   1/1/2002 Entertainment
    ## 106153         CA SF Bay Area  San Mateo            Entertainment
    ## 106154                                     1/1/2014        Others
    ## 106155         MA      Boston     Boston   1/1/2014        Others

Sector Analysis
---------------

Create three separate data frames D1, D2 and D3 for each of the 3 countries containing the observations of funding type FT falling within the 5-15 million USD range.

The three data frames should contain:

-   All the columns of the master\_frame along with the primary sector and the main sector
-   Total number (or count) of investments for each main sector in a separate column
-   The total amount invested in each main sector in a separate column

``` r
master.frame.prfrd <- master.frame.with.sect %>%
  filter(funding_round_type == 'venture' &
           raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)

D1 <- filter(master.frame.prfrd, country_code == "USA")
D2 <- filter(master.frame.prfrd, country_code == "GBR")
D3 <- filter(master.frame.prfrd, country_code == "IND")
```

Function to group by individual DFs by sectors based on investment

``` r
GroupBySect <- function(df){
  return(df %>% group_by(sector) %>% 
    summarize(count = n(),sum(raised_amount_usd,na.rm = T)) %>% 
    setNames(c('sector', 'count', 'amount')) %>% 
    arrange(desc(count),desc(amount)))
}
```

Call above function to group the individual DFs by sectors on investment

``` r
D1.inv.by.sect <- GroupBySect(D1)
D2.inv.by.sect <- GroupBySect(D2)
D3.inv.by.sect <- GroupBySect(D3)
```

Merge the aggregated values with the master frame based on Sectors

``` r
D1 <- merge(D1, D1.inv.by.sect, by = "sector")
D2 <- merge(D2, D2.inv.by.sect, by = "sector")
D3 <- merge(D3, D3.inv.by.sect, by = "sector")
head(D1)
```

    ##                sector        primary                     company_permalink
    ## 1 Automotive...Sports     Automotive               /organization/silvercar
    ## 2 Automotive...Sports Transportation                /organization/surf-air
    ## 3 Automotive...Sports         Sports               /organization/whoop-inc
    ## 4 Automotive...Sports      Aerospace                  /organization/kymeta
    ## 5 Automotive...Sports     Automotive /organization/miles-electric-vehicles
    ## 6 Automotive...Sports Transportation                /organization/surf-air
    ##                           funding_round_permalink funding_round_type
    ## 1 /funding-round/9fda197ed5492c9e68e60e6fd5d13835            venture
    ## 2 /funding-round/35662852d0984319f7551e812f95997c            venture
    ## 3 /funding-round/c3fd2a200ff7d7577d65aa0dbcb9a6c7            venture
    ## 4 /funding-round/df91c069e014e2c2e446f6d72d9e8245            venture
    ## 5 /funding-round/01c4e9b7581d97929778ed56bb223c8a            venture
    ## 6 /funding-round/f78d4337d3daa258459fa0b875bb8435            venture
    ##   funding_round_code  funded_at raised_amount_usd                    name
    ## 1                  B 23-09-2014           1.4e+07               Silvercar
    ## 2                  B 07-08-2014           8.0e+06                Surf Air
    ## 3                  A 04-06-2014           6.0e+06                   WHOOP
    ## 4                    21-08-2012           1.2e+07                  Kymeta
    ## 5                    22-02-2008           1.5e+07 Miles Electric Vehicles
    ## 6                  B 03-06-2013           7.0e+06                Surf Air
    ##                homepage_url               secondary    status country_code
    ## 1      http://silvercar.com              E-Commerce operating          USA
    ## 2    http://www.surfair.com                  Travel operating          USA
    ## 3      http://www.whoop.com                    <NA> operating          USA
    ## 4 http://www.kymetacorp.com Communications Hardware operating          USA
    ## 5    http://www.milesev.com       Electric Vehicles operating          USA
    ## 6    http://www.surfair.com                  Travel operating          USA
    ##   state_code      region         city founded_at count     amount
    ## 1         TX      Austin       Austin   1/4/2012   157 1374304361
    ## 2         CA Los Angeles Santa Monica  5/11/2011   157 1374304361
    ## 3         MA      Boston       Boston 31-12-2011   157 1374304361
    ## 4         WA     Seattle      Redmond   1/1/2012   157 1374304361
    ## 5         CA Los Angeles Santa Monica   1/1/2004   157 1374304361
    ## 6         CA Los Angeles Santa Monica  5/11/2011   157 1374304361

``` r
head(D2)
```

    ##                sector    primary                    company_permalink
    ## 1 Automotive...Sports       Cars /organization/greenroad-technologies
    ## 2 Automotive...Sports Automotive              /organization/autoquake
    ## 3 Automotive...Sports Automotive              /organization/autoquake
    ## 4 Automotive...Sports Automotive              /organization/autoquake
    ## 5 Automotive...Sports Automotive      /organization/light-blue-optics
    ## 6 Automotive...Sports       Cars /organization/greenroad-technologies
    ##                           funding_round_permalink funding_round_type
    ## 1 /funding-round/19dc7a4e54c5e83b94bd845c2f7f014a            venture
    ## 2 /funding-round/4c8372dfdea687c5f5fbab39b3e44dab            venture
    ## 3 /funding-round/a4d5080cbda34c2ef4295d8fbe4e9ad5            venture
    ## 4 /funding-round/067d143de46ec298cfa1893682f9911a            venture
    ## 5 /funding-round/320fcbe59497214f44cf8bf84c073903            venture
    ## 6 /funding-round/02e891e7e3e9a18f06a6cdfca142a1d7            venture
    ##   funding_round_code  funded_at raised_amount_usd                   name
    ## 1                    31-01-2008          14500000 GreenRoad Technologies
    ## 2                  A 01-07-2006           6000000              Autoquake
    ## 3                  B 01-04-2008          11911764              Autoquake
    ## 4                  B 11-05-2008          11890000              Autoquake
    ## 5                  A 19-07-2009          15000000      Light Blue Optics
    ## 6                  F 19-05-2011          13000000 GreenRoad Technologies
    ##                     homepage_url            secondary    status
    ## 1       http://www.greenroad.com     Clean Technology operating
    ## 2       http://www.autoquake.com                 Cars  acquired
    ## 3       http://www.autoquake.com                 Cars  acquired
    ## 4       http://www.autoquake.com                 Cars  acquired
    ## 5 http://www.lightblueoptics.com Consumer Electronics operating
    ## 6       http://www.greenroad.com     Clean Technology operating
    ##   country_code state_code region      city founded_at count    amount
    ## 1          GBR         H9 London    London   1/1/2006    16 167051565
    ## 2          GBR         H9 London    London   7/8/2005    16 167051565
    ## 3          GBR         H9 London    London   7/8/2005    16 167051565
    ## 4          GBR         H9 London    London   7/8/2005    16 167051565
    ## 5          GBR         C3 London Cambridge   2/1/2004    16 167051565
    ## 6          GBR         H9 London    London   1/1/2006    16 167051565

``` r
head(D3)
```

    ##                sector        primary
    ## 1 Automotive...Sports     Automotive
    ## 2 Automotive...Sports      Aerospace
    ## 3 Automotive...Sports      Designers
    ## 4 Automotive...Sports Transportation
    ## 5 Automotive...Sports           Cars
    ## 6 Automotive...Sports     Automotive
    ##                               company_permalink
    ## 1                    /organization/ather-energy
    ## 2               /organization/mistral-solutions
    ## 3                     /organization/indianroots
    ## 4                  /organization/makemytrip-com
    ## 5           /organization/mahindra-first-choice
    ## 6 /organization/incredible-technologies-pvt-ltd
    ##                           funding_round_permalink funding_round_type
    ## 1 /funding-round/a3782f52b69e60629bcf7866ca8b1eca            venture
    ## 2 /funding-round/4688f3f8860d92330aa64f6f6aa0dd39            venture
    ## 3 /funding-round/f9e85b60f976b7a5e873de366db64159            venture
    ## 4 /funding-round/6bc57a33607a3317b89a4c97011cbfff            venture
    ## 5 /funding-round/fd1878def2e38049665344ee9f2b22eb            venture
    ## 6 /funding-round/6a4ce4ed8be26c7a2d0a24f4a92fd958            venture
    ##   funding_round_code  funded_at raised_amount_usd
    ## 1                  A 29-05-2015           1.2e+07
    ## 2                  B 06-02-2008           6.5e+06
    ## 3                  B 07-09-2014           5.0e+06
    ## 4                  B 14-12-2006           1.3e+07
    ## 5                    20-03-2015           1.5e+07
    ## 6                  A 11-09-2015           1.5e+07
    ##                              name                    homepage_url
    ## 1                    Ather Energy      http://www.atherenergy.com
    ## 2               Mistral Solutions http://www.mistralsolutions.com
    ## 3                     IndianRoots      http://www.indianroots.in/
    ## 4                  MakeMyTrip.com           http://makemytrip.com
    ## 5           Mahindra First Choice http://mahindrafirstchoice.com/
    ## 6 Incredible Technologies (CredR)           http://www.credr.com/
    ##           secondary    status country_code state_code    region      city
    ## 1 Electric Vehicles operating          IND         19 Bangalore Bangalore
    ## 2           Defense operating          IND         19 Bangalore Bangalore
    ## 3        E-Commerce operating          IND         10 New Delhi   Gurgaon
    ## 4            Travel       ipo          IND         10 New Delhi   Gurgaon
    ## 5            Retail operating          IND                               
    ## 6              <NA> operating          IND         16    Mumbai    Mumbai
    ##   founded_at count    amount
    ## 1   1/4/2013    13 136900000
    ## 2   1/1/1997    13 136900000
    ## 3   1/1/2013    13 136900000
    ## 4   1/1/2000    13 136900000
    ## 5   2/8/2007    13 136900000
    ## 6   1/1/2014    13 136900000

**Total number of investments (count)**

``` r
# USA
sum(D1.inv.by.sect$count)
```

    ## [1] 11343

``` r
# GBR
sum(D2.inv.by.sect$count)
```

    ## [1] 587

``` r
# IND
sum(D3.inv.by.sect$count)
```

    ## [1] 317

**Total amount of investment**

``` r
# USA 
sum(D1.inv.by.sect$amount)
```

    ## [1] 101535779825

``` r
# GBR
sum(D2.inv.by.sect$amount)
```

    ## [1] 5093543406

``` r
# IND
sum(D3.inv.by.sect$amount)
```

    ## [1] 2847673602

**Top sector (based on count of investments)**

``` r
# USA
D1.inv.by.sect$sector[1]
```

    ## [1] "Others"

``` r
# GBR
D2.inv.by.sect$sector[1]
```

    ## [1] "Others"

``` r
# IND
D3.inv.by.sect$sector[1]
```

    ## [1] "Others"

**Second-best sector (based on count of investments)**

``` r
# USA
D1.inv.by.sect$sector[2]
```

    ## [1] "Social..Finance..Analytics..Advertising"

``` r
# GBR
D2.inv.by.sect$sector[2]
```

    ## [1] "Social..Finance..Analytics..Advertising"

``` r
# IND
D3.inv.by.sect$sector[2]
```

    ## [1] "Social..Finance..Analytics..Advertising"

**Third-best sector (based on count of investments)**

``` r
# USA
D1.inv.by.sect$sector[3]
```

    ## [1] "Cleantech...Semiconductors"

``` r
# GBR
D2.inv.by.sect$sector[3]
```

    ## [1] "Cleantech...Semiconductors"

``` r
# IND
D3.inv.by.sect$sector[3]
```

    ## [1] "News..Search.and.Messaging"

**Number of investments in the top sector**

``` r
# USA
D1.inv.by.sect$count[1]
```

    ## [1] 2758

``` r
# GBR
D2.inv.by.sect$count[1]
```

    ## [1] 140

``` r
# IND
D3.inv.by.sect$count[1]
```

    ## [1] 103

\*\* Number of investments in the second-best sector\*\*

``` r
# USA
D1.inv.by.sect$count[2]
```

    ## [1] 2590

``` r
# GBR
D2.inv.by.sect$count[2]
```

    ## [1] 128

``` r
# IND
D3.inv.by.sect$count[2]
```

    ## [1] 59

\*\* Number of investments in third-best sector\*\*

``` r
# USA
D1.inv.by.sect$count[3]
```

    ## [1] 2192

``` r
# GBR
D2.inv.by.sect$count[3]
```

    ## [1] 116

``` r
# IND
D3.inv.by.sect$count[3]
```

    ## [1] 49

Function to group by individual DFs by comapny names based on sectors & investment

``` r
GroupByComp <- function(df, sectdf, rank){
  return(df %>%
           filter(sector == sectdf$sector[rank]) %>%
           group_by(name) %>%
           summarise(sum(raised_amount_usd, na.rm = T)) %>%
           setNames(c("Name", "Investment_Amount")) %>%
           arrange(desc(Investment_Amount)))
}
```

\*\* For the top sector count-wise, which company received the highest investment\*\*

``` r
D1.inv.sect1.by.comp <- GroupByComp(D1, D1.inv.by.sect, 1)
D2.inv.sect1.by.comp <- GroupByComp(D2, D2.inv.by.sect, 1)
D3.inv.sect1.by.comp <- GroupByComp(D3, D3.inv.by.sect, 1)
```

The company which received the highest investment in top sector count-wise:

``` r
# USA
D1.inv.sect1.by.comp$Name[1]
```

    ## [1] "Virtustream"

``` r
# GBR
D2.inv.sect1.by.comp$Name[1]
```

    ## [1] "Electric Cloud"

``` r
# IND
D3.inv.sect1.by.comp$Name[1]
```

    ## [1] "FirstCry.com"

\*\* For the second-best sector count-wise,which company received the highest investment?\*\*

``` r
D1.inv.sect2.by.comp <- GroupByComp(D1, D1.inv.by.sect, 2)
D2.inv.sect2.by.comp <- GroupByComp(D2, D2.inv.by.sect, 2)
D3.inv.sect2.by.comp <- GroupByComp(D3, D3.inv.by.sect, 2)
```

The company which received the highest investment in second-best sector count-wise:

``` r
# USA
D1.inv.sect2.by.comp$Name[1]
```

    ## [1] "SST Inc. (Formerly ShotSpotter)"

``` r
# GBR
D2.inv.sect2.by.comp$Name[1]
```

    ## [1] "Celltick Technologies"

``` r
# IND
D3.inv.sect2.by.comp$Name[1]
```

    ## [1] "Manthan Systems"
