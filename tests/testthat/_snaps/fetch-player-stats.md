# fetch_player_stats_afltables returns expected column names and types

    Code
      col_names
    Output
       [1] "Season"                  "Round"                  
       [3] "Date"                    "Local.start.time"       
       [5] "Venue"                   "Attendance"             
       [7] "First.name"              "Surname"                
       [9] "ID"                      "Jumper.No."             
      [11] "Playing.for"             "Kicks"                  
      [13] "Marks"                   "Handballs"              
      [15] "Disposals"               "Goals"                  
      [17] "Behinds"                 "Hit.Outs"               
      [19] "Tackles"                 "Rebounds"               
      [21] "Inside.50s"              "Clearances"             
      [23] "Clangers"                "Frees.For"              
      [25] "Frees.Against"           "Brownlow.Votes"         
      [27] "Contested.Possessions"   "Uncontested.Possessions"
      [29] "Contested.Marks"         "Marks.Inside.50"        
      [31] "One.Percenters"          "Bounces"                
      [33] "Goal.Assists"            "Time.on.Ground"         
      [35] "Substitute"              "Umpire.1"               
      [37] "Umpire.2"                "Umpire.3"               
      [39] "Umpire.4"                "Home.team"              
      [41] "HQ1G"                    "HQ1B"                   
      [43] "HQ2G"                    "HQ2B"                   
      [45] "HQ3G"                    "HQ3B"                   
      [47] "HQ4G"                    "HQ4B"                   
      [49] "HQETG"                   "HQETB"                  
      [51] "Home.score"              "Away.team"              
      [53] "AQ1G"                    "AQ1B"                   
      [55] "AQ2G"                    "AQ2B"                   
      [57] "AQ3G"                    "AQ3B"                   
      [59] "AQ4G"                    "AQ4B"                   
      [61] "AQETG"                   "AQETB"                  
      [63] "Away.score"              "HQ1P"                   
      [65] "HQ2P"                    "HQ3P"                   
      [67] "HQ4P"                    "HQETP"                  
      [69] "AQ1P"                    "AQ2P"                   
      [71] "AQ3P"                    "AQ4P"                   
      [73] "AQETP"                   "Player"                 
      [75] "Team"                    "url"                    
      [77] "Age"                     "Career.Games"           
      [79] "Coach"                   "DOB"                    
      [81] "Home.Away"              

---

    Code
      col_classes
    Output
                       Season                   Round                    Date 
                    "integer"             "character"                  "Date" 
             Local.start.time                   Venue              Attendance 
                    "integer"             "character"               "integer" 
                   First.name                 Surname                      ID 
                  "character"             "character"               "integer" 
                   Jumper.No.             Playing.for                   Kicks 
                  "character"             "character"               "integer" 
                        Marks               Handballs               Disposals 
                    "integer"               "integer"               "integer" 
                        Goals                 Behinds                Hit.Outs 
                    "integer"               "integer"               "integer" 
                      Tackles                Rebounds              Inside.50s 
                    "integer"               "integer"               "integer" 
                   Clearances                Clangers               Frees.For 
                    "integer"               "integer"               "integer" 
                Frees.Against          Brownlow.Votes   Contested.Possessions 
                    "integer"               "integer"               "integer" 
      Uncontested.Possessions         Contested.Marks         Marks.Inside.50 
                    "integer"               "integer"               "integer" 
               One.Percenters                 Bounces            Goal.Assists 
                    "integer"               "integer"               "integer" 
               Time.on.Ground              Substitute                Umpire.1 
                    "integer"             "character"             "character" 
                     Umpire.2                Umpire.3                Umpire.4 
                  "character"             "character"             "character" 
                    Home.team                    HQ1G                    HQ1B 
                  "character"               "integer"               "integer" 
                         HQ2G                    HQ2B                    HQ3G 
                    "integer"               "integer"               "integer" 
                         HQ3B                    HQ4G                    HQ4B 
                    "integer"               "integer"               "integer" 
                        HQETG                   HQETB              Home.score 
                    "integer"               "integer"               "integer" 
                    Away.team                    AQ1G                    AQ1B 
                  "character"               "integer"               "integer" 
                         AQ2G                    AQ2B                    AQ3G 
                    "integer"               "integer"               "integer" 
                         AQ3B                    AQ4G                    AQ4B 
                    "integer"               "integer"               "integer" 
                        AQETG                   AQETB              Away.score 
                    "integer"               "integer"               "integer" 
                         HQ1P                    HQ2P                    HQ3P 
                    "integer"               "integer"               "integer" 
                         HQ4P                   HQETP                    AQ1P 
                    "integer"               "integer"               "integer" 
                         AQ2P                    AQ3P                    AQ4P 
                    "integer"               "integer"               "integer" 
                        AQETP                  Player                    Team 
                    "integer"             "character"             "character" 
                          url                     Age            Career.Games 
                  "character"               "numeric"               "integer" 
                        Coach                     DOB               Home.Away 
                  "character"             "character"             "character" 

# fetch_player_stats_footywire returns expected column names and types

    Code
      col_names
    Output
       [1] "Date"           "Season"         "Round"          "Venue"         
       [5] "Player"         "Team"           "Opposition"     "Status"        
       [9] "Match_id"       "GA"             "CP"             "UP"            
      [13] "ED"             "DE"             "CM"             "MI5"           
      [17] "One.Percenters" "BO"             "TOG"            "K"             
      [21] "HB"             "D"              "M"              "G"             
      [25] "B"              "T"              "HO"             "I50"           
      [29] "CL"             "CG"             "R50"            "FF"            
      [33] "FA"             "AF"             "SC"             "CCL"           
      [37] "SCL"            "SI"             "MG"             "TO"            
      [41] "ITC"            "T5"            

---

    Code
      col_classes
    Output
                Date         Season          Round          Venue         Player 
              "Date"      "numeric"    "character"    "character"    "character" 
                Team     Opposition         Status       Match_id             GA 
         "character"    "character"    "character"      "numeric"      "numeric" 
                  CP             UP             ED             DE             CM 
           "numeric"      "numeric"      "numeric"      "numeric"      "numeric" 
                 MI5 One.Percenters             BO            TOG              K 
           "numeric"      "numeric"      "numeric"      "numeric"      "numeric" 
                  HB              D              M              G              B 
           "numeric"      "numeric"      "numeric"      "numeric"      "numeric" 
                   T             HO            I50             CL             CG 
           "numeric"      "numeric"      "numeric"      "numeric"      "numeric" 
                 R50             FF             FA             AF             SC 
           "numeric"      "numeric"      "numeric"      "numeric"      "numeric" 
                 CCL            SCL             SI             MG             TO 
           "numeric"      "numeric"      "numeric"      "numeric"      "numeric" 
                 ITC             T5 
           "numeric"      "numeric" 

