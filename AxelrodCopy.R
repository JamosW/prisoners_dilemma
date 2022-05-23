
#containment for functions step 1
generate_strats <- function(x) {
  

  choices <- c(1, 0)
  
  all_d <- function(move = NULL, opp.choices = NULL) choices[2] 
  
  #Tit for tat strategy
  tit_for_tat <- function(move, opp.choices = NULL) {
    
    choice <- ifelse(move == 1, choices[1], 
                     ifelse(move > 1, opp.choices[move - 1]))#previous choice scan for defection
    return(choice)
  }
  
  #permanent retaliation strategy
  perm_retal <- function(move, opp.choices = NULL) {
    

      choice <- ifelse(move == 1, choices[1],
                       ifelse((move > 1 & any(opp.choices %in% choices[2])),   choices[2], 
                              #store opp choices in a vector, stored in higher function
                              choices[1])) #when the first 2 conditions aren't met, cooperate
    
    return(choice)
  }
  
  random <- function(move = NULL, opp.choices = NULL) sample(choices,1) #random choice strategy
  
  downing <- function() {## to be done in the future
    prev.choice
    opp.choices[length(opp.choices)]
  }
  
  joss <- function(move, opp.choices) {
    
    choice <- ifelse(move == 1, choices[1],
                        ifelse((move > 1 & opp.choices[move - 1]  == choices[2]), opp.choices[move - 1],
                        sample(choices, prob = c(0.9,0.1)))) #10% chance of defecting when the opp past choice was cooperate
    return(choice)
  }
  
  
  tit_for_two <- function(move, opp.choices) {
    
    choice <- ifelse(move <= 2, choices[1], #first two moves cooperate
                        ifelse(((move > 2) & any(opp.choices[(move-2):(move-1)] %in% choices[1])), choices[1],#opp past two choices evaluated
                               choices[2]))
    return(choice)
  }
  
  
  tester <- function(move, opp.choices = NULL) {
    
      test <- any(opp.choices %in% choices[2]) #any defection in opp past?
      
      choice <- ifelse(move == 1, choices[2], ifelse(
        move %in% 2:3, choices[1], ifelse(
          move > 3 & test, tit_for_tat(move, opp.choices), #tests if there has been retaliation in the past
                            ifelse((move %% 2 == 0), choices[2], choices[1])))) #alternate every move hereafter
    
      return(choice)
    } #on a side note, if_else or case_when don't work well with functions because they check for type and therefore they run the function even when false

    
  #create a list of the stratagies
  strats <- list("Always Defect" = all_d, "Tit for Tat" = tit_for_tat, "Permanent Retaliation" = perm_retal,
                 "Random" = random, "Joss" = joss, "Tester" = tester ,"Tit for Two" = tit_for_two)
  
  return(strats)
}

strats <- generate_strats()

#step 2, two stratagies face off
battle <- function(strat1,strat2, move = 1, payoffs = NULL) { 
  
  if (is.numeric(move)) {
    
    #score check to be used inside and outside of loop (first choice)
    score_check <- function(str1, str2) {
      
      dplyr::case_when(str1 == choices[1] & str2 == choices[1] ~ rep(payoffs[2], 2),
                       str1 == choices[1] & str2 == choices[2] ~ c(payoffs[4],payoffs[1]),
                       str1 == choices[2] & str2 == choices[1] ~ c(payoffs[1],payoffs[4]),
                       TRUE ~ rep(payoffs[3],2))
    }
    
    #store strategy choices, given amount of moves
    str_choices <- list(s1 = strat1(1), s2 = strat2(1))
    
    choices <- c(1,0)#possible choices 1 = cooperate
    #check payoffs for null and type
    if(!is.null(payoffs) & length(payoffs) == 4 & is.numeric(payoffs)) {
      payoffs = payoffs
    } else {
      payoffs <- c(5,3,1,0) #payoffs
    }
    scores <- c(score_check(str_choices$s1, str_choices$s2))#first choice store before loop

    if(move > 1) {
      for (i in length(scores):move ) {
        
        #call the functions hand them opp choices
        s1 <- str_choices[[1]][i] <- strat1(i, str_choices$s2)
        s2 <- str_choices[[2]][i] <- strat2(i, str_choices$s1)
        
        score <- score_check(s1, s2)
        
        scores[((i * 2) - 1):(i * 2)] <- score
      }
    }
    
  } else {
    stop("move must be numeric or integer")
  }
    
    return(scores[seq(1, length(scores), by = 2)])
}

battle_royale <- function(strats1,strats2, moves, payoffs = NULL) {
  
  #battle each strategy and convert values to tibble
  dat <- map(strats1, function(a) {
    as_tibble(
      map(strats2, function(b) {
        battle(a,b, moves, payoffs)
      }))
  }) %>%  
    map(function(x) {
      rbind(x, map(x, sum)) #summarise each column
    }) 
  
  table <- dat %>% 
    #Return the cumulative score for each strategy in data frame format then row sum and colum sum
    map_dfr( function(x) {
      x[(moves + 1 ), ]
    }) %>% 
    #summarise each column at the bottom
    rbind(score = map(., sum)) %>% 
    #row sum
    mutate(score = apply(.,1, sum)) %>% 
    t()
  
  dimnames(table) <- list(c(names(strats1), "Score Earned"), c(names(strats2), "Opponent Score"))
  
  
  return(list(dat, table) %>% setNames(c("data","table")))
}

myplot <- function(data, moves, strt1, strt2) {
  p <- ggplot(data = as_tibble(data)) + 
    geom_path(aes(x = 1:(length(data)), y = cumsum(data))) +
    labs(x = "Moves",
         y = "Cumulative sum", 
         title = paste(strt1, "vs", strt2)) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  
  info <- list(data, length(data), strt1, strt2)
  return(list(p, info))
}

#function for conditional text
cond_text <- function(list) {
  #takes plot data and generates info for conditional text
  info <- list[[2]] %>% setNames(c("data", "moves", "strategy1", "strategy2"))
  
  #message to be displayed based on condition
  messages <- list(
    function(str1, str2, string) {
      #message for long series of mutual cooperation or defection
      paste(str1, "and", str2, string)
    },
    #message for retaliatory series of defection
    function(str1, str2){paste(str1, "defected early and", 
                               str2, 
                               "defected thereafter, they both continued to defect for the rest of the way")},
    #a tester conditional message
    function(str1, str2, s1m ,s2m, xtra = NULL) {paste(str1, "defected round 1,", str2, s2m, s1m, xtra)},
    #message for random strategy
    function(str1, str2, string, fd) {paste(str1,
                                        "Played randomly and" ,str2, string)}
  )
  
  #list of conditions
  cond <- list(
    #check for series of defection after round 1
    all(info$data[2:info$moves] == 1), 
    #check for Tester
    any(c(info$strategy1,info$strategy2) == "Tester"),
    #check for series of cooperation
    all(info$data[4:info$moves] == 3),
    #is it random?
    any(c(info$strategy1,info$strategy2) == "Random") 
  )
  
  #round data to generate dynamic data stats
  round_data <- function(s) {
    fd = which(info$data[seq(s,length(info$data), 2)] %in% c(1,5))[1]
     return(list(fd = fd))
  }
                               
  #returns the string using round_data, messages and conditions
  str_return <- if(cond[[1]]) {
      if(info$data[1] == 1) {
        messages[[1]](info$strategy1, info$strategy2, "defected every round they played")
      } else if(info$data[1] == 5) {
        messages[[2]](info$strategy1, info$strategy2)
      } else {
        messages[[2]](info$strategy2, info$strategy1)
      }
  } else if(cond[[2]]) {
        if(all(info$data[4:info$moves] == 3) & info$strategy1 == "Tester") {
          messages[[3]](info$strategy1, info$strategy2, s2m = "appeared to retaliate",
                        s1m = info$strategy1, xtra = "therefore played tit for tat the rest of the way")
        } else if(all(info$data[4:info$moves] == 1 & info$strategy1 == "Tester")) {
          messages[[3]](info$strategy1, info$strategy2, s2m = "appeared to retaliate",
                        s1m = info$strategy1, xtra = "tried to make truce but was forced to defect the rest of the way")
        } else if(all(info$data[4:info$moves] == 3) & info$strategy2 == "Tester") {
          messages[[3]](info$strategy2, info$strategy1, s2m = "appeared to retaliate",
                        s1m = info$strategy2, xtra = "therefore played tit for tat the rest of the way")
        } else if(all(info$data[4:info$moves] == 1) & info$strategy2 == "Tester") {
          messages[[3]](info$strategy2, info$strategy1, s2m = "appeared to retaliate,",
                        s1m = info$strategy2, xtra = "tried to make truce but was forced to defect the rest of the way")
        }
  } else if(cond[[3]]) {
    if(all(info$data == 3)) {
      messages[[1]](info$strategy1, info$strategy2, "cooperated every round they played")
    }
  } else if(cond[[4]]) {
    if(info$strategy1 == "Random" & info$strategy2 != "Random") {
      messages[[4]](info$strategy1, info$strategy2, "tried to remain sane ")
    } else if(info$strategy2 == "Random" & info$strategy1 != "Random") {
      messages[[4]](info$strategy2, info$strategy1, "tried to remain sane ")
    } else {
      "Random Showdown"
    }
  } else {
    "Place holder Text, Working on it!"
  }
 
return(str_return)
}

#heatmap to be rendered
heatm <- function(table) {
  #turn the matrix table into a tibble, melt (for heat map), add another col (for heat map)
  as_tibble(table) %>%
    select(!`Opponent Score`) %>% 
    slice_head(n = c(nrow(.) - 1)) %>% 
    reshape2::melt() %>% 
    mutate(Strategies = rep( colnames(table)[1:(ncol(table) -1)], ncol(table) -1 )) %>%
    ggplot() +
    geom_tile(aes(fill = value, x= variable, y=  Strategies), colour = "white") +
    scale_fill_gradient(low = "white", high = "red") +
    labs(x = "Score Earned", y = "Opponent Score") +
    theme(axis.text.x = element_text(angle = 90, size = 12),
          axis.text.y = element_text(size = 12))
}

#plot for all the strategies to be drawn in line format
allPlot <- function(table) {
  ggplot(table) + map2(table, names(table), function(a,b) {
    geom_line( aes(x = 1:length(a), y = a, colour = b), size = 1)
  }) + theme_light() + scale_color_brewer(palette = "Set2") +
    labs(x = "Rounds",
         y= "Score",
         colour = "Strategies",
         title = "Cumulative Score of tournament strategies") +
    theme(plot.title = element_text(hjust = 0.5))
}

stratText <- function(strat) {
  switch(strat,
         "Always Defect" = paste(strat, "never wants to lose so it always defects."),
         "Permanent Retaliation" = paste(strat, "plans on cooperating every round, if defected against it will defect indefintely"),
         "Tit for Tat" =paste(strat, "Cooperates on the first round and imitates its opponent's previous move thereafter"),
         "Tit for Two" = paste(strat, "Cooperates unless defected against twice in a row."),
         "Random" = paste(strat, " opperates randomly"),
         "Joss" = paste(strat, "is like Tit for Tat, it always defects immediately after the 
         other player defects. But instead of always cooperating after
         the other player cooperates, 10 percent of the time it
                       defects after the other player cooperates."),
         "Tester" = paste(strat, "defects on the very first move in order to test the other's response.
         If the other player ever defects, it apologizes by cooperating
         and playing Tit for for the rest of the game. Otherwise,
         it cooperates on the second and third moves but defects
                          every other move after that."))
}




