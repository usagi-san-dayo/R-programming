setRefClass(
    Class = "mineSweeper",
 
    fields = list(
    n_row = "numeric",
    n_col = "numeric",
    n_bomb = "numeric",
    df = "data.frame",
    exploredTile = "data.frame",
    flaggedTile = "data.frame",
    gameEnd = "logical",
    mode = "character",
    remainingCells = "numeric"
    ),
    
    methods = list(
      Initialize = function(){
        gameEnd <<- FALSE
        mode <<- "explore"
        remainingCells <<- n_row * n_col - n_bomb
        df <<- data.frame(matrix(rep(NA, n_row), nrow = n_row))[,numeric(0)]
        for(i in 1:n_col){
        df <<- cbind(df, rep(0,n_row))
        }

        exploredTile <<- data.frame(matrix(rep(F, n_row), nrow = n_row))[,numeric(0)]
        for(i in 1:n_col){
          exploredTile <<- cbind(exploredTile, rep(F,n_row))
        }
        
       flaggedTile <<- data.frame(matrix(rep(F, n_row), nrow = n_row))[,numeric(0)]
       for(i in 1:n_col){
         flaggedTile <<- cbind( flaggedTile, rep(F,n_row))
       }
      },
      
      Start = function(){
        xy_bomb <- NULL
        for(i in 1:n_bomb){
          xy <- sample(setdiff(1:(n_row*n_col), xy_bomb),1)
          xy_bomb[i] <- xy
          df[(xy%/%n_col)+1,(xy%%n_row)+1] <<- (-1)
        }  
    
        for(i in 1:n_row){
          for(j in 1:n_col){
            if(df[i,j]!=-1){
              count <- 0
              if(i < n_row){
                if(df[i+1,j] == -1){count <- count + 1}
              }
              if(i > 1){
                if(df[i-1,j] == -1){count <- count + 1}
              }
              if(j < n_col){
                if(df[i,j+1] == -1){count <- count + 1}
              }
              if(j > 1){
                if(df[i,j-1] == -1){count <- count + 1}
              }
              if(i < n_row & j < n_col){
                if(df[i+1,j+1] == -1){count <- count + 1}
              }
              if(i < n_row & j > 1){
                if(df[i+1,j-1] == -1){count <- count + 1}
              }
              if(i > 1 & j < n_col){
                if(df[i-1,j+1] == -1){count <- count + 1}
              }
              if(i > 1 & j > 1){
                if(df[i-1,j-1] == -1){count <- count + 1}
              }
              df[i,j] <<- count
            }
          }
        }
      },

      Explore = function(x,y){
        if(x <= n_row & x >= 1 & y <= n_col & y >= 1){
          if(mode == "explore"){
            if(df[x,y] == -1 & exploredTile[x,y] == F){
              exploredTile[x,y] <<- T
              gameEnd <<- TRUE
              return()
            }else if(df[x,y] >= 1 & exploredTile[x,y] == F){ 
              exploredTile[x,y] <<- T
              return()
            }else if(df[x,y] == 0 & exploredTile[x,y] == F){
              if(x < n_row){
                exploredTile[x,y] <<- T
                Explore(x+1,y)
              }
              if(x > 1){
                exploredTile[x,y] <<- T
                Explore(x-1,y)
              }
              if(y < n_col){
                exploredTile[x,y] <<- T
                Explore(x,y+1)
              }
              if(y > 1){
                exploredTile[x,y] <<- T
                Explore(x,y-1)
              }
              if(x < n_row & y < n_col){
                exploredTile[x,y] <<- T
                Explore(x+1,y+1)
              }
              if(x < n_row & y > 1){
                exploredTile[x,y] <<- T
                Explore(x+1,y-1)
              }
              if(x > 1 & y < n_col){
                exploredTile[x,y] <<- T
                Explore(x-1,y+1)
              } 
              if(x > 1 & y > 1){
                exploredTile[x,y] <<- T
                Explore(x-1,y-1)
              }
            }
          }else{
            if(flaggedTile[x,y] == F){
              flaggedTile[x,y] <<- T
              return()
            }
          }
        }
      },

      GameStartPlot = function(){
        windows(n_col,n_row)
        plot.new()
        for(i in 1:n_row){
          for(j in 1: n_col){
            rect((j-1)/n_col , (1 - i/(n_row + 1)) , j/n_col, ( 1 + (1-i)/(n_row + 1)) , col = "grey", pin = c(n_col, (n_row + 1)), lty = 2)
          }
        }
        rect(0 , (1 - n_row/(n_row + 1)) , 1, 1 , pin = c(n_col, (n_row + 1)))
        
        rect(0.38, 0, 0.48, 1/(n_row + 1) - 1/(10*(n_row + 1)), col = rgb(0.25, 0.95, 0.81))
        text(list(x=0.43 ,y=1/(2*(n_row + 1))), labels = "explore") 
        
        rect(0.52, 0, 0.62, 1/(n_row + 1) - 1/(10*(n_row + 1)), col = rgb(0.25, 0.95, 0.81, alpha = 0.5))
        text(list(x=0.57 ,y=1/(2*(n_row + 1))), labels = "flag") 
        
        text(list(x=0.2 ,y=1/(2*(n_row + 1))), labels = paste0("remain:", remainingCells))
        text(list(x=0.8 ,y=1/(2*(n_row + 1))), labels = paste0("bombs:", n_bomb))
      },

      GamePlot = function(){
        library(png)
        pass <- getwd()
        setwd(paste0(pass, "/", "Picture"))
        bombImage <- readPNG("bomb.png")
        flagImage <- readPNG("flag.png")
        gameOverImage <- readPNG("gameover.png")
        gameClearImage <- readPNG("gameclear.png")
        setwd(pass)
        
        for(i in 1:n_row){
          for(j in 1:n_col){
            if(df[i,j] == 0 & exploredTile[i,j] == T){
              rect((j-1)/n_col , (1 - i/(n_row + 1)),j/n_col, ( 1 + (1-i)/(n_row + 1)), col = "white", pin = c(n_col, (n_row+1)))
            }
            else if(df[i,j] >= 1 & exploredTile[i,j] == T){
              rect((j-1)/n_col , (1 - i/(n_row + 1)),j/n_col, ( 1 + (1-i)/(n_row + 1)), col = "white", pin = c(n_col, (n_row + 1)))
              text(list(x=(2*j-1)/(2*n_col) ,y=(1 + (1-2*i)/(2*(n_row + 1)))), labels = as.character(df[i,j])) 
            }
            else if(df[i,j] == -1 & exploredTile[i,j] == T){
              if(mode == "explore"){
                for(m in 1:n_row){
                  for(n in 1:n_col){
                    if(df[m,n] == -1){
                      rect((n-1)/n_col , (1 - m/(n_row + 1)), n/n_col, ( 1 + (1-m)/(n_row + 1)), col = "white", pin = c(n_col, (n_row + 1)))
                      rasterImage(bombImage, (n-1)/n_col , (1 - m/(n_row + 1)),n/n_col, ( 1 + (1-m)/(n_row + 1)))
                    }
                  }
                }
              }
            }else if(flaggedTile[i,j] == T){
              rect((j-1)/n_col , (1 - i/(n_row + 1)), j/n_col, ( 1 + (1-i)/(n_row + 1)), col = "white", pin = c(n_col, (n_row + 1)))
              rasterImage(flagImage, (j-1)/n_col , (1 - i/(n_row + 1)),j/n_col, ( 1 + (1-i)/(n_row + 1)))
            }
          }
        }
        rect(0.1, 0, 0.3, 1/(n_row + 1) - 1/(10*(n_row + 1)), col = "white", lty = 0)
        text(list(x=0.2 ,y=1/(2*(n_row + 1))), labels = paste0("remain:", remainingCells))
        rect(0.7, 0, 0.9, 1/(n_row + 1) - 1/(10*(n_row + 1)), col = "white", lty = 0)
        text(list(x=0.8 ,y=1/(2*(n_row + 1))), labels = paste0("bombs:", n_bomb)) 
        if(mode == "explore"){
          rect(0.38, 0, 0.48, 1/(n_row + 1) - 1/(10*(n_row + 1)), col = "white")
          rect(0.38, 0, 0.48, 1/(n_row + 1) - 1/(10*(n_row + 1)), col = rgb(0.25, 0.95, 0.81))
          text(list(x=0.43 ,y=1/(2*(n_row + 1))), labels = "explore") 
          rect(0.52, 0, 0.62, 1/(n_row + 1) - 1/(10*(n_row + 1)), col = "white")
          rect(0.52, 0, 0.62, 1/(n_row + 1) - 1/(10*(n_row + 1)), col = rgb(0.25, 0.95, 0.81, alpha = 0.5))
          text(list(x=0.57 ,y=1/(2*(n_row + 1))), labels = "flag") 
        }else if(mode == "flag"){
          rect(0.38, 0, 0.48, 1/(n_row + 1) - 1/(10*(n_row + 1)), col = "white")
          rect(0.38, 0, 0.48, 1/(n_row + 1) - 1/(10*(n_row + 1)), col = rgb(0.25, 0.95, 0.81, alpha = 0.5))
          text(list(x=0.43 ,y=1/(2*(n_row + 1))), labels = "explore") 
          rect(0.52, 0, 0.62, 1/(n_row + 1) - 1/(10*(n_row + 1)), col = "white")
          rect(0.52, 0, 0.62, 1/(n_row + 1) - 1/(10*(n_row + 1)), col = rgb(0.25, 0.95, 0.81))
          text(list(x=0.57 ,y=1/(2*(n_row + 1))), labels = "flag") 
        }
        if(gameEnd == TRUE & remainingCells == 0){
          rasterImage(gameClearImage, 0, 0, 1, 1)
        }else if(gameEnd == TRUE & remainingCells > 0){
          rasterImage(gameOverImage, 0, 0, 1, 1)
        }
      },
      
      MousePointer = function(x,y){
        x0 <- 0
        y0 <- 0
        for(i in 1:(n_row+1)){
          if((1 - i/(n_row + 1)) <= y & y <= (1 + (1-i)/(n_row + 1))){
            y0 <- i
          }
        }
        for(i in 1:n_col){
          if((i-1)/n_col <= x & x <= i/n_col){
            x0 <- i
          }
        }
        return(list(y0, x0))
      },
      
      ConvertMode = function(x, y){
        if(y < ( 1 + (1-n_row)/(n_row + 1))){
          if(x >= 0.38 & x <= 0.48 & y <= 1/(n_row + 1) - 1/(10*(n_row + 1))){
            mode <<- "explore"
          }
          if(x >= 0.52 & x <= 0.62 & y <= 1/(n_row + 1) - 1/(10*(n_row + 1))){
            mode <<- "flag"
          }
        }
      },
      
      RemainingCells = function(){
        tileVec <- as.vector(exploredTile) 
        remainingCells <<- (n_row * n_col - n_bomb) - length(tileVec[tileVec == TRUE]) 
        if(remainingCells== 0){
          gameEnd <<- TRUE
        }
      }
    )
)

main <- function(){
  restart <- TRUE
  while(restart == TRUE){
    numberOfRows <- NULL
    numberOfCols <- NULL
    numberOfBombs <- NULL
    
    while(is.null(numberOfRows) | !is.numeric(as.numeric(numberOfRows))){
      numberOfRows <- readline("The number of rows:")
      if(is.na(as.numeric(numberOfRows))){
        numberOfRows <- NULL
      }
    }
    while(is.null(numberOfCols) | !is.numeric(as.numeric(numberOfCols))){
      numberOfCols <- readline("The number of cols:")
      if(is.na(as.numeric(numberOfCols))){
        numberOfCols <- NULL
      }
    }
    while(is.null(numberOfBombs) | !is.numeric(as.numeric(numberOfBombs))){
      numberOfBombs <- readline("The number of bombs:")
      if(is.na(as.numeric(numberOfBombs))){
        numberOfBombs <- NULL
      }
    }
  
    ms <- new("mineSweeper", n_row = as.numeric(numberOfRows), n_col = as.numeric(numberOfCols), n_bomb = as.numeric(numberOfBombs))
    ms$Initialize()
    ms$Start()
    ms$GameStartPlot()
    while(ms$gameEnd == FALSE){
      options(locatorBell = FALSE)
      position <- locator(1)
      set <- ms$MousePointer(position$x, position$y)[]
      ms$Explore(set[[1]], set[[2]])
      ms$ConvertMode(position[1]$x, position[2]$y)
      ms$RemainingCells()
      ms$GamePlot()
    }
    
    continue <- NULL
    while(is.null(continue) | !is.numeric(as.numeric(continue))){
      continue <- readline("Do you wanto to Restart? Yes:1 No:0 ")
      if(is.na(as.numeric(continue))){
        continue <- NULL
      }
    }
    if(continue == 0){restart <- FALSE}
  }
}

