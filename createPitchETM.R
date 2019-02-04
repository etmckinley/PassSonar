#Based on the createPitch function written by FC_Rstats https://github.com/FCrSTATS
#Modified by Eliot McKinley (etmckinley@gmail.com) to use yards as the unit of measurement,
#enable plotting of a half pitch, and midifications to allow faceting

createPitch <- function(xmax=115, ymax=80, grass_colour="white", line_colour="gray", background_colour="white", goal_colour="gray", data=NULL, halfPitch=FALSE){
  
  GoalWidth <- 8
  penspot <- 12
  boxedgeW <- 44
  boxedgeL <- 18
  box6yardW <- 20
  box6yardL <- 6
  corner_d=3
  centreCirle_d <- 20
  
  # The 18 Yard Box
  TheBoxWidth <- c(((ymax / 2) + (boxedgeW / 2)),((ymax / 2) - (boxedgeW / 2)))
  TheBoxHeight <- c(boxedgeL,xmax-boxedgeL)
  GoalPosts <- c(((ymax / 2) + (GoalWidth / 2)),((ymax / 2) - (GoalWidth / 2)))
  
  # The 6 Yard Box
  box6yardWidth <- c(((ymax / 2) + (box6yardW / 2)),((ymax / 2) - (box6yardW / 2)))
  box6yardHeight <- c(box6yardL,xmax-box6yardL)
  
  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  #### create leftD arc ####
  Dleft <- circleFun(c((penspot),(ymax/2)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  Dleft <- Dleft[which(Dleft$x >= (boxedgeL)),]
  
  ## create rightD arc  ####
  Dright <- circleFun(c((xmax-(penspot)),(ymax/2)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  Dright <- Dright[which(Dright$x <= (xmax-(boxedgeL))),]
  
  #### create center circle ####
  center_circle <- circleFun(c((xmax/2),(ymax/2)),centreCirle_d,npoints = 2000)
  
  
  if (halfPitch==FALSE){
    xmin=0
    ymin=0
    
    ## create corner flag radius ####
    TopLeftCorner <- circleFun(c(xmin,ymax),corner_d,npoints = 1000)
    TopLeftCorner <- TopLeftCorner[which(TopLeftCorner$x > (xmin)),]
    TopLeftCorner <- TopLeftCorner[which(TopLeftCorner$y < (ymax)),]
    TopRightCorner <- circleFun(c(xmax,ymax),corner_d,npoints = 1000)
    TopRightCorner <- TopRightCorner[which(TopRightCorner$x < (xmax)),]
    TopRightCorner <- TopRightCorner[which(TopRightCorner$y < (ymax)),]
    
    BottomLeftCorner <- circleFun(c(xmin,ymin),corner_d,npoints = 1000)
    BottomLeftCorner <- BottomLeftCorner[which(BottomLeftCorner$x > (xmin)),]
    BottomLeftCorner <- BottomLeftCorner[which(BottomLeftCorner$y > (ymin)),]
    
    BottomRightCorner <- circleFun(c(xmax,ymin),corner_d,npoints = 1000)
    BottomRightCorner <- BottomRightCorner[which(BottomRightCorner$x < (xmax)),]
    BottomRightCorner <- BottomRightCorner[which(BottomRightCorner$y > (ymin)),]
    
    
    
    ggplot(data=data) + #xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
      # add the theme 
      #theme_blankPitch() +
      # add the base rectangle of the pitch 
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour)+

      # add the 18 yard box Left
      geom_rect(aes(xmin=0, xmax=TheBoxHeight[1], ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = grass_colour, colour = line_colour) + 
      # add the 18 yard box Right
      geom_rect(aes(xmin=TheBoxHeight[2], xmax=xmax, ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = grass_colour, colour = line_colour) +
      # add the six yard box Left
      geom_rect(aes(xmin=0, xmax=box6yardHeight[1], ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = grass_colour, colour = line_colour)  +
      # add the six yard box Right
      geom_rect(aes(xmin=box6yardHeight[2], xmax=xmax, ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = grass_colour, colour = line_colour)  + 
      # Add half way line 
      geom_segment(aes(x = xmax/2, y = ymin, xend = xmax/2, yend = ymax),colour = line_colour) +
      # add left D 
      geom_path(data=Dleft, aes(x=x,y=y), colour = line_colour) + 
      # add Right D 
      geom_path(data=Dright, aes(x=x,y=y), colour = line_colour) +
      # add centre circle 
      geom_path(data=center_circle, aes(x=x,y=y), colour = line_colour) +
      
      # add penalty spot left 
      geom_point(aes(x = penspot , y = ymax/2), colour = line_colour) + 
      # add penalty spot right
      geom_point(aes(x = (xmax-(penspot)) , y = ymax/2), colour = line_colour) + 
      # add centre spot 
      geom_point(aes(x = (xmax/2) , y = ymax/2), colour = line_colour) +
      # add Corner Flag corners
      geom_path(data=TopLeftCorner, aes(x=x,y=y), colour = line_colour) +
      geom_path(data=TopRightCorner, aes(x=x,y=y), colour = line_colour) +
      geom_path(data=BottomLeftCorner, aes(x=x,y=y), colour = line_colour) +
      geom_path(data=BottomRightCorner, aes(x=x,y=y), colour = line_colour) +
      geom_segment(aes(x = xmin-0.2, y = GoalPosts[1], xend = xmin-0.2, yend = GoalPosts[2]),colour = goal_colour, size = 1) +
      # add the goal right
      geom_segment(aes(x = xmax+0.2, y = GoalPosts[1], xend = xmax+0.2, yend = GoalPosts[2]),colour = goal_colour, size = 1) +
      
      coord_fixed() +
      theme(rect = element_blank(),#, #remove additional ggplot2 features: lines, axis, etc...
            line = element_blank(), 
            #legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
}
  
  else{
    xmin=(xmax/2)
    ymin=0
    center_circle = center_circle[which(center_circle$x>=xmin),]
    
    ## create corner flag radius ####
    BottomRightCorner <- circleFun(c(xmax,ymin),corner_d,npoints = 1000)
    BottomRightCorner <- BottomRightCorner[which(BottomRightCorner$x < (xmax)),]
    BottomRightCorner <- BottomRightCorner[which(BottomRightCorner$y > (ymin)),]
    TopRightCorner <- circleFun(c(xmax,ymax),corner_d,npoints = 1000)
    TopRightCorner <- TopRightCorner[which(TopRightCorner$x < (xmax)),]
    TopRightCorner <- TopRightCorner[which(TopRightCorner$y < (ymax)),]
    
    ggplot(data=data) + #xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
      # add the theme 
      #theme_blankPitch() +
      # add the base rectangle of the pitch 
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour)+ 
      # add the 18 yard box offensive
      geom_rect(aes(xmin=TheBoxHeight[2], xmax=xmax, ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = grass_colour, colour = line_colour)+ 
      # add the six yard box offensive
      geom_rect(aes(xmin=box6yardHeight[2], xmax=xmax, ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = grass_colour, colour = line_colour)+  
      # add the arc circle 
      geom_path(data=Dright, aes(x=x,y=y), colour = line_colour)+
      #add center arc
      geom_path(data=center_circle, aes(x=x,y=y), colour = line_colour)+
      # add penalty spot 
      
      geom_point(aes(x = (xmax-(penspot)) , y = ymax/2), colour = line_colour) +
      # add centre spot 
      geom_point(aes(x = (xmax/2) , y = ymax/2), colour = line_colour) +
      #geom_point(aes(x = CentreSpot , y = penSpotOff), colour = line_colour) +
      # add Corner Flag corners
      geom_path(data=BottomRightCorner, aes(x=x,y=y), colour = line_colour) +
      geom_path(data=TopRightCorner, aes(x=x,y=y), colour = line_colour) +

      
      # add the goal right
      geom_segment(aes(x = xmax+0.2, y = GoalPosts[1], xend = xmax+0.2, yend = GoalPosts[2]),colour = goal_colour, size = 1) +
      # add the goal offensive
      
    
    coord_fixed() +
      theme(rect = element_blank(), #remove additional ggplot2 features: lines, axis, etc...
            line = element_blank(), 
            #legend.position = "none",
            axis.title.y = element_blank(), 
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
  }
  
  
  
}