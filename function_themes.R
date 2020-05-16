plotsize <- 1
font_size <- 16*plotsize

# font_import()
# loadfonts(device="win")


# default
{
  theme_basic <- function(legend_position="right",
                          grid.x=F,grid.y=F,grid=F,
                          font_size=16*plotsize,titlesize=NA,textsize=NA,stripsize=NA,
                          textface = "plain", titleface  ="plain",stripface="plain",
                          sub.hjust=0.5){

    textsize <- 0.7*font_size
    titlesize <- 0.8*font_size
    stripsize <- 0.8*font_size

    if(grid==T){grid.x <- T;grid.y <- T}

    theme(
      text = element_text(family = "Times New Roman", color = "black"),

      plot.margin = ggplot2::margin(2*plotsize,5*plotsize,2*plotsize,5*plotsize),
      plot.title       = element_text(size=titlesize,face=titleface,hjust=0.5),
      plot.subtitle    = element_text(size=titlesize,face=titleface,hjust=sub.hjust),
      plot.background  = element_rect(color = "white", fill = 'white'),
      panel.background = element_rect(fill="transparent", color="white",size=0.6*plotsize),
      panel.border     = element_rect(fill="transparent", color="black",size=0.6*plotsize),
      panel.ontop      = if(grid.x==T|grid.y==T){F}else{T},

      # panel.grid.major.x = element_line(color="grey90",size=0.1),
      panel.grid.major.x = if(grid.x==F){element_blank()}else{element_line(color="grey90",size=plotsize/4)},
      panel.grid.major.y = if(grid.y==F){element_blank()}else{element_line(color="grey90",size=plotsize/4)},
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),

      # axis.title.x       = element_blank(),
      axis.title.x       = element_text(size = titlesize,face = titleface, color = "black",margin=ggplot2::margin(4*plotsize,0,2*plotsize,0)),
      axis.title.y       = element_text(size = titlesize,face = titleface, color = "black",margin=ggplot2::margin(0,4*plotsize,0,2*plotsize)),
      axis.text.x        = element_text(size = textsize,face = textface, color = "black",margin=ggplot2::margin(2*plotsize,0,0,0)),
      axis.text.y        = element_text(size = textsize,face = textface, color = "black"),
      axis.title.y.right = element_blank(),
      axis.title.x.top   = element_blank(),

      axis.ticks =element_line(color="black",size=0.4*plotsize),
      axis.ticks.length  = unit(2*plotsize,"point"),

      strip.text.x  = element_text(face = stripface,size = stripsize, color = "black",margin=ggplot2::margin(0,0,2*plotsize,0)),
      strip.text.y  = element_text(face = stripface,size = stripsize, color = "black",margin=ggplot2::margin(0,plotsize,0,3*plotsize)),
      strip.background = element_rect(fill="transparent",color="transparent"),

      legend.title          = element_text(size = titlesize,face = titleface, color = "black", hjust=0, margin = ggplot2::margin(0,2*plotsize,0,2*plotsize)),
      legend.text           = element_text(size = textsize,face = textface, color = "black"),
      legend.title.align    = 0,
      legend.background     = element_blank(),
      legend.box.background = element_rect(color = "transparent", fill = "transparent"),
      legend.box.just       = "left",
      legend.key            = element_rect(fill = "transparent", colour = "black"),
      legend.key.size       = unit(20*plotsize,"point"))+
      theme_legend(legend_position)
  }

  # spatial
  {
    axis.title.y.left <- quote(element_text(size=titlesize,face=titleface,color="black",hjust=0.5,vjust=0.5))
    axis.title.y.right <- quote(element_text(size=titlesize,face=titleface,color="black",hjust=0.5,vjust=0.5,angle=270))
    axis.title.x.bottom <- quote(element_text(size=titlesize,face=titleface,color="black",hjust=0.5,vjust=0.5))
    axis.title.x.top <- quote(element_text(size=titlesize,face=titleface,color="black",hjust=0.5,vjust=0.5))
    axis.text.y.left <- quote(element_text(size=textsize,face=textface,color="black",hjust=0.5,vjust=0.5))
    axis.text.y.right <- quote(element_text(size=textsize,face=textface,color="black",hjust=0.5,vjust=0.5))
    axis.text.x.bottom <- quote(element_text(size=textsize,face=textface,color="black",hjust=0.5,vjust=0.5))
    axis.text.x.top <- quote(element_text(size=textsize,face=textface,color="black",hjust=0.5,vjust=0.5))


    theme_top_left <- function(legend_position="none",font_size=16*plotsize, textface = "plain", titleface = "plain",titlesize=NA,textsize=NA,y2.tit=F,y2.text=F){
      textsize <- 0.7*font_size;titlesize <- 0.8*font_size
      theme <- theme(axis.title.y.left=eval(axis.title.y.left),
                     axis.title.y.right=if(y2.tit==F){element_blank()}else{eval(axis.title.y.right)},
                     axis.title.x.bottom=element_blank(),
                     axis.title.x.top=element_blank(),
                     axis.text.y.left=eval(axis.text.y.left),
                     axis.text.y.right=if(y2.text==F){element_blank()}else{eval(axis.text.y.right)},
                     axis.text.x.bottom=element_blank(),
                     axis.text.x.top=element_blank())+
        theme_legend(legend_position)
    }
    theme_top_mid <- function(legend_position="none",font_size=16*plotsize, textface = "plain", titleface="plain",titlesize=NA,textsize=NA,y2.tit=F,y2.text=F){
      textsize <- 0.7*font_size;titlesize <- 0.8*font_size
      theme <- theme(axis.title.y.left=element_blank(),
                     axis.title.y.right=if(y2.tit==F){element_blank()}else{eval(axis.title.y.right)},
                     axis.title.x.bottom=element_blank(),
                     axis.title.x.top=element_blank(),
                     axis.text.y.left=element_blank(),
                     axis.text.y.right=if(y2.text==F){element_blank()}else{eval(axis.text.y.right)},
                     axis.text.x.bottom=element_blank(),
                     axis.text.x.top=element_blank())+theme_legend(legend_position)
    }
    theme_top_right <- function(legend_position="right",font_size=16*plotsize, textface = "plain", titleface="plain",titlesize=NA,textsize=NA,y2.tit=F,y2.text=F){
      textsize <- 0.7*font_size;titlesize <- 0.8*font_size
      theme <- theme(axis.title.y.left=element_blank(),
                     axis.title.y.right=if(y2.tit==F){element_blank()}else{eval(axis.title.y.right)},
                     axis.title.x.bottom=element_blank(),
                     axis.title.x.top=element_blank(),
                     axis.text.y.left=element_blank(),
                     axis.text.y.right=if(y2.text==F){element_blank()}else{eval(axis.text.y.right)},
                     axis.text.x.bottom=element_blank(),
                     axis.text.x.top=element_blank())+theme_legend(legend_position)
    }
    theme_mid_mid <- function(legend_position="none",font_size=16*plotsize, textface = "plain", titleface="plain",titlesize=NA,textsize=NA,y2.tit=F,y2.text=F){
      textsize <- 0.7*font_size;titlesize <- 0.8*font_size
      theme <- theme(axis.title.y.left=element_blank(),
                     axis.title.y.right=if(y2.tit==F){element_blank()}else{eval(axis.title.y.right)},
                     axis.title.x.bottom=element_blank(),
                     axis.title.x.top=element_blank(),
                     axis.text.y.left=element_blank(),
                     axis.text.y.right=if(y2.text==F){element_blank()}else{eval(axis.text.y.right)},
                     axis.text.x.bottom=element_blank(),
                     axis.text.x.top=element_blank())+theme_legend(legend_position)
    }
    theme_mid_right <- function(legend_position="right",font_size=16*plotsize, textface = "plain", titleface="plain",titlesize=NA,textsize=NA,y2.tit=F,y2.text=F){
      textsize <- 0.7*font_size;titlesize <- 0.8*font_size
      theme <- theme(axis.title.y.left=element_blank(),
                     axis.title.y.right=if(y2.tit==F){element_blank()}else{eval(axis.title.y.right)},
                     axis.title.x.bottom=element_blank(),
                     axis.title.x.top=element_blank(),
                     axis.text.y.left=element_blank(),
                     axis.text.y.right=if(y2.text==F){element_blank()}else{eval(axis.text.y.right)},
                     axis.text.x.bottom=element_blank(),
                     axis.text.x.top=element_blank())+theme_legend(legend_position)
    }
    theme_bottom_left <- function(legend_position="none",font_size=16*plotsize, textface = "plain", titleface="plain",titlesize=NA,textsize=NA,x.tit=T,y2.tit=F,y2.text=F){
      textsize <- 0.7*font_size;titlesize <- 0.8*font_size
      theme <- theme(axis.title.y.left=eval(axis.title.y.left),
                     axis.title.y.right=if(y2.tit==F){element_blank()}else{eval(axis.title.y.right)},
                     axis.title.x.bottom=if(x.tit==F){element_blank()}else{eval(axis.title.x.bottom)},
                     axis.title.x.top=element_blank(),
                     axis.text.y.left=eval(axis.text.y.left),
                     axis.text.y.right=if(y2.text==F){element_blank()}else{eval(axis.text.y.right)},
                     axis.text.x.bottom=eval(axis.text.x.bottom),
                     axis.text.x.top=element_blank())+theme_legend(legend_position)
    }
    theme_bottom_mid <- function(legend_position="none",font_size=16*plotsize, textface = "plain", titleface="plain",titlesize=NA,textsize=NA,x.tit=T,y2.tit=F,y2.text=F){
      textsize <- 0.7*font_size;titlesize <- 0.8*font_size
      theme <- theme(axis.title.y.left=element_blank(),
                     axis.title.y.right=if(y2.tit==F){element_blank()}else{eval(axis.title.y.right)},
                     axis.title.x.bottom=if(x.tit==F){element_blank()}else{eval(axis.title.x.bottom)},
                     axis.title.x.top=element_blank(),
                     axis.text.y.left=element_blank(),
                     axis.text.y.right=if(y2.text==F){element_blank()}else{eval(axis.text.y.right)},
                     axis.text.x.bottom=eval(axis.text.x.bottom),
                     axis.text.x.top=element_blank())+theme_legend(legend_position)
    }
    theme_bottom_right <- function(legend_position="right",font_size=16*plotsize, textface = "plain", titleface="plain",titlesize=NA,textsize=NA,x.tit=T,y2.tit=F,y2.text=F){
      textsize <- 0.7*font_size;titlesize <- 0.8*font_size
      theme <- theme(axis.title.y.left=element_blank(),
                     axis.title.y.right=if(y2.tit==F){element_blank()}else{eval(axis.title.y.right)},
                     axis.title.x.bottom=if(x.tit==F){element_blank()}else{eval(axis.title.x.bottom)},
                     axis.title.x.top=element_blank(),
                     axis.text.y.left=element_blank(),
                     axis.text.y.right=if(y2.text==F){element_blank()}else{eval(axis.text.y.right)},
                     axis.text.x.bottom=eval(axis.text.x.bottom),
                     axis.text.x.top=element_blank())+theme_legend(legend_position)
    }
  }
}

#	legend position
theme_legend <- function(legend_position = "right"){
  if(is.numeric(legend_position)){
    x <- theme(
      legend.position = legend_position,
      legend.justification = legend_position)
    legend_position<-"blip blop"
  }
  if(legend_position == "none"){
    x <- theme(
      legend.position = "none")
  }
  if(legend_position == "top_right"){
    x <- theme(
      legend.position = c(1,1),
      legend.justification = c(1,1))
  }
  if(legend_position == "top_mid"){
    x <- theme(
      legend.position = c(0.5,1),
      legend.justification = c(0.5,1))
  }
  if(legend_position == "top_mid_horizontal"){
    x <- theme(
      legend.position = c(0.5,1),
      legend.justification = c(0.5,1),
      legend.box = "horizontal",
      legend.box.just = "top")
  }
  if(legend_position == "top_right_horizontal"){
    x <- theme(
      legend.position = c(1,1),
      legend.justification = c(1,1),
      legend.box = "horizontal",
      legend.box.just = "top")
  }
  if(legend_position == "top_left_horizontal"){
    x <- theme(
      legend.position = c(0,1),
      legend.justification = c(0,1),
      legend.box = "horizontal",
      legend.box.just = "top")
  }
  if(legend_position == "top_left"){
    x <- theme(
      legend.position = c(0,1),
      legend.justification = c(0,1))
  }
  if(legend_position == "mid_right"){
    x <- theme(
      legend.position = c(1,0.5),
      legend.justification = c(1,0.5))
  }
  if(legend_position == "mid_left"){
    x <- theme(
      legend.position = c(0,0.5),
      legend.justification = c(0,0.5))
  }
  if(legend_position == "bottom_right"){
    x <- theme(
      legend.position = c(1,0),
      legend.justification = c(1,0))
  }
  if(legend_position == "bottom_mid"){
    theme(
      legend.position = c(0.5,0),
      legend.justification = c(0.5,0))
  }
  if(legend_position == "bottom_mid_horizontal"){
    x <- theme(
      legend.position = c(0.5,0),
      legend.justification = c(0.5,0),
      legend.box = "horizontal",
      legend.box.just = "top")
  }
  if(legend_position == "bottom_right_horizontal"){
    x <- theme(
      legend.position = c(1,0),
      legend.justification = c(1,0),
      legend.box = "horizontal",
      legend.box.just = "top")
  }
  if(legend_position == "bottom_left_horizontal"){
    x <- theme(
      legend.position = c(0,0),
      legend.justification = c(0,0),
      legend.box = "horizontal",
      legend.box.just = "top")
  }
  if(legend_position == "bottom_left"){
    x <- theme(
      legend.position = c(0,0),
      legend.justification = c(0,0))
  }
  if(legend_position == "above"){
    x <- theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "top")
  }
  if(legend_position == "above_right"){
    x <- theme(
      legend.position = "top",
      legend.justification = c(1,0),
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "top")
  }
  if(legend_position == "above_left"){
    x <- theme(
      legend.position = "top",
      legend.justification = c(0,0),
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "top")
  }
  if(legend_position == "below"){
    x <- theme(
      legend.position = "bottom",
      legend.direction = "horizontal")
  }
  if(legend_position == "below_right"){
    x <- theme(
      legend.position = "bottom",
      legend.justification = c(1,1),
      legend.direction = "vertical")
  }
  if(legend_position == "below_left"){
    x <- theme(
      legend.position = "bottom",
      legend.justification = c(0,1),
      legend.direction = "vertical")
  }
  if(legend_position == "right"){
    x <- theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.box="vertical",
      legend.justification = c(0,0.5))
  }

  return(x)

}