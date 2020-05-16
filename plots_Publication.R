# # Info 
# Shawn Pedron  -  spedron@uci.edu  -  created 1/28/2020

# This script will make plots for ABoVE data

# Setup
{
  rm(list=ls()) # clear global environment
  options(device = "windows") # set plotting devise to open in new window
  source("function_themes.R")
  
  
  # Packages
  library(ggplot2);library(viridis);library(ggplotify);library(extrafont);library(patchwork) # Plotting
  library(data.table);library("openxlsx");library(stringr) # Organizing data
  
  # capture a legend
  g_legend <- function(a.gplot) {
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
  }
  
  # data
  {
    # Surveys
    {
      data_surveys <- data.table(read.xlsx("Datasets/Raw/Succession field data_2.xlsx",  sheet = "tidy"))
      colnames(data_surveys) <- c("Site","Site.Age","Forest.Type","Canopy","White.spruce","Deciduous","Black.spruce","Bare","Herbaceous","Moss","Lichen")
      
      data_surveys[,c("White.spruce","Deciduous","Black.spruce","Bare","Herbaceous","Moss","Lichen"):=lapply(.SD,function(x) as.numeric(gsub(0,NA,x))),.SDcols=c("White.spruce","Deciduous","Black.spruce","Bare","Herbaceous","Moss","Lichen")]
      data_surveys[,Open:=100-rowSums(.SD,na.rm=T),.SDcols=c("White.spruce","Deciduous","Black.spruce","Herbaceous","Moss","Lichen")]
      data_surveys[Open<0,Open:=0]
      data_surveys[,Total:=rowSums(.SD,na.rm=T),.SDcols=c("Open","Lichen","White.spruce","Deciduous","Black.spruce","Herbaceous","Moss")]
      data_surveys[,Bare:=NULL][,Total:=NULL]
      data_surveys[Site.Age==201,Site.Age:=199]
      
      # recalculate forest type
      {
        data_surveys[,WS.mean:=mean(na.omit(White.spruce)),by=Site]
        data_surveys[,BS.mean:=mean(na.omit(Black.spruce)),by=Site]
        data_surveys[,Dec.mean:=mean(na.omit(Deciduous)),by=Site]
        
        data_surveys[is.na(BS.mean),BS.mean:=0][is.na(WS.mean),WS.mean:=0][is.na(Dec.mean),Dec.mean:=0]
        
        data_surveys[BS.mean>WS.mean&BS.mean>Dec.mean,Forest.type.all:="BS"]
        data_surveys[WS.mean>BS.mean&WS.mean>=Dec.mean,Forest.type.all:="WS"]
        data_surveys[Dec.mean>BS.mean&Dec.mean>WS.mean,Forest.type.all:="D"]
        
        data_surveys[Forest.Type=="Early recovery",Forest.type.all:="Early recovery"]
        data_surveys[Forest.Type=="recent fire",Forest.type.all:="recent fire"]
        
        data_forests <- unique(data_surveys[order(Site)&Canopy!="Ground"],by=c("Site","Forest.type.all"))[order(Site)]
        # data_forests[,c("BS.mean","WS.mean","Dec.mean"):=lapply(.SD,function(x) as.numeric(gsub(0,NA,x))),.SDcols=c("BS.mean","WS.mean","Dec.mean")]
        data_forests[,Total.mean:=sum(na.omit(WS.mean),na.omit(BS.mean),na.omit(Dec.mean)),by=1:nrow(data_forests)]
        
        data_surveys[,Forest.Type:=data_forests[data_surveys,on=.(Site),Forest.type.all,allow.cartesian=T]]
        data_surveys[,c("WS.mean","BS.mean","Dec.mean","Forest.type.all"):=NULL]
      }
      
      data_surveys <- melt(data_surveys,id.vars=c("Site","Site.Age","Forest.Type","Canopy"))
      data_surveys[,variable:=gsub("\\."," ",variable)]
      data_surveys[,variable:=factor(variable,ordered=T,levels=rev(c("Moss","Black spruce","Herbaceous","White spruce","Deciduous","Lichen","Open")))]
      data_surveys[Canopy=="Lower",Canopy:="Lower canopy"]
      data_surveys[Canopy=="Upper",Canopy:="Upper canopy"]
      data_surveys[,Canopy:=factor(Canopy,ordered=T,levels=rev(c("Ground","Lower canopy","Upper canopy")))]
      
      Site.Age.seq <- seq(0,300,20)
      
      write.csv(data_forests[,.(Site,Site.Age,Forest.Type)],"Datasets/Processed/data_forests.csv")
      write.csv(data_surveys,"Datasets/Processed/data_surveys.csv")
    }
    
    # main
    {
      data_age <- data.table(read.xlsx("Datasets/Raw/Alaska NDVI data March 2019 update_new 15.4 data 20200128.xlsx",  sheet = "Sheet1"))
      
      data_age[Group=="WS",Forest.Type:="White spruce"]
      data_age[Group=="BS",Forest.Type:="Black spruce"]
      data_age[Group=="D",Forest.Type:="Deciduous"]
      data_age[Group=="PB",Forest.Type:="Pre-Burn"]
      data_age[Site.Age<0,Forest.Type:="Pre-Burn"]
      data_age$Group <- NULL
      
      data_age[,Forest.Type:=factor(Forest.Type,ordered = TRUE, levels = c("Deciduous", "White spruce", "Black spruce", "Pre-Burn"))]
      data_age[,Site.name:=factor(Site.name)]
      
      data_age <- data_age[complete.cases(NDVI),]
      
      data_age[,NDVI_Site.Age.slope:=coef(summary(lm(NDVI~Site.Age)))[2,1],by=.(Site.name,Forest.Type)]
      data_age[,NDVI_Site.Age.pval:=coef(summary(lm(NDVI~Site.Age)))[-1,4],by=.(Site.name,Forest.Type)]
      data_age[,NDVI_Site.Age.r2:=summary(lm(NDVI~Site.Age))$r.squared,by=.(Site.name,Forest.Type)]
      
      data_age[,Age.Group:=cut(Site.Age,breaks=c(min(Site.Age),0,13,33,max(Site.Age)),include.lowest=T)]
      data_age[,YOB:=NDVI.year-Site.Age]
      
      summary(aov(NDVI_Site.Age.slope~Forest.Type+Age.Group,data=data_age[Site.Age>=0]))
      
      write.csv(data_age,"Datasets/Processed/data_age.csv")
    }
    
    # LAI
    {
      data_LAI <- data.table(read.csv("Datasets/Raw/LAI NDVI FRAC AGE_updted for 15.4 20180128.csv"))
      
      colnames(data_LAI)[1] <- "Site.name"
      data_LAI[,Forest.Type:=data_forests[data_LAI,on=.(Site=Site.name),Forest.type.all,allow.cartesian=T]]
      data_LAI[,Frac.dec:=data_forests[data_LAI,on=.(Site=Site.name),Dec.mean/Total.mean,allow.cartesian=T]]
      
      data_LAI$Forest.Type <- factor(as.character(data_LAI$Forest.Type),levels=c("D","BS","WS"))
      data_surveys[,Site.Age:=data_LAI[data_surveys,on=.(Site.name=Site),Site.Age,by=.EACHI,allow.cartesian=T]$Site.Age]
      
      data_LAI <- data_LAI[complete.cases(Forest.Type)]
      
      data_LAI[Forest.Type=="WS",Forest.Type:="White spruce"]
      data_LAI[Forest.Type=="BS",Forest.Type:="Black spruce"]
      data_LAI[Forest.Type=="D",Forest.Type:="Deciduous"]
      
      data_LAI[,Forest.Type:=factor(Forest.Type,ordered = TRUE, levels = c("Deciduous", "White spruce", "Black spruce", "Pre-Burn"))]
      
      # LAI corrections
      {
        # https://reader.elsevier.com/reader/sd/pii/S0034425799000565?token=56F100EA9FD28C1C1C7A50ECFD590BC40CD4E146EED115E1B7718F5263AAA76A57142855D2F21292E805909802417420
        # gamma.BS <- 1.35
        # alpha.BS <- 0.12
        # omega.BS <- 0.38
        
        # gamma.WS <- 2.08
        # alpha.WS <- 0.07
        # omega.WS <- 1
        
        # from Nikki
        gamma.BS <- 1.5
        alpha.BS <- 0.15
        omega.BS <- 0.9
        
        gamma.WS <- gamma.BS
        alpha.WS <- alpha.BS
        omega.WS <- omega.BS
        
        data_LAI[,LAI.corrected:=LAI]
        data_LAI[Forest.Type=="Black spruce",LAI.corrected:=(1-alpha.BS)*LAI.corrected*gamma.BS/omega.BS]
        data_LAI[Forest.Type=="White spruce",LAI.corrected:=(1-alpha.WS)*LAI.corrected*gamma.WS/omega.WS]
        
        # data_LAI[Forest.Type=="Black spruce",LAI:=(1-alpha.BS)*LAI*gamma.BS]
        # data_LAI[Forest.Type=="White spruce",LAI:=(1-alpha.WS)*LAI*gamma.WS]
      }
      
      write.csv(data_LAI,"Datasets/Processed/data_LAI.csv")
    }
  }
  
  # Plotting
  {
    # plotsize <- 1
    plotsize <- 3
    
    vals.color <- c("Black spruce" = "grey30", "Pre-Burn" = "blue", "Deciduous" = "green3", "White spruce" = "grey65")
    vals.shape <- c("Deciduous" = 21, "White spruce" = 24, "Black spruce" = 25, "Pre-Burn" = 22)
  }
}

# Plots
{
  # Fig. 4
  {
    # a
    {
      pointsize <- 2.2*plotsize
      guide <- guide_legend(override.aes=list(size=1.5*pointsize))
      
      Fig4a <- ggplot(data_age,aes(x=Site.Age,y=NDVI,fill=Forest.Type,shape=Forest.Type))+
        geom_vline(xintercept = 0, linetype = "dashed", size = 0.5*plotsize, color = "black", alpha = 1) +
        geom_point(size=pointsize,color="black",stroke = 0.2*pointsize)+
        scale_fill_manual(name=NULL, values=vals.color) + 
        scale_shape_manual(name=NULL, values=vals.shape) +
        scale_y_continuous(limits = c(0,1),expand = c(0.0005,0.0005), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
        labs(y = "NDVI", x = "Tree core based stand age", title = " ") + 
        theme_basic()+
        guides(fill=guide,shape=guide)
      # Fig4a
      
      ggsave("Fig4a.jpeg",Fig4a,width = 8*plotsize, height = 8*plotsize*0.7,limitsize = FALSE,path="Plots/Fig4")
    }
    
    # b
    {
      pointsize <- 2.2*plotsize
      guide <- guide_legend(override.aes=list(size=1.2*pointsize))
      
      Fig4b <- ggplot(data_age,aes(x=Site.Age,y=NDVI,group=interaction(Forest.Type,Site.name),color=Forest.Type))+
        geom_vline(xintercept = 0, linetype = "dashed", size = 0.5*plotsize, color = "black", alpha = 1) +
        # geom_point(size=pointsize,color="black",stroke = 0.2*pointsize)+
        geom_smooth(method="lm",se=FALSE,size=0.55*plotsize)+
        scale_color_manual(name=NULL, values=vals.color) + 
        scale_y_continuous(limits = c(0,1),expand = c(0.0005,0.0005), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
        labs(y = "NDVI", x = "Tree core based stand age", title = " ") + 
        theme_basic()+
        guides(color=guide)
      # Fig4b
      
      ggsave("Fig4b.jpeg",Fig4b,width = 8*plotsize, height = 8*plotsize*0.7,limitsize = FALSE,path="Plots/Fig4")
    }
    
    # c
    {
      pointsize <- 2.2*plotsize
      guide <- guide_legend(override.aes=list(size=1.2*pointsize))
      
      # get point mapping
      {
        Fig4c <- ggplot(data_age,aes(x=Site.Age,y=NDVI,group=interaction(Forest.Type,Site.name),fill=Forest.Type,shape=Forest.Type,color=Forest.Type,size=-log10(NDVI_Site.Age.pval)))+
          stat_smooth(method="lm",se=FALSE,size=0.55*plotsize)+
          geom_point(color="black",stroke = 0.2*pointsize)+
          scale_color_manual(name=NULL, values=vals.color)+
          scale_fill_manual(name=NULL, values=vals.color) + 
          scale_shape_manual(name=NULL, values=vals.shape) +
          scale_radius(range = c(1*plotsize, 6*plotsize))+
          scale_y_continuous(limits = c(0,1),expand = c(0.0005,0.0005), breaks = c(0,0.2,0.4,0.6,0.8,1))

        d1 <- data.table(ggplot_build(Fig4c)$data[[1]])
        d1 <- d1[,lapply(.SD,mean),.SDcols=c("x","y"),by=group]
        
        d2 <- data.table(ggplot_build(Fig4c)$data[[2]])
        
        d1[,fill:=d2[d1,on=.(group),unique(fill),allow.cartesian=T,by=.EACHI]$V1]
        d1[,shape:=d2[d1,on=.(group),unique(shape),allow.cartesian=T,by=.EACHI]$V1]
        d1[,size:=d2[d1,on=.(group),unique(size),allow.cartesian=T,by=.EACHI]$V1]
      }

      Fig4c <- ggplot(data_age,aes(x=Site.Age,y=NDVI,group=interaction(Forest.Type,Site.name),fill=Forest.Type,shape=Forest.Type,color=Forest.Type,size=-log10(NDVI_Site.Age.pval)))+
        geom_point(color="black",stroke = 0.2*pointsize)+
        geom_point(color="white",fill="white",size=6*plotsize,stroke = 0.2*pointsize,show.legend=F)+
        geom_vline(xintercept = 0, linetype = "dashed", size = 0.5*plotsize, color = "black", alpha = 1) +
        stat_smooth(method="lm",se=FALSE,size=0.55*plotsize)+
        geom_point(data=d1,inherit.aes=F,aes(x=x,y=y),shape=d1$shape,size=d1$size,fill=d1$fill,stroke = 0.2*pointsize)+
        scale_color_manual(name=NULL, values=vals.color)+
        scale_fill_manual(name=NULL, values=vals.color) + 
        scale_shape_manual(name=NULL, values=vals.shape) +
        scale_radius(range = c(1*plotsize, 6*plotsize))+
        scale_y_continuous(limits = c(0,1),expand = c(0.0005,0.0005), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
        labs(y = "NDVI", x = "Tree core based stand age", title = " ",size="-log10(p-value)") + 
        theme_basic()+
        guides(color=guide)
      Fig4c
      
      ggsave("Fig4c.jpeg",Fig4c,width = 8*plotsize, height = 8*plotsize*0.7,limitsize = FALSE,path="Plots/Fig4")
    }
  }
  
  # Fig. 5
  {
    
    for(lai in c("LAI","LAI.corrected")){
      # setup
      {
        font_size <- 20*plotsize
        
        xlim <- data_LAI[,range(na.omit(get(lai)))]
        ylim <- c(min(na.omit(data_LAI$NDVI)),0.9)
        
        # sub
        subfxn <- function(Forest.type){
          ggplot(data_LAI[Forest.Type==paste(Forest.type)],aes(x=get(lai),y=NDVI,fill=Frac.dec,color=Frac.dec,shape=Forest.Type))+
            geom_point(size=0.5*pointsize,color="black",stroke = 0.5*0.15*pointsize)+
            geom_smooth(aes(x=get(lai),y=NDVI),inherit.aes=FALSE,color="black",method="lm",formula=y~x,size=0.1*pointsize)+
            scale_fill_viridis(limits=c(0,1),expand=expand_scale(0,0),na.value="white",guide=guide)+
            scale_color_viridis(limits=c(0,1),expand=expand_scale(0,0),na.value="white",guide=guide)+
            scale_x_continuous(limits=xlim,expand=expand_scale(mult=0.1),sec.axis = sec_axis(~ . ,labels=NULL))+
            scale_y_continuous(limits=ylim,expand=expand_scale(mult=0.1),sec.axis = sec_axis(~ . ,labels=NULL))+
            scale_shape_manual(name=NULL, values=vals.shape) +
            labs(y="NDVI",x="LAI",subtitle=bquote(.(Forest.type)))+
            theme_basic()+
            guides(shape=FALSE,color=FALSE,fill=FALSE)+
            annotate("text",x=data_LAI[,directlabels::midrange(na.omit(get(lai)))],y=ylim[2]-0.01,size=0.13*font_size,hjust=1.1,vjust=0,
                     label=bquote(italic("R")^2*" = "*.(round(summary(lm(data_LAI[Forest.Type==paste(Forest.type),NDVI]~data_LAI[Forest.Type==paste(Forest.type),get(lai)]))$r.squared,3))))+
            annotate("text",x=data_LAI[,directlabels::midrange(na.omit(get(lai)))],y=ylim[2]-0.01,size=0.13*font_size,hjust=-0.1,vjust=0,
                     label=bquote("p-value = "*.(round(summary(lm(data_LAI[Forest.Type==paste(Forest.type),NDVI]~data_LAI[Forest.Type==paste(Forest.type),get(lai)]))$coefficients[2,4],3))))+
            theme(plot.margin = ggplot2::margin(1*plotsize,1*plotsize,1*plotsize,1*plotsize))
        }
      }
      
      pointsize <- 3*plotsize
      guide <- guide_colorbar(title.hjust=0.5,frame.colour = "black",frame.linewidth = 0.5*plotsize,ticks.linewidth = 1*plotsize,ticks.colour = "white",nbin = 200,draw.ulim=FALSE,draw.llim=FALSE)
      
      shape_legend <-  g_legend(ggplot(data_LAI,aes(x=get(lai),y=NDVI,shape=Forest.Type))+
                                  geom_point(size=pointsize)+scale_shape_manual(name=NULL, values=vals.shape,guide=guide_legend(override.aes=list(size=pointsize,stroke = 0.2*pointsize)))+theme_basic())
      
      
      Fig5.sub <- ggplot(data_LAI,aes(x=get(lai),y=NDVI,fill=Frac.dec,color=Frac.dec,shape=Forest.Type))+
        geom_smooth(aes(x=get(lai),y=NDVI),inherit.aes=FALSE,color="black",method="lm",formula=y~x,size=0.1*pointsize)+
        geom_point(size=pointsize,color="black",stroke = 0.15*pointsize)+
        scale_fill_viridis(limits=c(0,1),expand=expand_scale(0,0),na.value="white",guide=guide)+
        scale_color_viridis(limits=c(0,1),expand=expand_scale(0,0),na.value="white",guide=guide)+
        scale_x_continuous(limits=xlim,expand=expand_scale(mult=0.1),sec.axis = sec_axis(~ . ,labels=NULL))+
        scale_y_continuous(limits=ylim,expand=expand_scale(mult=0.1),sec.axis = sec_axis(~ . ,labels=NULL))+
        scale_shape_manual(name=NULL, values=vals.shape) +
        labs(y="NDVI",x="LAI",fill="Fraction\nDeciduous\n")+
        theme_basic()+theme(legend.position = "right",legend.key.height=unit(50*plotsize,"point"),legend.justification = c(0, 0.5),
                            legend.box.background = element_rect(color = "transparent",  fill = "transparent", size = 0.4*plotsize),
                            legend.title=element_text(size = 0.7*20*plotsize,face="plain", color = "black"))+
        guides(shape=FALSE,color=FALSE)+
        annotate("text",x=data_LAI[,directlabels::midrange(na.omit(get(lai)))],y=ylim[2],size=0.13*font_size,hjust=1.1,vjust=0,
                 label=bquote(italic("R")^2*" = "*.(round(summary(lm(NDVI~get(lai),data=data_LAI))$r.squared,3))))+
        annotate("text",x=data_LAI[,directlabels::midrange(na.omit(get(lai)))],y=ylim[2],size=0.13*font_size,hjust=-0.1,vjust=0,
                 label=bquote("p-value = "*.(round(summary(lm(NDVI~get(lai),data=data_LAI))$coefficients[2,4],3))))
      
      legend <- ggplotify::as.ggplot(g_legend(Fig5.sub))
      
      sub5 <- {
        {subfxn("Deciduous")+theme(axis.text.y=element_text(size=0.4*font_size,margin=ggplot2::margin(0,1*plotsize,0,0)))}|
          {subfxn("White spruce")+theme(axis.text.y=element_blank())}|
          {subfxn("Black spruce")+theme(axis.text.y=element_blank())}}*theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
                                                                             axis.text.x=element_text(size=0.4*font_size,margin=ggplot2::margin(1*plotsize,0,0,0)),
                                                                             plot.subtitle=element_text(size=0.5*font_size,margin=ggplot2::margin(0,0,1*plotsize,0)),plot.background=element_rect(fill="transparent"),
                                                                             axis.ticks         = element_line(color="black",size=0.3*plotsize),
                                                                             axis.ticks.length  = unit(1*plotsize,"point"))
      
      # Fig5.sub <- Fig5.sub + annotation_custom(patchworkGrob(sub5),xmin=1,xmax=Inf,ymin=-Inf,ymax=0.60)
      Fig5.sub <- {{(sub5/Fig5.sub+theme(legend.position="none"))+plot_layout(heights=c(0.33,1))}|legend}+plot_layout(widths=c(1,0.15))
      # Fig5.sub
      
      ggsave(paste0("Fig5.",lai,".jpeg"),Fig5.sub,width = 8*plotsize, height = 8*plotsize*0.7,limitsize = FALSE,path="Plots/Fig5")
    }
  }
  
  # Frac.D_NDVI
  {
    # setup
    {
      font_size <- 20*plotsize
      
      xlim <- c(min(na.omit(data_LAI$Frac.dec)),max(na.omit(data_LAI$Frac.dec)))
      ylim <- c(min(na.omit(data_LAI$NDVI)),0.9)
    }
    
    # plot
    pointsize <- 3*plotsize
    guide <- guide_colorbar(title.hjust=0.5,frame.colour = "black",frame.linewidth = 0.5*plotsize,ticks.linewidth = 1*plotsize,ticks.colour = "white",nbin = 200,draw.ulim=FALSE,draw.llim=FALSE)
    
    shape_legend <-  g_legend(ggplot(data_LAI,aes(x=LAI,y=NDVI,shape=Forest.Type))+
                                geom_point(size=pointsize)+scale_shape_manual(name=NULL, values=vals.shape,guide=guide_legend(override.aes=list(size=pointsize,stroke = 0.2*pointsize)))+theme_basic())

    Frac.D_NDVI <- ggplot(data_LAI[complete.cases(LAI)&Frac.dec>0],aes(x=Frac.dec,y=NDVI,fill=LAI,color=LAI,shape=Forest.Type))+
      geom_smooth(aes(x=Frac.dec,y=NDVI),inherit.aes=FALSE,color="black",method="lm",formula=y~x,size=0.1*pointsize)+
      geom_point(size=pointsize,color="black",stroke = 0.15*pointsize)+
      scale_fill_viridis(expand=expand_scale(0,0),na.value="white",guide=guide)+
      scale_color_viridis(expand=expand_scale(0,0),na.value="white",guide=guide)+
      scale_x_continuous(limits=xlim,expand=expand_scale(mult=0.1),sec.axis = sec_axis(~ . ,labels=NULL))+
      scale_y_continuous(expand=expand_scale(mult=0.1),sec.axis = sec_axis(~ . ,labels=NULL))+
      scale_shape_manual(name=NULL, values=vals.shape) +
      labs(y="NDVI",x="Fraction Deciduous",fill="LAI")+
      theme_basic()+theme(legend.position = "right",legend.key.height=unit(50*plotsize,"point"),legend.justification = c(0, 0.5),
                          legend.box.background = element_rect(color = "transparent",  fill = "transparent", size = 0.4*plotsize),
                          legend.title=element_text(size = 0.7*20*plotsize,face="plain", color = "black"))+
      guides(shape=FALSE,color=FALSE)+
      annotate("text",x=data_LAI[,directlabels::midrange(na.omit(Frac.dec))],y=0.889,size=0.13*font_size,hjust=1.1,vjust=0,
               label=bquote(italic("R")^2*" = "*.(round(summary(lm(NDVI~Frac.dec,data=data_LAI[complete.cases(LAI)&Frac.dec>0]))$r.squared,3))))+
      annotate("text",x=data_LAI[,directlabels::midrange(na.omit(Frac.dec))],y=0.889,size=0.13*font_size,hjust=-0.1,vjust=0,
               label=bquote("p-value << 0.001 "))+
      annotation_custom(ggplotGrob(as.ggplot(shape_legend)),xmin=-Inf,xmax=Inf,ymin=0.8,ymax=Inf)
    
    # Frac.D_NDVI
    
    ggsave(paste0("Frac.D_NDVI.jpeg"),Frac.D_NDVI,width = 8*plotsize, height = 8*plotsize*0.7,limitsize = FALSE,path="Plots")
  }
  
  # Survey grids
  {
    vals <- "grouped"

    for(vals in c("grouped","staggered")){
      
      plotsize <- 2

      if(vals=="grouped"){data_surveys[,variable:=factor(variable,ordered=T,levels=rev(c("Moss","Herbaceous","Lichen","Black spruce","White spruce","Deciduous","Open")))]}else{
        data_surveys[,variable:=factor(variable,ordered=T,levels=rev(c("Moss","Black spruce","Herbaceous","White spruce","Deciduous","Lichen","Open")))]
      }
      
      vals.color <- c("grey80",rev(viridis(6)));names(vals.color) <- levels(data_surveys$variable)

      # By site
      {
        surveys <- ggplot(data_surveys,aes(x=Site.Age,y=value,fill=variable))+
          geom_bar(position=position_fill(reverse=F,vjust=0),stat="identity")+
          labs(subtitle="By site")+
          facet_grid(Canopy~.)+
          scale_y_continuous(name="Coverage (%)",labels = c(0,25,50,75,100))+
          scale_fill_manual(name="Cover",values=vals.color)+
          theme_basic(grid.y=T)+
          scale_x_continuous(name="Stand age (years)",breaks=Site.Age.seq)+
          theme(
            plot.subtitle = element_text(hjust=0),
            panel.spacing.y = unit(0, "lines"),
            panel.spacing.x = unit(0.2*plotsize, "lines"),
            strip.background = element_blank(),
            strip.placement = "outside",
            axis.text.y=element_text(size=0.5*16*plotsize),
            legend.key=element_rect(color="transparent"));surveys
        
        ggsave(paste0("surveys.bysite",".png"),surveys,width=8*plotsize,height=5*plotsize,limitsize=F,path=paste0("Plots/Surveys/",vals))
      }
      
      # By age bin
      {
        for(bin.width in c(10,20,25,50)){
          Site.Age.seq <- seq(0,300,bin.width)
          
          data <- copy(data_surveys)
          data[,Site.Age.cut:=cut(Site.Age,Site.Age.seq,include.lowest=F)]
          data[,Site.Age.labs:=mean(as.numeric(unlist(str_extract_all(Site.Age.cut,"[:digit:]+")))),by=1:nrow(data_surveys)]

          data <- data[,.(mean(na.omit(value)),sd(na.omit(value))/sqrt(length(na.omit(value)))),by=.(Canopy,variable,Site.Age.labs)]
          setnames(data,c("V1","V2","Site.Age.labs"),c("value","se","Site.Age"))

          surveys <- ggplot(data,aes(x=Site.Age,y=value,fill=variable))+
            geom_bar(position=position_fill(reverse=F,vjust=0),stat="identity")+
            labs(subtitle=bquote("By binned stand age ("*.(bin.width)*" years)"))+
            facet_grid(Canopy~.)+
            scale_y_continuous(name="Coverage (%)",labels = c(0,25,50,75,100))+
            scale_fill_manual(name="Cover",values=vals.color)+
            theme_basic(grid.y=T)+
            scale_x_continuous(name="Stand age (years)",breaks=Site.Age.seq)+
            theme(
              plot.subtitle = element_text(hjust=0),
              panel.spacing.y = unit(0, "lines"),
              panel.spacing.x = unit(0.2*plotsize, "lines"),
              strip.background = element_blank(),
              strip.placement = "outside",
              axis.text.y=element_text(size=0.5*16*plotsize),
              legend.key=element_rect(color="transparent"));surveys
          
          ggsave(paste0("surveys.agebins.",bin.width,".png"),surveys,width=8*plotsize,height=5*plotsize,limitsize=F,path=paste0("Plots/Surveys/",vals))
        }
      }
      
      # By forest bin
      {
        data <- copy(data_surveys)
        data[Forest.Type=="BS",Forest.Type:="Black spruce"]
        data[Forest.Type=="WS",Forest.Type:="White spruce"]
        data[Forest.Type=="D",Forest.Type:="Deciduous"]
        data[Forest.Type=="recent fire",Forest.Type:="Recently burned"]
        data[Forest.Type=="Early recovery",Forest.Type:="Early fire recovery"]
        data[,Forest.Type:=factor(Forest.Type,ordered=T,levels=c("Recently burned","Early fire recovery","Deciduous","White spruce","Black spruce"))]

        surveys <- ggplot(data,aes(x=Forest.Type,y=value,fill=variable))+
          geom_bar(position=position_fill(reverse=F,vjust=0),stat="identity")+
          labs(subtitle=bquote("By forest type"),x="")+
          facet_grid(Canopy~.)+
          scale_y_continuous(name="Coverage (%)",labels = c(0,25,50,75,100))+
          scale_fill_manual(name="Cover",values=vals.color)+
          theme_basic(grid.y=T)+
          theme(
            plot.subtitle = element_text(hjust=0),
            panel.spacing.y = unit(0, "lines"),
            panel.spacing.x = unit(0.2*plotsize, "lines"),
            strip.background = element_blank(),
            strip.placement = "outside",
            axis.text.y=element_text(size=0.5*16*plotsize),
            legend.key=element_rect(color="transparent"));surveys
        
        ggsave(paste0("surveys.forestbins.",".png"),surveys,width=10*plotsize,height=5*plotsize,limitsize=F,path=paste0("Plots/Surveys/",vals))
      }
      
      # By Stand age: forest facet
      {
        plotsize <- 4
        
        bin.width <- 50
        Site.Age.seq <- seq(0,300,bin.width)
        
        data <- copy(data_surveys)
        data[Forest.Type=="BS",Forest.Type:="Black spruce"]
        data[Forest.Type=="WS",Forest.Type:="White spruce"]
        data[Forest.Type=="D",Forest.Type:="Deciduous"]
        data[Forest.Type=="recent fire",Forest.Type:="Recently burned"]
        data[Forest.Type=="Early recovery",Forest.Type:="Early fire recovery"]
        data[,Forest.Type:=factor(Forest.Type,ordered=T,levels=c("Recently burned","Early fire recovery","Deciduous","White spruce","Black spruce"))]

        data[,Site.Age.cut:=cut(Site.Age,Site.Age.seq,include.lowest=T)]
        data[,Site.Age.labs:=mean(as.numeric(unlist(str_extract_all(Site.Age.cut,"[:digit:]+")))),by=1:nrow(data_surveys)]

        data <- data[,.(mean(na.omit(value)),sd(na.omit(value))/sqrt(length(na.omit(value)))),by=.(Forest.Type,Canopy,variable,Site.Age.labs)]
        setnames(data,c("V1","V2","Site.Age.labs"),c("value","se","Site.Age"))

        surveys <- ggplot(data,aes(x=Site.Age,y=value,fill=variable))+
          geom_bar(position=position_fill(reverse=F,vjust=0),stat="identity")+
          labs(subtitle=bquote("By binned stand age ("*.(bin.width)*" years)"))+
          facet_grid(Canopy~Forest.Type)+
          scale_y_continuous(name="Coverage (%)",labels = c(0,25,50,75,100))+
          scale_fill_manual(name="Cover",values=vals.color)+
          theme_basic(grid.y=T)+
          scale_x_continuous(name="Stand age (years)",breaks=Site.Age.seq)+
          theme(
            plot.subtitle = element_text(hjust=0),
            panel.spacing.y = unit(0, "lines"),
            panel.spacing.x = unit(0.4*plotsize, "lines"),
            strip.background = element_blank(),
            strip.placement = "outside",
            axis.text.y=element_text(size=0.5*16*plotsize),
            legend.key=element_rect(color="transparent"));surveys
        
        ggsave(paste0("surveys.forestfacet.agebins.",bin.width,"..png"),surveys,width=12*plotsize,height=5*plotsize,limitsize=F,path=paste0("Plots/Surveys/",vals))
      }
    }
  }
}













