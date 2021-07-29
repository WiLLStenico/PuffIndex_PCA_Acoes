
# Instalando e carregando pacotes -----------------------------------------

pacotes <- c("plotly", "tidyverse","knitr","kableExtra","car","rgl","gridExtra",
             "PerformanceAnalytics","reshape2","rayshader","psych","pracma",
             "polynom","rqPen","ggrepel","factoextra","sp","tmap","magick","gridExtra","sjmisc", "data.table", "jpeg" )

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


rm(pacotes)


################# IMG #######################################################


photo <- readJPEG("dados/fradinho_2.jpg")
#photo <- photo[,,1]+photo[,,2]+photo[,,3]
#photo <- photo/max(photo)
#writeJPEG(photo, "dados/b&w_photo.jpg")
#writeJPEG(scale(photo), "dados/b&w_photo_STD.jpg")


str(photo)

#view(photo)



r <- photo[,,1]
g <- photo[,,2]
b <- photo[,,3]

str(photo)


#redPal <- colorRampPalette(c("black", "red"))
#greenPal <- colorRampPalette(c("black", "green"))
#bluePal <- colorRampPalette(c("black", "blue"))

#x11(width=9, height=2.5)
#par(mfcol=c(1,3))

#image(x=seq(ncol(r)), y=seq(nrow(r)), z=t(r), asp=1, xaxt="n", yaxt="n", bty="n", xlab="", ylab="", main="red channel", col=redPal(256))
#image(x=seq(ncol(g)), y=seq(nrow(g)), z=t(g), asp=1, xaxt="n", yaxt="n", bty="n", xlab="", ylab="", main="green channel", col=greenPal(256))
#image(x=seq(ncol(b)), y=seq(nrow(b)), z=t(b), asp=1, xaxt="n", yaxt="n", bty="n", xlab="", ylab="", main="blue channel", col=bluePal(256))

view(photo)



photo.r.pca <- prcomp(r, center = FALSE)
photo.g.pca <- prcomp(g, center = FALSE)
photo.b.pca <- prcomp(b, center = FALSE)

summary(photo.r.pca, maxsum = 4)
summary(photo.g.pca)
summary(photo.b.pca)

summary(photo.r.pca)$importance[2:3,1:10]




rgb.pca <- list(photo.r.pca, photo.g.pca, photo.b.pca)

nrow(photo)

for (i in seq.int(3, 500 , by = 25)) {#round(nrow(photo) - 10), length.out = 10)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
    
  }, simplify = 'array')
  writeJPEG(pca.img, paste('compressed/result_compressed_', round(i,0), '_components.jpg', sep = ''))
}

original <- file.info('dados/fradinho_2.jpg')$size / 1000
imgs <- dir('compressed/')

for (i in imgs) {
  full.path <- paste('compressed/', i, sep='')
  print(paste(i, ' size: ', file.info(full.path)$size / 1000, ' original: ', original, ' % diff: ', round((file.info(full.path)$size / 1000 - original) / original, 2) * 100, '%', sep = ''))
}
