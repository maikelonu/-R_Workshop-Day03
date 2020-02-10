![alt test](/R.jpg)

# -R_Workshop-Day03
Day03 of the 3-day R Workshop associated to https://www.youtube.com/c/maikelmendez
https://www.youtube.com/watch?v=ykX7Opd-F2w&list=PLeLqm9hls71fGhejxbcLtoee5DCuW0rE4

Vamos a estar trabajando con las plataformas https://edpuzzle.com/ (videos interactivos) y https://www.overleaf.com/ (LATEX) por lo que les ruego creen sus propias cuentas en ambos casos. 

Todo el material del curso está ya disponible en las siguientes plataformas:

# YouTube: https://www.youtube.com/c/maikelmendez (Videos Instruccionales)
# Github:  https://github.com/maikelonu (Código R, LATEX, PYTHON)
# Twitter: https://twitter.com/maikelmendezm (Avisos y Noticias)
# GoogleDrive: https://drive.google.com/open?id=1n-TGsKLQzsA-McrYBZwAznQ0-hDJxXkM (Textos)

En cuanto a la Instalación de software necesario para Windows-10 o 7:

Pueden ampararse en el video: https://www.youtube.com/watch?v=cZymL2d_haE&t=0s&list=PLeLqm9hls71e77dvYXazX5kf1Jw1NY_xA&index=4

1_Asegurarse de tener JAVA actualizado, si no lo tienen instalado, instalar jre-8u241-windows-x64.exe o superior
2_Instalar R-3.6.2 for Windows (32/64 bit) o superior (https://cran.r-project.org/bin/windows/base/)
3_Instale RStudio (1.2.5033) o superior (https://rstudio.com/products/rstudio/download/#download)
4_Una vez en Rstudio, ejecute el script R_Libraries_Maikel.R (https://github.com/maikelonu/Coding_Training_R_Workshop_Day01) y automaticamente les instalan todos los libraries desde CRAN.
5_Instale Editpad Lite (https://www.editpadlite.com/)
6_Instale Notepad ++ (https://notepad-plus-plus.org/downloads/)

Aquellos que desean correr LATEX localmente: (aunque mi recomendacion es trabajar en Overleaf!!!)

7_Instale miktex   (https://miktex.org/)
8_Instale Texmaker (https://www.xm1math.net/texmaker/)

 //////////////////////////////////////////////////////////////////////////////////
 INSTITUTO TECNOLOGICO DE COSTA RICA
 Escuela de Ingenieria en Construccion
 https://www.tec.ac.cr

 Eng. MSc. Maikel Mendez Morales
 email: maikel.mendez@gmail.com mamendez@itcr.ac.cr
 LinkedIn: https://www.linkedin.com/pub/maikel-mendez-morales/2a/738/1b9
 Google Scholar: https://scholar.google.com/citations?hl=en&user=JnmSVFYAAAAJ
 YouTube: https://www.youtube.com/channel/UCgpHYoMoZ059e508WVWiMhw
 //////////////////////////////////////////////////////////////////////////////////

 Workspace is cleared
rm(list = ls())

 Please install the following R-libraries in your Windows System:
install.packages("Agreement")
install.packages("arm")
install.packages("bbmle")
install.packages("binhf")
install.packages("car")
install.packages("corrgram")
install.packages("corrplot")
install.packages("DescTools")
install.packages("df.spss")
install.packages("dplyr")
install.packages("effects")
install.packages("foreign")
install.packages("GGally")
install.packages("ggalt")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("glmulti")
install.packages("Hmisc")
install.packages("lattice")
install.packages("lubridate")
install.packages("MASS")
install.packages("mgcv")
install.packages("mosaic")
install.packages("mosaicData")
install.packages("nls2")
install.packages("nlstools")
install.packages("pastecs")
install.packages("plyr")
install.packages("propagate")
install.packages("proto")
install.packages("reshape")
install.packages("reshape2")
install.packages("rJava")
install.packages("sjPlot")
install.packages("tidyr")
install.packages("vcd")
install.packages("vcdExtra")
install.packages("visreg")
install.packages("XLConnect")
install.packages("xlsx")
install.packages("JGR")
install.packages("rJava")
install.packages("JavaGD")
install.packages("iplots")
install.packages("Deducer")
install.packages("DeducerExtras")
install.packages("DeducerPlugInExample")
install.packages("DeducerPlugInScaling")
install.packages("DeducerSpatial")
install.packages("DeducerText")
install.packages("quantreg")
install.packages("JGR")
install.packages("rJava")
install.packages("JavaGD")
install.packages("iplots")
