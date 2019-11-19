![alt test](/R.jpg)

# -R_Workshop-Day03
Day03 of the 3-day R Workshop associated to https://www.youtube.com/c/maikelmendez
https://www.youtube.com/watch?v=ykX7Opd-F2w&list=PLeLqm9hls71fGhejxbcLtoee5DCuW0rE4

1_Copiar el directorio "FUNDA_R" hasta la partición de datos de la PC, en D:, caso contrario, copiarlo en C:
La idea, es que los estudiantes puedan copiar estos directorios y llevarselos a casa.
2_Instale Java 64 [jre-8u211-windows-x64.exe]
3_Instale R [R-3.6.1-win.exe]
4_Instale RStudio [RStudio-1.2.1335.exe]
5_Una vez en Rstudio, ejecute el script R_Libraries_Maikel.R y automaticamente les instala todos los libraries desde CRAN, pero anda con en 1 Gb.
6_Instale Anaconda (Python) [Anaconda3-2018.12-Windows-x86_64.exe]
7_Instale miktex   (LATEX) [basic-miktex-2.9.7100-x64.exe]
8_Instale Texmaker (LATEX) [Texmaker_5.0.2_Win_x64.msi]
9_Instale Editpad Lite [SetupEditPadLite.exe]
10_Instale Notepad ++ [npp.7.7.1.Installer.exe]

1_Favor bajar los contenidos de la carpeta FUNDA_R del siguiente enlace de Google Drive (alrededor de 2Gb):

https://drive.google.com/drive/folders/1fcU0xTnbPNqV_jkbsjUiTtnIwrDJCCAo?usp=sharing

2_Una vez en sus PCs (la mayoría laptops supongo), procedan a seguir las instrucciones del archivo FUNDA_R/Instalacion.txt. Pueden ampararse en el video: https://www.youtube.com/watch?v=cZymL2d_haE&t=0s&list=PLeLqm9hls71e77dvYXazX5kf1Jw1NY_xA&index=4
De antemano les manifiesto, todos estos instaladores son para Windows 7/10. Si alguien trabaja con LINUX (UBUNTU, OpenSUSE, Debian, CentOS) con mucho gusto le puedo ayudar. Por lo contrario, si trabajan con Mac....pues que pena, no les puedo ayudar.

3_La idea es que para el primer dia de clases, todos ustedes tengan ya instalado el software necesario en sus laptops.

4_Aquellos que puedan traer sus laptops a clases pues MEJOR, dado que estamos cortos de PCs, eso si, deben haber ejecutado toda la instalacion de software previamente.

5_Les dejo mis redes sociales, las cuales vamos a utilizar a lo largo del curso a manera de herramientas de trabajo:

# YouTube: https://www.youtube.com/c/maikelmendez
# Google Scholar: https://scholar.google.com/citations?user=JnmSVFYAAAAJ&hl=en
# Twitter: https://twitter.com/maikelmendezm

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
