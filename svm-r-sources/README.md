# SVM-r-Sources

Esta es una coleccion de turinas para compilar documentos con formato LaTeX y Markdown desde RStudio.

Recomiendo copiar la carpeta svm-r-sources en el directorio raiz:

* En *Windows* en `C:`

* En *Linux* en `Home/<usr>`

* En *OS X* en `/`

## LaTeX

Para compilar los documentos es necesario tener la instalacion local de TeX y LaTeX. Puedes installar una de las siguientes distribuciones

1. [MikTeX](https://miktex.org/download)

2. [TeXLive](https://tug.org/texlive/)

En ambos casos, les recomiendo realizar la instalación completa.

## R

Necesitan tener instalado R o MRO.

1. [R](https://www.r-project.org/)

2. [MRO](https://mran.microsoft.com/download)

## RStudio

Instalen la version más reciente de RStudio.

1. [RStudio](https://www.rstudio.com/products/rstudio/download/)

## Uso

Editen el documento `*.Rmd` como usualmente se editan los documentos *Markdown*. Solo consideren que en el preambulo se invoca a los formatos LaTeX de esta libreria. 

El documento se compila desde RStudio con `knit`.
