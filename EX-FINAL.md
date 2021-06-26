Ex Final Diseño de Experimentos
================

## Brayan Alonso Reyes Sepulveda, cod: 1950182

## Analisis

Aplicaremos un analisis factorial (AFE) a la base de datos “DISE” la
cual contiene los datos de un grupo de aficionados a los videojuegos en
donde se midieron 7 variables con 30 observaciones y cada columna
recopila los siguientes datos:

ID: da un conteo del numero de encuestados.

Edad: la respectiva edad de cada persona en la poblacion muestral.

Hrs Juego\*Dia: cantidad de horas que invierte jugando algun videojuego
en horario diurno.

Hrs Juego\*Noche: cantidad de horas que invierte jugando algun
videojuego en horario nocturno.

Sistema O.: sistema operativo en el cual prefiere jugar, siendo Android=
1 y Windows= 0.

Dias\*Semana: cantidad de dias por semana en los que durante algun
momento del dia o noche decide jugar algun videojuego.

inversion monetaria: cantidad de dinero en pesos colombianos que ha
gastado en algun o algunos videojuegos.

## importar base de datos

``` r
library(readxl)
datos<- read_excel("C:/Users/Reyes/Desktop/UFPS/DISE.xlsx")
```

## Tipificacion

``` r
datost<- datos
datost<- scale(datost, center = T, scale = T)
datost<- as.data.frame(datost)
```

## Normalidad Multivariante

H0: Normalidad mutivariante  
H1: no normalidad multivariante  
Confianza: 95%  
Alfa = 5% = 0.05  
P value &gt; alfa: no se rechaza la H0 (Normalidad multivariante)  
P value &lt; alfa: se rechaza la H0 (no normalidad)

``` r
library(MVN)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## sROC 0.1-2 loaded

``` r
mvn(datost[2:7])
```

    ## $multivariateNormality
    ##              Test          Statistic            p value Result
    ## 1 Mardia Skewness   74.8338902328152 0.0471234320215575     NO
    ## 2 Mardia Kurtosis -0.208764890583585  0.834631780651667    YES
    ## 3             MVN               <NA>               <NA>     NO
    ## 
    ## $univariateNormality
    ##           Test            Variable Statistic   p value Normality
    ## 1 Shapiro-Wilk        Edad            0.9117  0.0164      NO    
    ## 2 Shapiro-Wilk    Hrs Juego*Dia       0.8802  0.0029      NO    
    ## 3 Shapiro-Wilk   Hrs Juego*Noche      0.9116  0.0163      NO    
    ## 4 Shapiro-Wilk     Sistema O.         0.6119  <0.001      NO    
    ## 5 Shapiro-Wilk     Dias*Semana        0.7084  <0.001      NO    
    ## 6 Shapiro-Wilk inversion monetaria    0.7290  <0.001      NO    
    ## 
    ## $Descriptives
    ##                      n          Mean Std.Dev     Median        Min       Max
    ## Edad                30 -9.769845e-16       1  0.2667689 -1.7339978 1.6006134
    ## Hrs Juego*Dia       30  2.963486e-17       1 -0.1606886 -1.3658527 2.8522219
    ## Hrs Juego*Noche     30 -1.184274e-16       1 -0.1574367 -1.7318041 2.9912979
    ## Sistema O.          30 -7.401848e-17       1 -0.7480970 -0.7480970 1.2921675
    ## Dias*Semana         30 -1.258415e-16       1  0.7101745 -1.9529799 0.7101745
    ## inversion monetaria 30 -2.635515e-17       1 -0.2415772 -0.8073742 3.8875376
    ##                           25th      75th       Skew   Kurtosis
    ## Edad                -1.0670756 0.7669606 -0.2159519 -1.0500432
    ## Hrs Juego*Dia       -0.7632706 0.4418935  0.9885252  0.3717177
    ## Hrs Juego*Noche     -0.7478245 0.6297469  0.7102146  0.8624932
    ## Sistema O.          -0.7480970 1.2921675  0.5259349 -1.7794099
    ## Dias*Semana         -0.8433322 0.7101745 -0.9402093 -0.7358711
    ## inversion monetaria -0.6769600 0.4305577  2.1913842  5.4098052

dado que P value &gt; alfa, no se rechaza la H0 por lo tanto hay
normalidad multivariante.

## Matriz de correlaciones

H0: Correlacion= 0 (no hay correlacion)  
H1: Correlacion diferente de 0 (si hay correlacion)  
es necesario rechazar la H0 para aplicar el AFE.

``` r
library(psych)
corr.test(datost[,2:7])
```

    ## Call:corr.test(x = datost[, 2:7])
    ## Correlation matrix 
    ##                      Edad Hrs Juego*Dia Hrs Juego*Noche Sistema O. Dias*Semana
    ## Edad                 1.00         -0.18           -0.17      -0.36       -0.20
    ## Hrs Juego*Dia       -0.18          1.00            0.12       0.30        0.36
    ## Hrs Juego*Noche     -0.17          0.12            1.00       0.10        0.09
    ## Sistema O.          -0.36          0.30            0.10       1.00        0.36
    ## Dias*Semana         -0.20          0.36            0.09       0.36        1.00
    ## inversion monetaria  0.24         -0.09           -0.27       0.28        0.33
    ##                     inversion monetaria
    ## Edad                               0.24
    ## Hrs Juego*Dia                     -0.09
    ## Hrs Juego*Noche                   -0.27
    ## Sistema O.                         0.28
    ## Dias*Semana                        0.33
    ## inversion monetaria                1.00
    ## Sample Size 
    ## [1] 30
    ## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
    ##                     Edad Hrs Juego*Dia Hrs Juego*Noche Sistema O. Dias*Semana
    ## Edad                0.00          1.00            1.00       0.74        1.00
    ## Hrs Juego*Dia       0.35          0.00            1.00       1.00        0.74
    ## Hrs Juego*Noche     0.36          0.52            0.00       1.00        1.00
    ## Sistema O.          0.05          0.11            0.60       0.00        0.74
    ## Dias*Semana         0.30          0.05            0.63       0.05        0.00
    ## inversion monetaria 0.21          0.63            0.16       0.13        0.08
    ##                     inversion monetaria
    ## Edad                               1.00
    ## Hrs Juego*Dia                      1.00
    ## Hrs Juego*Noche                    1.00
    ## Sistema O.                         1.00
    ## Dias*Semana                        0.93
    ## inversion monetaria                0.00
    ## 
    ##  To see confidence intervals of the correlations, print with the short=FALSE option

``` r
correlaciones<- corr.test(datost[,2:7])
correlaciones$r
```

    ##                           Edad Hrs Juego*Dia Hrs Juego*Noche  Sistema O.
    ## Edad                 1.0000000   -0.17737949     -0.17378989 -0.35659667
    ## Hrs Juego*Dia       -0.1773795    1.00000000      0.12103919  0.29958448
    ## Hrs Juego*Noche     -0.1737899    0.12103919      1.00000000  0.09968666
    ## Sistema O.          -0.3565967    0.29958448      0.09968666  1.00000000
    ## Dias*Semana         -0.1959853    0.35784507      0.09156653  0.36223597
    ## inversion monetaria  0.2365064   -0.09077801     -0.26621076  0.28422453
    ##                     Dias*Semana inversion monetaria
    ## Edad                -0.19598531          0.23650641
    ## Hrs Juego*Dia        0.35784507         -0.09077801
    ## Hrs Juego*Noche      0.09156653         -0.26621076
    ## Sistema O.           0.36223597          0.28422453
    ## Dias*Semana          1.00000000          0.32762919
    ## inversion monetaria  0.32762919          1.00000000

``` r
r<- as.matrix(correlaciones$r)
```

no se rechaza la H0, pero igual aplicaremos el AFE.

## Indicadores de aplicabilidad del AFE (bondad del ajuste)

### contraste de esfericidad de Bartltett

``` r
dim(datost)
```

    ## [1] 30  7

``` r
cortest.bartlett(r, n=30)
```

    ## $chisq
    ## [1] 26.51562
    ## 
    ## $p.value
    ## [1] 0.03293987
    ## 
    ## $df
    ## [1] 15

como el P value es menor a alfa, se rechaza la H0, por lo tanto las
correlaciones entre cada par de variables es nulo.  
es decir si es aplicable el AFE.

### medida de adecuacion muestral de Kaiser, Meyer y Oklin (KMO)

se mantiene en el modelo, si el KMO es igual o mayor a 0,7.  
se elimina una variable del modelo si el KMO es menor a 0.7.

``` r
KMO(r)
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = r)
    ## Overall MSA =  0.47
    ## MSA for each item = 
    ##                Edad       Hrs Juego*Dia     Hrs Juego*Noche          Sistema O. 
    ##                0.44                0.51                0.52                0.53 
    ##         Dias*Semana inversion monetaria 
    ##                0.55                0.34

KMO= 0.47 el modelo es inaceptable todas las varibles estan en valores
miserables y inaceptables :/ por lo tanto no se recomienda hacer el AFE.

## Determinacion del numero de factores a extraer.

### Metodo de los componentes principales iteradas.

``` r
fa.parallel(r, fm="pa", n.obs = 30, ylabel = "Eigenvalues")
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

![](EX-FINAL_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  2

Con el metodo de los ejes principales no se extraeria ningun factor.

### Metodo de los componentes principales.

sirve para modelos con normalidad multivariante.

``` r
fa.parallel(r, fm="pc", n.obs = 30, ylabel = "Eigenvalues")
```

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

![](EX-FINAL_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  1

Con el metodo de las componentes principales no se recomienda extraer
ningun factor.

### Metodo de la maxima verosimilitud.

sirve para modelos con normalidad multivariante.

``` r
fa.parallel(r, fm="ml", n.obs = 30, ylabel = "Eigenvalues")
```

![](EX-FINAL_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  1

Con el metodo de la maxima verosimilitud no se recomienda extraer
factores.

### Metodo paralelo con iteraciones.

sirve para modelos con normalidad multivariante.

``` r
library(MASS)
library(paran)
paran(r, iterations = 5000, graph = T)
```

    ## 
    ## Using eigendecomposition of correlation matrix.
    ## Computing: 10%  20%  30%  40%  50%  60%  70%  80%  90%  100%
    ## 
    ## 
    ## Results of Horn's Parallel Analysis for component retention
    ## 5000 iterations, using the mean estimate
    ## 
    ## -------------------------------------------------- 
    ## Component   Adjusted    Unadjusted    Estimated 
    ##             Eigenvalue  Eigenvalue    Bias 
    ## -------------------------------------------------- 
    ## 1           1.084920    2.843290      1.758369
    ## 2           1.193415    1.891501      0.698086
    ## -------------------------------------------------- 
    ## 
    ## Adjusted eigenvalues > 1 indicate dimensions to retain.
    ## (2 components retained)

![](EX-FINAL_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Metodos de extraccion de factores.

### metodo de analisis de los componentes principales(ACP).

``` r
acp<- principal(r, nfactors = 1, rotate = "none")
acp
```

    ## Principal Components Analysis
    ## Call: principal(r = r, nfactors = 1, rotate = "none")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                       PC1    h2   u2 com
    ## Edad                -0.55 0.305 0.70   1
    ## Hrs Juego*Dia        0.63 0.400 0.60   1
    ## Hrs Juego*Noche      0.27 0.074 0.93   1
    ## Sistema O.           0.77 0.594 0.41   1
    ## Dias*Semana          0.74 0.543 0.46   1
    ## inversion monetaria  0.21 0.044 0.96   1
    ## 
    ##                 PC1
    ## SS loadings    1.96
    ## Proportion Var 0.33
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 component is sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.19 
    ## 
    ## Fit based upon off diagonal values = 0.44

proportion var = 33%  
RMSR = 0.19

### metodo de los ejes principales o componentes principales iterados(CPI).

``` r
cpi<- fa(r, nfactors = 1, fm = "pa", rotate = "none", n.obs = 30)
cpi
```

    ## Factor Analysis using method =  pa
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "pa")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                       PA1    h2   u2 com
    ## Edad                -0.38 0.144 0.86   1
    ## Hrs Juego*Dia        0.46 0.216 0.78   1
    ## Hrs Juego*Noche      0.16 0.025 0.98   1
    ## Sistema O.           0.70 0.489 0.51   1
    ## Dias*Semana          0.63 0.393 0.61   1
    ## inversion monetaria  0.18 0.033 0.97   1
    ## 
    ##                 PA1
    ## SS loadings    1.30
    ## Proportion Var 0.22
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  1.01 with Chi Square of  26.52
    ## The degrees of freedom for the model are 9  and the objective function was  0.53 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.15 
    ## The df corrected root mean square of the residuals is  0.19 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  19.06  with prob <  0.025 
    ## The total number of observations was  30  with Likelihood Chi Square =  13.46  with prob <  0.14 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.314
    ## RMSEA index =  0.124  and the 90 % confidence intervals are  0 0.266
    ## BIC =  -17.15
    ## Fit based upon off diagonal values = 0.66
    ## Measures of factor score adequacy             
    ##                                                    PA1
    ## Correlation of (regression) scores with factors   0.83
    ## Multiple R square of scores with factors          0.69
    ## Minimum correlation of possible factor scores     0.38

proportion var = 22%  
RMSR = 0.15

### Metodo de la maxima verosimilitud.

``` r
mve<- fa(r, nfactors = 1, fm = "ml", rotate = "none", n.obs = 30)
mve
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "ml")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                       ML1    h2   u2 com
    ## Edad                -0.40 0.157 0.84   1
    ## Hrs Juego*Dia        0.46 0.210 0.79   1
    ## Hrs Juego*Noche      0.15 0.022 0.98   1
    ## Sistema O.           0.70 0.494 0.51   1
    ## Dias*Semana          0.60 0.355 0.65   1
    ## inversion monetaria  0.25 0.061 0.94   1
    ## 
    ##                 ML1
    ## SS loadings    1.30
    ## Proportion Var 0.22
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  1.01 with Chi Square of  26.52
    ## The degrees of freedom for the model are 9  and the objective function was  0.52 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.15 
    ## The df corrected root mean square of the residuals is  0.19 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  19.47  with prob <  0.021 
    ## The total number of observations was  30  with Likelihood Chi Square =  13.33  with prob <  0.15 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.334
    ## RMSEA index =  0.122  and the 90 % confidence intervals are  0 0.265
    ## BIC =  -17.28
    ## Fit based upon off diagonal values = 0.65
    ## Measures of factor score adequacy             
    ##                                                    ML1
    ## Correlation of (regression) scores with factors   0.82
    ## Multiple R square of scores with factors          0.67
    ## Minimum correlation of possible factor scores     0.35

proportion var = 22%  
RMSR = 0.15

### Conclusion.

como conclusion definimos como el metodo mas optimo aquel que nos de el
valor de Proportion var mas alto y el valor de RMSR mas bajo.  
tambien se debe dejar claro que el valor de proportion var estas sujeto
al criterio del investigador en cuanto a la aceptabilidad pero valores
muy bajos como en este caso &lt;30% indican que la extraccion de un
factor que me correlacione las variables analizadas no es posible para
esta base de datos.

## Representacion grafica de los factores extraidos.

### Metodo de analisis de los componentes principales (ACP)

``` r
#plot(acp,labels=row.names(r),cex =.7, ylim=c(-.8,.8))
```

### Metodo de los ejes principales o componentes principales iterados(CPI).

``` r
#plot(cpi, labels = row.names(r), cex =.7, ylim= c(-.8, .8))
```

### Metodo de la maxima verosimilitud (MVE).

``` r
#plot(mve, labels = row.names(r), cex =1, ylim= c(-.8, .8)) lo dejo como texto ya que como codigo me genera un error :/
```

## Obtencion de las puncuaciones factoriales.

### Metodo de analisis de los componentes principales (ACP)

``` r
acp1<- principal(datost[,2:7], nfactors = 1, rotate = "none", scores= T)
acp1$scores
```

    ##              PC1
    ##  [1,] -0.8483714
    ##  [2,] -1.1268796
    ##  [3,] -0.4198292
    ##  [4,] -0.6738512
    ##  [5,] -1.5461636
    ##  [6,] -1.0687217
    ##  [7,] -0.7596091
    ##  [8,]  0.1264376
    ##  [9,] -1.1176680
    ## [10,] -0.3602686
    ## [11,] -0.2576732
    ## [12,]  0.6460655
    ## [13,]  0.3395553
    ## [14,]  0.8227959
    ## [15,]  0.3098510
    ## [16,]  0.9284399
    ## [17,]  0.8919083
    ## [18,]  0.5554277
    ## [19,]  1.1407286
    ## [20,]  1.1978215
    ## [21,]  1.5661502
    ## [22,]  1.2928505
    ## [23,]  1.1900461
    ## [24,]  0.5784966
    ## [25,] -1.9455532
    ## [26,] -0.1830773
    ## [27,] -0.6539680
    ## [28,] -0.2206464
    ## [29,] -1.7103331
    ## [30,]  1.3060387

``` r
puntuacionesfactoriales_acp<- acp1$scores
puntuacionesfactoriales_acp<- as.data.frame(puntuacionesfactoriales_acp)
```

### Metodo de los ejes principales o componentes principales iterados(CPI)

``` r
cpi1<- fa(datost[,2:7], nfactors = 1, fm = "pa", rotate = "none", n.obs = 30, scores= "regression")
cpi1$scores
```

    ##                 PA1
    ##  [1,] -6.581073e-01
    ##  [2,] -1.169244e+00
    ##  [3,] -4.982862e-01
    ##  [4,] -8.095227e-01
    ##  [5,] -1.266827e+00
    ##  [6,] -9.146285e-01
    ##  [7,] -3.486883e-01
    ##  [8,]  1.916533e-05
    ##  [9,] -8.553897e-01
    ## [10,] -3.354471e-01
    ## [11,] -2.447690e-01
    ## [12,]  1.363890e-01
    ## [13,]  1.661009e-01
    ## [14,]  3.830587e-01
    ## [15,]  6.386256e-01
    ## [16,]  6.562451e-01
    ## [17,]  8.818200e-01
    ## [18,]  5.530696e-01
    ## [19,]  1.088636e+00
    ## [20,]  1.132791e+00
    ## [21,]  1.116645e+00
    ## [22,]  1.093444e+00
    ## [23,]  1.108373e+00
    ## [24,]  5.034016e-01
    ## [25,] -1.411969e+00
    ## [26,] -3.944325e-01
    ## [27,] -2.569439e-01
    ## [28,] -1.672952e-01
    ## [29,] -1.294335e+00
    ## [30,]  1.167268e+00

``` r
puntfact_cpi<- acp1$scores
puntfact_cpi<- as.data.frame(puntfact_cpi)
```

### Metodo de la maxima verosimilitud (MVE)

``` r
mve1<- fa(datost[,2:7], nfactors = 1, fm = "ml", rotate = "none", n.obs = 30, scores= "regression")
mve1$scores
```

    ##               ML1
    ##  [1,] -0.65227923
    ##  [2,] -0.97252742
    ##  [3,] -0.45444834
    ##  [4,] -0.75502399
    ##  [5,] -1.21168815
    ##  [6,] -0.87750048
    ##  [7,] -0.53288952
    ##  [8,] -0.05876415
    ##  [9,] -0.91022057
    ## [10,] -0.20557096
    ## [11,] -0.22060909
    ## [12,]  0.27439094
    ## [13,]  0.08354856
    ## [14,]  0.33866264
    ## [15,]  0.55041933
    ## [16,]  0.81710941
    ## [17,]  0.83623224
    ## [18,]  0.80999527
    ## [19,]  0.99346541
    ## [20,]  1.03913193
    ## [21,]  1.23781374
    ## [22,]  1.06703565
    ## [23,]  1.03977537
    ## [24,]  0.50784932
    ## [25,] -1.44149419
    ## [26,] -0.36184045
    ## [27,] -0.44842866
    ## [28,] -0.29502737
    ## [29,] -1.28805103
    ## [30,]  1.09093379

``` r
puntfact_mve<- mve1$scores
puntfact_mve<- as.data.frame(puntfact_mve)
```

## Obtencion de los factores extraidos.

#### Para ACP

``` r
factor.scores(r, acp, method = "Thurstone")
```

    ## $scores
    ## NULL
    ## 
    ## $weights
    ##                            PC1
    ## Edad                -0.2816716
    ## Hrs Juego*Dia        0.3226900
    ## Hrs Juego*Noche      0.1389745
    ## Sistema O.           0.3934561
    ## Dias*Semana          0.3760931
    ## inversion monetaria  0.1064692
    ## 
    ## $r.scores
    ##     PC1
    ## PC1   1
    ## 
    ## $R2
    ## [1] 1

Z1= -0.28Edad + 0.32Hrs.juego.dia + 0.13Hrs.juego.noche + 0.39S.O. +
0.37Dias.semana + 0.10Inversion

#### Para CPI

``` r
factor.scores(r, cpi, method = "Thurstone")
```

    ## $scores
    ## NULL
    ## 
    ## $weights
    ##                             PA1
    ## Edad                -0.08629229
    ## Hrs Juego*Dia        0.15676208
    ## Hrs Juego*Noche      0.02792587
    ## Sistema O.           0.49063470
    ## Dias*Semana          0.38746000
    ## inversion monetaria -0.04280011
    ## 
    ## $r.scores
    ##     PA1
    ## PA1   1
    ## 
    ## $R2
    ## [1] 0.6882656

Z1= -0.08Edad + 0.15Hrs.juego.dia + 0.03Hrs.juego.noche + 0.49S.O. +
0.39Dias.semana - 0.04Inversion

#### Para MVE

``` r
factor.scores(r, mve, method = "Thurstone")
```

    ## $scores
    ## NULL
    ## 
    ## $weights
    ##                             ML1
    ## Edad                -0.15302580
    ## Hrs Juego*Dia        0.18919477
    ## Hrs Juego*Noche      0.04980965
    ## Sistema O.           0.45311763
    ## Dias*Semana          0.30132268
    ## inversion monetaria  0.08562939
    ## 
    ## $r.scores
    ##     ML1
    ## ML1   1
    ## 
    ## $R2
    ## [1] 0.6737454

Z1= -0.15Edad + 0.18Hrs.juego.dia + 0.04Hrs.juego.noche + 0.45S.O. +
0.30Dias.semana + 0.08Inversion

## Agregar el factor extraido al dataframe original.

``` r
datos_puntuaciones<- c(datos, puntuacionesfactoriales_acp)
datos_puntuaciones<- as.data.frame(datos_puntuaciones)
```
