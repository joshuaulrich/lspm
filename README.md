# LSPM

An R package which implements Ralph Vince's Leverage Space Portfolio Model.

## Installation
You can start from a docker image [`r-base`](https://hub.docker.com/r/library/r-base/).
```bash
$ docker run -ti --rm r-base:3.3.1 /bin/bash
root@e930a49959cb:/# R
> install.packages("DEoptim")
> install.packages("LSPM", repos="http://R-Forge.R-project.org")
```

## Usage
Calculate optimal f on a coin toss example:
```bash
> library("LSPM")
> trades <- c(2,-1)
> probs <- c(0.5,0.5)

# Create a Leverage Space Portfolio object
# lsp() function demands at least one negative trade
> lspobj <- lsp(trades,probs)
> lspobj
           V1
f         0.1
Max Loss -1.0
     probs V1
[1,]   0.5  2
[2,]   0.5 -1

# Calculate optimal f
> result <- optimalf(lspobj)
> result
$f
[1] 0.25

$G
[1] 1.06066

# Put the optimal f values into the lsp object
> lspobj$f <- result$f  
> lspobj
            V1
f         0.25
Max Loss -1.00
     probs V1
[1,]   0.5  2
[2,]   0.5 -1
```

Calculate the risk of ruin and risk of drawdown, where b=0.6:
```bash
> probRuin(lspobj, 0.4, 26)
 [1] 0.0000000 0.2500000 0.2500000 0.2500000 0.3125000 0.3125000 0.3671875
 [8] 0.3671875 0.3671875 0.3896484 0.4539878 0.4790026 0.5028713 0.5256465
[15] 0.5473783 0.5681145 0.5879007 0.6067804 0.6247952 0.6419846 0.6583865
[22] 0.6740371 0.6889706 0.7032199 0.7168164 0.7297901
>  probRuin(lspobj, 0.4, 26, calc.max=26)
 [1] 0.0000000 0.2493711 0.2493711 0.2493711 0.3118924 0.3118924 0.3667098
 [8] 0.3667098 0.3667098 0.3891169 0.3891169 0.4132729 0.4132729 0.4364889
[15] 0.4364889 0.4364889 0.4470916 0.4470916 0.4595458 0.4595458 0.4595458
[22] 0.4658324 0.4658324 0.4735831 0.4735831 0.4818164
> probDrawdown(lspobj, 0.4, 26, calc.max=26)
 [1] 0.0000000 0.2503204 0.3751524 0.5003436 0.5942160 0.6723093 0.7346524
 [8] 0.7856471 0.8265849 0.8596809 0.8864591 0.9081249 0.9258387 0.9400631
[15] 0.9514036 0.9606889 0.9682324 0.9743507 0.9792542 0.9832409 0.9864369
[22] 0.9889942 0.9911027 0.9927947 0.9941800 0.9952924
```

### Links
* http://lspm.r-forge.r-project.org/ -- different example
* https://r-forge.r-project.org/R/?group_id=485 -- downloads
* http://rpackages.ianhowson.com/rforge/LSPM/ -- functions and parameters explained
