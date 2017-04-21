# Morfologiškai anotuoto tekstyno nuskaitymas ---------------------------------

aplankai <- list("fiction", "documents", "periodicals", "scientific_texts")
adresai <- paste0("C:\\Users\\neringab\\Desktop\\Magistras\\Magistrinis\\MATAS\\", 
                  aplankai)
tekst.pavad <- lapply(adresai, list.files)

tekst.nuor <- Map(paste0, adresai, "\\", tekst.pavad)

nuskaitymui <- function(nuoroda){
  lapply(1:length(nuoroda), function(i) 
    tolower(readLines(nuoroda[i], encoding = "UTF-8")))
}

# Nuskaitomi tekstai
tekstai <- lapply(tekst.nuor, nuskaitymui)
length(tekstai)
sapply(tekstai, length)

## ------------------------------------------------------------------------- ##
# Funkcija, skirta išrikiuotos dažnių lentelės sudarymui. 
## ------------------------------------------------------------------------- ##
table.visa <- function(tekst.katalogas) {
  sort(table(tekst.katalogas), decreasing = TRUE)
}

dažniai.visi <- lapply(žodžiai, lapply, table.visa)

## ------------------------------------------------------------------------- ##
# Funkcija, skirta Zipfo dėsnio parametrams apskaičiuoti. Jai paduodamas
# dažnių lentelių sąrašas. `return.k` nurodo kokį parametrą norime, kad
# grąžintų.
## ------------------------------------------------------------------------- ##
zipfui <- function(dažnių.lentelė, return.k){
  f.r <- list()
  r <- list()
  lent.zipf <- list()
  C <- vector("numeric")
  mod <- list()
  a <- vector("numeric")
  for (j in 1:(length(dažnių.lentelė))){
    f.r[[j]] <- as.vector(dažnių.lentelė[[j]])
    r[[j]] <- rank(-f.r[[j]], ties.method = "average")
    mod[[j]] <- lm(log(f.r[[j]]) ~ log(r[[j]]))
    C[j] <- coef(mod[[j]])[1]
    a[j] <- coef(mod[[j]])[2]
  }
  return(get(return.k))
}

# -----------------------------------------------------------------------------
# Sakinių struktūros analizė --------------------------------------------------

splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

## ------------------------------------------------------------------------- ##
# Funkcija, skirta sakinių kalbos dalims išrinkti ir
# suskaidyti į sakinius (į sakinius skaido funkcija `splitAt`). Anotuotame 
# tekste paliekamos tik tos eilutės, kurios prasideda `sep` (žymi skyrybos 
# ženklą pagal kurį skaidomas sakinys) ir `word` (žymi žodžio formą).
## ------------------------------------------------------------------------- ##
sakiniams <- function(tekstas){
  index.palikti <- grep("^<sep|^<word", tekstas)
  tekstas <- tekstas[index.palikti]
  sakinio.pab <- grep("<sep=\"\\!?\\.?\\??\">", tekstas)
  sakiniais <- splitAt(tekstas, sakinio.pab)
  palikti.word <- lapply(sakiniais, grep, pattern = "^<word")
  pattern <- "type\\=\"[a-zA-Ząčęėįšųūž\\. ]+"
  for (i in 1:length(sakiniais)){
    sakiniais[[i]] <- sakiniais[[i]][palikti.word[[i]]]
    m <- regexpr(pattern, sakiniais[[i]])
    žodžiai.tag <- regmatches(sakiniais[[i]], m)
    sakiniais[[i]] <- gsub("type\\=\"([a-zA-Ząčęėįšųūž ]+)", "\\1", žodžiai.tag)
  }
  sakiniais[which(lapply(sakiniais, length) == 0)] <- NULL
  return(sakiniais)
}

tagai.tekstams <- lapply(tekstai, lapply, sakiniams)

## ------------------------------------------------------------------------- ##
# Funkcija, skirta sakinių kalbos dalių kodavimui. Čia daiktavardžiai,
# veiksmažodžiai žymimi atitinkamai D ir V, kitos kalbos dalys -- `-` simboliu.
# Keli iš eilės einantys simboliai `-` apjungiami su gsub.
## ------------------------------------------------------------------------- ##
f.kodavimui1 <- function(vekt){
  daiktav.i <- grep("dktv", vekt)
  veiksm.i <- grep("vksm", vekt)
  kiti.i <- grep( "^((?!dktv|vksm).)*$", vekt, perl = TRUE)
  vekt[daiktav.i] <- "D"
  vekt[veiksm.i] <- "V"
  vekt[kiti.i] <- "-"
  vekt <- paste(vekt, collapse = "")
  vekt <- gsub("\\-+", "\\-", vekt)
}

kodas1.tekstai <- lapply(tagai.tekstams, sapply, sapply, f.kodavimui1)
## ------------------------------------------------------------------------- ##
# Funkcija, skirta II tipo kodavimui. Paimami I tipo kodai, papildomai
# apjungiami keli iš eilės einantys „D“ arba „V“ simboliai.
## ------------------------------------------------------------------------- ##
f.kodavimui2 <- function(vekt){
  vekt <- gsub("V+", "V", vekt)
  vekt <- gsub("D+", "D", vekt)
}

kodas2.tekstai <- sapply(kodas1.tekstai, sapply, sapply, f.kodavimui2)

# -----------------------------------------------------------------------------
# Zipfo dėsnis sakinių kodams -------------------------------------------------

dažniai1 <- lapply(kodas1.tekstai, sapply, table.visa)
dažniai2 <- lapply(kodas2.tekstai, sapply, table.visa)

modelis1.zipf <- rep(list(list()), 4)
a1.zipf <- rep(list(list()), 4)
c1.zipf <- rep(list(list()), 4)
f.r1 <- rep(list(list()), 4)
rangai1 <- rep(list(list()), 4)
for(i in 1:4){
  modelis1.zipf[[i]] <- zipfui(dažniai1[[i]], "mod")
  a1.zipf[[i]] <- zipfui(dažniai1[[i]], "a")
  c1.zipf[[i]] <- zipfui(dažniai1[[i]], "C")
  f.r1[[i]] <- zipfui(dažniai1[[i]], "f.r")
  rangai1[[i]] <- zipfui(dažniai1[[i]], "r")
}

modelis2.zipf <- rep(list(list()), 4)
a2.zipf <- rep(list(list()), 4)
c2.zipf <- rep(list(list()), 4)
f.r2 <- rep(list(list()), 4)
rangai2 <- rep(list(list()), 4)
for(i in 1:4){
  modelis2.zipf[[i]] <- zipfui(dažniai2[[i]], "mod")
  a2.zipf[[i]] <- zipfui(dažniai2[[i]], "a")
  c2.zipf[[i]] <- zipfui(dažniai2[[i]], "C")
  f.r2[[i]] <- zipfui(dažniai2[[i]], "f.r")
  rangai2[[i]] <- zipfui(dažniai2[[i]], "r")
}


# -----------------------------------------------------------------------------
# Brėžiami visi prognozuoti dažniai pagal Zipfo dėsnį
# -----------------------------------------------------------------------------

koef <- lapply(modelis1.zipf, lapply, coef)

makeTransparent <- function(someColor, alpha = 100){
  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){
    rgb(red = curcoldata[1], green = curcoldata[2], blue = curcoldata[3], 
        alpha = alpha, maxColorValue = 255)
  }
  )
}

plot("", ylim = c(0, 7), xlim = c(0, 8.3), ylab = expression(log~italic(f[z])), 
     xlab = expression(log~italic(z)))
sapply(1:4, function(i) sapply(koef[[i]], function(x) abline(a = x[1], b = x[2],
                                                             col = makeTransparent(i, alpha = 150), lwd = 3)))
legend("topright", c(expression(Fiction), expression(Documents), 
                     expression(Periodicals), expression(Scientific~texts)), 
       col = 1:4, lty = 1, lwd = 2,  bty='n')

# -----------------------------------------------------------------------------
# Žodžių dažnių log-log grafikas. Taškai grafike – stebėti žodžių dažniai 
# tekstuose, tiesė – prognozuoti dažniai pagal Zipfo dėsnį.
# -----------------------------------------------------------------------------

par(mfrow = c(1, 2), mar = c(0, 0.3, 3, 0.1), oma = c(4, 5, 0, 0))
plot(log(rangai1[[1]][[9]]), log(f.r1[[1]][[9]]), pch = 19, ylab = "", 
     xlab = "", las = 1, ylim = c(0, 6),
     main = expression(Fiction:~I~encoding),
     cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
abline(modelis1.zipf[[1]][[9]])
plot(log(rangai2[[1]][[9]]), log(f.r2[[1]][[9]]), pch = 19, ylab = "", 
     xlab = "", las = 1, ylim = c(0, 6), yaxt = "n",
     main = expression(Fiction:~II~encoding),
     cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
abline(modelis2.zipf[[1]][[9]])
title(ylab = expression(log~italic(f[z])), 
      xlab = expression(log~italic(z)), outer = TRUE, cex.lab = 1.5)

# -----------------------------------------------------------------------------
# Visų nagrinėjamų tekstų Zipfo dėsnio parametrų sklaidos diagrama
# skirtingoms sakinių koduotėms.
# -----------------------------------------------------------------------------

storis <- 1.2

layout(matrix(c(1, 2, 3, 3), ncol = 2, byrow = TRUE), heights = c(4, 0.3))
par(mai = c(1, 1, 0.5, 0), oma = c(0, 0, 0, 0))
plot(c1.zipf[[1]], a1.zipf[[1]], pch = 19, col = 1, ylab = expression(alpha),
     ylim=c(-1.1, -0.15), xlim = c(0, 7), main = expression(I~encoding),
     xlab = expression(log~italic(C)), cex = storis)
points(c1.zipf[[2]], a1.zipf[[2]], pch = 18, col = 2, cex = storis)
points(c1.zipf[[3]], a1.zipf[[3]], pch = 17, col = 3, cex = storis)
points(c1.zipf[[4]], a1.zipf[[4]], pch = 15, col = 4, cex = storis)
par(mai=c(1, 0.4, 0.5, 0.5))
plot(c2.zipf[[1]], a2.zipf[[1]], pch = 19, col = 1, yaxt = "n", 
     ylim=c(-1.1, -0.15), xlim = c(0, 7), xlab = expression(log~italic(C)),
     main = expression(II~encoding), ylab = expression(alpha), cex = storis)
points(c2.zipf[[2]], a2.zipf[[2]], pch = 18, col = 2, cex = storis)
points(c2.zipf[[3]], a2.zipf[[3]], pch = 17, col = 3, cex = storis)
points(c2.zipf[[4]], a2.zipf[[4]], pch = 15, col = 4, cex = storis)
par(mai=c(0,0,0,0))
plot.new()
legend("center", bty ="n",
       c(expression(Fiction), expression(Documents), 
         expression(Periodicals), expression(Scientific~texts)), 
       pch = c(19, 18, 17, 15), pt.cex = rep(storis, 5), col = 1:4,
       ncol = 2)
