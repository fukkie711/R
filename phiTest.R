dat <- read.table("clipboard", header = FALSE)
dat

#correct: 
#イェーツの補正あり:TRUE(デフォルト), なし:FALSE
#chisq.test(dat, correct = TRUE)
chisq.test(dat, correct = FALSE)
res <- chisq.test(dat)$p.value

if (res < 0.01) {
  print('◎') 
} else if (res < 0.05) {
  print('○') 
} else if (res < 0.1) {
  print('△')
} else print('???')