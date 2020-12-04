source("hydrology_dev.R")

# Emample plot:

grid <- expand_grid(A_E0 = 0:200, A_ba = seq(0, 10, .1))

grid$q_zul <- 1

for (i in 1:nrow(grid)){
  grid$q_zul[i] <- get_q_zulaessig(A_ba = grid$A_ba[i], A_E0 = grid$A_E0[i])
}


ggplot(grid, aes(x = A_ba, y = A_E0, col = q_zul, fill = q_zul))+ 
  geom_tile()+ scale_color_viridis_c() + scale_fill_viridis_c() +
  xlab("befestigte Fläche [km²]") + 
  ylab("Einzugsgebietsfläche bis Einleitstelle [km²]") +
  ggtitle(bquote("Zulässige Einleitmenge" ~Q[E1][','][zul]))

