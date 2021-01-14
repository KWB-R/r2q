source("hydrology_dev.R")

# Emample plot:

grid <- expand_grid(A_E0 = 1:200, A_ba = seq(.1, 10, .1))

grid$q_zul <- 1
grid$allowed_Aba <- 1
grid$allowed_fDA <- 1

for (i in 1:nrow(grid)){
  grid$q_zul[i] <- get_q_zulaessig(A_ba = grid$A_ba[i], A_E0 = grid$A_E0[i])
  grid$allowed_Aba[i] <- get_allowed_area(q_zul = grid$q_zul[i])
  grid$allowed_fDA[i] <- get_average_runoff_coef(A_ba = grid$A_ba[i], 
                                                 q_zul = grid$q_zul[i],
                                                 R_Spende = 150)
}


ggplot(grid, aes(x = A_ba, y = A_E0, col = q_zul, fill = q_zul))+ 
  geom_tile()+ scale_color_viridis_c() + scale_fill_viridis_c() +
  xlab("befestigte Fläche [km²]") + 
  ylab("Einzugsgebietsfläche bis Einleitstelle [km²]") +
  ggtitle(bquote("Zulässige Einleitmenge" ~Q[E1][','][zul]))




get_q_area_total <- function(dataframe=data, R_Spende=10){
  
  mean(dataframe$f_DA)*sum(dataframe$A_ba)*R_Spende
  
}

