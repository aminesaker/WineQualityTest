
##Fonction myreg
myreg = function(data, idxp, idxc){
  fff = as.formula(paste(paste(colnames(data)[idxc],"~"),paste(colnames(data)[idxp], collapse = "+")))
  return(lm(fff, data = data))
}

##Fonction erreur generique
erreur_gen = function(test, predictions){
 d = test - predictions
 d = d^2
 eqm = mean(d)
 return (eqm)
}

# Fonction separation
separation = function(data, idxp, idxc, n){
 eqm_add = 0
 for (i in 1:n){
   idx = sample(dim(data)[1],ceiling(dim(data)[1]*0.75))
   app = data[idx, ]
   test = data[-idx, ]
   reg = myreg(app, idxp, idxc)
   predict = predict(reg , test)
   eqm_add = eqm_add + erreur_gen(test[,idxc], predict) 
 }
 eqm_mean = eqm_add/n
 return (eqm_mean)
}
## Fonction valildation croisée
validation_croisee = function(data, idxp, idxc, K){
    EQM_j = 0
    nb_ligne = nrow(data)
    nb_indiv = floor(nb_ligne/K)
    idx = sample(nb_ligne)
    j = 1
    k = nb_indiv
   for(i in 1:(K-1)){
	test = data[idx[j:k],]
	apprentissage = data[-idx[j:k],]
	mod_i1 = myreg(apprentissage, idxp,idxc)
	predict_i1 = predict(mod_i1, test)
	val_to_compare = test[,idxc]
	EQM_i = EQM( val_to_compare, predict_i1)
	EQM_j = EQM_j+EQM_i
	j = nb_indiv + j
	k = k + nb_indiv
    }
	test = data[idx[j:length(idx)],]
	apprentissage = data[-idx[j:length(idx)],]
	mod_i1 = myreg(apprentissage, idxp,idxc)
	predict_i1 = predict(mod_i1, test)
	val_to_compare = test[,idxc]
	EQM_i = EQM( val_to_compare, predict_i1)
	EQM_j = EQM_j+EQM_i
	return(EQM_j/K)
}

##Fonction selection ascendante séparation
selec_asc_sep = function(data, idx_p, idx_c, K){
  best_eg = -1
  best_var = 0
  Mres = matrix( nrow = 2, ncol = length(idx_p))
  v_m = c()
  v_r = idx_p
  for( i in 1:length(idx_p)){
   eqm_min = -1
   x_min = 0
   for( x in 1:length(v_r)){
    eqm = separation( data, c(v_m, v_r[x]), idx_c, K)
    if ( eqm_min > eqm || eqm_min == -1){
     eqm_min = eqm
     x_min = x
    }
   }
   Mres[1,i] <- v_r[x_min]
   Mres[2,i] <- eqm_min
   v_m = c(v_m, v_r[x_min])
   v_r = v_r[-x_min]
   if ( best_eg > eqm_min || best_eg == -1){
    best_eg = eqm_min
    best_var = v_m 
   }
  }
  res = list(TabRes = Mres, var = best_var, eg = best_eg)
  return (res)
}

##Fonction selection ascendante validation croisée
selec_asc_val = function(data, idx_p, idx_c, K){
  best_eg = -1
  best_var = 0
  Mres = matrix( nrow = 2, ncol = length(idx_p))
  v_m = c()
  v_r = idx_p
  for( i in 1:length(idx_p)){
   eqm_min = -1
   x_min = 0
   for( x in 1:length(v_r)){
    eqm = validation_croisee( data, c(v_m, v_r[x]), idx_c, K)
    if ( eqm_min > eqm || eqm_min == -1){
     eqm_min = eqm
     x_min = x
    }
   }
   Mres[1,i] <- v_r[x_min]
   Mres[2,i] <- eqm_min
   v_m = c(v_m, v_r[x_min])
   v_r = v_r[-x_min]
   if ( best_eg > eqm_min || best_eg == -1){
    best_eg = eqm_min
    best_var = v_m 
   }
  }
  res = list(TabRes = Mres, var = best_var, eg = best_eg)
  return (res)
}







