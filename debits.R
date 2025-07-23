chemin_sorties <- "C:/_debits" #chemin de sortie pour les graphs et/ou les csv
sorties <- 3 #1= export graph ; 2= export csv ; 3= export graph + csv
direct <- 1 #1= toutes les donnees, y compris en direct, mais pas toutes bonnes ; 2= que les donnees bonnes
annee_cours <- 2025 #annee des donnees
dep <- 80 #departement
#G0170421 Bresle longroy
#G0170420 Bresle Ponts-et-Marais
    
  #### Chargement des packages et des fonctions ####
  # Chargement des packages #
  pacman::p_load(tidyverse,hubeau,ggpp)
  pacman::p_load_gh("theaubrasseur/fishyverse")

  #### Creation des chemins de sorties pour les plots et csv ####
  unlink(paste0(chemin_sorties,"/plots"), recursive=T) ; unlink(paste0(chemin_sorties,"/csv"), recursive=T)
  dir.create(chemin_sorties,recursive=T)
  if(sorties != 2){dir.create(str_c(chemin_sorties,"/plots"),recursive=T)}
  if(sorties != 1){dir.create(str_c(chemin_sorties,"/csv"),recursive=T)}

  #### Selection des stations ####
  stations <- get_hydrometrie_stations(code_departement= str_c(dep))#code utilise pour trouver les stations hydros de mesure de debits

  #### Sorties graphiques et csv ####
  for (case in levels(factor(stations$code_station))){
      
      # Extraction des debits #
      deb1 <- get_hydrometrie_obs_elab(code_entite=case,grandeur_hydro_elab="QmnJ",date_debut_obs_elab=as.Date("1900-01-01"),date_fin_obs_elab=as.Date("1950-12-31"))
      deb2 <- get_hydrometrie_obs_elab(code_entite=case,grandeur_hydro_elab="QmnJ",date_debut_obs_elab=as.Date("1951-01-01"),date_fin_obs_elab=as.Date("1999-12-31"))
      deb3 <- get_hydrometrie_obs_elab(code_entite=case,grandeur_hydro_elab="QmnJ",date_debut_obs_elab=as.Date("2000-01-01"))
      deb <- bind_rows(deb1,deb2,deb3)

      # Si la station contient des donnees, execute la suite de la boucle #
      if(nrow(deb) != 0){
        deb <- deb %>% filter(!is.na(resultat_obs_elab))
        
      # Filtrage des donnees selon le direct #
      if(direct == 2){
      deb <- deb %>% filter(code_qualification == 20)
      }
      
      # Formatage des donnees #    
      deb <- deb %>% inner_join(stations,"code_station") %>% 
        mutate(annee= year(date_obs_elab), mois= month(date_obs_elab,label=T), mois1= month(date_obs_elab), debit= resultat_obs_elab/1000, falsedate= as.Date(str_c("2000",strftime(date_obs_elab,"%m"),strftime(date_obs_elab,"%d"),sep="-"))) %>%
        filter(annee <= annee_cours, falsedate != as.Date("2000-02-29")) #le 29 fevrier est retire car il n'a lieu que lors des annees bisextile (366 jours). Comme il y'en a peu, la moyenne de débit pour cette date là n'est pas aussi fiable que pour les autres jours de l'annee.
      max_year_deb <- deb %>% filter(annee != annee_cours) ; max_year_deb <- max(max_year_deb$annee)
      deb <- deb %>%
        mutate(datayear= ifelse(annee==annee_cours,str_c(annee_cours),paste(min(annee),"à",max_year_deb)))
      
      # Export des donnees en .csv #
      if(sorties != 1){
      write.csv2(deb, str_c(chemin_sorties,"/csv/debits_",annee_cours,"_",distinct(deb,libelle_site),".csv"), row.names= F, fileEncoding= "UTF-16LE")
      }
      
      # Preparation des donnees et calculs #
      if(sorties != 2){
      min_annee <- min(deb$annee) #calcul de la premiere annee de production de donnee de la station
      qmj <- deb %>% group_by(falsedate, datayear) %>% reframe(debit= mean(debit)) #debits journaliers pour comparer l'annee en cours avec les annees precedentes
      qmm <- deb %>% group_by(mois, datayear) %>% reframe(debit= mean(debit)) #debits mensuels pour comparer l'annee en cours avec les annees precedentes
      qmm1 <- deb %>% group_by(mois1, datayear) %>% reframe(debit= mean(debit))#debits mensuels pour comparer l'annee en cours avec les annees precedentes pour le graphique journaliers + mensuels par intervalles de temps
      qm1 <- qmm %>% filter(datayear == annee_cours) #pour calculer la moyenne des debits mensuels pour l'annee en cours uniquement
      qm2 <- qmm %>% filter(datayear == paste(min(deb$annee),"à",max_year_deb)) #pour calculer la moyenne des debits mensuels pour toutes les annees sauf l'annee en cours
      all_qmj <- deb %>% group_by(falsedate) %>% reframe(debit= mean(debit)) #debits journaliers bruts
      all_qmm <- deb %>% group_by(mois1) %>% reframe(debit= mean(debit)) #debits mensuels bruts
      dates_mois <- as.Date(str_c("2000", all_qmm$mois1, "01", sep= "-")) #vecteur servant d'abscise pour le graphique des debits journaliers + mensuels 
      dates_mois1 <- as.Date(str_c("2000", qmm1$mois1, "01", sep= "-")) #vecteur servant d'abscise pour le graphique des debits journaliers + mensuels par intervalles de temps
      
    # Debits moyens journaliers # 
      plot <- ggplot(qmj, aes(x=falsedate, y=debit, group=datayear, color=datayear, fill=datayear)) +
        geom_line(linewidth=1) +
        scale_color_manual(values=c("darkorange", "dodgerblue"))+
        scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        geom_hline(yintercept=mean(qm2$debit,na.rm=T), linetype="dashed", color = "darkorange4", linewidth=1, na.rm=T) +
        geom_hline(yintercept=mean(qm1$debit,na.rm=T), linetype="dashed", color = "dodgerblue4", linewidth=1, na.rm=T) +
        annotate("text", y=max(qmj$debit)*1.05, x=as.Date("2000-11-30", "%Y-%m-%d"), label= paste0("Module= ",round(mean(qm2$debit),2)," m³/s"),colour = "darkorange4",size= 5,fontface= "bold")+
        annotate("text", y=max(qmj$debit)*1.01, x=as.Date("2000-11-30", "%Y-%m-%d"), label= paste0("Module ",annee_cours,"= ",round(mean(qm1$debit),2)," m³/s"),colour = "dodgerblue4",size= 5,fontface= "bold")+
        labs(title= str_c("Débits moyens journaliers - ", distinct(deb,libelle_site)), x="Mois", y="Débit (m³/s)", color="Débits",caption="Source des données: hydro.eaufrance.fr") +
        ggtitlex() + bottom_legend()
      plot
      ggsave(filename= str_c(chemin_sorties, "/plots/debits_journaliers_", annee_cours, "_", distinct(deb,libelle_site), ".png"), plot= plot, width= 12, height= 8)
      
    # Debits moyens mensuels #
      plot <- ggplot(qmm, aes(fill=datayear, y=debit, x=mois)) +
        geom_bar(position="dodge", stat="identity") +
        scale_fill_manual(values = c("darkorange","dodgerblue"))+
        geom_hline(yintercept=mean(qm2$debit,na.rm=T), linetype="dashed", color = "darkorange4", linewidth=1, na.rm=T) +
        geom_hline(yintercept=mean(qm1$debit,na.rm=T), linetype="dashed", color = "dodgerblue4", linewidth=1, na.rm=T) +
        geom_text(aes(label=round(debit,1)), vjust=1.5, position=position_dodge(0.9), size=5, fontface=2, color="white")+
        annotate("text", y=max(qmm$debit)*1.05, x=11, label= paste0("Module = ",round(mean(qm2$debit),2)," m³/s"),colour = "darkorange4",size= 5,fontface= "bold")+
        annotate("text", y=max(qmm$debit)*1.01, x=11, label= paste0("Module ",annee_cours,"= ",round(mean(qm1$debit),2)," m³/s"),colour = "dodgerblue4",size= 5,fontface= "bold")+
        labs(title= str_c("Débits moyens mensuels - ", distinct(deb,libelle_site)), x="Mois", y="Débit (m³/s)", fill="Débits", caption="Source des données: hydro.eaufrance.fr") +
        ggtitlex() + bottom_legend()
      plot
      ggsave(filename= str_c(chemin_sorties, "/plots/debits_mensuels_", annee_cours, "_", distinct(deb,libelle_site), ".png"), plot= plot, width= 12, height= 8)
      
    # Debits moyens journaliers et mensuels #
      plot <- ggplot() +
        geom_bar(data=all_qmm, aes(x=dates_mois,y=debit), stat="identity", fill="darkorange2", width=20, just=-0.25) +
        geom_line(data= all_qmj, aes(x=falsedate, y=debit), linewidth=1, color="dodgerblue2") +
        geom_hline(yintercept=quantile(all_qmj$debit, probs=(0.75), na.rm=T), linetype="dashed", color="blue3", linewidth=1, na.rm=T) +
        geom_hline(yintercept=mean(all_qmj$debit,na.rm=T), linetype="solid", color="black", linewidth=1, na.rm=T) +
        geom_hline(yintercept=quantile(all_qmj$debit, probs=(0.25), na.rm=T), linetype="dashed", color="green4", linewidth=1, na.rm=T) +
        annotate("text", y=max(all_qmj$debit)*1.09, x=as.Date("2000-11-30","%Y-%m-%d"), label=paste("Quantile 75% (QJ75)=", round(quantile(all_qmj$debit, probs=(0.75), na.rm=T),2),"m³/s"),colour="blue3",size=5, fontface="bold") +
        annotate("text", y=max(all_qmj$debit)*1.05, x=as.Date("2000-11-30","%Y-%m-%d"), label=paste("Module=", round(mean(all_qmj$debit),2),"m³/s"),colour="black", size=5, fontface="bold") +
        annotate("text", y=max(all_qmj$debit)*1.01, x=as.Date("2000-11-30","%Y-%m-%d"), label=paste("Quantile 25% (QJ25)=", round(quantile(all_qmj$debit, probs=(0.25), na.rm=T),2),"m³/s"), colour="green4", size=5, fontface="bold") +
        geom_text(data=all_qmm, aes(x=dates_mois, y=debit, label=round(debit,1)), vjust=2.5, hjust=0.5, nudge_x=15, size=6, fontface=2, color="white") +
        scale_x_date(date_breaks="1 month", date_labels="%b") +
        labs(title="Débits moyens mensuels et journaliers",subtitle=str_c(distinct(deb,libelle_site)," - ", min_annee," à ",annee_cours),y="Débit (m³/s)",x="Mois",caption="Source des données: hydro.eaufrance.fr") +
        ggtitlex()
      plot
      ggsave(filename= str_c(chemin_sorties, "/plots/debits_mensuels_et_journaliers", annee_cours, "_", distinct(deb,libelle_site), ".png"), plot= plot, width= 14, height= 10)
      
    # Debits moyens journaliers et mensuels par intervalles de temps #
      plot <- ggplot() +
        geom_bar(data=qmm1, aes(x=dates_mois1,y=debit,fill=datayear), stat="identity", width=20, position=position_dodge2nudge(padding=0, x=15)) +
        geom_line(data= qmj, aes(x=falsedate, y=debit, color=datayear), linewidth=1) +
        scale_fill_manual(values = c("darkorange","dodgerblue")) + scale_color_manual(values=c("darkorange", "dodgerblue")) +
        geom_hline(yintercept=mean(qm2$debit,na.rm=T), linetype="dashed", color = "darkorange4", linewidth=1, na.rm=T) +
        geom_hline(yintercept=mean(qm1$debit,na.rm=T), linetype="dashed", color = "dodgerblue4", linewidth=1, na.rm=T) +
        annotate("text", y=max(qmj$debit)*1.05, x=as.Date("2000-11-30", "%Y-%m-%d"), label= paste0("Module= ",round(mean(qm2$debit),2)," m³/s"),colour = "darkorange4",size= 5,fontface= "bold") +
        annotate("text", y=max(qmj$debit)*1.01, x=as.Date("2000-11-30", "%Y-%m-%d"), label= paste0("Module ",annee_cours,"= ",round(mean(qm1$debit),2)," m³/s"),colour = "dodgerblue4",size= 5,fontface= "bold") +
        geom_text(data=qmm1, aes(x=dates_mois1, y=debit, label=round(debit,1)), vjust=1.5, position=position_dodge2nudge(width= 20, x=15), size=5, fontface=2, color="white") +
        scale_x_date(date_breaks="1 month", date_labels="%b") +
        labs(title="Débits moyens mensuels et journaliers par intervalles de temps",subtitle=str_c(distinct(deb,libelle_site)," - ", min_annee," à ",annee_cours-1, " et ", annee_cours),y="Débit (m³/s)",x="Mois",caption="Source des données: hydro.eaufrance.fr",fill="Débits") +
        ggtitlex() + guides(color= "none") + bottom_legend()
      plot
      ggsave(filename= str_c(chemin_sorties, "/plots/debits_mensuels_et_journaliers_intervalles_temps_", annee_cours, "_", distinct(deb,libelle_site), ".png"), plot= plot, width= 14, height= 10)
      
      }}}
    
remove(all_qmj,all_qmm,deb1,deb2,deb3,plot,qmj,qmm,deb,qm1,qm2,qmm1,stations,case,min_annee,dates_mois,dates_mois1,max_year_deb)
beepr::beep(sound="complete")
