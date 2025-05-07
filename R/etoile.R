#' etoile_rooms_
#' 
#' @export
etoile_rooms_ <- function(){
  ajust <- NULL
  hauteur <-2.45
  larg <- long <- surf <- calc <- NULL
  tibble::tribble(
    ~id, ~desc, ~surf, ~isout, ~type, ~h, ~larg,~long, ~ajust,~comment, ~as_pvc,
    "R1", "chambre1", 12, F, "room", hauteur, 3.27, 3.65,0, "bureau", F,
    "R2", "chambre2", 12, F, "room", hauteur, 3.30, 3.68,0, "coucher", F,
    "R3", "sejour",   23, F, "room", hauteur, 4.60, 5.00,0, "sejour", F,
    "RK", "cuisine",  8, F,  "kitchen", hauteur, 2.75, 2.90, 0,NA, T,
    "RB", "salle_bain", 4, F, "water", hauteur, 1.90, 2.30, +0.5*0.7,"", F,
    "RC", "wc", 4, F, "water", hauteur, 0.93, 1.50, 0,"", T,
    "P0", "entree",   7.6, F, "passage", hauteur, 2.24, 2.89,-0.42*0.60, NA, F,
    "P1", "couloir1", 2.97, F, "passage", hauteur,0.86, 3.17, 0,NA, F,
    "P2", "couloir2", 2.97, F, "passage", hauteur, 0.87, 3.43, 0,NA, F,
    "X1", "balcon",  15.39, T,  "passage", hauteur, 0, 0, 0,"sejour", F,
    "X2", "balcon",   6.37, T, "passage", hauteur,0, 0, 0,"cuisine", F,
  ) |>
    dplyr::mutate(calc = round(larg*long+ajust,2)) |>
    tidyr::unite("super", surf,calc, remove=FALSE, sep = "/") |>
    dplyr::relocate(calc, .after='surf') |>
    data.table::data.table()
}

#' etoile_bom_
#' 
#' @export
etoile_bom_ <- function(){
  ajust <- NULL
  hauteur <-2.45
  larg <- long <- surf <- calc <- NULL
  tibble::tribble(
    ~status, ~nodeid, ~chapter, ~desc, ~surf, ~comment, 
    "done","R01", "specs", "plancher", 12, "definition", 
    "todo","R02", "planification","plancher", 12, "prévoir lampe + calendrier W12", 
    "done","R03", "financement","plancher", 12, "budget total", 
    "done","R04", "projection", "plancher", 12, "rendu 3D", 
    "done","R15", "cuisiniste", "plancher", 12, "selection&contrat", 
    "done","R04b", "meubles","plancher", 12, "detail volume", 
    "todo","R04d", "equipement","plancher", 12, "table induction NOW", 
    "todo","R04c", "SOL", "plancher", 12, "fournir dalles pvc", 
    "todo","R20", "DEPLACER", "plancher", 12, "1.frigo-buffet-fer, 2.meuble hauts",
    "todo","R21", "MURS", "plancher", 12, "ôter CARRELAGE",
    "todo","R23b", "MURS", "plancher", 12, "refaire, peindre MURS",
    "todo","R23", "PLAFOND", "plancher", 12, "re-peindre",
    "todo","R25", "SOL", "plancher", 12, "ôter, fourniture et pose", 
    "todo","R25b", "CUISINE","plancher", 12, "installer W12", 
    "todo","R22b", "MURS","plancher", 12, "déplacer EAU", 
    "todo","R22", "MURS","plancher", 12, "RECABLER +elec +lampe-évier", 
    "todo","R20b", "DEPLACER", "plancher", 12, "3.cuisinière, 4.évier",
    "todo","R31", "lumiere","plancher", 12, "brancher", 
    "todo","R32", "TEST","plancher", 12, "fonctionement", 
  ) |>
    dplyr::select(-dplyr::matches("^surf|^desc")) |>
    dplyr::arrange(desc(status),nodeid)  |>
    dplyr::filter(status=="todo") |>
    data.table::data.table()
}

#' etoile_fournisseur_
#' 
#' @export
etoile_fournisseur_ <- function(){
  ajust <- NULL
  hauteur <-2.45
  larg <- long <- surf <- calc <- NULL
  tibble::tribble(
    ~id, ~company, ~source, ~prix, ~adresse, ~hauteur, ~largeur, ~volume, ~url, ~mécanisme, ~surmesure,
    "C01", "cuisinella--", "plancher", 12, "st jean de vedas", 0,0,0,NA, 0, F,
    "C02", "comera+++","vendée.fr", 12, "ZAC du Fenouillet Avenue du Languedoc, 34470 Pérols ", 0,0,0,"www.comera-cuisines.fr", 20, T,
    "C03", "envia+",".DE .FR", 3700, "Chemin du Soriech 34970 Lattes", 0,0,0,"",0, F,
    "C04", "ixina","plancher", 4300, "Allée Pierre Lazareff, ZAC du Mas de Grille, 34430 Saint Jean de Vedas", 0,0,0,"https://www.ixina.fr/cuisines/bahia-conviviale",0, F,
    "C05", "socooc",".FR", 8000, "Chemin de Soriech 34970 Lattes", 0,0,0,NA,0, F,
    "C06", "but","plancher", 12, "lattes", 0,0,0,NA,0, F,
    "C07", "cuisine_plus","plancher", 8000, "soriech lattes", 0,0,0,NA,0, F,
    "C08", "leroy_merlin","plancher", 12, "lattes", 0,0,0,NA,0, F,
    "C09", "castorama","plancher", 12, "lattes", 0,0,0,NA,0, F,
    "C10", "bricomarché","marinelli", 2007, "Cd 112 la Plaine Espace Bocaud, 34830 Jacou", 2.16,255,2820, "https://www.bricomarche.com/p/cuisine-avec-meubles-premontes-caterina-chene-et-ardoise-255cm/0806812157891",0, F,
  ) |>
    dplyr::select(-dplyr::matches("^surf|^desc|^url")) |>
    dplyr::arrange(id)  |>
    data.table::data.table()
}