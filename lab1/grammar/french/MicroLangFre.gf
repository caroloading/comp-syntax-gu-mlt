--# -path=.:../abstract
concrete MicroLangFre of MicroLang = open MicroResFre, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    Utt = {s : Str} ;
    
    S  = {s : Str} ;
    VP = {verb : Verb ; obj: Str; compl : Agreement => Str; isPron: Bool} ; ---s special case of Mini
    Comp = {s : Agreement => Str} ;
    AP = Adjective ;
    CN = Noun ;
    NP = {s : Case => Str ; a : Agreement; isPron : Bool} ;
    Pron = {s : Case => Str ; a : Agreement} ;    
    Det = {s : Gender => Str ; n : Number} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Acc} ;

    PredVPS np vp = {
      s = np.s ! Nom ++ (preOrPost vp.isPron vp.obj (vp.verb.s ! agr2vform np.a)) ++ (vp.compl ! np.a)
    } ;
      
    UseV v = {
      verb = v ;
      obj = [];
      compl = \\agr => [];
      isPron = False
    } ;
      
    ComplV2 v2 np = {
      verb = v2 ;
      obj = np.s ! Acc;
      compl = \\agr => v2.c;
      isPron = np.isPron
    };
      
    UseComp comp = {
      verb = be_Verb ;     -- the verb is the copula "be"
      obj = [];
      compl = comp.s ;
      isPron = False
    } ;
      
    CompAP ap = ap ;
      
    AdvVP vp adv =
      vp ** { compl = \\agr => (vp.compl ! agr) ++ adv.s
    } ;
      
    DetCN d cn = {
      s = \\c => (d.s ! cn.g) ++ (cn.s ! (NF d.n)) ;
      a = Agr d.n cn.g ;
      isPron = False
    } ;
      
    UsePron p = p ** {isPron = True};
            
    a_Det = {s = table { Masc => "un"; Fem => "une"} ; n = Sg} ; 
    aPl_Det = {s = table { Masc => "des"; Fem => "des"} ; n = Pl} ;
    the_Det = {s = table { Masc => pre {"a"|"e"|"i"|"o"|"h"|"é" => "l'" ; _ => "le"}; Fem => pre {"a"|"e"|"i"|"o"|"h"|"é" => "l'" ; _ => "la"}} ; n = Sg} ;
    thePl_Det = {s = table { Masc => "les"; Fem => "les"}; n = Pl} ;
    
    UseN n = n ;
    
    AdjCN a cn = {
            s = \\nf => let agr = Agr (nform2number nf) cn.g
                        in (preOrPost a.isBef (a.s ! agr) (cn.s ! nf) )  ; 
            g = cn.g
        } ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Dat} ;

    in_Prep = {s = "dans"} ;
    on_Prep = {s = "sur"} ;
    with_Prep = {s = "avec"} ;

    he_Pron = {
      s = table {Nom => "il" ; Acc => pre {"a"|"e"|"i"|"o"|"h" => "l'" ; _ => "le"}; Dat => "lui"} ;
      a = Agr Sg Masc;
      } ;
    she_Pron = {
      s = table {Nom => "elle" ; Acc => pre {"a"|"e"|"i"|"o"|"h" => "l'" ; _ => "la"}; Dat => "elle"} ;
      a = Agr Sg Fem;
      } ;
    they_Pron = {
      s = table {Nom => "ils" ; Acc => "les"; Dat => "eux"} ;
      a = Agr Pl Masc ;
      } ;
    {- theyF_Pron = {
      s = table {Nom => "elles" ; Acc => "les"; Dat => "leur"} ;
      a = Agr Pl Fem ;
    }; -}
    

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "déjà" ;
lin animal_N = mkN "animal" Masc ;
lin apple_N = mkN "pomme" Fem ;
lin baby_N = mkN "bébé" Masc ;
lin bad_A = mkA "mauvais" True ;
lin beer_N = mkN "bière" Fem ;
lin big_A = mkA "grand" True ;
lin bike_N = mkN "vélo" Masc ;
lin bird_N = mkN "oiseau" Masc ;
lin black_A = mkA "noir" False ;
lin blood_N = mkN "sang" Masc ;
lin blue_A = mkA "bleu" False ;
lin boat_N = mkN "bateau" Masc ;
lin book_N = mkN "livre" Masc ;
lin boy_N = mkN "garçon" Masc ;
lin bread_N = mkN "pain" Masc ;
lin break_V2 = mkV2 "casser" ; --DONE
lin buy_V2 = mkV2 "acheter" ; --DONE
lin car_N = mkN "voiture" Fem ;
lin cat_N = mkN "chat" Masc ;
lin child_N = mkN "enfant" Masc ;
lin city_N = mkN "ville" Fem ;
lin clean_A = mkA "propre" False ;
lin clever_A = mkA "sage" False ;
lin cloud_N = mkN "nuage" Masc ;
lin cold_A = mkA "froid" False ;
lin come_V = mkV "venir" "vient" "viennent" "venait" "venu" ; --CHECK
lin computer_N = mkN "ordinateur" Masc ;
lin cow_N = mkN "vache" Fem ;
lin dirty_A = mkA "sale" False ;
lin dog_N = mkN "chien" Masc ;
lin drink_V2 = mkV2 (mkV "boire" "boit" "boivent" "buvait" "bu" ) ; -- CHECK
lin eat_V2 = mkV2 "manger" ; --CHECK
lin find_V2 = mkV2 "trouver"; -- DONE
lin fire_N = mkN "feu" Masc ;
lin fish_N = mkN "poisson" Masc ;
lin flower_N = mkN "fleur" Fem ;
lin friend_N = mkN "ami" Masc ;
lin girl_N = mkN "fille" Fem ;
lin good_A = mkA "bon" True ;
lin go_V = mkV "aller" "va" "vont" "allait" "allé"; -- CHECK
lin grammar_N = mkN "grammaire" Fem ;
lin green_A = mkA "vert" False ;
lin heavy_A = mkA "lourd" False ;
lin horse_N = mkN "cheval" Masc ;
lin hot_A = mkA "chaud" False ;
lin house_N = mkN "maison" Fem ;
-- lin john_PN = mkPN "John" ;
lin jump_V = mkV "sauter" ;
lin kill_V2 = mkV2 "tuer" ;
-- lin know_VS = mkVS (mkV "savoir" "knew" "known") ;
lin language_N = mkN "langue" Fem ;
lin live_V = mkV "vivre" "vit" "vivent" "vivait" "vécu";
lin love_V2 = mkV2 "aimer" ;
lin man_N = mkN "homme" Masc ;
lin milk_N = mkN "lait" Masc ;
lin music_N = mkN "musique" Fem ;
lin new_A = mkA "nouveau" True ;
lin now_Adv = mkAdv "maintenant" ;
lin old_A = mkA "vieux" True ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "jouer" ;
lin read_V2 = mkV2 (mkV "lire" "lit" "lisent" "lisait" "lu") ;
lin ready_A = mkA "prêt" False ;
lin red_A = mkA "rouge" False ;
lin river_N = mkN "rivière" Fem ;
lin run_V = mkV "courir" "court" "courent" "courait" "courru" ;
lin sea_N = mkN "mer" Fem ;
lin see_V2 = mkV2 (mkV "voir" "voit" "voient" "voyait" "vu" ) ;
lin ship_N = mkN "bateau" Masc ;
lin sleep_V = mkV "dormir" "dort" "dorment" "dormait" "dormi" ;
lin small_A = mkA "petit" True ;
lin star_N = mkN "étoile" Fem ;
lin swim_V = mkV "nager" ;
lin teach_V2 = mkV2 "enseigner" ;
lin train_N = mkN "train" Masc ;
lin travel_V = mkV "voyager" ;
lin tree_N = mkN "arbre" Masc ;
lin understand_V2 = mkV2 (mkV "comprendre" "comprend" "comprennent" "comprenait" "compris") ;
lin wait_V2 = mkV2 "attendre" ;
lin walk_V = mkV "marcher" ;
lin warm_A = mkA "chaud" False ; -- other translation? because same as hot
lin water_N = mkN "eau" Fem ;
lin white_A = mkA "blanc" False ;
lin wine_N = mkN "vin" Masc ;
lin woman_N = mkN "femme" Fem ;
lin yellow_A = mkA "jaune" False ;
lin young_A = mkA "jeune" True ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN = overload {
    mkN : Str -> Gender -> Noun   -- predictable noun, e.g. car-cars, boy-boys, fly-flies, bush-bushes
      = \n, g -> lin N (smartNoun n g) ;
    mkN : Str -> Str -> Gender -> Noun  -- irregular noun, e.g. man-men
      = \sg,pl, g -> lin N (mkNoun sg pl g) ;
    } ;

  mkA : Str -> Bool -> Adjective   -- predictable noun, e.g. car-cars, boy-boys, fly-flies, bush-bushes
      = \n, pos -> lin A (smartAdj n pos) ;

  mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (smartVerb s) ;
    mkV : (inf,presSg3, presPl3,past,part : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      = \inf,presSg3,presPl3,past,part -> lin V (irregVerb inf presSg3 presPl3 past part) ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (smartVerb s ** {c = []}) ;
    mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = []}) ;
    } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}
