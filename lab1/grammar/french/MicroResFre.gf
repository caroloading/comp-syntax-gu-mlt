resource MicroResFre = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc ;
  Gender = Masc | Fem;


  Agreement = Agr Number ; ---s Person to be added

  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
  AdjForm = AdjF Number Gender;
  VForm = Inf | PresSg3 | Past | PastPart | PresPart ; 

oper
  Noun = {s : Number => Str; g: Gender } ;

  mkNoun : Str -> Str -> Gender -> Noun = \sg, pl, g -> {
    s = table {Sg => sg ; Pl => pl};
    g = g
    } ;

  regNoun : Str -> Gender -> Noun = \sg, g -> mkNoun sg (sg + "s") g ;

  -- smart paradigm
  smartNoun : Str -> Gender -> Noun = \sg, g -> case sg of {
    x + "al" => mkNoun sg (x + "aux") g;
    _ + ("eu" | "eau") => mkNoun sg (sg + "x") g; 
    _ + ("s" | "x") => mkNoun sg sg g;
    ("chou" | "bijou" | "caillou" | "genou" | "pou" | "joujou" | "hibou") => mkNoun sg (sg + "x") g;
    "bail" => mkNoun sg "baux" g ;
    "corail" => mkNoun sg "coraux" g;
    "émail" => mkNoun sg "émaux" g;
    "soupirail" => mkNoun sg "soupiraux" g;
    "travail" => mkNoun sg "travaux" g;
    "vantail" => mkNoun sg "vantaux" g;
    "vitrail" => mkNoun sg "vitraux" g;
    ("chou" | "bijou" | "caillou" | "genou" | "pou" | "joujou" | "hibou") => mkNoun sg (sg + "aux") g;
    _          => regNoun sg g
    };

  Adjective = {s : AdjForm => Str} ;

  mkAdj : Str -> Str -> Str -> Str -> Adjective = \sgmasc, plmasc, sgfem, plfem -> {
    s = table { AdjF Sg Masc => sgmasc; 
                AdjF Pl Masc => plmasc;
                AdjF Sg Fem => sgfem;
                AdjF Pl Fem => plfem
              }
  } ;

  regAdj : Str -> Adjective = \sg -> mkAdj sg (sg + "s") (sg + "e") (sg + "es");

  smartNoun : Str -> Adjective = \sgmasc -> case sgmasc of {
    ai + "gu" => mkAdj sgmasc (sgmasc + "s") (ai + "güe") (ai + "gües" );
    _ + "e" => mkAdj sgmasc (sgmasc + "s") sgmasc (sgmasc + "s");
    veu + "f" => mkAdj sgmasc (sgmasc + "s") (veu + "ve")  (veu + "ves");
    fran + "c" => mkAdj sgmasc (sgmasc + "s") (fran + "che")  (fran + "ches");
    publi + "c" => mkAdj sgmasc (sgmasc + "s") (publi + "que")  (publi + "ques");
    jalou + "x" => mkAdj sgmasc sgmasc (jalou + "se") (jalou + "ses");
    potag + "er" => mkAdj sgmasc (sgmasc + "s") (potag + "ère") (jalou + "ères");
    moqu + "eur" => mkAdj sgmasc (sgmasc + "s") (moqu + "euse") (moqu + "euses") ;-- or "eure"
    salva + "teur" => mkAdj sgmasc (sgmasc + "s") (moqu + "trice") (moqu + "trices");
    b + "eau" => mkAdj sgmasc (sgmasc + "x") (b + "elle") (b + "elles")
    f + "ou" => mkAdj sgmasc (sgmasc + "s") (f + "olle") (f + "olles")
  };

  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,pres,past,pastpart,prespart : Str) -> Verb
    = \inf,pres,past,pastpart,prespart -> {
    s = table {
      Inf => inf ;
      PresSg3 => pres ;
      Past => past ;
      PastPart => pastpart ;
      PresPart => prespart
      }
    } ;

  regVerb : (inf : Str) -> Verb = \inf ->
    mkVerb inf (inf + "s") (inf + "ed") (inf + "ed") (inf + "ing") ;

  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \inf -> case inf of {
     pl  +  ("a"|"e"|"i"|"o"|"u") + "y" => regVerb inf ;
     cr  +  "y" =>  mkVerb inf (cr + "ies") (cr + "ied") (cr + "ied") (inf + "ing") ;
     lov + "e"  => mkVerb inf (inf + "s") (lov + "ed") (lov + "ed") (lov + "ing") ;
     kis + ("s"|"sh"|"x"|"o") => mkVerb inf (inf + "es") (inf + "ed") (inf + "ed") (inf + "ing") ;
     _ => regVerb inf
     } ;

  -- normal irregular verbs e.g. drink,drank,drunk
  irregVerb : (inf,past,pastpart : Str) -> Verb =
    \inf,past,pastpart ->
      let verb = smartVerb inf
      in mkVerb inf (verb.s ! PresSg3) past pastpart (verb.s ! PresPart) ;   

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  be_Verb : Verb = mkVerb "are" "is" "was" "been" "being" ; ---s to be generalized


---s a very simplified verb agreement function for Micro
  agr2vform : Agreement -> VForm = \a -> case a of {
    Agr Sg => PresSg3 ;
    Agr Pl => Inf
    } ;

}
