resource MicroResFre = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Acc | Dat;
  Gender = Masc | Fem;

  Agreement = Agr Number Gender ;
  NForm = NF Number;

  VForm = Inf | PresSg3 | PresPl3 | PastSg3 | PastPl3 | PastPart | PresPart ; 

oper

  Noun = {s : NForm => Str; g: Gender } ;

  nform2number : NForm -> Number = \nf -> case nf of {
    (NF n) => n 
  } ;

  mkNoun : Str -> Str -> Gender -> Noun = \sg, pl, g -> {
    s = table { NF Sg => sg ; NF Pl => pl};
    g = g
    } ;

  regNoun : Str -> Gender -> Noun = \sg, g -> mkNoun sg (sg + "s") g ;

  -- smart paradigm
  smartNoun : Str -> Gender -> Noun = \sg, g -> case sg of {
    _ + ("eu" | "eau") => mkNoun sg (sg + "x") g; 
    anim + "al" => mkNoun sg (anim + "aux") g;
    _          => regNoun sg g

    {- irrelevant for current vocabulary
    _ + ("s" | "x" | "z") => mkNoun sg sg g;
    ("bal" | "carnaval" | "chacal" | "festival" | "récital" | "régal" | "cal" | "serval") => regNoun sg g;
    ("chou" | "bijou" | "caillou" | "genou" | "pou" | "joujou" | "hibou") => mkNoun sg (sg + "x") g;
    "bail" => mkNoun sg "baux" g ;
    "corail" => mkNoun sg "coraux" g;
    "émail" => mkNoun sg "émaux" g;
    "soupirail" => mkNoun sg "soupiraux" g;
    "travail" => mkNoun sg "travaux" g;
    "vantail" => mkNoun sg "vantaux" g;
    "vitrail" => mkNoun sg "vitraux" g;
    _ + "ail" => regNoun sg g; -}
    };

  Adjective = {s : Agreement => Str; isBef: Bool} ; --isBef indicates if the adjective is before the noun (True) or after (False)

  mkAdj : Str -> Str -> Str -> Str -> Bool -> Adjective = \sgmasc, plmasc, sgfem, plfem, pos -> {
    s = table { Agr Sg Masc => sgmasc; 
                Agr Pl Masc => plmasc;
                Agr Sg Fem => sgfem;
                Agr Pl Fem => plfem
              };
    isBef = pos
  } ;

  regAdj : Str -> Str -> Bool -> Adjective = \sgmasc, sgfem, pos -> mkAdj sgmasc (sgmasc + "s") sgfem (sgfem + "s") pos ;

  smartAdj : Str -> Bool -> Adjective = \sgmasc, pos -> case sgmasc of {
    vi + "eux" => mkAdj (pre {"a"|"e"|"i"|"o"|"h"|"é" => vi + "eil" ; _ => sgmasc}) sgmasc (vi + "eille") (vi + "eilles") pos;
    propr + "e" => mkAdj sgmasc (sgmasc + "s") sgmasc (sgmasc + "s") pos;
    b + "eau" => mkAdj (pre {"a"|"e"|"i"|"o"|"h"|"é" => b + "el" ; _ => sgmasc}) (sgmasc + "x") (b + "elle") (b + "elles") pos;
    
    b + "on" => regAdj sgmasc (sgmasc + "ne") pos;
    fran + "c" => regAdj sgmasc (fran + "che") pos;
    _         => regAdj sgmasc (sgmasc + "e") pos

    {- irrelevant for current vocabulary
    ai + "gu" => mkAdj sgmasc (sgmasc + "s") (ai + "güe") (ai + "gües" );
    
    veu + "f" => mkAdj sgmasc (sgmasc + "s") (veu + "ve")  (veu + "ves");
    publi + "c" => mkAdj sgmasc (sgmasc + "s") (publi + "que")  (publi + "ques");
    jalou + "x" => mkAdj sgmasc sgmasc (jalou + "se") (jalou + "ses");
    potag + "er" => mkAdj sgmasc (sgmasc + "s") (potag + "ère") (potag + "ères");
    moqu + "eur" => mkAdj sgmasc (sgmasc + "s") (moqu + "euse") (moqu + "euses") ;-- or "eure"
    salva + "teur" => mkAdj sgmasc (sgmasc + "s") (salva + "trice") (salva + "trices");
    f + "ou" => mkAdj sgmasc (sgmasc + "s") (f + "olle") (f + "olles") -}
  };

  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,presSg,presPl,pastSg,pastPl,pastpart,prespart : Str) -> Verb
    = \inf,presSg,presPl,pastSg,pastPl,pastpart,prespart -> {
    s = table {
      Inf => inf ;
      PresSg3 => presSg ;
      PresPl3 => presPl ;
      PastSg3 => pastSg ;
      PastPl3 => pastPl ;
      PastPart => pastpart ;
      PresPart => prespart
      }
    } ;

  regVerb : (inf : Str) -> Verb = \inf -> case inf of {
    appel + "er" => mkVerb inf (appel + "e") (appel + "ent") (appel + "ait") (appel + "aient") (appel + "é") (appel + "ant") 
  };


  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \inf -> case inf of {
    fin + "ir" => mkVerb inf (fin + "it") (fin + "issent") (fin + "issait") (fin + "issaient") (fin + "i") (fin + "issant") ;
    mord + "re" => mkVerb inf mord (mord + "ent") (mord + "ait") (mord + "aient") (mord + "u") (mord + "ant");
    man + "ger" => mkVerb inf (man + "ge") (man + "gent") (man + "geait") (man + "geaient") (man + "gé") (man + "geant");
    ach + "eter" => mkVerb inf (ach + "ète") (ach + "ètent") (ach + "etait") (ach + "etaient") (ach + "eté") (ach + "etant");
    _ => regVerb inf
  } ;

  -- normal irregular verbs e.g. drink,drank,drunk
  irregVerb : (inf,presSg3, presPl3,past,pastpart : Str) -> Verb =
    \inf,presSg3, presPl3, past,pastpart -> case past of {
      buv + "ait" => mkVerb inf presSg3 presPl3 past (buv + "aient") pastpart (buv + "ant")
    } ;   

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  be_Verb : Verb = mkVerb "être" "est" "sont" "était" "étaient" "été" "étant" ; ---s to be generalized


---s a very simplified verb agreement function for Micro
  agr2vform : Agreement -> VForm = \a -> case a of {
    Agr Sg Fem => PresSg3 ;
    Agr Sg Masc => PresSg3 ;
    Agr Pl Fem => PresPl3 ;
    Agr Pl Masc => PresPl3
    } ;

}