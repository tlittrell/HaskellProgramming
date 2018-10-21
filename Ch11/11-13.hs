data OperatingSystem = 
      GnuPlusLinux
    | OpenBSD
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
      Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)


data Programmer =
    Programmer {os :: OperatingSystem,
                lang :: ProgLang}
    deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer {os = Mac, lang = Haskell}

-- We can reorder stuff when using record syntax
feelingWizardly :: Programmer
feelingWizardly = Programmer {lang = Agda, os = GnuPlusLinux}

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSD, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = map (\(x,y) -> Programmer {os = x, lang = y}) lang_os_list
    where lang_os_list = [(x,y) | x <- allOperatingSystems, y <- allLanguages]