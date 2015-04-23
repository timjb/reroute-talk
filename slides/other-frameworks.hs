-- Viele Frameworks verwenden einen Applikations-spezifischen Datentyp für Routen.
-- In unserem Beispiel würde man folgenden Datentyp verwenden:

data CurryRoutes =
    RootR
  | RecipeR RecipeId
  | SearchR RecipeCategory Int

-- Dann benötigt man zwei zueinander (halb-)inverse Funktionen
--
-- serialize :: CurryRoutes -> Text
-- parse :: Text -> Maybe CurryRoutes

-- Damit sichergestellt ist, dass sie tatsächlich halbinvers sind,
-- sollten Routen deklarativ festgelegt werden und aus dieser Deklaration
-- ein Parser sowie ein Pretty-printer generiert werden.

-- Möglichkeit 1: Die Bibliothek webroutes-boomerang stellt eine DSL bereit,
-- mit der es möglich ist, "bidirektionale" Parser zu schreiben

-- Möglichkeit 2: Benutze Template Haskell. Z.B. in Yesod:

mkYesod "CurrySite" [parseRoutes|
  / RootR GET
  /recipe/#RecipeId RecipeR GET
  /search/#RecipeCategory/#Int SearchR GET
|]

-- Dieser Code generiert Datatyp, Parser und Pretty-Printer

-- Vorteil: Wenig Code

-- Vorteil: Der Typchecker kann Totalität der Handler-Funktion überprüfen
-- Damit ist garantiert, dass deklarierte Routen auch verarbeitet werden.

-- Nachteil: Template Haskell

-- Nachteil: Man muss eine DSL oder spezielle Syntax lernen
