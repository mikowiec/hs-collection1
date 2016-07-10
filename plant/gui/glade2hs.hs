
module Main where

import Text.XML.HaXml
-- import XmlLib
-- import XmlParse
import Text.PrettyPrint.HughesPJ
-- import Pretty
import System

sub_of lbl = children `o` (keep /> tag lbl)

map_type = oo (\(Just t) -> literal (drop 3 t)) . textlabelled

widgets = mkElem "hs_widget" [txt `o` sub_of "name", map_type (sub_of "class")]
                `o` ((multi (keep /> tag "widget")) `with` ((ifTxt (\t -> if t == "Placeholder" then none else keep) keep)
                `o` children `o` tag "class" `o` children))


hs_record (name,tp) = 
    text name <> text " :: " <> text tp

hs_get (name,_) = 
    text name <> text " <- liftM fromWidget (xmlGetWidget xml \"" <> text name <> text "\")"

hs_ret (name,_) = text name

retr (CElem (Elem _ _ [CString _ name,CString _ tp])) = (name, tp)

main = do
    (infile,outmod) <- fix2Args
    is <- if infile == "-" then getContents else readFile infile
    let wf = if outmod == "-" then putStr else \s -> writeFile (outmod++".hs") s
    let Document _ _ e = xmlParse infile is
    let ws = map retr (widgets (CElem e))
    wf $ render $
        text "module " <> text outmod <> text " where\n" $$
        text "import Gtk" $$ text "import Glade" $$ text "import Monad\n" $$
        text "data Gui = Gui {" $$
        text "    " <> vcat (punctuate (text ",") (map hs_record ws)) $$
        text "}\n" $$
        text "gui :: XML -> IO Gui" $$       
        text "gui xml = do" $$
        text "    " <> (vcat (map hs_get ws) $$
                        text "return (Gui " <>
                        vcat (map hs_ret ws) <> text ")") $$
        text "" $$ text ""



